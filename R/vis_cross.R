#' Visualize cross-omics of one gene among pan-cancers
#'
#' @param gene  a gene symbol identifier (e.g., "TP53")
#' @param tumor_samples Select specific tumor samples. Default NULL, indicating all tumor samples.
#' @param tumor_projects Select specific TCGA projects. Default NULL, indicating all TCGA projects.
#' @param n_trans The number of sampling transcripts or specific transcript identifiers.
#' @param n_methy The number of sampling CpG sites or specific CpG identifiers.
#' @param seed The seed of sampling.
#' @param add_mean_trans Add overall column to display the mean values of all gene's transcripts.
#' @param add_mean_methy Add overall column to display the mean values of all gene's cpg sites.
#' @param pval_mrna The P value thresholds
#' @param return_list TRUE returns a list including plot object and data. FALSE just returns plot.
#'
#' @return funkyheatmap
#' @export
#'
vis_gene_cross_omics <- function(gene = "TP53",
                                 tumor_projects = NULL,
                                 tumor_samples = NULL,
                                 n_trans = 5, n_methy = 5, seed = 42,
                                 add_mean_trans = TRUE, add_mean_methy = TRUE, 
                                 pval_mrna = c(0.05, 0.01, 0.001),
                                 return_list = FALSE) {
  # # If input is gene signature, cannot stat and plot transcript&methylation data
  # is_signature <- grepl(" ", molecule)

  tcga_gtex <- load_data("tcga_gtex")
  tcga_clinical_fine <- load_data("tcga_clinical_fine")
  if (is.null(tumor_projects)) {
    tumor_projects <- as.character(sort(unique(tcga_gtex$tissue)))
  } else {
    tumor_projects <- tumor_projects[tumor_projects %in% unique(tcga_gtex$tissue)]
  }

  if (is.null(tumor_samples)) {
    tcga_gtex2 <- tcga_gtex
    tcga_clinical_fine2 <- tcga_clinical_fine
  } else {
    tcga_gtex2 <- tcga_gtex %>%
      dplyr::filter(.data$type2 == "normal" | .data$sample %in% tumor_samples)
    tcga_clinical_fine2 <- tcga_clinical_fine %>%
      dplyr::filter(.data$Code == "NT" | .data$Sample %in% tumor_samples)
  }

  tcga_gtex2 <- tcga_gtex2 %>% dplyr::filter(.data$tissue %in% tumor_projects)
  tcga_clinical_fine2 <- tcga_clinical_fine2 %>% dplyr::filter(.data$Cancer %in% tumor_projects)

  #### Omics--mRNA
  gene_mrna <- query_pancan_value(gene, "mRNA") %>%
    # .[["expression"]] %>%
    .[[1]] %>% # For single gene, the first name is 'expression'; For gene signature, the first name is 'value'
    as.data.frame() %>%
    dplyr::rename("Exp" = ".") %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_gtex2)
  if (all(is.na(gene_mrna$Exp))) stop("Please input a valid gne!")

  # median value, normal samples include GTEx
  gene_mrna_md <- gene_mrna %>%
    dplyr::group_by(.data$tissue, .data$type2) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    dplyr::mutate(Exp = (.data$Exp - min(.data$Exp)) / (max(.data$Exp) - min(.data$Exp))) %>% # 0~1 scale
    tidyr::pivot_wider(id_cols = .data$tissue, names_from = .data$type2, values_from = .data$Exp)

  gene_mrna_comp <- ggpubr::compare_means(Exp ~ type2,
    data = gene_mrna,
    group.by = "tissue", method = "wilcox.test"
  )

  gene_mrna_plot <- dplyr::left_join(gene_mrna_md, gene_mrna_comp[, c("tissue", "p", "p.adj")]) %>%
    as.data.frame() %>%
    dplyr::rename("id" = "tissue") %>%
    dplyr::mutate(Label = case_when(
      p < pval_mrna[3] ~ "***",
      p < pval_mrna[2] ~ "**",
      p < pval_mrna[1] ~ "*",
      TRUE ~ "-"
    )) %>%
    dplyr::select("id", "normal", "tumor", "Label")

  #### Omics--mutation
  gene_mut <- query_pancan_value(gene, "mutation") %>%
    as.data.frame() %>%
    dplyr::rename("Mut" = ".") %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_clinical_fine2, by = c("sample" = "Sample"))

  gene_mut_plot <- gene_mut %>%
    dplyr::group_by(.data$Cancer) %>%
    dplyr::summarise(Mut = mean(.data$Mut)) %>%
    as.data.frame() %>%
    dplyr::mutate(Ratio = NA)

  for (i in seq(nrow(gene_mut_plot))) {
    # i = 1
    gene_mut_plot$Ratio[i] <- list(c(Mut = gene_mut_plot$Mut[i], Wild = 1 - gene_mut_plot$Mut[i]))
  }

  gene_mut_plot <- gene_mut_plot %>%
    dplyr::mutate(Mut_str = scales::percent(.data$Mut, accuracy = 0.1)) %>%
    dplyr::select("Cancer", "Ratio", "Mut", "Mut_str")

  #### Omics--CNV
  opt_pancan <- .opt_pancan
  opt_pancan$toil_cnv$use_thresholded_data <- T
  gene_cnv <- query_pancan_value(
    gene, "cnv",
    opt_pancan = opt_pancan
  ) %>%
    as.data.frame() %>%
    # dplyr::select("data") %>%
    # dplyr::rename("CNV" = "data") %>%
    dplyr::select(1) %>% 
    dplyr::rename(CNV = 1) %>% 
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_clinical_fine2, by = c("sample" = "Sample"))

  gene_cnv_tb <- table(gene_cnv$Cancer, gene_cnv$CNV)
  gene_cnv_plot <- gene_cnv %>%
    dplyr::count(.data$Cancer, name = "CNV_total") %>%
    dplyr::mutate(CNV_Pie = NA, CNV_Del = NA, CNV_Amp = NA)

  for (i in seq(nrow(gene_cnv_plot))) {
    # i = 1
    Cancer <- gene_cnv_plot$Cancer[i]
    gene_cnv_plot$CNV_Pie[i] <- list(c(
      " -2" = gene_cnv_tb[Cancer, "-2"],
      " -1" = gene_cnv_tb[Cancer, "-1"],
      " 0" = gene_cnv_tb[Cancer, "0"],
      " 1" = gene_cnv_tb[Cancer, "1"],
      " 2" = gene_cnv_tb[Cancer, "2"]
    ))
    gene_cnv_plot$CNV_Del[i] <- gene_cnv_tb[Cancer, "-2"] + gene_cnv_tb[Cancer, "-1"]
    gene_cnv_plot$CNV_Amp[i] <- gene_cnv_tb[Cancer, "2"] + gene_cnv_tb[Cancer, "1"]
  }

  gene_cnv_plot <- gene_cnv_plot %>%
    dplyr::mutate(CNV_Amp = .data$CNV_Amp / .data$CNV_total) %>%
    dplyr::mutate(CNV_Amp_str = scales::percent(.data$CNV_Amp, accuracy = 0.1)) %>%
    dplyr::mutate(CNV_Del = .data$CNV_Del / .data$CNV_total) %>%
    dplyr::mutate(CNV_Del_str = scales::percent(.data$CNV_Del, accuracy = 0.1)) %>%
    dplyr::select("Cancer", "CNV_Pie", "CNV_Amp", "CNV_Amp_str", "CNV_Del", "CNV_Del_str")

  #### Omics--Transcript
  Trans_sub <- load_data("v2_tpc_id_help")$tcga$id_trans %>%
    dplyr::filter(.data$Symbol == gene) %>%
    dplyr::pull(.data$Level3)
  set.seed(seed)
  if (inherits(n_trans, "character")) {
    if (length(n_trans) > 15) stop("Less than 15 transcripts are supported!")
    trans_sle <- n_trans[n_trans %in% Trans_sub]
  } else if (inherits(n_trans, "numeric")) {
    if (n_trans > 15) stop("Less than 15 transcripts are supported!")
    trans_sle <- unique(sample(Trans_sub, n_trans, replace = n_trans > length(Trans_sub)))
  } else if (is.null(n_trans)) {
    trans_sle = NULL
  }

  if (length(trans_sle) == 0 & !add_mean_trans) {
    gene_trans_n <- 0
  } else {
    if (add_mean_trans) {trans_sle = c(trans_sle, gene)}
    gene_trans <- lapply(seq(trans_sle), function(i) {
      # i = 1
      gene_trans_tmp <- query_pancan_value(
        trans_sle[i], "transcript"
      ) %>%
        as.data.frame() %>%
        dplyr::select("expression") %>%
        dplyr::rename("Trans" = "expression") %>%
        tibble::rownames_to_column("sample") %>%
        dplyr::inner_join(tcga_gtex2) %>%
        dplyr::filter(.data$type2 == "tumor") %>%
        dplyr::group_by(.data$tissue) %>%
        dplyr::summarise(Trans = median(.data$Trans))
      colnames(gene_trans_tmp)[2] <- trans_sle[i]
      gene_trans_tmp %>% tibble::column_to_rownames("tissue")
    }) %>% do.call(cbind, .)
    gene_trans_plot <- gene_trans[, apply(gene_trans, 2, stats::sd) != 0, drop = FALSE] %>%
      as.data.frame() %>%
      tibble::rownames_to_column("tissue")
    if (gene %in% colnames(gene_trans_plot)){
      colnames(gene_trans_plot)[which(colnames(gene_trans_plot) %in% gene)] = "ENST_Overall"
    }
    gene_trans_n <- ncol(gene_trans_plot) - 1
    
  }

  #### Omics--Methylation
  Methy450_sub <- load_data("v2_tpc_id_help")$tcga$id_M450 %>%
    dplyr::filter(.data$Level3 == gene) %>%
    dplyr::pull(.data$CpG)
  set.seed(seed)
  if (inherits(n_methy, "character")) {
    if (length(n_methy) > 15) stop("Less than 15 CpG sites are supported!")
    cpg_sites <- n_methy[n_methy %in% Methy450_sub]
  } else if (inherits(n_methy, "numeric")) {
    if (n_methy > 15) stop("Less than 15 CpG sites are supported!")
    cpg_sites <- unique(sample(Methy450_sub, n_methy, replace = n_methy > length(Methy450_sub)))
  } else if (is.null(n_trans)) {
    cpg_sites = NULL
  }
  if (length(cpg_sites) == 0 & !add_mean_methy) {
    gene_methy_n <- 0
  } else {
    if (add_mean_methy) {cpg_sites = c(cpg_sites, gene)}
    gene_methy_cpgs <- lapply(seq(cpg_sites), function(i) {
      # i = 1
      print(i)
      opt_pancan <- .opt_pancan
      if (cpg_sites[i] == gene){
        # Gene level
        opt_pancan$toil_methylation$aggr <- "NA"
        opt_pancan$toil_methylation$rule_out <- NULL
      } else {
        # CpG site level
        opt_pancan$toil_methylation$aggr <- "mean"
        opt_pancan$toil_methylation$rule_out <- setdiff(Methy450_sub, cpg_sites[i])
      }
      gene_methy_tmp <- query_pancan_value(
        gene, "methylation",
        opt_pancan = opt_pancan
      ) %>%
        as.data.frame() %>%
        dplyr::select("data") %>%
        dplyr::rename("Methy" = "data") %>%
        tibble::rownames_to_column("sample") %>%
        # dplyr::inner_join(tcga_gtex2) %>%
        dplyr::inner_join(tcga_clinical_fine2, by = c("sample" = "Sample")) %>%
        dplyr::filter(.data$Code != "NT") %>%
        dplyr::group_by(.data$Cancer) %>%
        dplyr::summarise(Methy = median(.data$Methy))
      colnames(gene_methy_tmp)[2] <- cpg_sites[i]
      gene_methy_tmp %>% tibble::column_to_rownames("Cancer")
    }) %>%
      do.call(cbind, .) %>%
      tibble::rownames_to_column("Cancer")
    gene_methy_plot <- gene_methy_cpgs
    if (gene %in% colnames(gene_methy_plot)){
      colnames(gene_methy_plot)[which(colnames(gene_methy_plot) %in% gene)] = "cg_Overall"
    }
    gene_methy_n <- ncol(gene_methy_plot) - 1
  }

  #### Ready for Plot
  gene_cross_dat <- data.frame(id = tumor_projects) %>%
    dplyr::left_join(gene_mrna_plot) %>%
    dplyr::left_join(gene_mut_plot, by = c("id" = "Cancer")) %>%
    dplyr::left_join(gene_cnv_plot, by = c("id" = "Cancer"))

  idx_1 <- which(sapply(gene_cross_dat$Ratio, is.null))
  for (i in idx_1) {
    gene_cross_dat$Ratio[[i]] <- c(Mut = NA, Wild = NA)
  }
  idx_2 <- which(sapply(gene_cross_dat$CNV_Pie, is.null))
  for (i in idx_2) {
    gene_cross_dat$CNV_Pie[[i]] <- c(` -2` = NA, ` -1` = NA, ` 0` = NA, ` 1` = NA, ` 2` = NA)
  }

  if (gene_trans_n > 0) {
    gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_trans_plot, by = c("id" = "tissue"))
  }
  if (gene_methy_n > 0) {
    gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_methy_plot, by = c("id" = "Cancer"))
  }

  label_name <- colnames(gene_cross_dat)
  label_name[1:12] <- c(
    "", "Normal Exp", "Tumor Exp", "T vs. N(Wilcox)",
    "Mutation Dist", "Mutation PCT", "",
    "CNV Dist", "CNV Amp", "", "CNV Del", ""
  )
  column_info <- data.frame(
    # columns to vis
    id = colnames(gene_cross_dat),
    # column name
    name = label_name,
    # group name
    group = c(
      "TCGA", rep("Expression Profile", 3), rep("Mutation Profile", 3), rep("CNV Profile", 5),
      rep("Transcript Profile", gene_trans_n), rep("Methylation Profile", gene_methy_n)
    ),
    # geom type
    geom = c(
      "text", "bar", "bar", "text", "pie", "bar", "text",
      "pie", "bar", "text", "bar", "text",
      rep("funkyrect", gene_trans_n), rep("funkyrect", gene_methy_n)
    ),
    # palette type
    palette = c(
      NA, "Tumor-Exp", "Tumor-Exp", NA, "Mut Status", "Mut-Ratio", NA,
      "CNV Status", "CNV-Amp", NA, "CNV-Del", NA,
      rep("Scale.Transcript.Level", gene_trans_n), rep("Scale.Methylation.Level", gene_methy_n)
    ),
    # column width
    width = c(
      5, 4, 4, 3, 2, 4, 4,
      2, 4, 4, 4, 4, rep(1.1, gene_trans_n), rep(1.1, gene_methy_n)
    )
  )

  column_info$options <- NA
  for (i in seq(nrow(column_info))) {
    column_info$options[i] <- list(list())
  }
  column_info$options[[1]] <- list()
  column_info$options[[2]] <- list(scale = F, hjust = 1)
  column_info$options[[3]] <- list(scale = F)
  column_info$options[[6]] <- list(scale = F)
  column_info$options[[7]] <- list(label = "Mut_str", overlay = T)
  column_info$options[[9]] <- list(scale = F)
  column_info$options[[10]] <- list(label = "CNV_Amp_str", overlay = T)
  column_info$options[[11]] <- list(scale = F)
  column_info$options[[12]] <- list(label = "CNV_Del_str", overlay = T)

  column_groups <- data.frame(
    level1 = c("TCGA", rep(paste("Gene:", gene), 5)),
    level2 = c("", "Expression Profile", "Mutation Profile", "CNV Profile", "Transcript Profile", "Methylation Profile"),
    group = c("TCGA", "Expression Profile", "Mutation Profile", "CNV Profile", "Transcript Profile", "Methylation Profile"),
    palette = "palette1"
  )

  palettes <- list(
    palette1 = c("#66c2a5", "#66c2a5"),
    `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6],
    `Mut-Ratio` = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    `Mut Status` = c(Mut = "red", Wild = "grey"),
    `CNV Status` = c(" -2" = "#5e3c99", " -1" = "#b2abd2", " 0" = "#f7f7f7", " 1" = "#fdb863", " 2" = "#e66101"),
    `CNV-Amp` = RColorBrewer::brewer.pal(n = 9, name = "Oranges"),
    `CNV-Del` = RColorBrewer::brewer.pal(n = 9, name = "Purples"),
    Scale.Methylation.Level = RColorBrewer::brewer.pal(n = 9, name = "BuPu"),
    Scale.Transcript.Level = RColorBrewer::brewer.pal(n = 9, name = "Greens")
  )


  position_arguments <- funkyheatmap::position_arguments(
    expand_xmax = 6,
    col_annot_angle = 35,
    col_width = 1.1,
    col_space = 0.2,
    col_annot_offset = 4
  )

  g <- funkyheatmap::funky_heatmap(gene_cross_dat,
    column_info = column_info,
    column_groups = column_groups,
    # row_info = row_info,
    palettes = palettes,
    position_args = position_arguments
  )


  if (return_list) {
    return(list(plot = g, data = gene_cross_dat))
  } else {
    return(g)
  }
}



#' Visualize cross-omics of one pathway among pan-cancers
#'
#' @param pw  pathway name
#' @param tumor_samples Select specific tumor samples. Default NULL, indicating all tumor samples.
#' @param tumor_projects Select specific TCGA projects. Default NULL, indicating all TCGA projects.
#' @param pval_mrna The P value thresholds
#' @param return_list TRUE returns a list including plot object and data. FALSE just returns plot.
#'
#' @return funkyheatmap
#' @export
#'
vis_pathway_cross_omics <- function(pw = "HALLMARK_ADIPOGENESIS",
                                    tumor_projects = NULL,
                                    tumor_samples = NULL,
                                    pval_mrna = c(0.05, 0.01, 0.001),
                                    return_list = FALSE) {
  tcga_gtex <- load_data("tcga_gtex")
  tcga_clinical_fine <- load_data("tcga_clinical_fine")
  if (is.null(tumor_projects)) {
    tumor_projects <- as.character(sort(unique(tcga_gtex$tissue)))
  } else {
    tumor_projects <- tumor_projects[tumor_projects %in% unique(tcga_gtex$tissue)]
  }

  if (is.null(tumor_samples)) {
    tcga_gtex2 <- tcga_gtex
    tcga_clinical_fine2 <- tcga_clinical_fine
  } else {
    tcga_gtex2 <- tcga_gtex %>%
      dplyr::filter(.data$type2 == "normal" | .data$sample %in% tumor_samples)
    tcga_clinical_fine2 <- tcga_clinical_fine %>%
      dplyr::filter(.data$Code == "NT" | .data$Sample %in% tumor_samples)
  }

  tcga_gtex2 <- tcga_gtex2 %>% dplyr::filter(.data$tissue %in% tumor_projects)
  tcga_clinical_fine2 <- tcga_clinical_fine2 %>% dplyr::filter(.data$Cancer %in% tumor_projects)

  #### Omics--mRNA
  toil_sig_score <- rbind(load_data("tcga_PW"), load_data("gtex_PW"))
  pw_mrna <- toil_sig_score[, pw, drop = FALSE] %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_gtex2)
  colnames(pw_mrna)[2] <- "Exp"

  pw_mrna_md <- pw_mrna %>%
    dplyr::group_by(.data$tissue, .data$type2) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    dplyr::mutate(Exp = (.data$Exp - min(.data$Exp)) / (max(.data$Exp) - min(.data$Exp))) %>% # 0~1 scale
    tidyr::pivot_wider(id_cols = .data$tissue, names_from = .data$type2, values_from = .data$Exp)

  pw_mrna_comp <- ggpubr::compare_means(Exp ~ type2,
    data = pw_mrna,
    group.by = "tissue", method = "wilcox.test"
  )

  pw_mrna_plot <- dplyr::left_join(pw_mrna_md, pw_mrna_comp[, c("tissue", "p", "p.adj")]) %>%
    as.data.frame() %>%
    dplyr::rename("id" = "tissue") %>%
    dplyr::mutate(Label = case_when(
      p < pval_mrna[3] ~ "***",
      p < pval_mrna[2] ~ "**",
      p < pval_mrna[1] ~ "*",
      TRUE ~ "-"
    )) %>%
    dplyr::select("id", "normal", "tumor", "Label")

  #### Omics--mutation
  # pw_mut_cnv = load_data("tcga_PW_cross")
  pw_mut_cnv <- calc_pw_mut_cnv(tumor_samples = tumor_samples)

  pw_mut_stat <- dplyr::filter(pw_mut_cnv, Type == "Mut") %>%
    dplyr::filter(.data$PW == pw)

  pw_mut_plot <- pw_mut_stat %>%
    dplyr::select(-"Type", -"PW") %>%
    dplyr::rename("Mut_ratio" = "Ratio", "Mut_me" = "Count") %>%
    dplyr::mutate(Mut_ratio_str = scales::percent(.data$Mut_ratio, accuracy = 0.1), .before = 3) %>%
    dplyr::mutate(Mut_me_str = round(.data$Mut_me, digits = 2), .before = 5) %>%
    dplyr::mutate(Mut_pie = NA, .before = 2)

  for (i in seq(nrow(pw_mut_plot))) {
    # i = 1
    pw_mut_plot$Mut_pie[i] <- list(c(Mut = pw_mut_plot$Mut_ratio[i], Wild = 1 - pw_mut_plot$Mut_ratio[i]))
  }

  #### Omics--CNV
  pw_cnv_stat <- dplyr::filter(pw_mut_cnv, .data$Type != "Mut") %>%
    dplyr::filter(.data$PW == pw)

  pw_cnv_plot <- pw_cnv_stat %>%
    dplyr::select(-"PW", -"Ratio") %>%
    tidyr::pivot_wider(names_from = .data$Type, values_from = .data$Count) %>%
    dplyr::mutate(CNV_Pie = NA)

  for (i in seq(nrow(pw_cnv_plot))) {
    # i = 1
    pw_cnv_plot$CNV_Pie[i] <- list(c(
      "Amp" = pw_cnv_plot$Amp[i],
      "Del" = pw_cnv_plot$Del[i],
      "Non" = pw_cnv_plot$Non[i]
    ))
  }

  pw_cnv_plot <- pw_cnv_plot %>%
    dplyr::mutate(
      CNV_Amp = .data$Amp,
      CNV_Amp_str = round(.data$Amp, 1),
      CNV_Del = .data$Del,
      CNV_Del_str = round(.data$Del, 1)
    ) %>%
    dplyr::select(-"Non", -"Amp", -"Del")


  #### Ready for Plot
  pw_cross_dat <- data.frame(id = tumor_projects) %>%
    dplyr::left_join(pw_mrna_plot) %>%
    dplyr::left_join(pw_mut_plot, by = c("id" = "Cancer")) %>%
    dplyr::left_join(pw_cnv_plot, by = c("id" = "Cancer"))

  idx_1 <- which(sapply(pw_cross_dat$Mut_pie, is.null))
  for (i in idx_1) {
    pw_cross_dat$Mut_pie[[i]] <- c(Mut = NA, Wild = NA)
  }
  idx_2 <- which(sapply(pw_cross_dat$CNV_Pie, is.null))
  for (i in idx_2) {
    pw_cross_dat$CNV_Pie[[i]] <- c(` -2` = NA, ` -1` = NA, ` 0` = NA, ` 1` = NA, ` 2` = NA)
  }

  column_info <- data.frame(
    # columns to vis
    id = colnames(pw_cross_dat),
    # column name
    name = c(
      "", "Normal ssGSEA", "Tumor ssGSEA", "T vs. N(Wilcox)",
      "Mutation Dist Mean", "Mutation PCT Mean", "", "Mut.Genes Mean", "",
      "CNV Dist Mean", "Amp.Genes Mean", "", "Del.Genes Mean", ""
    ),
    # group name
    group = c(
      "TCGA", rep("Expression Profile", 3), rep("Mutation Profile", 5),
      rep("CNV Profile", 5)
    ),
    # geom type
    geom = c(
      "text", "bar", "bar", "text", "pie", "bar", "text", "bar", "text", "pie",
      "bar", "text", "bar", "text"
    ),
    # palette type
    palette = c(
      NA, "Tumor-Exp", "Tumor-Exp", NA, "Mut Status", "Mut-Ratio", NA, "Mut-Count", NA, "CNV Status",
      "CNV-Amp", NA, "CNV-Del", NA
    ), # "CNV-Pie"
    # column width
    width = c(5, 4, 4, 4, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4)
  )

  column_info$options <- NA
  for (i in seq(nrow(column_info))) {
    column_info$options[i] <- list(list())
  }
  column_info$options[[2]] <- list(scale = F, hjust = 1)
  column_info$options[[3]] <- list(scale = F)
  column_info$options[[6]] <- list(scale = F)
  column_info$options[[7]] <- list(label = "Mut_ratio_str", overlay = T)
  column_info$options[[9]] <- list(label = "Mut_me_str", overlay = T)
  column_info$options[[12]] <- list(label = "CNV_Amp_str", overlay = T)
  column_info$options[[14]] <- list(label = "CNV_Del_str", overlay = T)

  column_groups <- data.frame(
    level1 = c("TCGA", rep(paste("Pathway: ", pw), 3)),
    level2 = c("", "Expression Profile", "Mutation Profile", "CNV Profile"),
    group = c("TCGA", "Expression Profile", "Mutation Profile", "CNV Profile"),
    palette = c("palette1")
  )
  palettes <- list(
    palette1 = c("#66c2a5", "#66c2a5"),
    `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6],
    `Mut Status` = c(Mut = "red", Wild = "grey"),
    `Mut-Ratio` = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    `Mut-mean` = RColorBrewer::brewer.pal(n = 9, name = "RdPu"),
    `CNV Status` = c(Amp = "#d53e4f", Del = "#3288bd", Non = "grey"),
    `CNV-Amp` = c("#fee08b", "#fdae61", "#f46d43", "#d53e4f"),
    `CNV-Del` = c("#e6f598", "#abdda4", "#66c2a5", "#3288bd")
  )

  position_arguments <- funkyheatmap::position_arguments(
    expand_xmax = 6,
    col_annot_angle = 35,
    col_width = 1.1,
    col_space = 0.2,
    col_annot_offset = 4
  )
  g <- funkyheatmap::funky_heatmap(pw_cross_dat,
    column_info = column_info,
    column_groups = column_groups,
    palettes = palettes,
    position_args = position_arguments
  )
  if (return_list) {
    return(list(plot = g, data = pw_cross_dat))
  } else {
    return(g)
  }
}





#' Prepare Mut/CNV data of pathways given specific samples
#'
#' @param tumor_samples Select specific tumor samples. Default NULL, indicating all tumor samples.
#'
#' @export
#'
calc_pw_mut_cnv <- function(tumor_samples = NULL) {
  tcga_clinical_fine <- load_data("tcga_clinical_fine")
  if (is.null(tumor_samples)) {
    return(load_data("tcga_PW_cross"))
  }
  tcga_clinical_fine2 <- tcga_clinical_fine %>%
    dplyr::filter(.data$Code == "NT" | .data$Sample %in% tumor_samples)

  tcga_PW_cross_raw <- load_data("tcga_PW_cross_raw")


  pw_mut_stat <- tcga_PW_cross_raw$pw_mut_one[tcga_clinical_fine2$Sample, ] %>%
    na.omit() %>%
    tibble::rownames_to_column("Sample") %>%
    tidyr::pivot_longer(!.data$Sample, names_to = "PW") %>%
    dplyr::inner_join(tcga_clinical_fine2[, 1:2]) %>%
    dplyr::group_by(.data$Cancer, .data$PW) %>%
    dplyr::summarise(Ratio = mean(.data$value > 0), Count = mean(.data$value)) %>%
    dplyr::ungroup()

  pw_cnv_posi_stat <- tcga_PW_cross_raw$pw_cnv_posi %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(tcga_clinical_fine2[, 1:2], .) %>%
    dplyr::group_by(.data$Cancer) %>%
    dplyr::summarise(across(!.data$Sample, mean)) %>%
    as.data.frame()
  pw_cnv_nega_stat <- tcga_PW_cross_raw$pw_cnv_nega %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(tcga_clinical_fine2[, 1:2], .) %>%
    dplyr::group_by(.data$Cancer) %>%
    dplyr::summarise(across(!.data$Sample, mean)) %>%
    as.data.frame()
  pw_cnv_zero_stat <- tcga_PW_cross_raw$pw_cnv_zero %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(tcga_clinical_fine2[, 1:2], .) %>%
    dplyr::group_by(.data$Cancer) %>%
    dplyr::summarise(across(!.data$Sample, mean)) %>%
    as.data.frame()

  pw_cnv_stat <- rbind(
    pw_cnv_posi_stat %>%
      dplyr::mutate(Type = "Amp", .before = 2),
    pw_cnv_nega_stat %>%
      dplyr::mutate(Type = "Del", .before = 2),
    pw_cnv_zero_stat %>%
      dplyr::mutate(Type = "Non", .before = 2)
  )
  pw_cnv_stat <- pw_cnv_stat %>%
    tidyr::pivot_longer(contains("_"), names_to = c("PW"), values_to = "Count") %>%
    dplyr::group_by(.data$Cancer, .data$PW) %>%
    dplyr::mutate(Ratio = .data$Count / sum(.data$Count))

  pw_mut_cnv <- rbind(
    pw_mut_stat %>% dplyr::mutate(Type = "Mut", .before = 2),
    pw_cnv_stat
  ) %>% dplyr::mutate(PW = as.character(.data$PW))
  return(pw_mut_cnv)
}
