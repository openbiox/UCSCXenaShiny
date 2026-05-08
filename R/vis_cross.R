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
    .[[1]] %>%
    as.data.frame() %>%
    dplyr::rename("Exp" = ".") %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_gtex2)
  if (all(is.na(gene_mrna$Exp))) stop("Please input a valid gene!")

  gene_mrna_md <- gene_mrna %>%
    dplyr::group_by(.data$tissue, .data$type2) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    dplyr::mutate(Exp = (.data$Exp - min(.data$Exp)) / (max(.data$Exp) - min(.data$Exp))) %>%
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

  for (i in seq_len(nrow(gene_mut_plot))) {
    gene_mut_plot$Ratio[i] <- list(c(Mut = as.numeric(gene_mut_plot$Mut[i]), Wild = as.numeric(1 - gene_mut_plot$Mut[i])))
  }

  gene_mut_plot <- gene_mut_plot %>%
    dplyr::mutate(Mut_str = scales::percent(.data$Mut, accuracy = 0.1)) %>%
    dplyr::select("Cancer", "Ratio", "Mut", "Mut_str")

  #### Omics--CNV
  opt_pancan <- .opt_pancan
  opt_pancan$toil_cnv$use_thresholded_data <- T
  gene_cnv <- query_pancan_value(gene, "cnv", opt_pancan = opt_pancan) %>%
    as.data.frame() %>%
    dplyr::select(1) %>%
    dplyr::rename(CNV = 1) %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::inner_join(tcga_clinical_fine2, by = c("sample" = "Sample"))

  gene_cnv_tb <- table(gene_cnv$Cancer, gene_cnv$CNV)
  gene_cnv_plot <- gene_cnv %>%
    dplyr::count(.data$Cancer, name = "CNV_total") %>%
    dplyr::mutate(CNV_Pie = NA, CNV_Del = NA, CNV_Amp = NA)

  for (i in seq_len(nrow(gene_cnv_plot))) {
    Cancer <- gene_cnv_plot$Cancer[i]
    gene_cnv_plot$CNV_Pie[i] <- list(c(
      " -2" = as.numeric(gene_cnv_tb[Cancer, "-2"]),
      " -1" = as.numeric(gene_cnv_tb[Cancer, "-1"]),
      " 0" = as.numeric(gene_cnv_tb[Cancer, "0"]),
      " 1" = as.numeric(gene_cnv_tb[Cancer, "1"]),
      " 2" = as.numeric(gene_cnv_tb[Cancer, "2"])
    ))
    gene_cnv_plot$CNV_Del[i] <- (gene_cnv_tb[Cancer, "-2"] + gene_cnv_tb[Cancer, "-1"]) / gene_cnv_plot$CNV_total[i]
    gene_cnv_plot$CNV_Amp[i] <- (gene_cnv_tb[Cancer, "2"] + gene_cnv_tb[Cancer, "1"]) / gene_cnv_plot$CNV_total[i]
  }

  gene_cnv_plot <- gene_cnv_plot %>%
    dplyr::mutate(CNV_Amp_str = scales::percent(.data$CNV_Amp, accuracy = 0.1)) %>%
    dplyr::mutate(CNV_Del_str = scales::percent(.data$CNV_Del, accuracy = 0.1)) %>%
    dplyr::select("Cancer", "CNV_Pie", "CNV_Amp", "CNV_Amp_str", "CNV_Del", "CNV_Del_str")

  #### Omics--Transcript
  Trans_sub <- load_data("v2_tpc_id_help")$tcga$id_trans %>%
    dplyr::filter(.data$Symbol == gene) %>%
    dplyr::pull(.data$Level3)
  set.seed(seed)
  if (inherits(n_trans, "character")) {
    trans_sle <- n_trans[n_trans %in% Trans_sub]
  } else if (inherits(n_trans, "numeric")) {
    trans_sle <- unique(sample(Trans_sub, min(n_trans, length(Trans_sub))))
  } else {
    trans_sle <- NULL
  }

  if (length(trans_sle) == 0 & !add_mean_trans) {
    gene_trans_n <- 0
  } else {
    if (add_mean_trans) {
      trans_sle <- c(trans_sle, gene)
    }
    gene_trans <- lapply(seq_along(trans_sle), function(i) {
      gene_trans_tmp <- query_pancan_value(trans_sle[i], "transcript") %>%
        as.data.frame() %>%
        dplyr::select(1) %>%
        dplyr::rename("Trans" = 1) %>%
        tibble::rownames_to_column("sample") %>%
        dplyr::inner_join(tcga_gtex2) %>%
        dplyr::filter(.data$type2 == "tumor") %>%
        dplyr::group_by(.data$tissue) %>%
        dplyr::summarise(Trans = median(.data$Trans, na.rm = T))
      colnames(gene_trans_tmp)[2] <- trans_sle[i]
      gene_trans_tmp %>% tibble::column_to_rownames("tissue")
    }) %>% do.call(cbind, .)
    gene_trans_plot <- gene_trans %>%
      as.data.frame() %>%
      tibble::rownames_to_column("tissue")
    if (gene %in% colnames(gene_trans_plot)) colnames(gene_trans_plot)[which(colnames(gene_trans_plot) %in% gene)] <- "ENST_Overall"
    gene_trans_n <- ncol(gene_trans_plot) - 1
  }

  #### Omics--Methylation
  Methy450_sub <- load_data("v2_tpc_id_help")$tcga$id_M450 %>%
    dplyr::filter(.data$Level3 == gene) %>%
    dplyr::pull(.data$CpG)
  set.seed(seed)
  if (inherits(n_methy, "character")) {
    cpg_sites <- n_methy[n_methy %in% Methy450_sub]
  } else if (inherits(n_methy, "numeric")) {
    cpg_sites <- unique(sample(Methy450_sub, min(n_methy, length(Methy450_sub))))
  } else {
    cpg_sites <- NULL
  }
  if (length(cpg_sites) == 0 & !add_mean_methy) {
    gene_methy_n <- 0
  } else {
    if (add_mean_methy) {
      cpg_sites <- c(cpg_sites, gene)
    }
    gene_methy_plot <- lapply(seq_along(cpg_sites), function(i) {
      opt_pancan <- .opt_pancan
      if (cpg_sites[i] == gene) {
        opt_pancan$toil_methylation$aggr <- "NA"
      } else {
        opt_pancan$toil_methylation$aggr <- "mean"
        opt_pancan$toil_methylation$rule_out <- setdiff(Methy450_sub, cpg_sites[i])
      }
      gene_methy_tmp <- query_pancan_value(gene, "methylation", opt_pancan = opt_pancan) %>%
        as.data.frame() %>%
        dplyr::select(1) %>%
        dplyr::rename("Methy" = 1) %>%
        tibble::rownames_to_column("sample") %>%
        dplyr::inner_join(tcga_clinical_fine2, by = c("sample" = "Sample")) %>%
        dplyr::filter(.data$Code != "NT") %>%
        dplyr::group_by(.data$Cancer) %>%
        dplyr::summarise(Methy = median(.data$Methy, na.rm = T))
      colnames(gene_methy_tmp)[2] <- cpg_sites[i]
      gene_methy_tmp %>% tibble::column_to_rownames("Cancer")
    }) %>%
      do.call(cbind, .) %>%
      tibble::rownames_to_column("Cancer")
    if (gene %in% colnames(gene_methy_plot)) colnames(gene_methy_plot)[which(colnames(gene_methy_plot) %in% gene)] <- "cg_Overall"
    gene_methy_n <- ncol(gene_methy_plot) - 1
  }

  #### Ready for Plot
  gene_cross_dat <- data.frame(id = tumor_projects) %>%
    dplyr::left_join(gene_mrna_plot) %>%
    dplyr::left_join(gene_mut_plot, by = c("id" = "Cancer")) %>%
    dplyr::left_join(gene_cnv_plot, by = c("id" = "Cancer"))

  idx_1 <- which(sapply(gene_cross_dat$Ratio, is.null))
  for (i in idx_1) gene_cross_dat$Ratio[[i]] <- c(Mut = NA_real_, Wild = NA_real_)
  idx_2 <- which(sapply(gene_cross_dat$CNV_Pie, is.null))
  for (i in idx_2) gene_cross_dat$CNV_Pie[[i]] <- c(` -2` = NA_real_, ` -1` = NA_real_, ` 0` = NA_real_, ` 1` = NA_real_, ` 2` = NA_real_)

  if (gene_trans_n > 0) gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_trans_plot, by = c("id" = "tissue"))
  if (gene_methy_n > 0) gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_methy_plot, by = c("id" = "Cancer"))

  column_info <- data.frame(
    id = colnames(gene_cross_dat),
    name = c("", "Normal Exp", "Tumor Exp", "T vs. N(Wilcox)", "Mutation Dist", "Mutation PCT", "", "CNV Dist", "CNV Amp", "", "CNV Del", "", rep("Transcript", gene_trans_n), rep("Methylation", gene_methy_n)),
    group = c("TCGA", rep("Expression Profile", 3), rep("Mutation Profile", 3), rep("CNV Profile", 5), rep("Transcript Profile", gene_trans_n), rep("Methylation Profile", gene_methy_n)),
    geom = c("text", "bar", "bar", "text", "pie", "bar", "text", "pie", "bar", "text", "bar", "text", rep("funkyrect", gene_trans_n), rep("funkyrect", gene_methy_n)),
    palette = c(NA, "Tumor-Exp", "Tumor-Exp", NA, "Mut Status", "Mut-Ratio", NA, "CNV Status", "CNV-Amp", NA, "CNV-Del", NA, rep("Scale.Transcript.Level", gene_trans_n), rep("Scale.Methylation.Level", gene_methy_n)),
    width = c(5, 4, 4, 3, 2, 4, 4, 2, 4, 4, 4, 4, rep(1.1, gene_trans_n), rep(1.1, gene_methy_n))
  )

  column_info$options <- NA
  for (i in seq_len(nrow(column_info))) column_info$options[i] <- list(list())
  column_info$options[[2]] <- list(scale = F, hjust = 1)
  column_info$options[[3]] <- list(scale = F)
  column_info$options[[7]] <- list(label = "Mut_str", overlay = T)
  column_info$options[[10]] <- list(label = "CNV_Amp_str", overlay = T)
  column_info$options[[12]] <- list(label = "CNV_Del_str", overlay = T)

  column_groups <- data.frame(
    level1 = c("TCGA", rep(paste("Gene:", gene), length(unique(column_info$group)) - 1)),
    level2 = c("", unique(column_info$group)[-1]),
    group = unique(column_info$group),
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

  g <- funkyheatmap::funky_heatmap(gene_cross_dat, column_info = column_info, column_groups = column_groups, palettes = palettes)
  if (return_list) {
    return(list(plot = g, data = gene_cross_dat))
  } else {
    return(g)
  }
}

#' Visualize cross-omics of one gene in PCAWG
#'
#' @param gene  a gene symbol identifier (e.g., "TP53")
#' @param tumor_projects Select specific PCAWG projects. Default NULL, indicating all.
#' @param n_promoter number of promoters to show.
#' @param add_mean_promoter whether to add median promoter activity.
#' @param promoter_type one of "relative", "raw", "outlier".
#' @param return_list TRUE returns a list including plot object and data. FALSE just returns plot.
#'
#' @return funkyheatmap
#' @export
vis_pcawg_gene_cross_omics <- function(gene = "TP53",
                                       tumor_projects = NULL,
                                       n_promoter = 0,
                                       add_mean_promoter = FALSE,
                                       promoter_type = c("relative", "raw", "outlier"),
                                       return_list = FALSE) {
  promoter_type <- match.arg(promoter_type)
  pcawg_info <- load_data("pcawg_info")
  if (is.null(tumor_projects)) {
    tumor_projects <- sort(unique(pcawg_info$dcc_project_code))
  }
  pcawg_info2 <- pcawg_info %>% dplyr::filter(.data$dcc_project_code %in% tumor_projects)

  #### Omics--mRNA
  gene_mrna_res <- query_pancan_value(gene, "mRNA", database = "pcawg")
  gene_mrna <- gene_mrna_res[[1]] %>%
    as.data.frame() %>%
    dplyr::rename("Exp" = ".") %>%
    tibble::rownames_to_column("icgc_specimen_id") %>%
    dplyr::inner_join(pcawg_info2)
  gene_mrna_md <- gene_mrna %>%
    dplyr::group_by(.data$dcc_project_code, .data$type2) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    tidyr::pivot_wider(id_cols = .data$dcc_project_code, names_from = .data$type2, values_from = .data$Exp)
  if (!"tumor" %in% colnames(gene_mrna_md)) gene_mrna_md$tumor <- NA
  if (!"normal" %in% colnames(gene_mrna_md)) gene_mrna_md$normal <- NA
  all_vals <- c(gene_mrna_md$tumor, gene_mrna_md$normal)
  min_v <- min(all_vals, na.rm = T)
  max_v <- max(all_vals, na.rm = T)
  if (max_v != min_v) {
    gene_mrna_md$tumor <- (gene_mrna_md$tumor - min_v) / (max_v - min_v)
    gene_mrna_md$normal <- (gene_mrna_md$normal - min_v) / (max_v - min_v)
  }
  gene_mrna_comp <- ggpubr::compare_means(Exp ~ type2, data = gene_mrna, group.by = "dcc_project_code", method = "wilcox.test")
  gene_mrna_plot <- dplyr::left_join(gene_mrna_md, gene_mrna_comp[, c("dcc_project_code", "p")]) %>%
    dplyr::rename("id" = "dcc_project_code") %>%
    dplyr::mutate(Label = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "-")) %>%
    dplyr::select("id", "normal", "tumor", "Label")

  #### Omics--mutation
  gene_mut <- query_pancan_value(gene, "mutation", database = "pcawg") %>%
    dplyr::rename("sample" = "sampleID") %>%
    dplyr::inner_join(pcawg_info2, by = c("sample" = "donor"))
  gene_mut_plot <- gene_mut %>%
    dplyr::group_by(.data$dcc_project_code) %>%
    dplyr::summarise(Mut = mean(!is.na(.data$genes))) %>%
    as.data.frame() %>%
    dplyr::mutate(Ratio = NA)
  for (i in seq_len(nrow(gene_mut_plot))) {
    gene_mut_plot$Ratio[i] <- list(c(Mut = as.numeric(gene_mut_plot$Mut[i]), Wild = as.numeric(1 - gene_mut_plot$Mut[i])))
  }
  gene_mut_plot <- gene_mut_plot %>%
    dplyr::mutate(Mut_str = scales::percent(.data$Mut, accuracy = 0.1)) %>%
    dplyr::select("dcc_project_code", "Ratio", "Mut_str")

  #### Omics--Fusion
  gene_fusion <- query_pancan_value(gene, "fusion", database = "pcawg")[[1]] %>%
    as.data.frame() %>%
    dplyr::rename("Fusion" = ".") %>%
    tibble::rownames_to_column("icgc_specimen_id") %>%
    dplyr::inner_join(pcawg_info2)
  gene_fusion_plot <- gene_fusion %>%
    dplyr::group_by(.data$dcc_project_code) %>%
    dplyr::summarise(Fusion_ratio = mean(.data$Fusion == 1, na.rm = TRUE)) %>%
    as.data.frame() %>%
    dplyr::mutate(Fusion_str = scales::percent(.data$Fusion_ratio, accuracy = 0.1))

  #### Omics--miRNA
  gene_mi_res <- tryCatch(
    {
      query_pancan_value(gene, "miRNA", database = "pcawg")
    },
    error = function(e) NULL
  )
  if (!is.null(gene_mi_res)) {
    gene_mi <- gene_mi_res[[1]] %>%
      as.data.frame() %>%
      dplyr::rename("mi" = ".") %>%
      tibble::rownames_to_column("icgc_specimen_id") %>%
      dplyr::inner_join(pcawg_info2)
    gene_mi_plot <- gene_mi %>%
      dplyr::group_by(.data$dcc_project_code) %>%
      dplyr::summarise(mi = median(.data$mi, na.rm = TRUE)) %>%
      as.data.frame()
    if (max(gene_mi_plot$mi, na.rm = T) != min(gene_mi_plot$mi, na.rm = T)) {
      gene_mi_plot$mi <- (gene_mi_plot$mi - min(gene_mi_plot$mi, na.rm = T)) / (max(gene_mi_plot$mi, na.rm = T) - min(gene_mi_plot$mi, na.rm = T))
    }
  } else {
    gene_mi_plot <- data.frame(dcc_project_code = tumor_projects, mi = NA)
  }

  #### Omics--Promoter
  Promoter_sub <- load_data("v2_tpc_id_help")$pcawg$id_pro %>%
    dplyr::filter(.data$gene == gene) %>%
    dplyr::pull(.data$Level3)
  if (inherits(n_promoter, "character")) {
    promoters_sle <- n_promoter[n_promoter %in% Promoter_sub]
  } else if (inherits(n_promoter, "numeric") && n_promoter > 0) {
    set.seed(123)
    promoters_sle <- unique(sample(Promoter_sub, min(n_promoter, length(Promoter_sub))))
  } else {
    promoters_sle <- NULL
  }

  if (length(promoters_sle) == 0 && !add_mean_promoter) {
    gene_pro_n <- 0
  } else {
    if (add_mean_promoter) promoters_sle <- c(promoters_sle, gene)
    gene_pro_plot <- lapply(seq_along(promoters_sle), function(i) {
      if (promoters_sle[i] == gene) {
        gene_pro_tmp <- get_pcawg_promoter_value(Promoter_sub, type = promoter_type)
        if (is.null(gene_pro_tmp) || all(is.na(gene_pro_tmp[[1]]))) {
          return(NULL)
        }
        gene_pro_tmp_val <- as.data.frame(gene_pro_tmp[[1]]) %>% rowMeans(na.rm = TRUE)
        gene_pro_tmp_final <- list(data.frame(Pro = gene_pro_tmp_val) %>% tibble::rownames_to_column("icgc_specimen_id"))
      } else {
        gene_pro_tmp_res <- tryCatch(
          {
            get_pcawg_promoter_value(promoters_sle[i], type = promoter_type)
          },
          error = function(e) NULL
        )
        if (is.null(gene_pro_tmp_res) || all(is.na(gene_pro_tmp_res[[1]]))) {
          return(NULL)
        }
        gene_pro_tmp_final <- list(as.data.frame(gene_pro_tmp_res[[1]]) %>% dplyr::rename("Pro" = 1) %>% tibble::rownames_to_column("icgc_specimen_id"))
      }
      gene_pro_df <- gene_pro_tmp_final[[1]] %>%
        dplyr::inner_join(pcawg_info2) %>%
        dplyr::group_by(.data$dcc_project_code) %>%
        dplyr::summarise(Pro = median(.data$Pro, na.rm = TRUE))
      colnames(gene_pro_df)[2] <- promoters_sle[i]
      gene_pro_df %>% tibble::column_to_rownames("dcc_project_code")
    })
    gene_pro_plot <- gene_pro_plot[!sapply(gene_pro_plot, is.null)]
    if (length(gene_pro_plot) == 0) {
      gene_pro_n <- 0
    } else {
      gene_pro_plot <- do.call(cbind, gene_pro_plot) %>% tibble::rownames_to_column("dcc_project_code")
      if (gene %in% colnames(gene_pro_plot)) colnames(gene_pro_plot)[which(colnames(gene_pro_plot) %in% gene)] <- "Pro_Overall"
      gene_pro_n <- ncol(gene_pro_plot) - 1
    }
  }

  #### Ready for Plot
  gene_cross_dat <- data.frame(id = tumor_projects) %>%
    dplyr::left_join(gene_mrna_plot) %>%
    dplyr::left_join(gene_mut_plot, by = c("id" = "dcc_project_code")) %>%
    dplyr::left_join(gene_fusion_plot, by = c("id" = "dcc_project_code")) %>%
    dplyr::left_join(gene_mi_plot, by = c("id" = "dcc_project_code"))
  if (gene_pro_n > 0) gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_pro_plot, by = c("id" = "dcc_project_code"))
  idx_1 <- which(sapply(gene_cross_dat$Ratio, is.null))
  for (i in idx_1) gene_cross_dat$Ratio[[i]] <- c(Mut = NA_real_, Wild = NA_real_)

  column_info <- data.frame(
    id = colnames(gene_cross_dat),
    name = c("", "Normal Exp", "Tumor Exp", "T vs. N(Wilcox)", "Mutation Dist", "Mutation PCT", "Fusion PCT", "", "miRNA Exp", rep("Promoter", gene_pro_n)),
    group = c("PCAWG", rep("Expression Profile", 3), rep("Mutation Profile", 2), "Fusion Profile", "", "miRNA Profile", rep("Promoter Profile", gene_pro_n)),
    geom = c("text", "bar", "bar", "text", "pie", "text", "bar", "text", "bar", rep("funkyrect", gene_pro_n)),
    palette = c(NA, "Tumor-Exp", "Tumor-Exp", NA, "Mut Status", NA, "Fusion-Ratio", NA, "Scale.miRNA.Level", rep("Scale.Promoter.Level", gene_pro_n)),
    width = c(5, 4, 4, 3, 2, 4, 4, 4, 4, rep(1.1, gene_pro_n))
  )
  column_info$options <- NA
  for (i in seq_len(nrow(column_info))) column_info$options[i] <- list(list())
  column_info$options[[2]] <- list(scale = F, hjust = 1)
  column_info$options[[3]] <- list(scale = F)
  column_info$options[[6]] <- list(label = "Mut_str", overlay = T)
  column_info$options[[8]] <- list(label = "Fusion_str", overlay = T)

  column_groups <- data.frame(
    level1 = c("PCAWG", rep(paste("Gene:", gene), length(unique(column_info$group)) - 1)),
    level2 = c("", unique(column_info$group)[-1]), group = unique(column_info$group), palette = "palette1"
  )
  palettes <- list(
    palette1 = c("#66c2a5", "#66c2a5"), `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6],
    `Mut Status` = c(Mut = "red", Wild = "grey"), `Fusion-Ratio` = RColorBrewer::brewer.pal(n = 9, name = "Blues"),
    `Scale.Promoter.Level` = RColorBrewer::brewer.pal(n = 9, name = "Greens"),
    `Scale.miRNA.Level` = RColorBrewer::brewer.pal(n = 9, name = "Oranges")
  )
  g <- funkyheatmap::funky_heatmap(gene_cross_dat, column_info = column_info, column_groups = column_groups, palettes = palettes)
  if (return_list) {
    return(list(plot = g, data = gene_cross_dat))
  } else {
    return(g)
  }
}

#' Visualize cross-omics of one gene in CCLE
#'
#' @param gene  a gene symbol identifier (e.g., "TP53")
#' @param tumor_projects Select specific CCLE tissues. Default NULL, indicating all.
#' @param n_protein number of antibodies to show.
#' @param add_mean_protein whether to add median protein expression.
#' @param return_list TRUE returns a list including plot object and data. FALSE just returns plot.
#'
#' @return funkyheatmap
#' @export
vis_ccle_gene_cross_omics <- function(gene = "TP53",
                                      tumor_projects = NULL,
                                      n_protein = 0,
                                      add_mean_protein = FALSE,
                                      return_list = FALSE) {
  ccle_info <- load_data("ccle_info_fine")
  if (is.null(tumor_projects)) {
    tumor_projects <- sort(unique(ccle_info$Site_Primary))
  }
  ccle_info2 <- ccle_info %>% dplyr::filter(.data$Site_Primary %in% tumor_projects)

  shorten_ccle_site <- function(x) {
    x <- stringr::str_replace_all(x, "_", " ")
    x <- stringr::str_to_title(x)
    x <- stringr::str_replace(x, "Haematopoietic And Lymphoid Tissue", "Hema & Lymph")
    x <- stringr::str_replace(x, "Central Nervous System", "CNS")
    x <- stringr::str_replace(x, "Upper Aerodigestive Tract", "Upper Aero")
    x <- stringr::str_replace(x, "Autonomic Ganglia", "Auto Ganglia")
    x
  }

  #### Omics--mRNA
  gene_mrna_res <- query_pancan_value(gene, "mRNA", database = "ccle")
  gene_mrna <- gene_mrna_res[[1]] %>%
    as.data.frame() %>%
    dplyr::rename("Exp" = ".") %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(ccle_info2)
  gene_mrna_plot <- gene_mrna %>%
    dplyr::group_by(.data$Site_Primary) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    dplyr::rename("id" = "Site_Primary")
  if (max(gene_mrna_plot$Exp, na.rm = T) != min(gene_mrna_plot$Exp, na.rm = T)) {
    gene_mrna_plot$Exp <- (gene_mrna_plot$Exp - min(gene_mrna_plot$Exp, na.rm = T)) / (max(gene_mrna_plot$Exp, na.rm = T) - min(gene_mrna_plot$Exp, na.rm = T))
  }

  #### Omics--mutation
  gene_mut <- query_pancan_value(gene, "mutation", database = "ccle") %>%
    dplyr::rename("Sample" = "sampleID") %>%
    dplyr::inner_join(ccle_info2)
  gene_mut_plot <- gene_mut %>%
    dplyr::group_by(.data$Site_Primary) %>%
    dplyr::summarise(Mut = mean(!is.na(.data$genes))) %>%
    as.data.frame() %>%
    dplyr::mutate(Ratio = NA)
  for (i in seq_len(nrow(gene_mut_plot))) {
    gene_mut_plot$Ratio[i] <- list(c(Mut = as.numeric(gene_mut_plot$Mut[i]), Wild = as.numeric(1 - gene_mut_plot$Mut[i])))
  }
  gene_mut_plot <- gene_mut_plot %>%
    dplyr::mutate(Mut_str = scales::percent(.data$Mut, accuracy = 0.1)) %>%
    dplyr::select("Site_Primary", "Ratio", "Mut_str")

  #### Omics--CNV
  gene_cnv_res <- query_pancan_value(gene, "cnv", database = "ccle")
  gene_cnv <- gene_cnv_res[[1]] %>%
    as.data.frame() %>%
    dplyr::rename("CNV" = ".") %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(ccle_info2)
  gene_cnv$CNV_cat <- case_when(gene_cnv$CNV > 0.3 ~ "Gain", gene_cnv$CNV < -0.3 ~ "Loss", TRUE ~ "Neutral")
  gene_cnv_tb <- table(gene_cnv$Site_Primary, gene_cnv$CNV_cat)
  gene_cnv_plot <- gene_cnv %>%
    dplyr::count(.data$Site_Primary, name = "CNV_total") %>%
    dplyr::mutate(CNV_Pie = NA, CNV_Gain = NA, CNV_Loss = NA)
  for (i in seq_len(nrow(gene_cnv_plot))) {
    Site <- gene_cnv_plot$Site_Primary[i]
    gene_cnv_plot$CNV_Pie[i] <- list(c(
      "Gain" = as.numeric(ifelse("Gain" %in% colnames(gene_cnv_tb), gene_cnv_tb[Site, "Gain"], 0)),
      "Neutral" = as.numeric(ifelse("Neutral" %in% colnames(gene_cnv_tb), gene_cnv_tb[Site, "Neutral"], 0)),
      "Loss" = as.numeric(ifelse("Loss" %in% colnames(gene_cnv_tb), gene_cnv_tb[Site, "Loss"], 0))
    ))
    gene_cnv_plot$CNV_Gain[i] <- ifelse("Gain" %in% colnames(gene_cnv_tb), gene_cnv_tb[Site, "Gain"], 0) / gene_cnv_plot$CNV_total[i]
    gene_cnv_plot$CNV_Loss[i] <- ifelse("Loss" %in% colnames(gene_cnv_tb), gene_cnv_tb[Site, "Loss"], 0) / gene_cnv_plot$CNV_total[i]
  }
  gene_cnv_plot <- gene_cnv_plot %>%
    dplyr::mutate(CNV_Gain_str = scales::percent(.data$CNV_Gain, accuracy = 0.1), CNV_Loss_str = scales::percent(.data$CNV_Loss, accuracy = 0.1)) %>%
    dplyr::select("Site_Primary", "CNV_Pie", "CNV_Gain_str", "CNV_Loss_str")

  #### Omics--Protein
  Protein_all <- .all_ccle_proteins
  Protein_sub <- Protein_all[grepl(paste0("^", gene), Protein_all, ignore.case = TRUE)]
  if (inherits(n_protein, "character")) {
    prot_sle <- n_protein[n_protein %in% Protein_sub]
  } else if (inherits(n_protein, "numeric") && n_protein > 0) {
    set.seed(123)
    prot_sle <- unique(sample(Protein_sub, min(n_protein, length(Protein_sub))))
  } else {
    prot_sle <- NULL
  }

  if (length(prot_sle) == 0 && !add_mean_protein) {
    gene_prot_n <- 0
  } else {
    if (add_mean_protein) prot_sle <- c(prot_sle, gene)
    gene_prot_plot <- lapply(seq_along(prot_sle), function(i) {
      if (prot_sle[i] == gene) {
        if (length(Protein_sub) == 0) {
          return(NULL)
        }
        gene_prot_tmp <- get_ccle_protein_value(Protein_sub)
        if (is.null(gene_prot_tmp) || all(is.na(gene_prot_tmp[[1]]))) {
          return(NULL)
        }
        gene_prot_tmp_val <- as.data.frame(gene_prot_tmp[[1]][, -1]) %>% rowMeans(na.rm = TRUE)
        gene_prot_tmp_final <- list(data.frame(Prot = gene_prot_tmp_val) %>% tibble::rownames_to_column("Sample"))
      } else {
        gene_prot_res <- tryCatch(
          {
            get_ccle_protein_value(prot_sle[i])
          },
          error = function(e) NULL
        )
        if (is.null(gene_prot_res) || all(is.na(gene_prot_res[[1]]))) {
          return(NULL)
        }
        gene_prot_tmp_final <- list(as.data.frame(gene_prot_res[[1]]) %>% dplyr::rename("Prot" = 1) %>% tibble::rownames_to_column("Sample"))
      }
      gene_prot_df <- gene_prot_tmp_final[[1]] %>%
        dplyr::inner_join(ccle_info2) %>%
        dplyr::group_by(.data$Site_Primary) %>%
        dplyr::summarise(Prot = median(.data$Prot, na.rm = TRUE))
      colnames(gene_prot_df)[2] <- prot_sle[i]
      gene_prot_df %>% tibble::column_to_rownames("Site_Primary")
    })
    gene_prot_plot <- gene_prot_plot[!sapply(gene_prot_plot, is.null)]
    if (length(gene_prot_plot) == 0) {
      gene_prot_n <- 0
    } else {
      gene_prot_plot <- do.call(cbind, gene_prot_plot) %>% tibble::rownames_to_column("Site_Primary")
      if (gene %in% colnames(gene_prot_plot)) colnames(gene_prot_plot)[which(colnames(gene_prot_plot) %in% gene)] <- "Prot_Overall"
      gene_prot_n <- ncol(gene_prot_plot) - 1
    }
  }

  #### Ready for Plot
  gene_cross_dat <- data.frame(id = tumor_projects) %>%
    dplyr::left_join(gene_mrna_plot) %>%
    dplyr::left_join(gene_mut_plot, by = c("id" = "Site_Primary")) %>%
    dplyr::left_join(gene_cnv_plot, by = c("id" = "Site_Primary"))
  if (gene_prot_n > 0) gene_cross_dat <- dplyr::left_join(gene_cross_dat, gene_prot_plot, by = c("id" = "Site_Primary"))
  idx_1 <- which(sapply(gene_cross_dat$Ratio, is.null))
  for (i in idx_1) gene_cross_dat$Ratio[[i]] <- c(Mut = NA_real_, Wild = NA_real_)
  idx_2 <- which(sapply(gene_cross_dat$CNV_Pie, is.null))
  for (i in idx_2) gene_cross_dat$CNV_Pie[[i]] <- c(Gain = NA_real_, Neutral = NA_real_, Loss = NA_real_)
  gene_cross_dat$id_label <- shorten_ccle_site(gene_cross_dat$id)

  base_cols <- c("id_label", "Exp", "Ratio", "Mut_str", "CNV_Pie", "CNV_Gain_str", "CNV_Loss_str")
  display_cols <- if (gene_prot_n > 0) c(base_cols, colnames(gene_prot_plot)[-1]) else base_cols
  column_info <- data.frame(
    id = display_cols,
    name = c("", "Median Exp", "Mutation Dist", "Mutation PCT", "CNV Dist", "Gain (>0.3)", "Loss (<-0.3)", rep("Protein", gene_prot_n)),
    group = c("CCLE", "Expression Profile", rep("Mutation Profile", 2), rep("CNV Profile", 3), rep("Protein Profile", gene_prot_n)),
    geom = c("text", "bar", "pie", "text", "pie", "text", "text", rep("funkyrect", gene_prot_n)),
    palette = c(NA, "Tumor-Exp", "Mut Status", NA, "CNV Status", NA, NA, rep("Scale.Protein.Level", gene_prot_n)),
    width = c(8, 4, 2, 4, 2, 4, 4, rep(1.1, gene_prot_n))
  )
  column_info$options <- NA
  for (i in seq_len(nrow(column_info))) column_info$options[i] <- list(list())
  column_info$options[[1]] <- list(hjust = 0)
  column_info$options[[2]] <- list(scale = F)

  column_groups <- data.frame(
    level1 = c("CCLE", rep(paste("Gene:", gene), length(unique(column_info$group)) - 1)),
    level2 = c("", unique(column_info$group)[-1]), group = unique(column_info$group), palette = "palette1"
  )
  palettes <- list(
    palette1 = c("#66c2a5", "#66c2a5"), `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6],
    `Mut Status` = c(Mut = "red", Wild = "grey"), `CNV Status` = c("Gain" = "#e66101", "Neutral" = "#f7f7f7", "Loss" = "#5e3c99"),
    `Scale.Protein.Level` = RColorBrewer::brewer.pal(n = 9, name = "Reds")
  )
  g <- funkyheatmap::funky_heatmap(gene_cross_dat, column_info = column_info, column_groups = column_groups, palettes = palettes)
  if (return_list) {
    return(list(plot = g, data = gene_cross_dat))
  } else {
    return(g)
  }
}

#' Visualize cross-omics of one pathway in PCAWG
#'
#' @param pw  pathway name
#' @param tumor_projects Select specific PCAWG projects. Default NULL, indicating all.
#' @param return_list TRUE returns a list including plot object and data. FALSE just returns plot.
#'
#' @return funkyheatmap
#' @export
vis_pcawg_pathway_cross_omics <- function(pw = "HALLMARK_ADIPOGENESIS",
                                          tumor_projects = NULL,
                                          return_list = FALSE) {
  pcawg_info <- load_data("pcawg_info")
  if (is.null(tumor_projects)) {
    tumor_projects <- sort(unique(pcawg_info$dcc_project_code))
  }
  pcawg_info2 <- pcawg_info %>% dplyr::filter(.data$dcc_project_code %in% tumor_projects)
  pcawg_pw <- load_data("pcawg_PW")
  pw_mrna <- pcawg_pw[rownames(pcawg_pw) %in% pcawg_info2$icgc_specimen_id, pw, drop = FALSE] %>%
    tibble::rownames_to_column("icgc_specimen_id") %>%
    dplyr::inner_join(pcawg_info2)
  colnames(pw_mrna)[2] <- "Exp"
  pw_mrna_plot <- pw_mrna %>%
    dplyr::group_by(.data$dcc_project_code, .data$type2) %>%
    dplyr::summarise(Exp = median(.data$Exp)) %>%
    as.data.frame() %>%
    tidyr::pivot_wider(id_cols = .data$dcc_project_code, names_from = .data$type2, values_from = .data$Exp)
  if (!"tumor" %in% colnames(pw_mrna_plot)) pw_mrna_plot$tumor <- NA
  if (!"normal" %in% colnames(pw_mrna_plot)) pw_mrna_plot$normal <- NA
  all_vals <- c(pw_mrna_plot$tumor, pw_mrna_plot$normal)
  min_v <- min(all_vals, na.rm = T)
  max_v <- max(all_vals, na.rm = T)
  if (max_v != min_v) {
    pw_mrna_plot$tumor <- (pw_mrna_plot$tumor - min_v) / (max_v - min_v)
    pw_mrna_plot$normal <- (pw_mrna_plot$normal - min_v) / (max_v - min_v)
  }
  pw_mrna_comp <- ggpubr::compare_means(Exp ~ type2, data = pw_mrna, group.by = "dcc_project_code", method = "wilcox.test")
  pw_mrna_plot <- dplyr::left_join(pw_mrna_plot, pw_mrna_comp[, c("dcc_project_code", "p")]) %>%
    dplyr::rename("id" = "dcc_project_code") %>%
    dplyr::mutate(Label = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "-")) %>%
    dplyr::select("id", "normal", "tumor", "Label")
  pw_cross_dat <- data.frame(id = tumor_projects) %>% dplyr::left_join(pw_mrna_plot)
  column_info <- data.frame(
    id = colnames(pw_cross_dat), name = c("", "Normal ssGSEA", "Tumor ssGSEA", "T vs. N(Wilcox)"),
    group = c("PCAWG", rep("Expression Profile", 3)), geom = c("text", "bar", "bar", "text"),
    palette = c(NA, "Tumor-Exp", "Tumor-Exp", NA), width = c(5, 4, 4, 4)
  )
  column_info$options <- NA
  for (i in seq(nrow(column_info))) column_info$options[i] <- list(list())
  column_info$options[[2]] <- list(scale = F, hjust = 1)
  column_info$options[[3]] <- list(scale = F)
  column_groups <- data.frame(
    level1 = c("PCAWG", rep(paste("Pathway:", pw), 1)), level2 = c("", "Expression Profile"),
    group = c("PCAWG", "Expression Profile"), palette = "palette1"
  )
  palettes <- list(palette1 = c("#66c2a5", "#66c2a5"), `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6])
  g <- funkyheatmap::funky_heatmap(pw_cross_dat, column_info = column_info, column_groups = column_groups, palettes = palettes)
  if (return_list) {
    return(list(plot = g, data = pw_cross_dat))
  } else {
    return(g)
  }
}
