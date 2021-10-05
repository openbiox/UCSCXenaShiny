#' Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))
#' @import ggplot2 dplyr tibble
#' @param Gene a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param Mode "Boxplot" or "Violinplot" to represent data
#' @param Show.P.value `TRUE` or `FALSE` whether to count P value
#' @param Method default method is wilcox.test
#' @param Show.P.label `TRUE` or `FALSE` present p value with number or label `*`, `**`, `***` and `****`
#' @param values the color to fill tumor or normal
#' @param TCGA.only include samples only from TCGA dataset
#' @param draw_quantiles draw quantiles for violinplot
#' @param trim whether trim the violin
#' @param data_type choose gene profile type,
#' including "mRNA", "transcript", "protein", "mutation", "cnv" (-2, -1, 0, 1, 2),
#' "cnv_gistic2", "methylation", "miRNA".
#' @return a `ggplot` object
#' @examples
#' \dontrun{
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = FALSE, Show.P.label = FALSE)
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = FALSE, Show.P.label = FALSE)
#' }
#' @export
#'
vis_toil_TvsN <- function(Gene = "TP53", Mode = c("Boxplot", "Violinplot"),
                          data_type = "mRNA", Show.P.value = TRUE,
                          Show.P.label = TRUE, Method = c("wilcox.test", "t.test"),
                          values = c("#DF2020", "#DDDF21"),
                          TCGA.only = FALSE, draw_quantiles = c(0.25, 0.5, 0.75),
                          trim = TRUE) {
  tcga_gtex <- load_data("tcga_gtex")

  Mode <- match.arg(Mode)
  Method <- match.arg(Method)

  if (!Method %in% c("wilcox.test", "t.test")) {
    stop("only support wilcox.test or t.test")
  }

  if (!Mode %in% c("Boxplot", "Violinplot")) {
    stop("only support Boxplot or Violinplot")
  }

  if (!data_type %in% c("mRNA", "miRNA", "transcript", "methylation")) {
    stop("data_type ", data_type, " does not support in this function!")
  }
  t1 <- query_pancan_value(Gene, data_type = data_type)
  unit <- switch(data_type,
    cnv = NULL,
    mutation = NULL,
    t1[[2]]
  )
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  tcga_gtex <- tcga_gtex %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::distinct(.data$sample, .keep_all = TRUE)

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2 == "tumor", ]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2 == "normal", ]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex <- t2 %>% dplyr::select("tpm", "tissue", "type2", "sample")
  tcga_gtex$type2 <- factor(tcga_gtex$type2, levels = c("tumor", "normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal), ]
  tcga_gtex_withNormal <- tcga_gtex_withNormal %>%
    dplyr::mutate(dataset = ifelse(stringr::str_sub(.data$sample, 1, 4) == "TCGA", "TCGA", "GTEX"))
  tcga_gtex_MESO <- tcga_gtex[tcga_gtex$tissue == "MESO", ]
  tcga_gtex_UVM <- tcga_gtex[tcga_gtex$tissue == "UVM", ]
  if (TCGA.only == TRUE) {
    tcga_gtex_withNormal <- tcga_gtex_withNormal %>% dplyr::filter(.data$dataset == "TCGA")
  }
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- tcga_gtex_withNormal %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method, group.by = "tissue")
    pv <- pv %>% dplyr::select(c("tissue", "p", "p.signif", "p.adj"))
    message("Counting P value finished")
  }
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "tissue", y = "tpm", fill = "type2")) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab(NULL) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values)
    p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO) +
      ggplot2::geom_boxplot(data = tcga_gtex_UVM)

    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )

    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(
        x = .data$tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(
        x = .data$tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "tissue", y = "tpm", fill = "type2")) +
      geom_split_violin(
        draw_quantiles = draw_quantiles,
        trim = trim,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      ) +
      ggplot2::xlab("") +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_set(ggplot2::theme_classic(base_size = 20)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )

    p + geom_split_violin(
      data = tcga_gtex_MESO,
      mapping = aes_string(x = "tissue", y = "tpm", fill = "type2"),
      draw_quantiles = draw_quantiles,
      trim = trim,
      linetype = "solid",
      color = "black",
      size = 0.2,
      na.rm = TRUE,
      position = "identity"
    ) +
      geom_split_violin(
        data = tcga_gtex_UVM,
        mapping = ggplot2::aes_string(x = "tissue", y = "tpm", fill = "type2"),
        draw_quantiles = draw_quantiles,
        trim = trim,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      ) #+
    # ggplot2::scale_x_discrete(limits = levels(tcga_gtex$tissue))
    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )

    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = .data$tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = .data$tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }
  return(p)
}

#' Visualize Single Gene Univariable Cox Result from Toil Data Hub
#'
#' @inheritParams vis_toil_TvsN
#' @param measure a survival measure, e.g. "OS".
#' @param threshold a expression cutoff, `0.5` for median.
#' @param data_type choose gene profile type, including "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"
#' @return a `ggplot` object
#' @examples
#' \dontrun{
#' p <- vis_unicox_tree(Gene = "TP53")
#' }
#' @export
vis_unicox_tree <- function(Gene = "TP53", measure = "OS", data_type = "mRNA", threshold = 0.5, values = c("grey", "#E31A1C", "#377DB8")) {
  tcga_surv <- load_data("tcga_surv")
  tcga_gtex <- load_data("tcga_gtex")

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  # we filter out normal tissue
  tcga_gtex <- tcga_gtex %>% dplyr::filter(.data$type2 != "normal")

  message(paste0("Get data value for ", Gene))
  s <- data.frame(sample = names(t1), values = t1)
  ## we use median cutoff here
  ss <- s %>%
    dplyr::inner_join(tcga_surv, by = "sample") %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)
  unicox_res_all_cancers <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    if (threshold == 0.5) {
      sss_can <- sss_can %>%
        dplyr::mutate(group = ifelse(.data$values > stats::median(.data$values), "high", "low")) %>%
        dplyr::mutate(group = factor(.data$group, levels = c("low", "high")))
    }

    if (threshold == 0.25) {
      sss_can <- sss_can %>%
        dplyr::mutate(group = ifelse(.data$values > stats::quantile(.data$values)[4], "high",
          ifelse(.data$values < stats::quantile(.data$values)[2], "low", "middle")
        )) %>%
        dplyr::filter(group != "middle") %>%
        dplyr::mutate(group = factor(.data$group, levels = c("low", "high")))
    }

    unicox_res_genes <- ezcox::ezcox(
      sss_can %>%
        dplyr::select(c("values", paste0(measure, ".time"), measure)) %>%
        na.omit(),
      covariates = "values",
      time = paste0(measure, ".time"),
      status = measure,
      verbose = FALSE
    )

    unicox_res_genes$cancer <- cancer
    unicox_res_genes$measure <- measure
    return(unicox_res_genes)
  })) %>% magrittr::set_names(tissues)

  unicox_res_all_cancers <- unicox_res_all_cancers %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  unicox_res_all_cancers_df <- do.call(rbind.data.frame, unicox_res_all_cancers)
  unicox_res_all_cancers_df <- unicox_res_all_cancers_df %>%
    dplyr::mutate(HR_log = log(.data$HR)) %>%
    dplyr::mutate(lower_95_log = log(.data$lower_95)) %>%
    dplyr::mutate(upper_95_log = log(.data$upper_95)) %>%
    dplyr::mutate(Type = ifelse(.data$p.value < 0.05 & .data$HR_log > 0, "Risky", ifelse(.data$p.value < 0.05 & .data$HR_log < 0, "Protective", "NS"))) %>%
    dplyr::mutate(Type = factor(Type, levels = c("NS", "Risky", "Protective")))
  ## visualization
  p <- ggplot2::ggplot(
    data = unicox_res_all_cancers_df,
    aes_string(x = "cancer", y = "HR_log", ymin = "lower_95_log", ymax = "upper_95_log", color = "Type")
  ) +
    ggplot2::theme_bw() +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "ln (Hazard Ratio)") +
    ggtitle(paste0(Gene, if (startsWith(data_type, "mRNA") | startsWith(data_type, "miRNA")) " Expression" else "")) +
    ggplot2::theme(
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggplot2::scale_color_manual(values = values) +
    ggplot2::geom_hline(yintercept = c(0), linetype = "dashed")
  return(p)
}

#' Visualize Single Gene Expression in Anatomy Location
#'
#' @inheritParams vis_toil_TvsN
#' @inheritParams ggplot2::scale_colour_viridis_d
#' @param Gender a string, "Female" (default) or "Male".
#' @param data_type choose gene profile type, including "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"
#' @return a `ggplot` object
#' @importFrom stats complete.cases median
#' @export

vis_pancan_anatomy <- function(Gene = "TP53",
                               Gender = c("Female", "Male"),
                               data_type = "mRNA",
                               option = "D") {
  Gender <- match.arg(Gender)

  # Loading while pass checking
  if (eval(parse(text = "!requireNamespace('gganatogram')"))) {
    stop("Please install 'gganatogram' package firstly!")
  }
  hgMale_key <- eval(parse(text = "gganatogram::hgMale_key"))
  hgFemale_key <- eval(parse(text = "gganatogram::hgFemale_key"))
  gganatogram <- eval(parse(text = "gganatogram::gganatogram"))

  TCGA.organ <- load_data("TCGA.organ")
  tcga_gtex <- load_data("tcga_gtex")
  tcga_gtex <- tcga_gtex %>% dplyr::distinct(sample, .keep_all = TRUE)

  t1 <- query_pancan_value(Gene, data_type = data_type)
  unit <- switch(data_type,
    cnv = NULL,
    mutation = NULL,
    t1[[2]]
  )
  if (is.list(t1)) t1 <- t1[[1]]

  if (data_type == "cnv") data_type <- "GISTIC2 thresholded CNV"
  if (data_type == "cnv_gistic2") data_type <- "CNV"

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  message(paste0("Get data value for ", Gene))
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  t2 <- t2 %>%
    dplyr::left_join(TCGA.organ, by = c("tissue" = "TCGA")) %>%
    mutate(group = paste(.data$tissue, .data$type2, sep = "_"))
  # Male
  Male_input <- t2 %>%
    dplyr::full_join(hgMale_key, by = "organ") %>%
    dplyr::filter(.data$organ != "") %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(
      tpmMedian = median(.data$tpm),
      tissue = .data$tissue,
      type.x = .data$type.x,
      type = .data$type2,
      organ = .data$organ,
      color = .data$colour
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(value = .data$tpmMedian)
  # Female
  Female_input <- t2 %>%
    dplyr::full_join(hgFemale_key, by = "organ") %>%
    dplyr::filter(.data$organ != "") %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(
      tpmMedian = median(.data$tpm),
      tissue = .data$tissue,
      type.x = .data$type.x,
      type = .data$type2,
      organ = .data$organ,
      color = .data$colour
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(value = .data$tpmMedian)
  if (Gender == "Male") {
    p <- gganatogram(
      data = Male_input,
      fillOutline = "white",
      organism = "human",
      sex = "male",
      fill = "value"
    ) +
      facet_wrap(~type) +
      labs(fill = unit) +
      coord_cartesian(ylim = c(-120, 0)) +
      theme_void(base_size = 15) +
      scale_fill_continuous(low = "#3CB371", high = "#DC143C") +
      ggtitle(paste0(data_type, " - ", Gene)) +
      labs(caption = "data source: TCGA + GTEx") +
      theme(plot.title = element_text(hjust = 0.5))

    data_input <- Male_input
  } else {
    p <- gganatogram(
      data = Female_input,
      fillOutline = "white",
      organism = "human",
      sex = "female",
      fill = "value"
    ) +
      facet_wrap(~type) +
      coord_cartesian(ylim = c(-120, 0)) +
      labs(fill = unit) +
      theme_void(base_size = 15) +
      scale_fill_continuous(low = "#3CB371", high = "#DC143C") +
      ggtitle(paste0(data_type, " - ", Gene)) +
      labs(caption = "data source: TCGA + GTEx") +
      theme(plot.title = element_text(hjust = 0.5))

    data_input <- Female_input
  }

  return(list(plot = p, data = data_input))
}

#' Heatmap for Correlation between Gene and Immune Signatures
#'
#' @inheritParams vis_toil_TvsN
#' @param cor_method correlation method
#' @param Immune_sig_type quantification method, default is "Cibersort"
#' @param Plot output the plot directly, default 'TRUE'
#' @examples
#' \dontrun{
#' p <- vis_gene_immune_cor(Gene = "TP53")
#' }
#' @export
vis_gene_immune_cor <- function(Gene = "TP53", cor_method = "spearman", data_type = "mRNA", Immune_sig_type = "Cibersort", Plot = "TRUE") {
  tcga_pan_immune_signature <- load_data("tcga_pan_immune_signature")
  tcga_gtex <- load_data("tcga_gtex")

  # we filter out normal tissue
  tcga_gtex <- tcga_gtex %>% dplyr::filter(.data$type2 != "normal")

  tcga_pan_immune_signature <- tcga_pan_immune_signature %>%
    tidyr::pivot_longer(3:ncol(.), names_to = "sample", values_to = "score") %>%
    dplyr::mutate(sample = stringr::str_sub(.data$sample, 1, 15))

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  if (data_type == "cnv") data_type <- "GISTIC2 thresholded CNV"
  if (data_type == "cnv_gistic2") data_type <- "CNV"

  message(paste0("Get data value for ", Gene))
  s <- data.frame(sample = names(t1), values = t1)

  ss <- s %>%
    dplyr::inner_join(tcga_pan_immune_signature, by = "sample") %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")

  sss <- split(ss, ss$tissue)
  tissues <- names(sss)
  cor_gene_immune <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    ## filter cibersort data here
    sss_can_class <- sss_can %>% dplyr::filter(.data$Source == Immune_sig_type)
    cells <- unique(sss_can_class$SetName)
    cor_res_class_can <- purrr::map(cells, purrr::safely(function(i) {
      # i = cells[1]
      dd <- sss_can_class[sss_can_class$SetName == i, ]
      suppressWarnings(dd <- stats::cor.test(dd$values, dd$score, method = cor_method))
      ddd <- data.frame(gene = Gene, immune_cells = i, cor = dd$estimate, p.value = dd$p.value, stringsAsFactors = FALSE)
      return(ddd)
    })) %>% magrittr::set_names(cells)
    cor_res_class_can <- cor_res_class_can %>%
      purrr::map(~ .x$result) %>%
      purrr::compact()
    cor_res_class_can_df <- do.call(rbind.data.frame, cor_res_class_can)
    cor_res_class_can_df$cancer <- cancer
    return(cor_res_class_can_df)
  })) %>% magrittr::set_names(tissues)

  cor_gene_immune <- cor_gene_immune %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  cor_gene_immune_df <- do.call(rbind.data.frame, cor_gene_immune)
  data <- cor_gene_immune_df
  data$pstar <- ifelse(data$p.value < 0.05,
    ifelse(data$p.value < 0.001, "***", ifelse(data$p.value < 0.01, "**", "*")),
    ""
  )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "cancer", y = "immune_cells")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"), colour = "white", size = 1) +
    ggplot2::scale_fill_gradient2(low = "#377DB8", mid = "white", high = "#E31A1C") +
    ggplot2::geom_text(ggplot2::aes_string(label = "pstar"), col = "black", size = 5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(fill = paste0(" * p < 0.05", "\n\n", "** p < 0.01", "\n\n", "*** p < 0.001", "\n\n", "Correlation")) +
    ggtitle(paste0("The correlation between ", Gene, " ", data_type, " with immune signatures"))
  
  if (Plot == TRUE) {
    return(p)
  } else {
    pdata = ss
    plist = list()
    plist[[1]] = p 
    plist[[2]] = pdata
    names(plist) = c("plot","pdata")
    return(plist)
  }
}


#' Visualize Correlation between Gene and TMB (Tumor Mutation Burden)
#'
#' @inheritParams vis_gene_immune_cor
#' @examples
#' \dontrun{
#' p <- vis_gene_tmb_cor(Gene = "TP53")
#' }
#' @export
vis_gene_tmb_cor <- function(Gene = "TP53", cor_method = "spearman", data_type = "mRNA", Plot = "TRUE") {
  tcga_tmb <- load_data("tcga_tmb")
  tcga_gtex <- load_data("tcga_gtex")

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>%
    dplyr::inner_join(tcga_tmb, by = c("sample" = "Tumor_Sample_ID")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)

  cor_gene_tmb <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd <- stats::cor.test(sss_can$values, sss_can$Non_silent_per_Mb, type = cor_method)
    ddd <- data.frame(gene = Gene, cor = dd$estimate, p.value = dd$p.value, stringsAsFactors = F)
    ddd$cancer <- cancer
    return(ddd)
  })) %>% magrittr::set_names(tissues)

  cor_gene_tmb <- cor_gene_tmb %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  cor_gene_tmb_df <- do.call(rbind.data.frame, cor_gene_tmb)
  data <- cor_gene_tmb_df
  data$pstar <- ifelse(data$p.value < 0.05,
    ifelse(data$p.value < 0.01, "**", "*"),
    ""
  )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "cancer", y = "gene")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"), colour = "white", size = 1) +
    ggplot2::scale_fill_gradient2(low = "#2b8cbe", mid = "white", high = "#e41a1c") +
    ggplot2::geom_text(ggplot2::aes_string(label = "pstar"), col = "black", size = 5) +
    ggplot2::theme_minimal() + # 不要背景
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(), # 不要title
      axis.ticks.x = ggplot2::element_blank(), # 不要x轴
      axis.title.y = ggplot2::element_blank(), # 不要y轴
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # 调整x轴文字
      axis.text.y = ggplot2::element_text(size = 8)
    ) + # 调整y轴文字
    # 调整legen
    ggplot2::labs(fill = paste0(" * p < 0.05", "\n\n", "** p < 0.01", "\n\n", "Correlation"))
  if (Plot == TRUE) {
    return(p)
  } else {
    pdata = ss
    plist = list()
    plist[[1]] = p 
    plist[[2]] = pdata
    names(plist) = c("plot","pdata")
    return(plist)
  }
  
}

#' Visualize Correlation between Gene and MSI (Microsatellite instability)
#'
#' @inheritParams vis_gene_immune_cor
#' @examples
#' \dontrun{
#' p <- vis_gene_msi_cor(Gene = "TP53")
#' }
#' @export
vis_gene_msi_cor <- function(Gene = "TP53", cor_method = "spearman", data_type = "mRNA", Plot = "TRUE") {
  tcga_msi <- load_data("tcga_MSI")
  tcga_gtex <- load_data("tcga_gtex")

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>%
    dplyr::mutate(Barcode = stringr::str_sub(sample, 1, 12)) %>%
    dplyr::inner_join(tcga_msi, by = c("Barcode" = "Barcode")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)

  cor_gene_msi <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd <- stats::cor.test(sss_can$values, sss_can$Total_nb_MSI_events, type = cor_method)
    ddd <- data.frame(gene = Gene, cor = dd$estimate, p.value = dd$p.value, stringsAsFactors = F)
    ddd$cancer <- cancer
    return(ddd)
  })) %>% magrittr::set_names(tissues)

  cor_gene_msi <- cor_gene_msi %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  cor_gene_msi_df <- do.call(rbind.data.frame, cor_gene_msi)
  data <- cor_gene_msi_df
  data$pstar <- ifelse(data$p.value < 0.05,
    ifelse(data$p.value < 0.01, "**", "*"),
    ""
  )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "cancer", y = "gene")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"), colour = "white", size = 1) +
    ggplot2::scale_fill_gradient2(low = "#2b8cbe", mid = "white", high = "#e41a1c") +
    ggplot2::geom_text(ggplot2::aes_string(label = "pstar"), col = "black", size = 5) +
    ggplot2::theme_minimal() + # 不要背景
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(), # 不要title
      axis.ticks.x = ggplot2::element_blank(), # 不要x轴
      axis.title.y = ggplot2::element_blank(), # 不要y轴
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # 调整x轴文字
      axis.text.y = ggplot2::element_text(size = 8)
    ) + # 调整y轴文字
    # 调整legen
    ggplot2::labs(fill = paste0(" * p < 0.05", "\n\n", "** p < 0.01", "\n\n", "Correlation"))
  # p
  if (Plot == TRUE) {
    return(p)
  } else {
    pdata = ss
    plist = list()
    plist[[1]] = p 
    plist[[2]] = pdata
    names(plist) = c("plot","pdata")
    return(plist)
  }
}

#' Visualize Correlation between Gene and Tumor Stemness
#'
#' @inheritParams vis_gene_immune_cor
#' @examples
#' \dontrun{
#' p <- vis_gene_stemness_cor(Gene = "TP53")
#' }
#' @export
vis_gene_stemness_cor <- function(Gene = "TP53", cor_method = "spearman", data_type = "mRNA", Plot = "TRUE") {
  tcga_stemness <- load_data("tcga_stemness")
  tcga_gtex <- load_data("tcga_gtex")

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>%
    dplyr::inner_join(tcga_stemness, by = c("sample")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)

  cor_gene_stemness <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd <- stats::cor.test(sss_can$values, sss_can$RNAss, type = cor_method)
    ddd <- data.frame(gene = Gene, cor = dd$estimate, p.value = dd$p.value, stringsAsFactors = FALSE)
    ddd$cancer <- cancer
    return(ddd)
  })) %>% magrittr::set_names(tissues)

  cor_gene_stemness <- cor_gene_stemness %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  cor_gene_stemness_df <- do.call(rbind.data.frame, cor_gene_stemness)
  data <- cor_gene_stemness_df
  data$pstar <- ifelse(data$p.value < 0.05,
    ifelse(data$p.value < 0.01, "**", "*"),
    ""
  )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "cancer", y = "gene")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"), colour = "white", size = 1) +
    ggplot2::scale_fill_gradient2(low = "#2b8cbe", mid = "white", high = "#e41a1c") +
    ggplot2::geom_text(ggplot2::aes_string(label = "pstar"), col = "black", size = 5) +
    ggplot2::theme_minimal() + # 不要背景
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(), # 不要title
      axis.ticks.x = ggplot2::element_blank(), # 不要x轴
      axis.title.y = ggplot2::element_blank(), # 不要y轴
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # 调整x轴文字
      axis.text.y = ggplot2::element_text(size = 8)
    ) + # 调整y轴文字
    # 调整legen
    ggplot2::labs(fill = paste0(" * p < 0.05", "\n\n", "** p < 0.01", "\n\n", "Correlation"))
  
  if (Plot == TRUE) {
    return(p)
  } else {
    pdata = ss
    plist = list()
    plist[[1]] = p 
    plist[[2]] = pdata
    names(plist) = c("plot","pdata")
    return(plist)
  }
}

#' Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs Normal (TCGA & GTEx))
#' @import ggplot2 dplyr tibble
#' @inheritParams vis_toil_TvsN
#' @param Cancer select cancer cohort(s).
#' @return a `ggplot` object.
#' @export
#'
vis_toil_TvsN_cancer <- function(Gene = "TP53", Mode = c("Violinplot", "Dotplot"),
                                 data_type = "mRNA", Show.P.value = FALSE,
                                 Show.P.label = FALSE, Method = "wilcox.test",
                                 values = c("#DF2020", "#DDDF21"),
                                 TCGA.only = FALSE, Cancer = "ACC") {
  tcga_gtex <- load_data("tcga_gtex")

  Mode <- match.arg(Mode)
  if (!Method %in% c("wilcox.test", "t.test")) {
    stop("only support wilcox.test or t.test")
  }

  t1 <- query_pancan_value(Gene, data_type = data_type)
  unit <- switch(data_type,
    cnv = NULL,
    mutation = NULL,
    t1[[2]]
  )
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  tcga_gtex <- tcga_gtex %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::distinct(.data$sample, .keep_all = TRUE)

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2 == "tumor", ]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2 == "normal", ]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex <- t2 %>% dplyr::select("tpm", "tissue", "type2", "sample")
  tcga_gtex$type2 <- factor(tcga_gtex$type2, levels = c("tumor", "normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal), ]
  tcga_gtex_withNormal <- tcga_gtex_withNormal %>%
    dplyr::mutate(dataset = ifelse(stringr::str_sub(.data$sample, 1, 4) == "TCGA", "TCGA", "GTEX"))
  if (TCGA.only == TRUE) {
    tcga_gtex_withNormal <- tcga_gtex_withNormal %>% dplyr::filter(.data$dataset == "TCGA")
  }
  tcga_gtex_withNormal <- tcga_gtex_withNormal %>% dplyr::filter(.data$tissue %in% Cancer)
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    if (TCGA.only == TRUE) stop("Can't compare with only one group")
    message("Counting P value")
    pv <- tcga_gtex_withNormal %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method)
    pv <- pv %>% dplyr::select(c("p", "p.signif", "p.adj"))
    message("Counting P value finished")
  }
  data <- tcga_gtex_withNormal
  if (Mode == "Dotplot") {
    p <- ggpubr::ggdotplot(
      tcga_gtex_withNormal,
      x = "type2", y = "tpm", fill = "type2", color = "type2", size = 0.6, binwidth = 0.3
    ) +
      ggplot2::xlab(NULL) +
      theme_set(theme_classic(base_size = 20)) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5, size = 20)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = "none", legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::scale_color_manual(values = values)


    # p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "type2", y = "tpm", fill = "type2")) +
    #   # ggplot2::geom_boxplot() +
    #   ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", position = "identity",size = 0.6) +
    #   ggplot2::xlab(NULL) +
    #   ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
    #   ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
    #   ggplot2::guides(fill = guide_legend(title = NULL)) +
    #   ggplot2::theme(
    #     legend.background = element_blank(),
    #     legend.position = "none", legend.justification = c(0, 0)
    #   ) +
    #   ggplot2::scale_fill_manual(values = values) +
    #   ggplot2::scale_color_manual(values = values)

    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )

    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      if (TCGA.only == TRUE) stop("Can't compare with only one group")
      p <- p + ggplot2::geom_text(aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      if (TCGA.only == TRUE) stop("Can't compare with only one group")
      p <- p + ggplot2::geom_text(aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "type2", y = "tpm", fill = "type2")) +
      ggplot2::geom_violin(trim = FALSE) +
      ggplot2::geom_boxplot(width = 0.1, fill = "white") +
      ggplot2::xlab("") +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_classic(base_size = 20) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = .5, vjust = .5, size = 20)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )

    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )

    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      if (TCGA.only == TRUE) stop("Can't compare with only one group")
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      if (TCGA.only == TRUE) stop("Can't compare with only one group")
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }

  if (TCGA.only) {
    p <- p + ggplot2::ggtitle(paste0("TCGA: ", paste(Cancer, collapse = "/")))
  } else {
    p <- p + ggplot2::ggtitle(paste0("TCGA/GTEx: ", paste(Cancer, collapse = "/")))
  }

  return(p)
}

#' Visualize Gene-Gene Correlation in TCGA
#'
#' @import ggplot2 dplyr ppcor
#' @param Gene1 a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param Gene2 a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param data_type1 choose gene profile type for the first gene, including "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"
#' @param data_type2 choose gene profile type for the second gene, including "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"
#' @param purity_adj whether performing partial correlation adjusted by purity
#' @param use_regline if `TRUE`, add regression line.
#' @param filter_tumor whether use tumor sample only, default `TRUE`
#' @param alpha dot alpha.
#' @param color dot color.
#' @export
vis_gene_cor <- function(Gene1 = "CSF1R",
                         Gene2 = "JAK3",
                         data_type1 = "mRNA",
                         data_type2 = "mRNA",
                         use_regline = TRUE,
                         purity_adj = TRUE,
                         alpha = 0.5,
                         color = "#000000",
                         filter_tumor = TRUE) {
  if (!requireNamespace("cowplot")) {
    install.packages("cowplot")
  }

  if (!data_type1 %in% c("mRNA", "miRNA", "transcript", "methylation", "protein", "cnv_gistic2")) {
    stop("data_type ", data_type1, " does not support in this function!")
  }

  if (!data_type2 %in% c("mRNA", "miRNA", "transcript", "methylation", "protein", "cnv_gistic2")) {
    stop("data_type ", data_type2, " does not support in this function!")
  }

  tcga_gtex <- load_data("tcga_gtex")
  tcga_purity <- load_data("tcga_purity")

  tcga_purity$CPE <- as.numeric(tcga_purity$CPE)
  tcga_gtex <- tcga_gtex %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::distinct(.data$sample, .keep_all = TRUE)

  t1 <- query_pancan_value(Gene1, data_type = data_type1)
  if (is.list(t1)) t1 <- t1[[1]]
  t3 <- query_pancan_value(Gene2, data_type = data_type2)
  if (is.list(t3)) t3 <- t3[[1]]

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")

  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  df <- data.frame(
    sample = t2$sample,
    tissue = t2$tissue,
    type2 = t2$type2,
    gene1 = t2$tpm,
    gene2 = t4$tpm,
    stringsAsFactors = F
  )
  df %>%
    dplyr::left_join(tcga_purity, by = "sample") -> df
  if (filter_tumor == TRUE) {
    df %>% dplyr::filter(.data$type2 == "tumor") -> df
  }
  # plot refer to https://drsimonj.svbtle.com/pretty-scatter-plots-with-ggplot2

  if (purity_adj) {
    df %>% dplyr::filter(!is.na(.data$CPE)) -> df
    partial_cor_res <- ezcor_partial_cor(data = df, var1 = "gene1", var2 = "gene2", var3 = "CPE", sig_label = TRUE)
    cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2")
    p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
      ggplot2::geom_point(shape = 16, size = 1.5, show.legend = FALSE, alpha = alpha, color = color) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
      ggplot2::labs(x = Gene1, y = Gene2) +
      ggplot2::ggtitle("TCGA PANCAN dataset") +
      ggplot2::annotate(
        "text",
        -Inf, Inf,
        hjust = -0.1, vjust = 1,
        label = paste0(
          "Cor: ", round(cor_res$cor, 2),
          " ", cor_res$pstar, "\n", "Cor_adj: ",
          round(partial_cor_res$cor_partial, 2), " ",
          partial_cor_res$pstar
        ),
        size = 5, colour = "black"
      )
  } else {
    cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2")
    p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
      ggplot2::geom_point(shape = 16, size = 1.5, show.legend = FALSE, alpha = alpha, color = color) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
      ggplot2::labs(x = Gene1, y = Gene2) +
      ggplot2::ggtitle("TCGA PANCAN dataset") +
      ggplot2::annotate(
        "text",
        -Inf, Inf,
        hjust = -0.1, vjust = 1,
        label = paste0(
          "Cor: ", round(cor_res$cor, 2), " ",
          cor_res$pstar
        ),
        size = 5, colour = "black"
      )
  }



  if (use_regline) p <- p + ggplot2::geom_smooth(method = stats::lm)

  return(p)
}


#' Visualize Gene-Gene Correlation in a TCGA Cancer Type
#'
#' @import ggplot2 dplyr ppcor
#' @inheritParams vis_gene_cor
#' @param cancer_choose TCGA cohort name, e.g. "ACC".
#' @param cor_method correlation method.
#' @param use_all use all sample, default `FALSE`.
#' @export
vis_gene_cor_cancer <- function(Gene1 = "CSF1R",
                                Gene2 = "JAK3",
                                data_type1 = "mRNA",
                                data_type2 = "mRNA",
                                purity_adj = TRUE,
                                cancer_choose = "GBM",
                                use_regline = TRUE,
                                cor_method = "spearman",
                                use_all = FALSE,
                                alpha = 0.5,
                                color = "#000000") {
  if (!requireNamespace("cowplot")) {
    install.packages("cowplot")
  }

  tcga_gtex <- load_data("tcga_gtex")
  tcga_purity <- load_data("tcga_purity")

  tcga_purity$CPE <- as.numeric(tcga_purity$CPE)
  tcga_gtex <- tcga_gtex %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::distinct(.data$sample, .keep_all = TRUE)

  t1 <- query_pancan_value(Gene1, data_type = data_type1)
  if (is.list(t1)) t1 <- t1[[1]]
  t3 <- query_pancan_value(Gene2, data_type = data_type2)
  if (is.list(t3)) t3 <- t3[[1]]

  if (data_type1 == "cnv") data_type1 <- "GISTIC2 thresholded CNV"
  if (data_type1 == "cnv_gistic2") data_type1 <- "CNV"
  if (data_type2 == "cnv") data_type2 <- "GISTIC2 thresholded CNV"
  if (data_type2 == "cnv_gistic2") data_type2 <- "CNV"

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")

  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")

  # merge
  t2 <- t2 %>% inner_join(t4[, c("sample", "tpm")], by = "sample")

  df <- data.frame(
    sample = t2$sample,
    tissue = t2$tissue,
    type2 = t2$type2,
    gene1 = t2$tpm.x,
    gene2 = t2$tpm.y,
    stringsAsFactors = F
  )

  df %>%
    dplyr::left_join(tcga_purity, by = "sample") %>%
    dplyr::filter(.data$type2 == "tumor") -> df

  if (!use_all) {
    df <- df %>% dplyr::filter(.data$tissue %in% cancer_choose)
  }

  # plot refer to https://drsimonj.svbtle.com/pretty-scatter-plots-with-ggplot2

  if (purity_adj) {
    df %>% filter(!is.na(.data$CPE)) -> df
    tryCatch(
      {
        partial_cor_res <- ezcor_partial_cor(data = df, var1 = "gene1", var2 = "gene2", var3 = "CPE", sig_label = TRUE, cor_method = cor_method)
      },
      error = function(e) {
        stop("Error occurs when adjust purity, may be no purity data avaiable in this cohort.")
      }
    )
    cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2", cor_method = cor_method)
    # https://drsimonj.svbtle.com/pretty-scatter-plots-with-ggplot2
    p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
      ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE, alpha = alpha, color = color) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = Gene1, y = Gene2) +
      ggplot2::annotate(
        "text",
        -Inf, Inf,
        hjust = -0.1, vjust = 1,
        label = paste0(
          "Cor: ", round(cor_res$cor, 2), " ",
          cor_res$pstar, "\n", "Cor_adj: ",
          round(partial_cor_res$cor_partial, 2), " ", partial_cor_res$pstar
        ),
        size = 8, colour = "black"
      ) +
      ggplot2::labs(
        x = paste(Gene1, data_type1),
        y = paste(Gene2, data_type2)
      )
  } else {
    cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2", cor_method = cor_method)
    p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
      ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE, alpha = alpha, color = color) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = Gene1, y = Gene2) +
      ggplot2::annotate(
        "text",
        -Inf, Inf,
        hjust = -0.1, vjust = 1,
        label = paste0("Cor: ", round(cor_res$cor, 2), " ", cor_res$pstar),
        size = 8, colour = "black"
      ) +
      ggplot2::labs(
        x = paste(Gene1, data_type1),
        y = paste(Gene2, data_type2)
      )
  }

  if (use_all) {
    p <- p + ggplot2::ggtitle(paste0("TCGA: All data (n=", dim(df)[1], ")"))
  } else {
    p <- p + ggplot2::ggtitle(paste0("TCGA: ", paste(cancer_choose, collapse = "/"), " (n=", dim(df)[1], ")"))
  }

  if (use_regline) {
    p <- p + ggplot2::geom_smooth(method = stats::lm)
  }
  p <- p + ggplot2::theme(legend.position = "none")
  return(p)
}

#' Heatmap for Correlation between Gene and Tumor Immune Infiltration (TIL)
#'
#' @inheritParams vis_toil_TvsN
#' @param cor_method correlation method
#' @param sig Immune Signature, default: result from TIMER
#' @param Plot output the plot directly, default 'TRUE'
#' @examples
#' \dontrun{
#' p <- vis_gene_TIL_cor(Gene = "TP53")
#' }
#' @export
vis_gene_TIL_cor <- function(Gene = "TP53",
                             cor_method = "spearman",
                             data_type = "mRNA",
                             sig = c(
                               "B cell_TIMER",
                               "T cell CD4+_TIMER",
                               "T cell CD8+_TIMER",
                               "Neutrophil_TIMER",
                               "Macrophage_TIMER",
                               "Myeloid dendritic cell_TIMER"
                             ),
                             Plot = "TRUE") {
  tcga_TIL <- load_data("tcga_TIL")
  cell_type <- colnames(tcga_TIL)[-1]
  source <- sapply(stringr::str_split(cell_type, "_"), function(x) x[2])

  tcga_gtex <- load_data("tcga_gtex")

  # we filter out normal tissue
  tcga_gtex <- tcga_gtex %>% dplyr::filter(.data$type2 != "normal")

  t1 <- query_pancan_value(Gene, data_type = data_type)
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  if (data_type == "cnv") data_type <- "GISTIC2 thresholded CNV"
  if (data_type == "cnv_gistic2") data_type <- "CNV"

  message(paste0("Get data value for ", Gene))
  s <- data.frame(sample = names(t1), values = t1)

  ss <- s %>%
    dplyr::inner_join(tcga_TIL, by = c("sample" = "cell_type")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")

  sss <- split(ss, ss$tissue)
  tissues <- names(sss)
  cor_gene_immune <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    ## filter cibersort data here
    sss_can_class <- sss_can %>% select(c("sample", "values", sig))
    sss_can_class <- sss_can_class %>% tidyr::pivot_longer(cols = c(3:ncol(.)), names_to = "SetName", values_to = "score")
    sss_can_class <- sss_can_class[complete.cases(sss_can_class), ]
    cells <- unique(sss_can_class$SetName)
    cor_res_class_can <- purrr::map(cells, purrr::safely(function(i) {
      # i = cells[1]
      dd <- sss_can_class[sss_can_class$SetName == i, ]
      dd <- suppressWarnings(stats::cor.test(dd$values, dd$score, method = cor_method))
      ddd <- data.frame(gene = Gene, immune_cells = i, cor = dd$estimate, p.value = dd$p.value, stringsAsFactors = FALSE)
      return(ddd)
    })) %>% magrittr::set_names(cells)
    cor_res_class_can <- cor_res_class_can %>%
      purrr::map(~ .x$result) %>%
      purrr::compact()
    cor_res_class_can_df <- do.call(rbind.data.frame, cor_res_class_can)
    cor_res_class_can_df$cancer <- cancer
    return(cor_res_class_can_df)
  })) %>% magrittr::set_names(tissues)

  cor_gene_immune <- cor_gene_immune %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  cor_gene_immune_df <- do.call(rbind.data.frame, cor_gene_immune)
  data <- cor_gene_immune_df
  data$pstar <- ifelse(data$p.value < 0.05,
    ifelse(data$p.value < 0.001, "***", ifelse(data$p.value < 0.01, "**", "*")),
    ""
  )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "cancer", y = "immune_cells")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"), colour = "white", size = 1) +
    ggplot2::scale_fill_gradient2(low = "#377DB8", mid = "white", high = "#E31A1C") +
    ggplot2::geom_text(ggplot2::aes_string(label = "pstar"), col = "black", size = 5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(fill = paste0(" * p < 0.05", "\n\n", "** p < 0.01", "\n\n", "*** p < 0.001", "\n\n", "Correlation")) +
    ggtitle(paste0("The correlation between ", Gene, " ", data_type, " with immune signatures"))

  if (Plot == TRUE) {
    return(p)
  } else {
    pdata = ss
    plist = list()
    plist[[1]] = p 
    plist[[2]] = pdata
    names(plist) = c("plot","pdata")
    return(plist)
  }
}
