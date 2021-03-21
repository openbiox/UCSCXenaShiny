#' Visualize single gene expression from toil data hub
#' @inheritParams ggpubr::ggboxplot
#' @param angle.x angle for x lab
#' @param ... other parameters passing to [ggpubr::ggboxplot()]
#' @return a `ggplot` object
#' @export
vis_toil_gene <- function(data, x = "primary_site",
                          y = "expression",
                          color = "sample_type",
                          palette = "jco",
                          xlab = "Primary site",
                          ylab = "Expression",
                          title = NULL,
                          facet.by = NULL,
                          angle.x = 45,
                          ...) {
  ggpubr::ggboxplot(data,
    x = x, y = y, color = color,
    palette = palette, xlab = xlab, ylab = ylab,
    title = title, facet.by = NULL, ...
  ) +
    ggpubr::rotate_x_text(angle = angle.x)
}

# Visualize Pan-cancer TPM (Tumor (TCGA) vs normal (TCGA & GTEX))
#' Visualize single gene expression from toil data hub
#' @import ggplot2 dplyr tibble
#' @param Gene Gene symbal for comparision
#' @param Mode Boxplot or Violinplot to represent data
#' @param Show.P.value `TRUE` or `FALSE` whether to count P value
#' @param Method default method is wilcox.test
#' @param Show.P.label `TRUE` or `FALSE` present p value with number or label `*`, `**`, `***` and `****`
#' @param values the color to fill tumor or normal
#' @param TCGA.only include samples only from TCGA dataset
#' @return a `ggplot` object
#' @examples
#' \donttest{
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = FALSE, Show.P.label = FALSE)
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = FALSE, Show.P.label = FALSE)
#' }
#' @export
#'
vis_toil_TvsN <- function(Gene = "TP53", Mode = "Boxplot", Show.P.value = TRUE, Show.P.label = TRUE, Method = "wilcox.test", values = c("#DF2020", "#DDDF21"), TCGA.only = FALSE) {
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())

  t1 <- get_pancan_gene_value(identifier = Gene)$expression

  tcga_gtex <- tcga_gtex %>% dplyr::group_by(.data$tissue) %>% dplyr::distinct(.data$sample, .keep_all = TRUE)
  
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
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
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
    print(p)
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "tissue", y = "tpm", fill = "type2")) +
      geom_split_violin(
        draw_quantiles = c(0.25, 0.5, 0.75),
        trim = TRUE,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      ) +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
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
      draw_quantiles = c(0.25, 0.5, 0.75),
      trim = TRUE,
      linetype = "solid",
      color = "black",
      size = 0.2,
      na.rm = TRUE,
      position = "identity"
    ) +
      geom_split_violin(
        data = tcga_gtex_UVM,
        mapping = ggplot2::aes_string(x = "tissue", y = "tpm", fill = "type2"),
        draw_quantiles = c(0.25, 0.5, 0.75),
        trim = TRUE,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      ) +
      ggplot2::scale_x_discrete(limits = levels(tcga_gtex$tissue))
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
    print(p)
  }
  return(p)
}

#' Visualize single gene uni-cox result
#'
#' @inheritParams vis_toil_TvsN
#' @param measure a survival measure, e.g. "OS".
#' @param threshold a expression cutoff, `0.5` for median.
#' @return a `ggplot` object
#' @examples
#' \donttest{
#' p <- vis_unicox_tree(Gene = "TP53")
#' }
#' @export
vis_unicox_tree <- function(Gene = "TP53", measure = "OS", threshold = 0.5, values = c("grey", "#E31A1C", "#377DB8")) {
  ## 写在 R 内的数据集需要更严格的引用方式
  data("toil_surv", package = "UCSCXenaShiny", envir = environment())
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())

  t1 <- get_pancan_gene_value(Gene)$expression
  
  #we filter out normal tissue
  tcga_gtex = tcga_gtex %>% dplyr::filter(.data$type2 != "normal")
  
  message(paste0("Get gene expression for ", Gene))
  s <- data.frame(sample = names(t1), values = t1)
  ## we use median cutoff here
  ss <- s %>%
    dplyr::inner_join(toil_surv, by = "sample") %>%
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

    if (measure == "OS") {
      unicox_res_genes <- ezcox::ezcox(sss_can,
        covariates = "values",
        time = "OS.time",
        status = "OS",
        verbose = FALSE
      )
    }

    if (measure == "PFI") {
      unicox_res_genes <- ezcox::ezcox(sss_can,
        covariates = "values",
        time = "PFI.time",
        status = "PFI",
        verbose = FALSE
      )
    }

    if (measure == "DSS") {
      unicox_res_genes <- ezcox::ezcox(sss_can,
        covariates = "values",
        time = "DSS.time",
        status = "DSS",
        verbose = FALSE
      )
    }

    if (measure == "DFI") {
      unicox_res_genes <- ezcox::ezcox(sss_can,
        covariates = "values",
        time = "DFI.time",
        status = "DFI",
        verbose = FALSE
      )
    }

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
    ggplot2::labs(x = "", y = "log Hazard Ratio") +
    ggplot2::theme(
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggplot2::scale_color_manual(values = values)
  return(p)
}

#' Visualize single gene expression in anatomy location
#'
#' @inheritParams vis_toil_TvsN
#' @inheritParams ggplot2::scale_colour_viridis_d
#' @param Gender a string, "Female" (default) or "Male".
#' @return a `ggplot` object
#' @importFrom stats complete.cases median
#' @export

vis_pancan_anatomy <- function(Gene = "TP53", Gender = c("Female", "Male"), option = "D") {
  Gender <- match.arg(Gender)

  if (!requireNamespace("gganatogram")) {
    stop("Please install 'gganatogram' package firstly!")
  }

  # data("t1", package = "UCSCXenaShiny", envir = environment())
  data("TCGA.organ", package = "UCSCXenaShiny", envir = environment())
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  tcga_gtex <- tcga_gtex %>% dplyr::distinct(sample, .keep_all = TRUE)

  t1 <- get_pancan_gene_value(identifier = Gene)$expression

  message(paste0("Get gene expression for ", Gene))
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
    dplyr::full_join(gganatogram::hgMale_key, by = "organ") %>%
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
    dplyr::full_join(gganatogram::hgFemale_key, by = "organ") %>%
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
    p1 <- gganatogram::gganatogram(
      data = Male_input,
      fillOutline = "white",
      organism = "human",
      sex = "male",
      fill = "value"
    ) +
      facet_wrap(~type) +
      # scale_fill_gradient(low = "white", high = "red") +
      labs(fill = "Log2(TPM + 0.001)") +
      coord_cartesian(ylim = c(-120, 0)) +
      theme_void() +
      scale_fill_viridis_c(option = option) +
      ggtitle(paste0(Gene, " Male: TCGA + GTEX")) +
      theme(plot.title = element_text(hjust = 0.5))

    p1
    return(p1)
  }

  if (Gender == "Female") {
    p2 <- gganatogram::gganatogram(
      data = Female_input,
      fillOutline = "white",
      organism = "human",
      sex = "female",
      fill = "value"
    ) +
      facet_wrap(~type) +
      # scale_fill_gradient(low = "white", high = "red") +
      labs(fill = "Log2(TPM + 0.001)") +
      coord_cartesian(ylim = c(-120, 0)) +
      theme_void() +
      scale_fill_viridis_c(option = option) +
      ggtitle(paste0(Gene, " Female: TCGA + GTEX")) +
      theme(plot.title = element_text(hjust = 0.5))
    p2
    return(p2)
  }
}

#' Heatmap visualization (correlation between immune signatures and gene)
#'
#' @inheritParams vis_toil_TvsN
#' @param Cor_method correlation method
#' @param Immune_sig_type quantification method, default is "Cibersort"
#' @examples
#' \donttest{
#' p <- vis_gene_immune_cor(Gene = "TP53")
#' }
#' @export
vis_gene_immune_cor <- function(Gene = "TP53", Cor_method = "spearman", Immune_sig_type = "Cibersort") {
  data("tcga_pan_immune_signature", package = "UCSCXenaShiny", envir = environment())
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  
  #we filter out normal tissue
  tcga_gtex = tcga_gtex %>% dplyr::filter(.data$type2 != "normal")
  
  tcga_pan_immune_signature <- tcga_pan_immune_signature %>%
    tidyr::pivot_longer(3:ncol(.), names_to = "sample", values_to = "score") %>%
    dplyr::mutate(sample = stringr::str_sub(.data$sample, 1, 15))

  t1 <- get_pancan_gene_value(identifier = Gene)$expression

  message(paste0("Get gene expression for ", Gene))
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
      dd <- stats::cor.test(dd$values, dd$score, method = Cor_method)
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
    ggtitle(paste0("The correlation between ", Gene, " with immune signatures"))
  print(p)
  return(p)
}


#' Visualize TMB and gene
#'
#' @inheritParams vis_gene_immune_cor
#' @examples
#' \donttest{
#' p <- vis_gene_tmb_cor(Gene = "TP53")
#' }
#' @export
vis_gene_tmb_cor <- function(Gene = "TP53", Cor_method = "spearman") {
  data("tcga_tmb", package = "UCSCXenaShiny", envir = environment())
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>%
    dplyr::inner_join(tcga_tmb, by = c("sample" = "Tumor_Sample_ID")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)

  cor_gene_tmb <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd <- stats::cor.test(sss_can$values, sss_can$Non_silent_per_Mb, type = Cor_method)
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
  print(p)
  return(p)
}

#' Visualize gene and stemness
#'
#' @inheritParams vis_gene_immune_cor
#' @examples
#' \donttest{
#' p <- vis_gene_stemness_cor(Gene = "TP53")
#' }
#' @export
vis_gene_stemness_cor <- function(Gene = "TP53", Cor_method = "spearman") {
  data("tcga_stemness", package = "UCSCXenaShiny", envir = environment())
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>%
    dplyr::inner_join(tcga_stemness, by = c("sample")) %>%
    dplyr::inner_join(tcga_gtex[, c("tissue", "sample")], by = "sample")
  sss <- split(ss, ss$tissue)
  tissues <- names(sss)

  cor_gene_stemness <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd <- stats::cor.test(sss_can$values, sss_can$RNAss, type = Cor_method)
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
  print(p)
  return(p)
}

# Visualize Single cancer TPM (Tumor (TCGA) vs normal (TCGA & GTEX))
#' Visualize single gene expression from toil data hub (Single cancer type)
#' @import ggplot2 dplyr tibble
#' @param Gene Gene symbal for comparision
#' @param Mode Boxplot or Violinplot to represent data
#' @param Show.P.value `TRUE` or `FALSE` whether to count P value
#' @param Method default method is wilcox.test
#' @param Show.P.label `TRUE` or `FALSE` present p value with number or label `*`, `**`, `***` and `****`
#' @param values the color to fill tumor or normal
#' @param TCGA.only include samples only from TCGA dataset
#' @param Cancer select a cancer
#' @return a `ggplot` object
#' @export
#'
vis_toil_TvsN_cancer <- function(Gene = "TP53", Mode = "Violinplot", Show.P.value = TRUE, Show.P.label = TRUE, Method = "wilcox.test", values = c("#DF2020", "#DDDF21"), TCGA.only = FALSE, Cancer = "ACC") {
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  
  t1 <- get_pancan_gene_value(identifier = Gene)$expression
  
  tcga_gtex <- tcga_gtex %>% dplyr::group_by(.data$tissue) %>% dplyr::distinct(.data$sample, .keep_all = TRUE)
  
  #tcga_gtex <- tcga_gtex %>% dplyr::distinct(sample, .keep_all = TRUE)
  
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
  tcga_gtex_withNormal = tcga_gtex_withNormal %>% dplyr::filter(.data$tissue == Cancer)
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- tcga_gtex_withNormal %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method)
    pv <- pv %>% dplyr::select(c("p", "p.signif", "p.adj"))
    message("Counting P value finished")
  }
  data = tcga_gtex_withNormal
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "type2", y = "tpm", fill = "type2")) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_dotplot(binaxis='y', stackdir='center',position = "identity") +
      #ggplot2::geom_jitter(aes_string(color = "type2"),shape=16, position=position_jitter(0.2), size = 2) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = "none", legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values)+
      ggplot2::scale_color_manual(values = values)
    # p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO) +
    #   ggplot2::geom_boxplot(data = tcga_gtex_UVM)
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
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
      p <- p + ggplot2::geom_text(aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    print(p)
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes_string(x = "type2", y = "tpm", fill = "type2")) +
      ggplot2::geom_violin(trim=FALSE) +
      ggplot2::geom_boxplot(width=0.1, fill="white") +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::xlab("") +
      # ggplot2::ggtitle(.data$tissue) +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_set(ggplot2::theme_classic(base_size = 20)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
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
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = 1.5,
        y = max(data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    print(p)
  }
  return(p)
}

#' Visualize Gene-gene correlation in TCGA dataset
#' 
#' @import ggplot2 dplyr ppcor
#' @param Gene1 the first gene
#' @param Gene2 the second gene
#' @param purity_adj whether performing partial correlation adjusted by purity
#' @param split whether split by TCGA tumor tissue
#' @export
vis_gene_cor <- function(Gene1 = "CSF1R", Gene2 = "JAK3", purity_adj = TRUE, split = FALSE){
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  data("tcga_purity", package = "UCSCXenaShiny", envir = environment())
  tcga_purity$CPE = as.numeric(tcga_purity$CPE)
  tcga_gtex <- tcga_gtex %>% dplyr::group_by(.data$tissue) %>% dplyr::distinct(.data$sample, .keep_all = TRUE)
  t1 <- get_pancan_gene_value(identifier = Gene1)$expression
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  t3 <- get_pancan_gene_value(identifier = Gene2)$expression
  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  df <- data.frame(sample = t2$sample,tissue = t2$tissue, type2 = t2$type2, gene1 = t2$tpm, gene2 = t4$tpm,stringsAsFactors = F)
  df %>% dplyr::left_join(tcga_purity,by = "sample") %>% filter(type2 == "tumor") -> df
  #plot refer to https://drsimonj.svbtle.com/pretty-scatter-plots-with-ggplot2
  if(split == FALSE){
    if(purity_adj == TRUE){
      df %>% filter(!is.na(CPE)) -> df
      partial_cor_res <- ezcor_partial_cor(data = df, var1 = "gene1", var2 = "gene2", var3 = "CPE",sig_label = TRUE)
      cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2")
      df$pc <- predict(prcomp(~gene1+gene1, df))[,1]
      x= quantile(df$gene1)[1]; y = quantile(df$gene2)[5]
      p <- ggplot2::ggplot(df, aes(gene1, gene2,color = pc)) +
        ggplot2::geom_point(shape = 16, size = 1.5, show.legend = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
        ggplot2::labs(x = Gene1, y = Gene2) +
        ggplot2::ggtitle("TCGA PANCAN dataset") +
        ggplot2::annotate("text", label = paste0("Cor: ", round(cor_res$cor,2), " ",cor_res$pstar,"\n","Cor_adj: ", round(partial_cor_res$cor_partial,2)," ",partial_cor_res$pstar),x = x + 1, y = y, size = 4, colour = "black")
    } else{
      cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2")
      df$pc <- predict(prcomp(~gene1+gene1, df))[,1]
      x= quantile(df$gene1)[1]; y = quantile(df$gene2)[5]
      p <- ggplot2::ggplot(df, aes(gene1, gene2,color = pc)) +
        ggplot2::geom_point(shape = 16, size = 1.5, show.legend = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
        ggplot2::labs(x = Gene1, y = Gene2) +
        ggplot2::ggtitle("TCGA PANCAN dataset") +
        ggplot2::annotate("text", label = paste0("Cor: ", round(cor_res$cor,2), " ",cor_res$pstar),x = x + 1, y = y, size = 4, colour = "black")
    }
    
  }
  return(p)
}








# Global variables --------------------------------------------------------

utils::globalVariables(
  c(
    ".",
    "tcga_gtex",
    "tcga_stemness",
    "tcga_tmb",
    "toil_surv",
    "TCGA.organ",
    "Type",
    "group"
  )
)
