#' Visualize CCLE Gene Expression
#' @import ggplot2 dplyr tibble forcats
#' @param Gene a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param data_type support genomic profile for CCLE, currently "mRNA", "protein","cnv" are supported
#' @param use_log if `TRUE`, log values.
#' @return a `ggplot` object
#' @export
vis_ccle_tpm <- function(Gene = "TP53", data_type = "mRNA", use_log = FALSE) {
  if (!requireNamespace("cowplot")) {
    install.packages("cowplot")
  }

  ccle_info <- load_data("ccle_info")

  if (!data_type %in% c("mRNA", "protein", "cnv")) {
    stop("data_type ", data_type, " does not support in this function!")
  }
  t1 <- query_pancan_value(Gene, data_type = data_type, database = "ccle")
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

  if (use_log) t1 <- log2(t1 + 1)

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info %>% dplyr::select(c("CCLE_name", "Cell_line_aliases", "Gender", "Site_Primary")),
      by = c("cell" = "CCLE_name")
    )

  p <- t2 %>% ggplot2::ggplot(aes_string(x = "Site_Primary", y = "tpm", fill = "Site_Primary")) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(paste0(
      Gene, " ",
      if (data_type == "cnv") "CNV" else data_type,
      if (data_type == "cnv") "" else paste0(" (", unit, ")")
    )) +
    cowplot::theme_cowplot() +
    ggplot2::theme(
      legend.background = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, color = "black", hjust = 1)
    )
  return(p)
}

#' Visualize CCLE Gene Expression Correlation
#' @import ggplot2 dplyr tibble forcats
#' @import ggplot2 dplyr ppcor
#' @inheritParams vis_gene_cor
#' @param cor_method correlation method
#' @param use_log_x if `TRUE`, log X values.
#' @param use_log_y if `TRUE`, log Y values.
#' @param SitePrimary select cell line origin tissue.
#' @param use_all use all sample, default `FALSE`.
#' @return a `ggplot` object
#' @export

vis_ccle_gene_cor <- function(Gene1 = "CSF1R",
                              Gene2 = "JAK3",
                              data_type1 = "mRNA",
                              data_type2 = "mRNA",
                              cor_method = "spearman",
                              use_log_x = FALSE,
                              use_log_y = FALSE,
                              use_regline = TRUE,
                              SitePrimary = "prostate",
                              use_all = FALSE,
                              alpha = 0.5, color = "#000000") {
  if (!requireNamespace("cowplot")) {
    install.packages("cowplot")
  }
  ccle_info <- load_data("ccle_info")

  if (!data_type1 %in% c("mRNA", "protein", "cnv")) {
    stop("data_type ", data_type1, " does not support in this function!")
  }
  if (!data_type2 %in% c("mRNA", "protein", "cnv")) {
    stop("data_type ", data_type2, " does not support in this function!")
  }

  t1 <- query_pancan_value(Gene1, data_type = data_type1, database = "ccle")
  unit1 <- switch(data_type1,
    cnv = NULL,
    mutation = NULL,
    t1[[2]]
  )
  if (is.list(t1)) t1 <- t1[[1]]

  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }
  if (use_log_x) t1 <- log2(t1 + 1)

  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))

  t3 <- query_pancan_value(Gene2, data_type = data_type2, database = "ccle")
  unit2 <- switch(data_type2,
    cnv = NULL,
    mutation = NULL,
    t1[[2]]
  )
  if (is.list(t3)) t3 <- t3[[1]]

  if (data_type1 == "cnv") data_type1 <- "CNV"
  if (data_type2 == "cnv") data_type2 <- "CNV"

  if (all(is.na(t3))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }
  if (use_log_y) t3 <- log2(t3 + 1)

  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))

  t2 <- t2 %>% inner_join(t4[, c("cell", "tpm")], by = "cell")

  df <- data.frame(sample = t2$cell, gene1 = t2$tpm.x, gene2 = t2$tpm.y, Site_Primary = t2$Site_Primary, stringsAsFactors = F)
  if (!use_all) {
    df <- df %>% dplyr::filter(.data$Site_Primary %in% SitePrimary)
  }
  cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2", cor_method = cor_method)

  p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
    ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE, alpha = alpha, color = color) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    ggplot2::labs(x = Gene1, y = Gene2) +
    ggplot2::annotate(
      "text",
      -Inf, Inf,
      hjust = -0.1, vjust = 1,
      label = paste0("Cor: ", round(cor_res$cor, 2), " ", cor_res$pstar),
      size = 8,
      colour = "black"
    ) +
    ggplot2::labs(
      x = paste(Gene1, data_type1),
      y = paste(Gene2, data_type2)
    )

  if (use_regline) p <- p + ggplot2::geom_smooth(method = stats::lm)

  return(p)
}
