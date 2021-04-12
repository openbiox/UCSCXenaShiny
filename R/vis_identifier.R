#' Visualize Identifier-identifier Correlation
#'
#' NOTE: the dataset must be dense matrix in UCSC Xena data hubs.
#'
#' @param id1 the first molecule identifier.
#' @param id2 the second molecule identifier.
#' @param dataset1 the dataset to obtain `id1`.
#' @param dataset2 the dataset to obtain `id2`.
#' @param samples default is `NULL`, can be common sample names for two datasets.
#' @param use_ggstats if `TRUE`, use [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot) package for plotting.
#' @param use_simple_axis_label if `TRUE` (default), use simple axis labels.
#' Otherwise, data subtype will be labeled.
#' @param line_color set the color for regression line.
#' @param alpha set the alpha for dots.
#' @param ... other parameters passing to [ggscatter](http://rpkgs.datanovia.com/ggpubr/reference/ggscatter.html).
#' @export
#' @examples
#' \dontrun{
#' dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
#' id1 <- "TP53"
#' id2 <- "KRAS"
#' vis_identifier_cor(dataset, id1, dataset, id2)
#'
#' samples <- c(
#'   "TCGA-D5-5538-01", "TCGA-VM-A8C8-01",
#'   "TCGA-ZN-A9VQ-01", "TCGA-EE-A17X-06",
#'   "TCGA-05-4420-01"
#' )
#' vis_identifier_cor(dataset, id1, dataset, id2, samples)
#'
#' dataset1 <- "TCGA-BLCA.htseq_counts.tsv"
#' dataset2 <- "TCGA-BLCA.gistic.tsv"
#' id1 <- "TP53"
#' id2 <- "KRAS"
#' vis_identifier_cor(dataset1, id1, dataset2, id2)
#' }
vis_identifier_cor <- function(dataset1, id1, dataset2, id2, samples = NULL,
                               use_ggstats = FALSE,
                               use_simple_axis_label = TRUE,
                               line_color = "blue", alpha = 0.5, ...) {
  stopifnot(length(id1) == 1, length(id2) == 1)

  id1_value <- get_data(dataset1, id1)
  id2_value <- get_data(dataset2, id2)

  df <- dplyr::inner_join(
    dplyr::tibble(
      sample = names(id1_value),
      X = as.numeric(id1_value)
    ),
    dplyr::tibble(
      sample = names(id2_value),
      Y = as.numeric(id2_value)
    ),
    by = "sample"
  )

  if (!is.null(samples)) {
    df <- dplyr::filter(df, .data$sample %in% samples)
  }

  if (!use_ggstats) {
    eval(parse(text = "library(ggpubr)"))
    p <- do.call("ggscatter", list(
      data = df,
      x = "X", y = "Y",
      xlab = if (use_simple_axis_label) id1 else paste0(id1, "(", attr(id1_value, "label"), ")"),
      ylab = if (use_simple_axis_label) id2 else paste0(id2, "(", attr(id2_value, "label"), ")"),
      alpha = alpha,
      add = "reg.line",
      add.params = list(color = line_color, fill = "lightgray"),
      cor.coef = TRUE, ...
    ))
  } else {
    if (!requireNamespace("ggstatsplot")) {
      install.packages("ggstatsplot")
    }
    eval(parse(text = "library(ggstatsplot)"))
    p <- do.call("ggscatterstats", list(
      data = df,
      x = "X", y = "Y",
      xlab = if (use_simple_axis_label) id1 else paste0(id1, "(", attr(id1_value, "label"), ")"),
      ylab = if (use_simple_axis_label) id2 else paste0(id2, "(", attr(id2_value, "label"), ")"),
      ...
    ))
  }
  p
}
