#' Visualize Identifier-identifier Correlation
#' 
#' NOTE: the dataset must be dense matrix in UCSC Xena data hubs.
#'
#' @param id1 the first molecule identifier.
#' @param id2 the second molecule identifier.
#' @param dataset1 the dataset to obtain `id1`.
#' @param dataset2 the dataset to obtain `id2`.
#' @param samples default is `NULL`, can be common sample names for two datasets.
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
#' }
vis_identifier_cor <- function(
  dataset1, id1, dataset2, id2, samples = NULL, 
  line_color = "blue", alpha = 0.5, ...) {
  stopifnot(length(id1) == 1, length(id2) == 1)

  id1_value <- get_data(dataset1, id1)
  id2_value <- get_data(dataset2, id2)

  df <- dplyr::inner_join(
    dplyr::tibble(
      name = names(id1_value),
      x = as.numeric(id1_value)
    ),
    dplyr::tibble(
      name = names(id2_value),
      Y = as.numeric(id2_value)
    ),
    by = "name"
  )
  colnames(df) <- c("sample", id1, id2)

  if (!is.null(samples)) {
    df <- dplyr::filter(df, .data$sample %in% samples)
  }

  eval(parse(text = "library(ggpubr)"))
  p <- do.call("ggscatter", list(
    data = df,
    x = id1, y = id2,
    alpha = alpha,
    add = "reg.line",
    add.params = list(color = line_color, fill = "lightgray"),
    cor.coef = TRUE, ...
  ))
  p
}
