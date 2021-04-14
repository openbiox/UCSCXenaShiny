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
#' @return a (gg)plot object.
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

#' Visualize Correlation for Multiple Identifiers
#'
#' NOTE: the dataset must be dense matrix in UCSC Xena data hubs.
#'
#' @inheritParams ggstatsplot::ggcorrmat
#' @inheritParams vis_identifier_cor
#' @param ids the molecule identifiers.
#' @param dataset the dataset to obtain identifiers.
#' @param color_low the color code for lower value mapping.
#' @param color_high the color code for higher value mapping.
#' @param ... other parameters passing to [ggstatsplot::ggcorrmat].
#' @export
#' @return a (gg)plot object.
#' @examples
#' \dontrun{
#' dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
#' ids <- c("TP53", "KRAS", "PTEN")
#' vis_identifier_multi_cor(dataset, ids)
#' }
vis_identifier_multi_cor <- function(dataset, ids, samples = NULL,
                                     matrix.type = c("full", "upper", "lower"),
                                     type = c("parametric", "nonparametric", "robust", "bayes"),
                                     partial = FALSE,
                                     sig.level = 0.05,
                                     p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                     color_low = "#E69F00",
                                     color_high = "#009E73",
                                     ...) {
  stopifnot(length(ids) >= 2)
  colors <- c(color_low, "white", color_high)
  matrix.type <- match.arg(matrix.type)
  type <- match.arg(type)
  p.adjust.method <- match.arg(p.adjust.method)

  df <- purrr::map(ids, function(x) {
    message("Querying data of identifier ", x, " from dataset: ", dataset)
    data <- get_data(dataset, x)
    data <- dplyr::tibble(
      sample = names(data),
      y = as.numeric(data)
    )
    colnames(data)[2] <- x
    data
  }) %>%
    purrr::reduce(dplyr::full_join, by = "sample")

  if (!is.null(samples)) {
    df <- dplyr::filter(df, .data$sample %in% samples)
  }

  if (!requireNamespace("ggstatsplot")) {
    install.packages("ggstatsplot")
  }

  p <- ggstatsplot::ggcorrmat(
    data = df,
    matrix.type = matrix.type,
    type = type,
    partial = partial,
    sig.level = sig.level,
    p.adjust.method = p.adjust.method,
    ...
  )

  p
}

#' Visualize Comparison of an Molecule Identifier between Groups
#'
#' NOTE: the dataset must be dense matrix in UCSC Xena data hubs.
#'
#' @inheritParams ggstatsplot::ggbetweenstats
#' @inheritParams vis_identifier_cor
#' @param id the molecule identifier.
#' @param dataset the dataset to obtain identifiers.
#' @param fun_type select the function to compare groups.
#' @param grp_df When `dataset` and `id` are all not `NULL`, it should be a `data.frame` with 2 or 3 columns.
#' - The first column refers to sample ID.
#' - The second column refers to groups indicated in axis X.
#' - The third column is optional, which indicates facet variable.
#' When any of `dataset` and `id` is `NULL`, it should be a `data.frame` with 3 or 4 columns.
#' - The first column refers to sample ID.
#' - The second column refers to values indicated in axis Y.
#' - The third column refers to groups indicated in axis X.
#' - The fourth column is optional, which indicates facet variable.
#' @param ... other parameters passing to [ggstatsplot::ggbetweenstats] or [ggstatsplot::ggwithinstats].
#' @export
#' @return a (gg)plot object.
#' @examples
#' \dontrun{
#' library(UCSCXenaTools)
#' expr_dataset <- "TCGA.LUAD.sampleMap/HiSeqV2_percentile"
#' cli_dataset <- "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
#' id <- "TP53"
#' cli_df <- XenaGenerate(
#'   subset = XenaDatasets == "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
#' ) %>%
#'   XenaQuery() %>%
#'   XenaDownload() %>%
#'   XenaPrepare()
#'   
#' # group data.frame with 2 columns
#' vis_identifier_grp_comparison(expr_dataset, id, cli_df[, c("sampleID", "gender")])
#' # group data.frame with 3 columns
#' vis_identifier_grp_comparison(
#'   expr_dataset, id,
#'   cli_df[, c("sampleID", "pathologic_M", "gender")] %>%
#'     dplyr::filter(pathologic_M %in% c("M0", "MX"))
#' )
#' 
#' # When not use the value of `identifier` from `dataset`
#' vis_identifier_grp_comparison(grp_df = cli_df[, c(1, 2, 71)])
#' vis_identifier_grp_comparison(grp_df = cli_df[, c(1, 2, 71, 111)])
#' }
#' 
vis_identifier_grp_comparison <- function(dataset = NULL, id = NULL, grp_df, samples = NULL,
                                          fun_type = c("betweenstats", "withinstats"),
                                          type = c("parametric", "nonparametric", "robust", "bayes"),
                                          pairwise.comparisons = TRUE,
                                          p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                          ggtheme = cowplot::theme_cowplot(),
                                          ...) {
  stopifnot(ncol(grp_df) > 1)
  fun_type <- match.arg(fun_type)
  type <- match.arg(type)
  p.adjust.method <- match.arg(p.adjust.method)
  colnames(grp_df)[1] <- "sample"

  if (!is.null(dataset) && !is.null(id)) {
    message("Querying data of identifier ", id, " from dataset ", dataset, " for comparison")
    id_value <- get_data(dataset, id)
    df <- dplyr::tibble(
      sample = names(id_value),
      X = as.numeric(id_value)
    )
    colnames(df)[2] <- id
    
    df <- dplyr::inner_join(df, grp_df, by = "sample")
    
    do_grp <- ncol(grp_df) >= 3
  } else {
    message("Directly use 'grp_df' for comparison analysis.")
    df <- grp_df 
    do_grp <- ncol(grp_df) >= 4
  }

  if (!is.null(samples)) {
    df <- dplyr::filter(df, .data$sample %in% samples)
  }

  if (!requireNamespace("ggstatsplot")) {
    install.packages("ggstatsplot")
  }

  if (do_grp) {
    fun <- if (fun_type == "betweenstats") ggstatsplot::grouped_ggbetweenstats else ggstatsplot::grouped_ggwithinstats

    p <- fun(
      data = df,
      x = !!colnames(df)[3],
      y = !!colnames(df)[2],
      grouping.var = !!colnames(df)[4],
      type  = type,
      pairwise.comparisons = pairwise.comparisons,
      p.adjust.method = p.adjust.method,
      ggtheme = ggtheme,
      ...
    )
  } else {
    fun <- if (fun_type == "betweenstats") ggstatsplot::ggbetweenstats else ggstatsplot::ggwithinstats

    p <- fun(
      data = df,
      x = !!colnames(df)[3],
      y = !!colnames(df)[2],
      type  = type,
      pairwise.comparisons = pairwise.comparisons,
      p.adjust.method = p.adjust.method,
      ggtheme = ggtheme,
      ...
    )
  }

  p
}
