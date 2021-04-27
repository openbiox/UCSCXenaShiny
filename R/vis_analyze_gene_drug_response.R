#' Visualize Gene and Drug-Target Association with CCLE Data
#'
#' See [analyze_gene_drug_response_asso] for examples.
#'
#' @export
#' @param output_form `plotly` or `ggplot2`.
#' @inheritParams vis_toil_TvsN
#' @param x_axis_type set the value type for X axis.
#' @return `plotly` or `ggplot2` object.
vis_gene_drug_response_asso <- function(Gene = "TP53",
                                        x_axis_type = c("mean.diff", "median.diff"),
                                        output_form = c("plotly", "ggplot2")) {
  x_axis_type <- match.arg(x_axis_type)
  output_form <- match.arg(output_form)

  if (!requireNamespace("plotly")) install.packages("plotly")
  if (!requireNamespace("ggrepel")) install.packages("ggrepel")

  df <- analyze_gene_drug_response_asso(Gene, combine = TRUE) # Combine as a signature if more than 1 gene

  df$p_log <- -log10(df$p.value)
  df$text <- paste(
    "Gene(s): ", paste(Gene, collapse = "/"),
    "<br>Drug: ", df$drugs,
    "<br>Target: ", df$Target,
    "<br>Correlation: ", round(df$cor, digits = 3),
    "<br><i>P</i> value: ", round(df$p.value, digits = 3),
    "<br>FDR: ", round(df$fdr, digits = 3),
    "<br>Number of Cell Lines: ", df$num_of_cell_lines
  )

  p <- ggplot(data = df, aes_string(
    x = x_axis_type,
    y = "p_log",
    color = "cor",
    text = "text"
  )) +
    geom_point() +
    ggtitle(paste0(
      if (length(Gene) > 1) {
        paste0("Signature (", paste(Gene, collapse = "&"), ")")
      } else {
        Gene
      },
      " and Drug-Target Response Association"
    )) +
    labs(x = if (x_axis_type == "mean.diff") {
      "Mean of expression difference between high and low IC50 cell lines"
    } else {
      "Median of expression difference between high and low IC50 cell lines"
    }, y = "-log10(P-value)") +
    cowplot::theme_cowplot() +
    scale_color_gradient2(low = scales::muted("blue"), high = scales::muted("red"), midpoint = 0) +
    theme(
      plot.title = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = -log10(0.05), linetype = 2, size = 0.5, alpha = 0.5)
  if (output_form == "plotly") p <- plotly::ggplotly(p, tooltip = "text")
  return(p)
}


#' Visualize Gene and Drug Response Difference with CCLE Data
#'
#' See [analyze_gene_drug_response_diff] for examples.
#'
#' @export
#' @inheritParams vis_toil_TvsN
#' @param tissue select cell line origin tissue.
#' @param alpha set alpha for dots.
#' @return a `ggplot` object.
vis_gene_drug_response_diff <- function(Gene = "TP53", tissue = "lung",
                                        Show.P.label = TRUE,
                                        Method = "wilcox.test",
                                        values = c("#DF2020", "#DDDF21"),
                                        alpha = 0.5) {
  df <- analyze_gene_drug_response_diff(Gene, tissue = tissue, combine = TRUE) # Combine as a signature if more than 1 gene

  if (Show.P.label) {
    message("Counting P value")
    pv <- ggpubr::compare_means(IC50 ~ group, data = df, method = Method, group.by = "drug_target")
    pv <- pv %>%
      dplyr::arrange(.data$p)
    pv$drug_target <- factor(pv$drug_target, levels = unique(pv$drug_target))
    df$drug_target <- factor(df$drug_target, levels = unique(pv$drug_target))
    message("Counting P value finished")
    pv$y.position <- 8.5
  }

  p <- ggpubr::ggdotplot(
    df,
    x = "group", y = "IC50", color = "group", fill = "group",
    add = "mean_sd", facet.by = "drug_target", alpha = alpha, size = 0.6
  ) +
    labs(x = "Drug -> Target", y = "IC50 (uM)")

  if (Show.P.label) {
    p <- p + ggpubr::stat_pvalue_manual(pv, label = "p.signif", tip.length = 0.01) +
      ggplot2::scale_y_continuous(limits = c(0, 10))
  }

  p <- p +
    ggplot2::scale_color_manual(values = values) +
    ggplot2::scale_fill_manual(values = values) +
    cowplot::theme_cowplot() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = c(0.85, 0.1)
    )
  return(p)
}
