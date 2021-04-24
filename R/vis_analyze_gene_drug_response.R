#' Visualize Gene and Drug-Target Association
#'
#' See [analyze_gene_drug_response_asso] for examples.
#'
#' @export
#' @param output_form `plotly` or `ggplot2`.
#' @inheritParams vis_toil_TvsN
#' @return `plotly` or `ggplot2` object.
vis_gene_drug_response_asso <- function(Gene = "TP53", 
                                        x_axis_type = c("mean.diff", "median.diff"),
                                        output_form = "plotly") {
  x_axis_type <- match.arg(x_axis_type)
  if (!requireNamespace("plotly")) install.packages("plotly")
  if (!requireNamespace("ggrepel")) install.packages("ggrepel")
  
  df <- analyze_gene_drug_response_asso(Gene)
  df$drugs_targets <- paste0(df$drugs, "->", df$Target)
  df$cor_type <- ifelse(df$cor >= 0, "pos", "neg")
  
  df$fdr_log <- -log10(df$fdr)
  df$text <- paste(
    "Gene: ", df$genes,
    "<br>Correlation: ", round(df$cor, digits = 3),
    "<br>Drugs: ", df$drugs,
    "<br>Target: ", df$Target,
    "<br>FDR: ", round(df$fdr, digits = 3),
    "<br>Number of Cells: ", df$num_of_cells
  )
  
  p <- ggplot(data = df, aes_string(
    x = x_axis_type,
    y = "fdr_log",
    size = "cor",
    color = "cor_type",
    text = "text"
  )) +
    geom_point() +
    ggtitle(paste0(unique(df$genes), " and Drug-Target Response Association")) +
    labs(x = if (x_axis_type == "mean.diff") {
      "Mean of expression difference between high and low IC50 cells"
    } else {
      "Median of expression difference between high and low IC50 cells"
    }, y = "-log10(FDR)") +
    theme_minimal(base_size = 15) +
    scale_color_manual(values = c("#377EB8", "#E41A1C")) +
    scale_size(range = c(1, 5)) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  if (output_form == "plotly") p <- plotly::ggplotly(p, tooltip = "text")
  return(p)
}


#' Visualize Gene and Drug Response Difference
#' 
#' See [analyze_gene_drug_response_diff] for examples.
#' 
#' @export
#' @inheritParams vis_toil_TvsN
#' @param tissue select cell line origin tissue.
#' @return a `ggplot` object.
vis_gene_drug_response_diff <- function(Gene = "TP53", tissue = "prostate",
                                        Show.P.value = TRUE, Show.P.label = TRUE, 
                                        Method = "wilcox.test") {
  df <- analyze_gene_drug_response_diff(Gene, tissue = tissue)

  p <- df %>% ggplot(aes_string(x = "drug", y = "IC50", color = "group")) +
    geom_boxplot() +
    labs(x = "Drug", y = "IC50 (uM)") +
    ggpubr::rotate_x_text(45)
    
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- df %>%
      ggpubr::compare_means(IC50 ~ group, data = ., method = Method, group.by = "drug")
    pv <- pv %>% dplyr::select(c("drug", "p", "p.signif", "p.adj"))
    message("Counting P value finished")
  }

  if (Show.P.value == TRUE & Show.P.label == TRUE) {
    p <- p + ggplot2::geom_text(aes(
      x = .data$drug,
      y = max(df$IC50) * 1.1,
      label = .data$p.signif
    ),
    data = pv,
    inherit.aes = FALSE
    )
  }
  return(p)
}