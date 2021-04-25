#' Visualize Gene and Drug-Target Association with CCLE Data
#'
#' See [analyze_gene_drug_response_asso] for examples.
#'
#' @export
#' @param output_form `plotly` or `ggplot2`.
#' @inheritParams vis_toil_TvsN
#' @return `plotly` or `ggplot2` object.
vis_gene_drug_response_asso <- function(Gene = "TP53",
                                        x_axis_type = c("mean.diff", "median.diff"),
                                        output_form = c("plotly", "ggplot2")) {
  x_axis_type <- match.arg(x_axis_type)
  output_form <- match.arg(output_form)

  if (!requireNamespace("plotly")) install.packages("plotly")
  if (!requireNamespace("ggrepel")) install.packages("ggrepel")

  df <- analyze_gene_drug_response_asso(Gene)
  df$cor_type <- ifelse(df$cor >= 0, "pos", "neg")
  df$cor_abs <- abs(round(df$cor, digits = 3))

  df$fdr_log <- -log10(df$fdr)
  df$text <- paste(
    "Gene: ", df$genes,
    "<br>Correlation: ", round(df$cor, digits = 3),
    "<br>Drug: ", df$drugs,
    "<br>Target: ", df$Target,
    "<br>FDR: ", round(df$fdr, digits = 3),
    "<br>Number of Cell Lines: ", df$num_of_cells
  )

  p <- ggplot(data = df, aes_string(
    x = x_axis_type,
    y = "fdr_log",
    size = "cor_abs",
    color = "cor_type",
    text = "text"
  )) +
    geom_point() +
    ggtitle(paste0(
      if (length(Gene) > 1) {
        paste0("Signature (", paste(unique(df$genes), collapse = "&"), ")")
      } else {
        unique(df$genes)
      },
      " and Drug-Target Response Association"
    )) +
    labs(x = if (x_axis_type == "mean.diff") {
      "Mean of expression difference between high and low IC50 cell lines"
    } else {
      "Median of expression difference between high and low IC50 cell lines"
    }, y = "-log10(FDR)") +
    cowplot::theme_cowplot() +
    scale_color_manual(values = c("#377EB8", "#E41A1C")) +
    scale_size(range = c(0.1, 4)) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
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
#' @return a `ggplot` object.
vis_gene_drug_response_diff <- function(Gene = "TP53", tissue = "lung",
                                        Show.P.label = TRUE,
                                        Method = "wilcox.test",
                                        values = c("#DF2020", "#DDDF21")) {
  
  # tissue_list <- c("prostate", "central_nervous_system", "urinary_tract", "haematopoietic_and_lymphoid_tissue", 
  #                  "kidney", "thyroid", "soft_tissue", "skin", "salivary_gland", 
  #                  "ovary", "lung", "bone", "endometrium", "pancreas", "breast", 
  #                  "large_intestine", "upper_aerodigestive_tract", "autonomic_ganglia", 
  #                  "stomach", "liver", "biliary_tract", "pleura", "oesophagus")
  
  df <- analyze_gene_drug_response_diff(Gene, tissue = tissue)
  
  if (Show.P.label) {
    message("Counting P value")
    pv <- ggpubr::compare_means(IC50 ~ group, data = df, method = Method, group.by = "drug_target")
    pv <- pv %>% dplyr::select(c("drug_target", "p", "p.signif", "p.adj")) %>% 
      dplyr::arrange(.data$p)
    pv$drug_target <- factor(pv$drug_target, levels = unique(pv$drug_target))
    df$drug_target <- factor(df$drug_target, levels = unique(pv$drug_target))
    message("Counting P value finished")
  }

  p <- df %>%
    ggplot(aes_string(x = "drug_target", y = "IC50", color = "group")) +
    geom_boxplot(width = 0.3) +
    facet_wrap(~drug_target, scales = "free") +
    labs(x = "Drug -> Target", y = "IC50 (uM)")

  if (Show.P.label) {
    p <- p + ggplot2::geom_text(aes(
      x = .data$drug_target,
      y = max(df$IC50) * 1.1,
      label = .data$p.signif
    ),
    data = pv,
    inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::scale_color_manual(values = values) +
    cowplot::theme_cowplot() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(p)
}
