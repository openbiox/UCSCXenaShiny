#' Visualize gene and drug-target association
#' @export
#' @inheritParams vis_toil_TvsN
vis_gene_drug_response_asso <- function(Gene = "TP53", output_form = "plotly"){
  df <- analyze_gene_drug_response_asso(Gene)
  if(!requireNamespace("ggrepel")){stop("please install ggrepel")}
  df$drugs_targets = paste0(df$drugs,"_",df$Target)
  df$cor_type = ifelse(df$cor >= 0,"pos","neg")
  df$num_of_cells_scale = scale(df$num_of_cells)
  p <- ggplot(data=df, aes(x= cor, 
                           y=-log10(fdr), 
                           size = num_of_cells_scale, 
                           color = cor_type,
                           text = paste("Genes: ", genes,
                                        "<br>Correlation: ", round(cor,digits = 3),
                                        "<br>Drugs: ", drugs,
                                        "<br>Target: ", Target,
                                        "<br>FDR: ", round(fdr,digits = 3),
                                        "<br>Number of Cells: ", num_of_cells)
  )) + 
    geom_point() + 
    ggtitle(paste0(unique(df$genes)," Drug-Target Correlation")) +
    labs(x = "Correlation",y = "-log10(FDR)") +
    theme_minimal(base_size = 15) +
    scale_color_manual(values = c("#377EB8","#E41A1C")) +
    scale_size(range = c(1,5)) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) 
  if(output_form == "plotly")   p <- plotly::ggplotly(p, tooltip = "text")
  return(p)
}

