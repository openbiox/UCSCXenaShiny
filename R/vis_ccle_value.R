#' Visualize CCLE Gene Expression in TPM Format
#' @import ggplot2 dplyr tibble forcats
#' @param Gene Gene symbal for comparision
#' @param phenotype Different parameters for phenotype
#' @param data_type support genomic profile for CCLE, currently "mRNA", "protein","cnv" are supported
#' @return a `ggplot` object
#' @export
vis_ccle_tpm <- function(Gene = "TP53", data_type = "mRNA",phenotype = "Type") {
  ccle_info <- load_data("ccle_info")
  
  if (!data_type %in% c("mRNA", "protein","cnv")) {
    stop("data_type ", data_type, " does not support in this function!")
  }
  #t1 <- get_ccle_gene_value(identifier = Gene)$expression
  t1 <- query_value(identifier = Gene, data_type = data_type, database = "ccle")
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
  
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))

  t2[[phenotype]] <- forcats::fct_reorder(t2[[phenotype]], t2$tpm)

  # t2 %>% mutate(phenotype = fct_reorder(phenotype, tpm, .fun='median')) -> t2

  p <- t2 %>% ggplot2::ggplot(aes_string(x = phenotype, y = "tpm", fill = phenotype)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(axis.text.x = element_text(angle = 90)) +
    ggplot2::guides(fill = guide_legend(title = NULL)) +
    ggplot2::theme(
      legend.background = element_blank(),
      legend.position = "none"
    )
  return(p)
}
