#' Visualize CCLE Gene Expression in TPM Format
#' @import ggplot2 dplyr tibble forcats
#' @param Gene Gene symbal for comparision
#' @param x.axis Different parameters for x.axis
#' @return a `ggplot` object
#' @export
vis_ccle_tpm <- function(Gene = "TP53", x.axis = "Type") {
  ccle_info <- load_data("ccle_info")

  t1 <- get_ccle_gene_value(identifier = Gene)$expression
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))

  t2[[x.axis]] <- forcats::fct_reorder(t2[[x.axis]], t2$tpm)

  # t2 %>% mutate(x.axis = fct_reorder(x.axis, tpm, .fun='median')) -> t2

  p <- t2 %>% ggplot2::ggplot(aes_string(x = x.axis, y = "tpm", fill = x.axis)) +
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
