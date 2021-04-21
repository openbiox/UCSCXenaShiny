#' Visualize CCLE Gene Expression
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
    ggplot2::ylab(paste0(Gene,"_",data_type,"_",unit)) +
    ggplot2::theme_classic(base_size = 15) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, color = "black")) +
    ggplot2::guides(fill = guide_legend(title = NULL)) +
    #ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
    ggplot2::theme(
      legend.background = element_blank(),
      legend.position = "none"
    )
  return(p)
}

#' Visualize CCLE Gene Expression Correlation
#' @import ggplot2 dplyr tibble forcats
#' @import ggplot2 dplyr ppcor
#' @inheritParams vis_gene_cor
#' @return a `ggplot` object
#' @export

vis_ccle_gene_cor <- function(Gene1 = "CSF1R",
                              Gene2 = "JAK3",
                              data_type1 = "mRNA",
                              data_type2 = "mRNA",
                              cor_method = "spearman"){
  # Gene1 = "CSF1R";data_type1 = "mRNA"
  # Gene2 = "JAK3";data_type2 = "mRNA"
  # cor_method = "spearman"
  
  t1 <- query_value(identifier = Gene1, data_type = data_type1, database = "ccle")
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
  
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))
  
  t3 <- query_value(identifier = Gene2, data_type = data_type2, database = "ccle")
  unit2 <- switch(data_type2,
                  cnv = NULL,
                  mutation = NULL,
                  t1[[2]]
  )
  if (is.list(t3)) t3 <- t3[[1]]
  
  if (all(is.na(t3))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }
  
  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::inner_join(ccle_info, by = c("cell" = "CCLE_name"))
  
  #merge
  t2 <- t2 %>% inner_join(t4[,c("cell","tpm")], by = "cell")
  
  df <- data.frame(sample = t2$cell, gene1 = t2$tpm.x, gene2 = t2$tpm.y, stringsAsFactors = F)
  
  cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2", cor_method = cor_method)
  df$pc <- predict(prcomp(~ gene1 + gene1, df))[, 1]
  x <- quantile(df$gene1)[1]
  y <- quantile(df$gene2)[5]
  p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2", color = "pc")) +
    ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    ggplot2::labs(x = Gene1, y = Gene2) +
    # ggplot2::ggtitle(paste0("CCLE ", cancer_choose, " dataset")) +
    ggplot2::annotate("text", label = paste0("Cor: ", round(cor_res$cor, 2), " ", cor_res$pstar), x = x + 1, y = y, size = 10, colour = "black") +
    ggplot2::geom_smooth(method = stats::lm) +
    ggplot2::labs(color = "")
  return(p)
}


