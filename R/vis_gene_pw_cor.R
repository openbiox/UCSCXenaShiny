#' Visualize Correlation between Gene and Pathway signature Score
#' @param Gene 	a molecular identifier (e.g., "TP53") or a formula
#' specifying genomic signature ("TP53 + 2 * KRAS - 1.3 * PTEN").
#' @param data_type choose gene profile type, including
#' "mRNA", "transcript", "protein", "mutation", "cnv", "methylation", "miRNA".
#' @param pw_name the queried Pathway name, see the supported pathway from 'load("toil_sig_score")'default is NULL
#' @param cancer_choose select cancer cohort(s)
#' @param use_regline if TRUE, add regression line.
#' @param cor_method select correlation coefficient (pearson/spearman)
#' @param use_all use all sample, default FALSE.
#' @param alpha dot alpha.
#' @param color dot color.
#' @param filter_tumor whether use tumor sample only, default TRUE
#' @param opt_pancan specify one dataset for some molercular profiles

#' @return a `ggplot` object or  dataframe
#' @examples
#' \dontrun{
#' vis_gene_pw_cor(Gene = "TP53", data_type = "mRNA", 
#'                 pw_name = "HALLMARK_ADIPOGENESIS",
#'                 cancer_choose = "BRCA")
#' }

#' @export
#'
vis_gene_pw_cor <- function(Gene = "TP53",
                            data_type = "mRNA",
                            pw_name = "HALLMARK_ADIPOGENESIS",
                            cancer_choose = "GBM",
                            use_regline = TRUE,
                            cor_method = "spearman",
                            use_all = FALSE,
                            alpha = 0.5,
                            color = "#000000",
                            filter_tumor = TRUE,
                            opt_pancan = .opt_pancan){
  if (!file.exists(file.path(get_zenodo_dir(), "tcga_PW.rda"))) {
    print("This is the first download from zenodo, please wait a few minutes.")
  }
  toil_sig_score <- load_data("tcga_PW")
  toil_sig_meta  <- load_data("tcga_PW_meta")
  if (!is.null(pw_name)) {
    if (!(pw_name %in% toil_sig_meta$ID)) {
      stop("You need provide valid pathway name (see load_data('tcga_PW_meta'))")
    }
  }
  # gene expression
  t1 <- query_pancan_value(Gene, data_type = data_type, opt_pancan = opt_pancan)
  if (is.null(t1[[1]])) {
    warning("No data available", immediate. = TRUE)
    return(NULL)
  }
  if (is.list(t1)) t1 <- t1[[1]]
  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }
  message(paste0("Get data value for ", Gene))
  s <- data.frame(Sample = names(t1), values = t1)


  # message("Mode-3: one specific pathway correlated to one specific cancer")
  toil_sig_score <- load_data("tcga_PW")

  tcga_gtex <- load_data("tcga_gtex")
  if(use_all){
    cancer_choose2 = as.character(unique(tcga_gtex$tissue))
  } else {
    cancer_choose2 = cancer_choose
  }
  if(filter_tumor){
    filter_tumor2 = 'tumor'
  } else {
    filter_tumor2 = c('tumor','normal')
  }
  tcga_sp <- tcga_gtex %>%
    dplyr::filter(.data$type2 == filter_tumor2) %>%
    dplyr::filter(.data$tissue %in% cancer_choose2) %>%
    dplyr::pull(sample) %>%
    as.character()

  res_pan_spe <- toil_sig_score[rownames(toil_sig_score) %in% tcga_sp, pw_name, drop = F] %>%
    as.data.frame() %>% tibble::rownames_to_column("Sample") %>%  
    tidyr::pivot_longer(!"Sample", names_to = "pw_name", values_to = "pw_score") %>%
    dplyr::left_join(s) %>%
    dplyr::mutate(identifier = Gene, .before = 1) %>%
    dplyr::mutate(Cancer = tcga_gtex$tissue[match(.data$Sample, tcga_gtex$sample)], .before = 3) %>%
    tibble()

  cor_res <- ezcor(data = as.data.frame(res_pan_spe), var1 = "values", var2 = "pw_score", cor_method = cor_method)
  p <- ggplot2::ggplot(res_pan_spe, aes_string(x = "values", y = "pw_score")) +
    ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE, alpha = alpha, color = color) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = paste(Gene, data_type), y = pw_name) +
    ggplot2::annotate(
      "text",
      -Inf, Inf,
      hjust = -0.1, vjust = 1,
      label = paste0("Cor: ", round(cor_res$cor, 2), " ", cor_res$pstar),
      size = 8, colour = "black"
    )

  if (use_regline) p <- p + ggplot2::geom_smooth(method = stats::lm)

  return(p)
}