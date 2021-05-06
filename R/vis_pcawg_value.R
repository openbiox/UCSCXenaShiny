#' Visualize PCAWG molecular expression
#' @inheritParams vis_toil_TvsN
#' @return a `ggplot` object
#' @examples
#' \dontrun{
#' p <- vis_pcawg_dist(Gene = "TP53")
#' }
#' @export
#' 
vis_pcawg_dist <- function(Gene = "TP53",
                           Mode = c("Boxplot", "Violinplot"),
                           data_type = "mRNA", Show.P.value = TRUE,
                           Show.P.label = TRUE, Method = c("wilcox.test", "t.test"),
                           values = c("#DF2020", "#DDDF21"),
                           draw_quantiles = c(0.25, 0.5, 0.75),
                           trim = TRUE){
  # Gene = "TP53"
  # Mode="Boxplot" 
  # Method = "wilcox.test"
  # data_type = "mRNA"
  # Show.P.label <- FALSE
  # Show.P.value = TRUE
  # values = c("#DF2020", "#DDDF21")
  
  Mode <- match.arg(Mode)
  Method <- match.arg(Method)
  
  if (!Method %in% c("wilcox.test", "t.test")) {
    stop("only support wilcox.test or t.test")
  }
  
  if (!Mode %in% c("Boxplot", "Violinplot")) {
    stop("only support Boxplot or Violinplot")
  }
  
  data(list  = "pheno_pcawg_specimen",package = "UCSCXenaShiny", envir = environment())
  t1 <- query_pancan_value(Gene, database = "pcawg")
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
    tibble::rownames_to_column(var = "icgc_specimen_id") %>%
    dplyr::inner_join(pheno_pcawg_specimen, by = "icgc_specimen_id")
  
  #table(pheno_pcawg_specimen$dcc_specimen_type)
  pcawg_data <- t2 %>% dplyr::select("tpm", "dcc_project_code", "type2", "icgc_specimen_id")
  
  
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- pcawg_data %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method, group.by = "dcc_project_code")
    pv <- pv %>% dplyr::select(c("dcc_project_code", "p", "p.signif", "p.adj"))
    message("Counting P value finished")
  }
  
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(pcawg_data, aes_string(x = "dcc_project_code", y = "tpm", fill = "type2")) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab(NULL) +
      # ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values)
    
    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )
    
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(
        x = .data$dcc_project_code,
        y = max(pcawg_data$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(
        x = .data$dcc_project_code,
        y = max(pcawg_data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }
  
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(pcawg_data, aes_string(x = "dcc_project_code", y = "tpm", fill = "type2")) +
      geom_split_violin(
        draw_quantiles = draw_quantiles,
        trim = trim,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      ) +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::xlab("") +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_set(ggplot2::theme_classic(base_size = 20)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = ggplot2::element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )
    
    p <- p + ggplot2::ylab(
      if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")
    )
    
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = .data$dcc_project_code,
        y = max(pcawg_data$tpm) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(ggplot2::aes(
        x = .data$dcc_project_code,
        y = max(pcawg_data$tpm) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
      )
    }
  }
  return(p)
}


