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

#' Visualize Single Gene Univariable Cox Result from PCAWG Data Hub
#'
#' @inheritParams vis_toil_TvsN
#' @param measure a survival measure, e.g. "OS".
#' @param threshold a expression cutoff, `0.5` for median.
#' @param data_type choose gene profile type, including "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"
#' @return a `ggplot` object
#' @examples
#' \dontrun{
#' p <- vis_pcawg_unicox_tree(Gene = "TP53")
#' }
#' @export


vis_pcawg_unicox_tree <- function(Gene = "TP53", measure = "OS", data_type = "mRNA", threshold = 0.5, values = c("grey", "#E31A1C", "#377DB8")) {
  
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
  
  # we filter out normal tissue
  pheno_pcawg_specimen <- pheno_pcawg_specimen %>% dplyr::filter(.data$type2 != "normal")
  s <- data.frame(icgc_specimen_id = names(t1), values = t1)
  ss = s %>% 
    dplyr::inner_join(pheno_pcawg_specimen, by = "icgc_specimen_id") %>%
    dplyr::select(values,icgc_specimen_id,dcc_project_code,OS,OS.time)
  
  sss <- split(ss, ss$dcc_project_code)
  tissues <- names(sss)
  unicox_res_all_cancers <- purrr::map(tissues, purrr::safely(function(cancer) {
    # cancer = "ACC"
    sss_can <- sss[[cancer]]
    if (threshold == 0.5) {
      sss_can <- sss_can %>%
        dplyr::mutate(group = ifelse(.data$values > stats::median(.data$values), "high", "low")) %>%
        dplyr::mutate(group = factor(.data$group, levels = c("low", "high")))
    }
    
    if (threshold == 0.25) {
      sss_can <- sss_can %>%
        dplyr::mutate(group = ifelse(.data$values > stats::quantile(.data$values)[4], "high",
                                     ifelse(.data$values < stats::quantile(.data$values)[2], "low", "middle")
        )) %>%
        dplyr::filter(group != "middle") %>%
        dplyr::mutate(group = factor(.data$group, levels = c("low", "high")))
    }
    
    
    unicox_res_genes <- ezcox::ezcox(
      sss_can,
      covariates = "values",
      time = paste0(measure, ".time"),
      status = measure,
      verbose = FALSE
    )
    
    unicox_res_genes$cancer <- cancer
    unicox_res_genes$measure <- measure
    return(unicox_res_genes)
  })) %>% magrittr::set_names(tissues)
  
  unicox_res_all_cancers <- unicox_res_all_cancers %>%
    purrr::map(~ .x$result) %>%
    purrr::compact()
  unicox_res_all_cancers_df <- do.call(rbind.data.frame, unicox_res_all_cancers)
  unicox_res_all_cancers_df <- unicox_res_all_cancers_df %>%
    dplyr::mutate(HR_log = log(.data$HR)) %>%
    dplyr::mutate(lower_95_log = log(.data$lower_95)) %>%
    dplyr::mutate(upper_95_log = log(.data$upper_95)) %>%
    dplyr::mutate(Type = ifelse(.data$p.value < 0.05 & .data$HR_log > 0, "Risky", ifelse(.data$p.value < 0.05 & .data$HR_log < 0, "Protective", "NS"))) %>%
    dplyr::mutate(Type = factor(Type, levels = c("NS", "Risky", "Protective")))
  
  
  unicox_res_all_cancers_df <- unicox_res_all_cancers_df[complete.cases(unicox_res_all_cancers_df),]
  ## visualization
  p <- ggplot2::ggplot(
    data = unicox_res_all_cancers_df,
    aes_string(x = "cancer", y = "HR_log", ymin = "lower_95_log", ymax = "upper_95_log", color = "Type")
  ) +
    ggplot2::theme_bw() +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "log (Hazard Ratio)") +
    ggtitle(paste0(Gene, " Expression")) +
    ggplot2::theme(
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggplot2::scale_color_manual(values = values) +
    ggplot2::geom_hline(yintercept = c(0), linetype = "dashed")
  
  return(p)
  
}

#' Visualize PCAWG Gene Expression Correlation
#' @import ggplot2 dplyr tibble forcats
#' @import ggplot2 dplyr ppcor
#' @inheritParams vis_gene_cor
#' @param cor_method correlation method
#' @param use_log_x if `TRUE`, log X values.
#' @param use_log_y if `TRUE`, log Y values.
#' @param SitePrimary select cell line origin tissue.
#' @param use_all use all sample, default `FALSE`.
#' @return a `ggplot` object
#' @export

vis_ccle_gene_cor <- function(Gene1 = "CSF1R",
                              Gene2 = "JAK3",
                              data_type1 = "mRNA",
                              data_type2 = "mRNA",
                              cor_method = "spearman",
                              use_log_x = FALSE,
                              use_log_y = FALSE,
                              use_regline = TRUE,
                              dcc_project_code = "BLCA-US",
                              use_all = FALSE,
                              alpha = 0.5, color = "#000000") {
  if (!requireNamespace("cowplot")) {
    install.packages("cowplot")
  }
  
  data(list  = "pheno_pcawg_specimen",package = "UCSCXenaShiny", envir = environment())
  
  if (!data_type1 %in% c("mRNA", "protein", "cnv")) {
    stop("data_type ", data_type1, " does not support in this function!")
  }
  if (!data_type2 %in% c("mRNA", "protein", "cnv")) {
    stop("data_type ", data_type2, " does not support in this function!")
  }
  
  t1 <- query_pancan_value(Gene1, data_type = data_type1, database = "pcawg")
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
  if (use_log_x) t1 <- log2(t1 + 1)
  
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "icgc_specimen_id") %>%
    dplyr::inner_join(pheno_pcawg_specimen, by = c("icgc_specimen_id"))
  
  t3 <- query_pancan_value(Gene2, data_type = data_type2, database = "pcawg")
  unit2 <- switch(data_type2,
                  cnv = NULL,
                  mutation = NULL,
                  t1[[2]]
  )
  if (is.list(t3)) t3 <- t3[[1]]
  
  if (data_type1 == "cnv") data_type1 <- "CNV"
  if (data_type2 == "cnv") data_type2 <- "CNV"
  
  if (all(is.na(t3))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }
  if (use_log_y) t3 <- log2(t3 + 1)
  
  t4 <- t3 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "icgc_specimen_id") %>%
    dplyr::inner_join(pheno_pcawg_specimen, by = c("icgc_specimen_id"))
  
  t2 <- t2 %>% inner_join(t4[, c("icgc_specimen_id", "tpm")], by = "icgc_specimen_id")
  
  df <- data.frame(sample = t2$icgc_specimen_id, gene1 = t2$tpm.x, gene2 = t2$tpm.y, dcc_project_code = t2$dcc_project_code, stringsAsFactors = F)
  if (!use_all) {
    df <- df %>% dplyr::filter(.data$dcc_project_code %in% dcc_project_code)
  }
  cor_res <- ezcor(data = df, var1 = "gene1", var2 = "gene2", cor_method = cor_method)
  
  p <- ggplot2::ggplot(df, aes_string(x = "gene1", y = "gene2")) +
    ggplot2::geom_point(shape = 16, size = 3, show.legend = FALSE, alpha = alpha, color = color) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    ggplot2::labs(x = Gene1, y = Gene2) +
    ggplot2::annotate(
      "text",
      -Inf, Inf,
      hjust = -0.1, vjust = 1,
      label = paste0("Cor: ", round(cor_res$cor, 2), " ", cor_res$pstar),
      size = 8,
      colour = "black"
    ) +
    ggplot2::labs(
      x = paste(Gene1, data_type1),
      y = paste(Gene2, data_type2)
    )
  
  if (use_regline) p <- p + ggplot2::geom_smooth(method = stats::lm)
  
  return(p)
}