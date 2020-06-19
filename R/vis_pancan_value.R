#' Visualize single gene expression from toil data hub
#' @inheritParams ggpubr::ggboxplot
#' @param angle.x angle for x lab
#' @param ... other parameters passing to [ggpubr::ggboxplot()]
#' @return a `ggplot` object
#' @export
vis_toil_gene <- function(data, x = "primary_site",
                          y = "expression",
                          color = "sample_type",
                          palette = "jco",
                          xlab = "Primary site",
                          ylab = "Expression",
                          title = NULL,
                          facet.by = NULL,
                          angle.x = 45,
                          ...) {
  ggpubr::ggboxplot(data,
    x = x, y = y, color = color,
    palette = palette, xlab = xlab, ylab = ylab,
    title = title, facet.by = NULL, ...
  ) +
    ggpubr::rotate_x_text(angle = angle.x)
}

# Visualize Pan-cancer tpm (Tumor (TCGA) vs normal (GTEX))
#' Visualize single gene expression from toil data hub
#' @import ggplot2 dplyr tibble
#' @param Gene Gene symbal for comparision
#' @param Mode Boxplot or Violinplot to represent data
#' @param Show.P.value TRUE or FALSE whether to count P value
#' @param Method default method is wilcox.test
#' @param Show.P.label TRUE or FALSE present p value with number or label '* ** *** ****'
#' @param values the color to fill tumor or normal
#' @param TCGA.only include samples only from TCGA dataset
#' @return a `ggplot` object
#' @examples
#' \donttest{
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = F, Show.P.label = F)
#' p <- vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = F, Show.P.label = F)
#' }
#' @export
#'
vis_toil_TvsN <- function(Gene = "TP53", Mode = "Boxplot", Show.P.value = TRUE, Show.P.label = TRUE, Method = "wilcox.test", values = c("#DF2020", "#DDDF21"), TCGA.only = FALSE) {
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  dir.create(file.path(tempdir(), "UCSCXenaShiny"), recursive = TRUE, showWarnings = FALSE)
  tmpfile <- file.path(tempdir(), "UCSCXenaShiny", "toil_TvsN.rds")
  if (file.exists(tmpfile)) {
    t1 <- readRDS(tmpfile)
    if (attr(t1, "gene") != Gene) {
      t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
      attr(t1, "gene") <- Gene
      saveRDS(t1, file = tmpfile)
    }
  } else {
    t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
    attr(t1, "gene") <- Gene
    saveRDS(t1, file = tmpfile)
  }
  tcga_gtex <- tcga_gtex %>% dplyr::distinct(sample, .keep_all = TRUE)
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2 == "tumor", ]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2 == "normal", ]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex <- t2 %>% dplyr::select("tpm", "tissue", "type2", "sample")
  tcga_gtex$type2 <- factor(tcga_gtex$type2, levels = c("tumor", "normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal), ]
  tcga_gtex_withNormal <- tcga_gtex_withNormal %>% dplyr::mutate(dataset = ifelse(stringr::str_sub(sample,1,4) == "TCGA","TCGA","GTEX"))
  tcga_gtex_MESO <- tcga_gtex[tcga_gtex$tissue == "MESO", ]
  tcga_gtex_UVM <- tcga_gtex[tcga_gtex$tissue == "UVM", ]
  if (TCGA.only == TRUE) {
    tcga_gtex_withNormal = tcga_gtex_withNormal %>% dplyr::filter(dataset == "TCGA")
  }
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- tcga_gtex_withNormal %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method, group.by = "tissue")
    pv <- pv %>% select(tissue, p, p.signif, p.adj)
    message("Counting P value finished")
  }
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(tissue, tpm, fill = type2)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values)
    p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO) +
      ggplot2::geom_boxplot(data = tcga_gtex_UVM)
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = p.signif
      ),
      data = pv, inherit.aes = F
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(p, 2))
      ),
      data = pv, inherit.aes = F
      )
    }
    print(p)
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(x = tissue, y = tpm, fill = type2)) +
      geom_split_violin(
        draw_quantiles = c(0.25, 0.5, 0.75),
        trim = T,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = T,
        position = "identity"
      ) +
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      xlab("") +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )

    p + geom_split_violin(
      data = tcga_gtex_MESO,
      mapping = aes(x = tissue, y = tpm, fill = type2),
      draw_quantiles = c(0.25, 0.5, 0.75),
      trim = T,
      linetype = "solid",
      color = "black",
      size = 0.2,
      na.rm = T,
      position = "identity"
    ) +
      geom_split_violin(
        data = tcga_gtex_UVM,
        mapping = aes(x = tissue, y = tpm, fill = type2),
        draw_quantiles = c(0.25, 0.5, 0.75),
        trim = T,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = T,
        position = "identity"
      ) +
      ggplot2::scale_x_discrete(limits = levels(tcga_gtex$tissue))
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = pv$p.signif
      ),
      data = pv, inherit.aes = F
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(p, 2))
      ),
      data = pv, inherit.aes = F
      )
    }
    print(p)
  }
  return(p)
}

utils::globalVariables(
  c(
    ".",
    "tissue",
    "tpm",
    "type2",
    "p.signif",
    "p.adj"
  )
)

#' Visualize single gene uni-cox result
#' 
vis_unicox_tree <- function(Gene = "TP53"){
  data("toil_surv")
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  ##we use median cutoff here
  ss <- s %>% 
    dplyr::inner_join(toil_surv, by = "sample") %>%
    dplyr::inner_join(tcga_gtex[,c("tissue","sample")], by = "sample")
  sss <- split(ss,ss$tissue)
  tissues <- names(sss)
  unicox_res_all_cancers <- purrr::map(tissues,safely(function(cancer){
    #cancer = "ACC"
    sss_can <- sss[[cancer]]
    sss_can = sss_can %>%
      dplyr::mutate(group = ifelse(values>median(values),'high','low')) %>%
      dplyr::mutate(group = factor(group,levels = c("low","high")))
    
    unicox_res_genes <- ezcox::ezcox(sss_can, 
                              covariates = "values",
                              time = "OS.time", 
                              status = "OS",
                              verbose = F)
    unicox_res_genes$cancer = cancer
    return(unicox_res_genes)
  })) %>% set_names(tissues)
  
  unicox_res_all_cancers <- unicox_res_all_cancers %>% 
    map(~.x$result) %>%
    compact
  unicox_res_all_cancers_df <- do.call(rbind.data.frame,unicox_res_all_cancers)
  ##visualization
  p <- ggplot(data = unicox_res_all_cancers_df,
         aes(x = cancer,y = HR, ymin = lower_95, ymax = upper_95))+
    geom_pointrange()+
    coord_flip()
  return(p)
}

#’ heatmap visualization (correlation between immune signatures and gene)
#' 
vis_gene_immune_cor <- function(Gene = "TP53", Cor_method = "spearman",Immune_sig_type = "Cibersort"){
  data("immune_sig")
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  immune_sig = immune_sig %>%
    tidyr::pivot_longer(3:ncol(.),names_to = "sample",values_to = "score") %>%
    dplyr::mutate(sample = str_sub(sample,1,15))
  
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  
  ss <- s %>% 
    dplyr::inner_join(immune_sig, by = "sample") %>%
    dplyr::inner_join(tcga_gtex[,c("tissue","sample")], by = "sample")
  
  sss <- split(ss,ss$tissue)
  tissues <- names(sss)
  cor_gene_immune <- purrr::map(tissues,safely(function(cancer){
    #cancer = "ACC"
    sss_can <- sss[[cancer]]
    ##filter cibersort data here
    sss_can_class <- sss_can %>% filter(Source == Immune_sig_type) 
    cells <- unique(sss_can_class$SetName)
    cor_res_class_can <- purrr::map(cells,safely(function(i){
      #i = cells[1]
      dd = sss_can_class[sss_can_class$SetName == i,]
      dd  <- cor.test(dd$values,dd$score,type=Cor_method)
      ddd <- data.frame(gene = Gene,immune_cells= i,cor=dd$estimate,p.value=dd$p.value,stringsAsFactors = F)
      return(ddd)
    })) %>% set_names(cells)
    cor_res_class_can <- cor_res_class_can %>% 
      map(~.x$result) %>%
      compact
    cor_res_class_can_df <- do.call(rbind.data.frame,cor_res_class_can)
    cor_res_class_can_df$cancer = cancer
    return(cor_res_class_can_df)
  })) %>% set_names(tissues)
  
  cor_gene_immune <- cor_gene_immune %>% 
    map(~.x$result) %>%
    compact
  cor_gene_immune_df <- do.call(rbind.data.frame,cor_gene_immune)
  data <- cor_gene_immune_df
  data$pstar <- ifelse(data$p.value < 0.05,
                       ifelse(data$p.value < 0.01,"**","*"),
                       "")
  
  p <- ggplot(data, aes(cancer, immune_cells)) + 
    geom_tile(aes(fill = cor), colour = "white",size=1)+
    scale_fill_gradient2(low = "#2b8cbe",mid = "white",high = "#e41a1c")+
    geom_text(aes(label=pstar),col ="black",size = 5)+
    theme_minimal()+# 不要背景
    theme(axis.title.x=element_blank(),#不要title
          axis.ticks.x=element_blank(),#不要x轴
          axis.title.y=element_blank(),#不要y轴
          axis.text.x = element_text(angle = 45, hjust = 1),# 调整x轴文字
          axis.text.y = element_text(size = 8))+#调整y轴文字
    #调整legen
    labs(fill =paste0(" * p < 0.05","\n\n","** p < 0.01","\n\n","Correlation"))
  print(p)
  return(p)
}


#' visualize TMB and gene
#' 
#' 
vis_gene_tmb_cor <- function(Gene = "TP53", Cor_method = "spearman"){
  data("tmb_data")
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>% 
    dplyr::inner_join(tmb_data, by = c("sample" = "Tumor_Sample_ID")) %>%
    dplyr::inner_join(tcga_gtex[,c("tissue","sample")], by = "sample")
  sss <- split(ss,ss$tissue)
  tissues <- names(sss)
  
  cor_gene_tmb <- purrr::map(tissues,safely(function(cancer){
    #cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd  <- cor.test(sss_can$values,sss_can$Non_silent_per_Mb,type=Cor_method)
    ddd <- data.frame(gene = Gene,cor=dd$estimate,p.value=dd$p.value,stringsAsFactors = F)
    ddd$cancer = cancer
    return(ddd)
  })) %>% set_names(tissues)
  
  cor_gene_tmb <- cor_gene_tmb %>% 
    map(~.x$result) %>%
    compact
  cor_gene_tmb_df <- do.call(rbind.data.frame,cor_gene_tmb)
  data <- cor_gene_tmb_df
  data$pstar <- ifelse(data$p.value < 0.05,
                       ifelse(data$p.value < 0.01,"**","*"),
                       "")
  
  p <- ggplot(data, aes(cancer, gene)) + 
    geom_tile(aes(fill = cor), colour = "white",size=1)+
    scale_fill_gradient2(low = "#2b8cbe",mid = "white",high = "#e41a1c")+
    geom_text(aes(label=pstar),col ="black",size = 5)+
    theme_minimal()+# 不要背景
    theme(axis.title.x=element_blank(),#不要title
          axis.ticks.x=element_blank(),#不要x轴
          axis.title.y=element_blank(),#不要y轴
          axis.text.x = element_text(angle = 45, hjust = 1),# 调整x轴文字
          axis.text.y = element_text(size = 8))+#调整y轴文字
    #调整legen
    labs(fill =paste0(" * p < 0.05","\n\n","** p < 0.01","\n\n","Correlation"))
  print(p)
  return(p)
}

#' visualize gene and stemness
vis_gene_stemness_cor <- function(Gene = "TP53", Cor_method = "spearman"){
  data("stemness_data_RNA")
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  s <- data.frame(sample = names(t1), values = t1)
  ss <- s %>% 
    dplyr::inner_join(stemness_data_RNA, by = c("sample")) %>%
    dplyr::inner_join(tcga_gtex[,c("tissue","sample")], by = "sample")
  sss <- split(ss,ss$tissue)
  tissues <- names(sss)
  
  cor_gene_stemness <- purrr::map(tissues,safely(function(cancer){
    #cancer = "ACC"
    sss_can <- sss[[cancer]]
    dd  <- cor.test(sss_can$values,sss_can$RNAss,type=Cor_method)
    ddd <- data.frame(gene = Gene,cor=dd$estimate,p.value=dd$p.value,stringsAsFactors = F)
    ddd$cancer = cancer
    return(ddd)
  })) %>% set_names(tissues)
  
  cor_gene_stemness <- cor_gene_stemness %>% 
    map(~.x$result) %>%
    compact
  cor_gene_stemness_df <- do.call(rbind.data.frame,cor_gene_stemness)
  data <- cor_gene_stemness_df
  data$pstar <- ifelse(data$p.value < 0.05,
                       ifelse(data$p.value < 0.01,"**","*"),
                       "")
  
  p <- ggplot(data, aes(cancer, gene)) + 
    geom_tile(aes(fill = cor), colour = "white",size=1)+
    scale_fill_gradient2(low = "#2b8cbe",mid = "white",high = "#e41a1c")+
    geom_text(aes(label=pstar),col ="black",size = 5)+
    theme_minimal()+# 不要背景
    theme(axis.title.x=element_blank(),#不要title
          axis.ticks.x=element_blank(),#不要x轴
          axis.title.y=element_blank(),#不要y轴
          axis.text.x = element_text(angle = 45, hjust = 1),# 调整x轴文字
          axis.text.y = element_text(size = 8))+#调整y轴文字
    #调整legen
    labs(fill =paste0(" * p < 0.05","\n\n","** p < 0.01","\n\n","Correlation"))
  print(p)
  return(p)
}
