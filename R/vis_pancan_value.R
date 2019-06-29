# 可视化 Pan-cancer 值
#' Visualize single gene expression from toil data hub
#' @inheritParams ggpubr::ggboxplot
#' @param angle.x angle for x lab
#' @param ... other parameters passing to [ggpubr::ggboxplot()]
#' @return a `ggplot` object
#' @export
vis_toil_gene = function(data, x = "primary_site", 
                         y = "expression", 
                         color = "sample_type", 
                         palette = "jco",
                         xlab = "Primary site",
                         ylab = "Expression", 
                         title = NULL, 
                         facet.by = NULL, 
                         angle.x = 45,
                         ...
                         ) {
  ggpubr::ggboxplot(data, x = x, y = y, color = color,
                    palette = palette, xlab = xlab, ylab = ylab,
                    title = title, facet.by = NULL, ...) +
    ggpubr::rotate_x_text(angle = angle.x) 
}

# Visualize Pan-cancer tpm (Tumor (TCGA) vs normal (GTEX))
#' Visualize single gene expression from toil data hub
#' @inherit ggplot2
#' @inherit dplyr
#' @inheritParams tibble::rownames_to_column
#' @param Gene Gene symbal for comparision
#' @return a `ggplot` object
#' @export
data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
vis_toil_TvsN = function(Gene = "TP53"){
  t1 = get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  t2 = t1 %>% as.data.frame() %>% dplyr::rename("tpm"=".") %>% tibble::rownames_to_column(var = "sample") %>% dplyr::inner_join(tcga_gtex,by="sample")
  tcga_gtex = t2[,c(3,5,2)]
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2=="tumor",]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2=="normal",]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex$type2 <- factor(tcga_gtex$type2,levels=c("tumor","normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal),]
  tcga_gtex_MESO <- tcga_gtex[tcga_gtex$tissue=="MESO",]
  tcga_gtex_UVM <- tcga_gtex[tcga_gtex$tissue=="UVM",]
  ###
  ## 用one way anova计算 p value
  pvalues <- sapply(tcga_gtex_withNormal$tissue, function(x) {
    res <- aov(tpm ~ type2, data = subset(tcga_gtex_withNormal, tissue == x))
    summary(res)[[1]]$'Pr(>F)'[1] #
  })
  pv <- data.frame(tissue = tcga_gtex_withNormal$tissue, pvalue = pvalues)
  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  pv$sigcode <- cut(pv$pvalue, c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    labels=c('***', '**', '*', '.', ' '))
  ###
  p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(tissue, tpm, fill=type2)) + 
    ggplot2::geom_boxplot() + 
    ggplot2::geom_text(aes(tissue, y=max(tcga_gtex_withNormal$tpm) * 1.1, 
                  label=pv$sigcode),
              data=pv, inherit.aes=F) +
    ggplot2::xlab(NULL)+ggplot2::ylab(paste0(Gene," expression (TPM)")) + 
  ggplot2::theme_set(theme_set(theme_classic(base_size=20)))+
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) + #x轴label倾斜45度
  ggplot2::guides(fill = guide_legend(title = NULL)) + 
  ggplot2::theme(legend.background = element_blank(), #移除整体边框
        #图例的左下角置于绘图区域的左下角
        legend.position=c(0,0),legend.justification = c(0,0))
  p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO)+ ggplot2::geom_boxplot(data = tcga_gtex_UVM)
  print(p)
}
