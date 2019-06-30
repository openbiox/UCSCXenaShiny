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
#' @import ggplot2 dplyr tibble
#' @param Gene Gene symbal for comparision
#' @param Mode Boxplot or Violinplot to represent data
#' @param Show.P.value TRUE or FALSE whether to count P value, count P value will spend much time
#' @param Show.P.label TRUE or FALSE present p value with number or label '* ** ***'
#' @param values the color to fill tumor or normal
#' @return a `ggplot` object
#' @example vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = F, Show.P.label = F)
#' @example vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = F, Show.P.label = F)
#' @export
data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
vis_toil_TvsN = function(Gene = "TP53", Mode = "Boxplot", Show.P.value = TRUE, Show.P.label = TRUE, values = c("#DF2020", "#DDDF21")){
  t1 = get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  t2 = t1 %>% as.data.frame() %>% dplyr::rename("tpm"=".") %>% tibble::rownames_to_column(var = "sample") %>% dplyr::inner_join(tcga_gtex,by="sample")
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2=="tumor",]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2=="normal",]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex = t2 %>% dplyr::select("tpm","tissue","type2")
  tcga_gtex$type2 <- factor(tcga_gtex$type2,levels=c("tumor","normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal),]
  tcga_gtex_MESO <- tcga_gtex[tcga_gtex$tissue=="MESO",]
  tcga_gtex_UVM <- tcga_gtex[tcga_gtex$tissue=="UVM",]
  if(Show.P.value == FALSE) {Show.P.label <- FALSE}
  ## 用one way anova计算 p value
  if(Show.P.value == TRUE){
    message("Counting P value")
    pvalues <- sapply(tcga_gtex_withNormal$tissue, function(x) {
      res <- aov(tpm ~ type2, data = subset(tcga_gtex_withNormal, tissue == x))
      summary(res)[[1]]$'Pr(>F)'[1] #
    })
    pv <- data.frame(tissue = tcga_gtex_withNormal$tissue, pvalue = pvalues)
    ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    pv$sigcode <- cut(pv$pvalue, c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      labels=c('***', '**', '*', '.', ' '))
    message("Counting P value finished")
  }
  if(Mode == "Boxplot"){
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(tissue, tpm, fill=type2)) + 
      ggplot2::geom_boxplot() +
      ggplot2::xlab(NULL)+ggplot2::ylab(paste0(Gene," expression (TPM)")) + 
      ggplot2::theme_set(theme_set(theme_classic(base_size=20)))+
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) + #x轴label倾斜45度
      ggplot2::guides(fill = guide_legend(title = NULL)) + 
      ggplot2::theme(legend.background = element_blank(), #移除整体边框
                     #图例的左下角置于绘图区域的左下角
                     legend.position=c(0,0),legend.justification = c(0,0))+
      scale_fill_manual(values = values)
    p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO)+ ggplot2::geom_boxplot(data = tcga_gtex_UVM)
    if(Show.P.value == TRUE & Show.P.label == TRUE){
      p <- p + ggplot2::geom_text(aes(tissue, y=max(tcga_gtex_withNormal$tpm)*1.1, 
                                          label=pv$sigcode),
                                      data=pv, inherit.aes=F)
    }
    if(Show.P.value == TRUE & Show.P.label == FALSE){
      p <- p + ggplot2::geom_text(aes(tissue, y=max(tcga_gtex_withNormal$tpm)*1.1, 
                                      label=paste("p = ",round(pvalue, 2))),
                                  data=pv, inherit.aes=F)
    }
    print(p)
  }
  if(Mode == "Violinplot"){
    # p <- ggplot(tcga_gtex_withNormal, aes(x = tissue, y = tpm, fill = type2)) + geom_split_violin()
    p <- ggplot(tcga_gtex_withNormal, aes(x = tissue, y = tpm, fill = type2)) + #x对应肿瘤的类型，y对应表达量，fill填充对应组织的类型
      geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75), #画4分位线
                        trim = T, #是否修剪小提琴图的密度曲线
                        linetype = "solid", #周围线的轮廓
                        color = "black", #周围线颜色
                        size = 0.2,
                        na.rm = T,
                        position ="identity")+ #周围线粗细
      ylab(paste0(Gene, " expression (TPM)")) + xlab("") +
      # ylim(-4,9) +
      scale_fill_manual(values = values)+
      theme_set(theme_set(theme_classic(base_size=20)))+
      theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) + #x轴label倾斜45度
      
      guides(fill = guide_legend(title = NULL)) + 
      theme(legend.background = element_blank(), #移除整体边框
            #图例的左下角置于绘图区域的左下角
            legend.position=c(0,0),legend.justification = c(0,0))
    
    p + geom_split_violin(data = tcga_gtex_MESO,
                          mapping = aes(x = tissue, y = tpm, fill = type2),
                          draw_quantiles = c(0.25, 0.5, 0.75), #画4分位线
                          trim = T, #是否修剪小提琴图的密度曲线
                          linetype = "solid", #周围线的轮廓
                          color = "black", #周围线颜色
                          size = 0.2,
                          na.rm = T,
                          position ="identity") +
      geom_split_violin(data = tcga_gtex_UVM,
                        mapping = aes(x = tissue, y = tpm, fill = type2),
                        draw_quantiles = c(0.25, 0.5, 0.75), #画4分位线
                        trim = T, #是否修剪小提琴图的密度曲线
                        linetype = "solid", #周围线的轮廓
                        color = "black", #周围线颜色
                        size = 0.2,
                        na.rm = T,
                        position ="identity") +
      scale_x_discrete(limits = levels(tcga_gtex$tissue))
    if(Show.P.value == TRUE & Show.P.label == TRUE){
      p <- p + ggplot2::geom_text(aes(tissue, y=max(tcga_gtex_withNormal$tpm) + 1, 
                                      label=pv$sigcode),
                                  data=pv, inherit.aes=F)
    }
    if(Show.P.value == TRUE & Show.P.label == FALSE){
      p <- p + ggplot2::geom_text(aes(tissue, y=max(tcga_gtex_withNormal$tpm) + 1, 
                                      label=paste("p = ",round(pvalue, 2))),
                                  data=pv, inherit.aes=F)
    }
    print(p)
  }
}
