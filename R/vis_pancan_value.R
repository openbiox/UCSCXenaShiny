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