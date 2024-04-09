plot_cor_o2o = function(data, xlab, ylab, title, cor_method,
            point.args, smooth.line.args, xsidehistogram.args, ysidehistogram.args,
            axis_size, title_size, side_hist, custom_theme){

    names(cor_method) = ifelse(cor_method=="Pearson","parametric","nonparametric")
    
    p = ggscatterstats(
        data, x = !!colnames(data)[6], y = !!colnames(data)[10],
        xlab = xlab, ylab = ylab, title = title, type = names(cor_method),
        point.args = point.args, smooth.line.args = smooth.line.args, 
        xsidehistogram.args = xsidehistogram.args, ysidehistogram.args = ysidehistogram.args,
        bf.message = FALSE
    )
    # change theme
    p = p + custom_theme
    # theme text size
    p = p + 
        theme(text = element_text(size=axis_size),
              plot.title = element_text(size=title_size, hjust = 0.5))
    # refine subtitle
    pval = formatC(extract_stats(p)$subtitle_data$p.value, digits = 3, format = 'e')
    r = round(extract_stats(p)$subtitle_data$estimate,3)
    p$labels$subtitle = bquote(paste(widehat(italic(r))[.(cor_method)] == .(r), ', ' ,italic(p) == .(pval)))
    # remove hist
    if(side_hist=="NO"){
        p = p + theme(#ggside.panel.scale = 1,
                ggside.axis.text = element_blank(),
                ggside.axis.ticks = element_blank())
    }
    p
}



plot_cor_o2m = function(data, label_size, x_name, title_name, 
        negative_color, positive_color, axis_size, title_size,
        custom_theme){
    p = data %>% 
        dplyr::arrange(estimate) %>% 
        dplyr::mutate(cancer = factor(cancer, levels = cancer)) %>% 
        dplyr::mutate(group = estimate>0) %>% 
        ggplot(aes(x=cancer, y=estimate, fill=group)) + 
        geom_col(color="black") + 
        geom_text(aes(y=0,label=format(round(estimate,2), nsmall =2),
                    hjust = ifelse(estimate >= 0, 1.5, -0.5)),
                size = label_size) +
        xlab("") + ylab(x_name) + #转置
        ggtitle(label = title_name) +
        coord_flip() +
        scale_fill_manual(values = c(negative_color,positive_color))
    # change theme
    p = p + custom_theme
    p = p + 
        theme(legend.position = "none", 
            text = element_text(size=axis_size),
            plot.title = element_text(size=title_size, hjust = 0.5))
    return(p)
}
