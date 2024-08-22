## scatter plot for individual mode of TPC correlation analysis
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


## bar plot for pan-cancer mode of TPC correlation analysis
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


## violin plot for individual mode of TPC comparison analysis
plot_comb_o2o = function(data, xlab, ylab, title, comp_method, 
        point_size, point_alpha, group_1_color, group_2_color, 
        axis_size, title_size, custom_theme){

    names(comp_method) = ifelse(comp_method=="t-test","parametric","nonparametric")

    p = ggbetweenstats(
        data = data, x = !!colnames(data)[4], y = !!colnames(data)[3],
        xlab = xlab, ylab = ylab, title = title, type = names(comp_method),
        centrality.plotting = FALSE, median.color = 'black',
        bf.message = FALSE,
        point.args = list(size = point_size, alpha = point_alpha,
                          position = ggplot2::position_jitterdodge(dodge.width = 0.6), stroke = 0, na.rm = TRUE))
    p = p + ggplot2::scale_color_manual(values = c(group_1_color, group_2_color))
    # change theme
    p = p + custom_theme
    p = p + theme(text = element_text(size=axis_size),
                  legend.position = "none", 
			      plot.title = element_text(size=title_size, hjust = 0.5),
			      plot.subtitle = element_text(size = 14))
    pval = formatC(extract_stats(p)$subtitle_data$p.value, digits = 3, format = 'e')
    p$labels$subtitle = bquote(paste(.(comp_method),", ",italic(p) == .(pval)))
    return(p)
}

## line plot for pan-cancer mode of TPC comparison analysis
plot_comb_o2m = function(data1, data2, x_name, title_name, 
        group_1_color_2, group_2_color_2, axis_size, title_size,
        significance, label_size, custom_theme){
    ### p1
    data1_sub = data1 %>%
        dplyr::filter(cancer %in% as.character(unique(data2$cancer))) %>%
        dplyr::arrange(desc(cancer)) %>%
        dplyr::mutate(cancer = factor(cancer, levels = unique(cancer)))
		
    p1 = ggplot(data1_sub) +
        stat_summary(aes(x=cancer, y=value, color=group),
                    # colour = "gray",
                    position=position_dodge(width=0.5)) + 
        geom_jitter(aes(x=cancer, y=value, color=group),
                    position=position_dodge(width=0.5), size = 1, 
                    # colour = "gray", 
                    alpha = 0.4) +
        xlab("") + ylab(x_name) + ggtitle(label = title_name) + #转置
		scale_color_manual(values = c(group_1_color_2, group_2_color_2)) +
        coord_flip() + custom_theme
    p1 = p1 + theme(legend.position = "top", plot.margin = margin(0,0,0,0),
                  text = element_text(size=axis_size),
                  plot.title = element_text(size=title_size, hjust = 0.5))

    ### p2
    if(significance=="Value"){
        p2 = ggplot(data2) + 
            geom_text(aes(label=formatC(p.value, format = "e", digits = 2),
                            x=cancer, y=1), size = label_size) +
            coord_flip()
    } else if (significance=="Symbol"){
        p2 = data2 %>%
            dplyr::mutate(p.label=case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01 ~ "**",
                p.value < 0.05 ~ "*",
                TRUE ~ "ns"
            )) %>% ggplot() + 
            geom_text(aes(label=p.label, x=cancer, y=1), size = label_size) +
            coord_flip()
    }
    p2 = p2 + custom_theme +
        theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.title = element_blank(),
            axis.ticks.length.y = unit(0,"pt"),
            plot.margin = margin(0,0,0,0)) +
        theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
        theme(text = element_text(size=axis_size))

    p = p1 + p2 + patchwork::plot_layout(widths = c(5,0.3))
}


## line plot for individual mode of TPC survival analysis
plot_sur_o20 = function(data, plot_CI, plot_table, plot_ncensor, sur_method,
        one_log_color1, one_log_color2, axis_size, x_name, title_name, title_size,
        text_c1, text_c2, text_c3, axis_size_2, title_name_2){

    if(plot_CI == "NO"){
        conf.int = FALSE
        conf.int.style = "ribbon"
    } else if(plot_CI == "YES(ribbon)"){
        conf.int = TRUE
        conf.int.style = "ribbon"
    } else if(plot_CI == "YES(step)"){
        conf.int = TRUE
        conf.int.style = "step"
    }

    custom_theme <- function(plot_size) {
        theme_classic() %+replace%
        theme(
            plot.title=element_text(hjust=0.5, size = plot_size)
        )
    }

    if(plot_table=="NO" & plot_ncensor=="NO"){
        surv.plot.height = 1
        risk.table.height = 0 
        ncensor.plot.height = 0 
        risk.table = FALSE
        ncensor.plot = FALSE
    } else if (plot_table=="YES" & plot_ncensor=="NO"){
        surv.plot.height = 0.7
        risk.table.height = 0.3 
        ncensor.plot.height = 0
        risk.table = TRUE
        ncensor.plot = FALSE
    } else if (plot_table=="NO" & plot_ncensor=="YES"){
        surv.plot.height = 0.7
        risk.table.height = 0 
        ncensor.plot.height = 0.3 
        risk.table = FALSE
        ncensor.plot = TRUE
    } else if (plot_table=="YES" & plot_ncensor=="YES"){
        surv.plot.height = 0.7
        risk.table.height = 0.15
        ncensor.plot.height = 0.15
        risk.table = TRUE
        ncensor.plot = TRUE
    }

    if(sur_method=="Log-rank test"){	
        fit <- survfit(Surv(time, status) ~ Group, data = data)
        p <- ggsurvplot(fit, data = data,#data = group_sur_final(), 
                        pval = TRUE, pval.method = TRUE, 
                        palette = c(one_log_color1, one_log_color2), 
                        size = 1.2, font.legend = c(14, "black"), 
                        font.x = c(axis_size, "bold", "black"), 
                        font.y = c(axis_size,  "bold", "black"), 
                        font.tickslab = c(12, "bold", "black"), 
                        xlab = x_name,
                        title = title_name,
                        conf.int = conf.int,
                        conf.int.style = conf.int.style,
                        risk.table = risk.table, risk.table.col = "strata", risk.table.y.text = FALSE, 
                        ncensor.plot = ncensor.plot, 
                        surv.plot.height = surv.plot.height, 
                        risk.table.height = risk.table.height, 
                        ncensor.plot.height = ncensor.plot.height, 
                        ggtheme = custom_theme(title_size))
    }  else if (sur_method=="Univariate Cox regression"){
        fit = coxph(Surv(time, status) ~ Group , data = data)
        p = ggforest(fit,data = data,#fontsize = 1,
            cpositions = c(text_c1, text_c2, text_c3),
            fontsize = axis_size_2, main = title_name_2)
    }

}


## bar plot for pan-cancer mode of TPC survival analysis
# data$sur_res
# data$sur_dat
plot_sur_02m = function(data, sur_method, multi_log_color1, multi_log_color2,
        x_name, title_name, multi_log_line, axis_size, title_size, multi_log_label,
        label_size, cox_h_g1_color, cox_h_l1_color, x_name_2, title_name_2, 
        multi_cox_line, axis_size_2, title_size_2, multi_cox_label, label_size_2,
        custom_theme){
    if(sur_method=="Log-rank test"){
        dat = data$sur_res
        # print(head(dat))
        pval_df = dat %>%
            dplyr::select(Cancer, Group, rmean, p.value) %>% 
            dplyr::group_by(Cancer) %>% 
            dplyr::mutate(Risk = ifelse(rmean[1]>rmean[2],
                                        paste0("Low risk(", Group[1],")"), 
                                        paste0("High risk(", Group[1],")"))) %>% 
            dplyr::distinct(Cancer, p.value, Risk) %>% as.data.frame() %>% 
            dplyr::mutate(Cancer = factor(Cancer, levels = rev(sort(Cancer))))
        # print(head(pval_df))
        fill_cols = c(multi_log_color1, multi_log_color2)
        names(fill_cols) = c(
            paste0("Low risk(Group=",levels(data$sur_dat$Group)[1], ")"),
            paste0("High risk(Group=",levels(data$sur_dat$Group)[1], ")"))
        p1 = pval_df %>% 
            dplyr::mutate(pval_log = -log10(p.value)) %>% 
            dplyr::mutate(pval_log = ifelse(pval_log<10,pval_log, 10)) %>% 
            ggplot(aes(x = Cancer, y = pval_log, fill = Risk)) + 
            geom_col() +
            scale_fill_manual(values = fill_cols) + 
            xlab(NULL) + ylab(x_name) +
            ggtitle(label = title_name) +
            geom_hline(yintercept = -log10(multi_log_line), color = "red") +
            coord_flip() +
            custom_theme +
            # theme_minimal() +
            theme(legend.position = "top",
                plot.margin = margin(0,0,0,0),
                text = element_text(size=axis_size),
                plot.title = element_text(size=title_size, hjust = 0.5)
                )  
        if(multi_log_label=="Signif.(value)"){
            p2 =  ggplot(pval_df) +
                geom_text(aes(label=formatC(p.value, format = "e", digits = 2),
                            x=Cancer,y=1),size = label_size) +
                coord_flip()
        } else if (multi_log_label=="Signif.(symbol)"){
            p2 = pval_df %>%
                dplyr::mutate(p.label=case_when(
                p.value  < 0.001 ~ "***",
                p.value  < 0.01 ~ "**",
                p.value  < 0.05 ~ "*",
                TRUE ~ "ns"
                )) %>% ggplot() +
                geom_text(aes(label=p.label,x=Cancer,y=1),size = label_size) + 
                coord_flip()
        }
        p2 = p2 +
            custom_theme +
            # theme_minimal() +
            theme(axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.title = element_blank(),
                axis.ticks.length.y = unit(0,"pt"),
                plot.margin = margin(0,0,0,0)) +
            theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
        p = p1 + p2 + plot_layout(widths = c(5,1))
        p
    } else if (sur_method=="Univariate Cox regression") {
        dat = data$sur_dat
        sur_res = lapply(sort(unique(dat$Cancer)), function(x){
            sur_dat_sub = subset(dat, Cancer==x)
            fit = coxph(Surv(time, status) ~ Group , data = sur_dat_sub)
            summary(fit)$coefficients %>% as.data.frame() %>% 
            tibble::rownames_to_column("Group") %>% 
            dplyr::mutate(Cancer = x, .before = 1)
        }) %>% do.call(rbind, .)
        fill_cols = c(cox_h_g1_color, cox_h_l1_color)
        names(fill_cols) = c("HR>1","HR<1")
        p1 = sur_res %>%
            dplyr::rename(`p.value`=`Pr(>|z|)`, `HR` = `exp(coef)`) %>% 
            dplyr::select(Cancer, Group, HR, p.value) %>% 
            dplyr::mutate(Cancer = factor(Cancer, levels = rev(sort(Cancer)))) %>%
            dplyr::mutate(HR.label = paste0("HR=",formatC(HR, format = "e", digits = 2))) %>%
            dplyr::mutate(p.value_label = formatC(p.value, format = "e", digits = 2)) %>% 
            dplyr::mutate(p.value_symbol=case_when(
                p.value  < 0.001 ~ "***",
                p.value  < 0.01 ~ "**",
                p.value  < 0.05 ~ "*",
                TRUE ~ "ns"
            )) %>% 
            dplyr::mutate(pval_log = -log10(p.value)) %>% 
            dplyr::mutate(pval_log = ifelse(pval_log<10,pval_log, 10)) %>% 
            dplyr::mutate(Direct = ifelse(HR>1,"HR>1","HR<1")) %>% 
            ggplot(aes(x = Cancer, y = pval_log, fill = Direct)) + 
            geom_col(position="dodge") + 
            scale_fill_manual(values = fill_cols) + 
            xlab(NULL) + ylab(x_name_2) +
            ggtitle(label = title_name_2) +
            guides(fill = guide_legend(title = paste0("Group:",levels(data$sur_dat$Group)[2]))) +
            geom_hline(yintercept = -log10(multi_cox_line), color = "red") +
            coord_flip() +
            custom_theme + 
            # theme_minimal() +
            theme(legend.position = "top",
                text = element_text(size=axis_size_2),
                plot.title = element_text(size=isolate(title_size_2), hjust = 0.5)
                )  
        if(multi_cox_label=="HR value"){
            p = p1 + 
                geom_text(aes(y = max(pval_log),label = HR.label), size = label_size_2,
                        position=position_dodge(width=0.9), hjust = 1)
        } else if (multi_cox_label=="Signif.(symbol)"){
            p = p1 + 
                geom_text(aes(y = max(pval_log),label = p.value_symbol), size = label_size_2,
                        position=position_dodge(width=0.9), hjust = 1)
        } else if (multi_cox_label=="Signif.(value)"){
            p = p1 + 
                geom_text(aes(y = max(pval_log),label = p.value_label), size = label_size_2,
                        position=position_dodge(width=0.9), hjust = 1)
        }
    }
    p
}



