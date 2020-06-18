callModule(server.home_search_box, "homepage_pancan_search")

output$Xenasummary1 <- plotly::renderPlotly({
  p <- Xena_summary %>%
    ggplot(aes(x = Hub, y = n_cohort, fill = Hub)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 15) + # 去除背景色
    ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
    ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
    ggplot2::guides(fill = FALSE) +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_fill_manual(values = mycolor)
  plotly::ggplotly(p) %>% plotly::layout(showlegend = FALSE)
})

output$Xenasummary2 <- plotly::renderPlotly({
  p <- Xena_summary %>%
    ggplot(aes(x = Hub, y = n_dataset, fill = Hub)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 15) + # 去除背景色
    ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
    ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
    ggplot2::guides(fill = FALSE) +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_fill_manual(values = mycolor)
  plotly::ggplotly(p) %>% plotly::layout(showlegend = FALSE)
})

# output$Xenasummary <- plotly::renderPlotly({
#   p <- dat_datasets %>%
#     #  filter(XenaHostNames == "gdcHub") %>%
#     dplyr::rename(
#       Hub = XenaHostNames, Percent = Sample_percent,
#       Cohort = XenaCohorts, DatasetCount = N
#     ) %>%
#     ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = DatasetCount)) +
#     ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
#     ggplot2::coord_flip() +
#     ggplot2::labs(y = "", x = "") +
#     ggplot2::theme_bw(base_size = 15) + # 去除背景色
#     ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
#     ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
#     ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
#     ggplot2::theme(
#       axis.line.x = ggplot2::element_blank(),
#       axis.ticks.x = ggplot2::element_blank(),
#       axis.text.x = ggplot2::element_blank()
#     ) + # 去除x轴
#     ggplot2::guides(fill = F) +
#     ggplot2::guides(color = F) +
#     ggplot2::scale_fill_manual(values = mycolor)
#
#   plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
# })
# output$Xenasummary1 <- plotly::renderPlotly({
#   p <- dat_samples %>%
#     #  filter(XenaHostNames == "gdcHub") %>%
#     dplyr::rename(
#       Hub = XenaHostNames, Percent = SampleCount_percent,
#       Cohort = XenaCohorts, SampleCount = SampleCount_sum
#     ) %>%
#     ggplot2::ggplot(ggplot2::aes(x = Hub, y = Percent, fill = Cohort, label = SampleCount)) +
#     ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
#     ggplot2::coord_flip() +
#     ggplot2::labs(y = "", x = "") +
#     ggplot2::theme_bw(base_size = 15) + # 去除背景色
#     ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
#     ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
#     ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
#     ggplot2::theme(
#       axis.line.x = ggplot2::element_blank(),
#       axis.ticks.x = ggplot2::element_blank(),
#       axis.text.x = ggplot2::element_blank()
#     ) + # 去除x轴
#     ggplot2::guides(fill = F) +
#     ggplot2::guides(color = F) +
#     ggplot2::scale_fill_manual(values = mycolor)
#   plotly::ggplotly(p, tooltip = c("fill", "label")) %>% plotly::layout(showlegend = FALSE)
# })
