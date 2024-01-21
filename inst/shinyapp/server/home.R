callModule(server.home_search_box, "homepage_pancan_search")


# output$Xenasummary1 <- renderPlot({
#   p <- Xena_summary %>%
#     ggplot(aes(x = Hub, y = n_cohort, fill = Hub)) +
#     ggplot2::geom_bar(stat = "identity", width = 0.8) +
#     ggplot2::coord_flip() +
#     ggplot2::labs(x = NULL, y = NULL) +
#     ggplot2::theme_bw(base_size = 15) + # 去除背景色
#     ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
#     ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
#     ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
#     ggplot2::guides(fill = "none") +
#     ggplot2::guides(color = "none") +
#     ggplot2::scale_fill_manual(values = mycolor)
#   # plotly::ggplotly(p) %>% plotly::layout(showlegend = FALSE)
#   p
# })

# output$Xenasummary2 <- renderPlot({
#   p <- Xena_summary %>%
#     ggplot(aes(x = Hub, y = n_dataset, fill = Hub)) +
#     ggplot2::geom_bar(stat = "identity", width = 0.8) +
#     ggplot2::coord_flip() +
#     ggplot2::labs(x = NULL, y = NULL) +
#     ggplot2::theme_bw(base_size = 15) + # 去除背景色
#     ggplot2::theme(panel.grid = ggplot2::element_blank()) + # 去除网格线
#     ggplot2::theme(panel.border = ggplot2::element_blank()) + # 去除外层边框
#     ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) + # 沿坐标轴显示直线
#     ggplot2::guides(fill = FALSE) +
#     ggplot2::guides(color = FALSE) +
#     ggplot2::scale_fill_manual(values = mycolor)
#   # plotly::ggplotly(p) %>% plotly::layout(showlegend = FALSE)
#   p
# })




output$slick_output <- slickR::renderSlickR({
  # imgs <- list.files(
  #   system.file("app/www/slick_img", package = "coco"),
  #   pattern = ".PNG",
  #   full.names = TRUE
  # )
  imgs = paste0("slick_img/N",1:6,".png")

  # n1 <- htmltools::tags$p("UCSC Xena ssssssssssssssssssssssssssssssssssss", 
  #   style = htmltools::css(color = "#d93d45", "font-style" = "italic"))
  # n2 <- htmltools::tags$p("Grouping", style = htmltools::css(color = "#d93d45", "font-style" = "italic"))
  # n3 <- htmltools::tags$p("Cox-Analysis", style = htmltools::css(color = "#d93d45", "font-style" = "italic"))
  # n4 <- htmltools::tags$p("Distribution", style = htmltools::css(color = "#d93d45", "font-style" = "italic"))
  # n5 <- htmltools::tags$p("DE circRNAs", style = htmltools::css(color = "#d93d45", "font-style" = "italic"))
  x = slickR::slickR(imgs, height = 600, width = "80%", slideType = 'img-lazy')  +
    # (
    #   slickR::slickR(list(n1, n2), 
    #       slideType = "p", objLinks = c("https://xenabrowser.net/datapages/",NULL)) +
        slickR::settings(arrows = TRUE, dots = TRUE, autoplay = TRUE, autoplaySpeed = 2500)
    # )
    # )
  # y = slickR::slickR(list(n1, n2),slideType = 'iframe',height = 100, width = "60%")  +
  #       slickR::settings(arrows = FALSE)

  # x %synch% y
  x
})

