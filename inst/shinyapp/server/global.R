observeEvent(input$hiplot_switch, {
  message("Setting mirror, use hiplot: ", input$hiplot_switch)
  if (input$hiplot_switch) {
    # Check the XenaTools version
    if (utils::packageVersion("UCSCXenaTools") < "1.4.3") {
      show_alert(
        title = "Error !!",
        text = "Cannot work with Hiplot mirror if UCSCXenaTools version< 1.4.3, please upgrade it!",
        type = "error"
      )
    }
    options(use_hiplot = TRUE)
  } else {
    options(use_hiplot = FALSE)
  }
})
