# cBioPortal Analysis Server
# Note: Module files are already loaded by app_preset.R

# Initialize cBioPortal modules for the main "cBioPortal Analysis" tab
observeEvent(req(input$navbar=="cBioPortal Analysis"),{
  callModule(modules_cbioportal_correlation_Server, "modules_cbioportal_correlation")
  callModule(modules_cbioportal_study_selector_Server, "study_explorer")
}, once = TRUE)