# cBioPortal Analysis Server
source(module_file("06_cbioportal/modules-cbioportal-study-selector.R"), local = TRUE)
source(module_file("06_cbioportal/modules-cbioportal-cor-o2o.R"), local = TRUE)

# Initialize cBioPortal modules
callModule(modules_cbioportal_correlation_Server, "modules_cbioportal_correlation")

# Study explorer standalone module
callModule(modules_cbioportal_study_selector_Server, "study_explorer")