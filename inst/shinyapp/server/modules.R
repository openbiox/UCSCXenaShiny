html_spin = tagList(spin_1(), br(),
                    h2("Loading independent modules (Only needed for the first time)"))

# # quick module
observeEvent(req(input$navbar=="TCGA (GTEx): Molecular comparison"),{
  callModule(server.modules_1_tcga_01, "modules_1_tcga_01")
  callModule(server.modules_1_tcga_02, "modules_1_tcga_02")
  callModule(server.modules_1_tcga_08, "modules_1_tcga_08")
}, once = TRUE)  


observeEvent(req(input$navbar=="TCGA: Molecular correlation"),{
  callModule(server.modules_1_tcga_03, "modules_1_tcga_03")
  callModule(server.modules_1_tcga_04, "modules_1_tcga_04")
  callModule(server.modules_1_tcga_05, "modules_1_tcga_05")
  callModule(server.modules_1_tcga_06, "modules_1_tcga_06")
  callModule(server.modules_1_tcga_07, "modules_1_tcga_07")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Survival analysis"),{
  callModule(server.modules_1_tcga_09, "modules_1_tcga_09")
  callModule(server.modules_1_tcga_10, "modules_1_tcga_10")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Dimensionality reduction"),{
  callModule(server.modules_1_tcga_11, "modules_1_tcga_11")
}, once = TRUE)  

# observeEvent(req(input$navbar=="TCGA: Pathway cross analysis"),{
#   callModule(server.modules_1_tcga_12, "modules_1_tcga_12")
# }, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Molecular comparison"),{
  callModule(server.modules_2_pcawg_01, "modules_2_pcawg_01")
}, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Molecular correlation"),{
  callModule(server.modules_2_pcawg_02, "modules_2_pcawg_02")
}, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Survival analysis"),{
  callModule(server.modules_2_pcawg_03, "modules_2_pcawg_03")
  callModule(server.modules_2_pcawg_04, "modules_2_pcawg_04")
}, once = TRUE)  


observeEvent(req(input$navbar=="CCLE: Molecular comparison"),{
  callModule(server.modules_3_ccle_01, "modules_3_ccle_01")
}, once = TRUE)  

observeEvent(req(input$navbar=="CCLE: Molecular correlation"),{
  callModule(server.modules_3_ccle_02, "modules_3_ccle_02")
}, once = TRUE)   

observeEvent(req(input$navbar=="CCLE: Drug analysis"),{
  waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
  waiter$show()
  callModule(server.modules_3_ccle_03, "modules_3_ccle_03")
  callModule(server.modules_3_ccle_04, "modules_3_ccle_04")
  on.exit(waiter$hide())
}, once = TRUE)  

# cBioPortal modules
observeEvent(req(input$navbar=="cBioPortal: Molecular analysis"),{
  waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
  waiter$show()
  callModule(modules_cbioportal_correlation_Server, "modules_cbioportal_correlation")
  callModule(modules_cbioportal_study_selector_Server, "quick_study_explorer")
  on.exit(waiter$hide())
}, once = TRUE)



## TPC 

# TCGA pancan
observeEvent(req(input$navbar=="TCGA: Correlation Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
		callModule(server.modules_pancan_cor_o2o, "modules_pancan_cor_o2o")
		callModule(server.modules_pancan_cor_o2m, "modules_pancan_cor_o2m")
		callModule(server.modules_pancan_cor_m2o, "modules_pancan_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
		callModule(server.modules_pancan_comp_o2o, "modules_pancan_comp_o2o")
		callModule(server.modules_pancan_comp_o2m, "modules_pancan_comp_o2m")
		callModule(server.modules_pancan_comp_m2o, "modules_pancan_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Survival Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
		callModule(server.modules_pancan_sur_o2o, "modules_pancan_sur_o2o")
		callModule(server.modules_pancan_sur_o2m, "modules_pancan_sur_o2m")
		callModule(server.modules_pancan_sur_m2o, "modules_pancan_sur_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Cross-Omics Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
		callModule(server.modules_pancan_cross_gene_o2m, "modules_pancan_cross_gene_o2m")
		callModule(server.modules_pancan_cross_pw_o2m, "modules_pancan_cross_pw_o2m")
    on.exit(waiter$hide())
}, once = TRUE)  

# PCAWG 
observeEvent(req(input$navbar=="PCAWG: Correlation Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    callModule(server.modules_pcawg_cor_o2o, "modules_pcawg_cor_o2o")
    callModule(server.modules_pcawg_cor_o2m, "modules_pcawg_cor_o2m")
    callModule(server.modules_pcawg_cor_m2o, "modules_pcawg_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    callModule(server.modules_pcawg_comp_o2o, "modules_pcawg_comp_o2o")
    callModule(server.modules_pcawg_comp_o2m, "modules_pcawg_comp_o2m")
    callModule(server.modules_pcawg_comp_m2o, "modules_pcawg_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Survival Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    callModule(server.modules_pcawg_sur_o2o, "modules_pcawg_sur_o2o")
    callModule(server.modules_pcawg_sur_o2m, "modules_pcawg_sur_o2m")
    callModule(server.modules_pcawg_sur_m2o, "modules_pcawg_sur_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  


# CCLE
observeEvent(req(input$navbar=="CCLE: Correlation Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    callModule(server.modules_ccle_cor_o2o, "modules_ccle_cor_o2o")
    callModule(server.modules_ccle_cor_m2o, "modules_ccle_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE) 

observeEvent(req(input$navbar=="CCLE: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    callModule(server.modules_ccle_comp_o2o, "modules_ccle_comp_o2o")
    callModule(server.modules_ccle_comp_m2o, "modules_ccle_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE) 




# # identifier help
observeEvent(req(input$navbar=="TPC ID Query"),{
  waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
  waiter$show()
	callModule(server.modules_id_reference, "modules_id_reference")
  on.exit(waiter$hide())
}, once = TRUE) 




# # download data
observeEvent(req(input$navbar=="The integrated TPC data"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
	  callModule(server.modules_download_pancan, "modules_download_pancan")
    on.exit(waiter$hide())
}, once = TRUE) 

observeEvent(req(input$navbar=="The Repository Dataset"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
	  callModule(server.modules_download_dataset, "modules_download_dataset")
    on.exit(waiter$hide())
}, once = TRUE) 


# PharmacoGenomics
observeEvent(req(input$navbar %in% c(
  "Drug-Omics Correlation Analysis",
  "Feature Abundance Profile in Databases",
  "Dimension Reduction Profile of Cell Drug Sensitivity",
  "Feature Scaling Association Analysis",
  "Statistics and Annotations"
)),{

    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE, html = html_spin)
    waiter$show()
    message("Preprocessing drug omics data...")
    source(system.file("shinyapp/PGdata.R", package = "UCSCXenaShiny"))
    callModule(serverDrugOmicPair, "DrugOmicPair")
    callModule(serverFeatureAcrossType, "FeatureAcrossType")
    callModule(serverProfileDrugSens, "ProfileDrugSens")
    callModule(serverFeatureDatabaseSig, "FeatureDatabaseSig")
    callModule(serverStatAnno, "StatAnno")
    message("Done for loading data and modules.")
    on.exit(waiter$hide())
}, once = TRUE) 


callModule(server.home_search_box, "homepage_pancan_search")
callModule(server.home_daily_gene, "homepage_daily_gene")
