# # quick module
# # callModule(server.combo.sg.pancan.analysis, "combo.sg.pancan.analysis")
# observeEvent(req(input$navbar=="TEST"),{
#   callModule(server.modules_1_tcga_search, "id")
# }, once = TRUE)  

observeEvent(req(input$navbar=="TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)"),{
  callModule(server.modules_1_tcga_01, "modules_1_tcga_01")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA+GTEx: Molecular Profile Anatomy"),{
  callModule(server.modules_1_tcga_02, "modules_1_tcga_02")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecule-Molecule Correlation"),{
  callModule(server.modules_1_tcga_03, "modules_1_tcga_03")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Tumor Immune Infiltration"),{
  callModule(server.modules_1_tcga_04, "modules_1_tcga_04")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Immune Signature"),{
  callModule(server.modules_1_tcga_05, "modules_1_tcga_05")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and TMB/Stemness/MSI (Radar Show)"),{
  callModule(server.modules_1_tcga_06, "modules_1_tcga_06")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Pathway Score"),{
  callModule(server.modules_1_tcga_07, "modules_1_tcga_07")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Gene Mutation"),{
  callModule(server.modules_1_tcga_08, "modules_1_tcga_08")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecular Profile Kaplan-Meier Analysis"),{
  callModule(server.modules_1_tcga_09, "modules_1_tcga_09")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecular Profile Cox Regression Analysis"),{
  callModule(server.modules_1_tcga_10, "modules_1_tcga_10")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Dimension Reduction Distribution"),{
  callModule(server.modules_1_tcga_11, "modules_1_tcga_11")
}, once = TRUE)  

observeEvent(req(input$navbar=="CCLE: Molecular Profile Distribution Across Cancer Primary Sites"),{
  callModule(server.modules_ccle_dist, "modules_ccle_dist")
}, once = TRUE)  

observeEvent(req(input$navbar=="CCLE: Molecule-Molecule Correlation"),{
  callModule(server.modules_ccle_genecor, "modules_ccle_genecor")
}, once = TRUE)  

observeEvent(req(input$navbar=="CCLE: Drug Response Association"),{
  callModule(server.modules_ccle_drug_target_asso, "modules_ccle_drug_target_asso")
}, once = TRUE)  

observeEvent(req(input$navbar=="CCLE: Drug Response Difference"),{
  callModule(server.modules_ccle_drug_response_diff, "modules_ccle_drug_response_diff")
}, once = TRUE)  


observeEvent(req(input$navbar=="PCAWG: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)"),{
  callModule(server.modules_pcawg_dist, "modules_pcawg_dist")
}, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Molecular Profile Cox Regression Analysis"),{
  callModule(server.modules_pcawg_unicox, "modules_pcawg_unicox")
}, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Molecule-Molecule Correlation"),{
  callModule(server.modules_pcawg_gene_cor, "modules_pcawg_gene_cor")
}, once = TRUE)  

observeEvent(req(input$navbar=="PCAWG: Molecular Profile Kaplan-Meier Analysis"),{
  callModule(server.modules_pcawg_sur_plot, "modules_pcawg_sur_plot")
}, once = TRUE)  



# TCGA pancan
observeEvent(req(input$navbar=="TCGA: Association Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
		callModule(server.modules_pancan_cor_o2o, "modules_pancan_cor_o2o")
		callModule(server.modules_pancan_cor_o2m, "modules_pancan_cor_o2m")
		callModule(server.modules_pancan_cor_m2o, "modules_pancan_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
		callModule(server.modules_pancan_comp_o2o, "modules_pancan_comp_o2o")
		callModule(server.modules_pancan_comp_o2m, "modules_pancan_comp_o2m")
		callModule(server.modules_pancan_comp_m2o, "modules_pancan_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Survival Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
		callModule(server.modules_pancan_sur_o2o, "modules_pancan_sur_o2o")
		callModule(server.modules_pancan_sur_o2m, "modules_pancan_sur_o2m")
		callModule(server.modules_pancan_sur_m2o, "modules_pancan_sur_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  



# PCAWG 
observeEvent(req(input$navbar=="PCAWG: Association Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
    callModule(server.modules_pcawg_cor_o2o, "modules_pcawg_cor_o2o")
    callModule(server.modules_pcawg_cor_o2m, "modules_pcawg_cor_o2m")
    callModule(server.modules_pcawg_cor_m2o, "modules_pcawg_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
    callModule(server.modules_pcawg_comp_o2o, "modules_pcawg_comp_o2o")
    callModule(server.modules_pcawg_comp_o2m, "modules_pcawg_comp_o2m")
    callModule(server.modules_pcawg_comp_m2o, "modules_pcawg_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Survival Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
    callModule(server.modules_pcawg_sur_o2o, "modules_pcawg_sur_o2o")
    callModule(server.modules_pcawg_sur_o2m, "modules_pcawg_sur_o2m")
    callModule(server.modules_pcawg_sur_m2o, "modules_pcawg_sur_m2o")
    on.exit(waiter$hide())
}, once = TRUE)  


# CCLE
observeEvent(req(input$navbar=="CCLE: Association Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
    callModule(server.modules_ccle_cor_o2o, "modules_ccle_cor_o2o")
    callModule(server.modules_ccle_cor_m2o, "modules_ccle_cor_m2o")
    on.exit(waiter$hide())
}, once = TRUE) 

observeEvent(req(input$navbar=="CCLE: Comparison Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
    callModule(server.modules_ccle_comp_o2o, "modules_ccle_comp_o2o")
    callModule(server.modules_ccle_comp_m2o, "modules_ccle_comp_m2o")
    on.exit(waiter$hide())
}, once = TRUE) 







# # identifier help
observeEvent(req(input$navbar=="TPC ID Query"),{
  waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
  waiter$show()
	callModule(server.modules_id_reference, "modules_id_reference")
  on.exit(waiter$hide())
}, once = TRUE) 




# # download data
observeEvent(req(input$navbar=="Based on TCGA/PCAWG/CCLE Analysis"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
	  callModule(server.modules_download_pancan, "modules_download_pancan")
    on.exit(waiter$hide())
}, once = TRUE) 

observeEvent(req(input$navbar=="Based on Repository Datasets"),{
    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
    waiter$show()
	  callModule(server.modules_download_dataset, "modules_download_dataset")
    on.exit(waiter$hide())
}, once = TRUE) 


# PharmacoGenomics
observeEvent(req(input$navbar %in% c(
  "Drugs-omics pairs Analysis",
  "Profiles Display: Features across different types",
  "Profiles Display: Profile of drug sensitivity",
  "Features database significant analysis",
  "Statistics and Annotations"
)),{

    waiter <- waiter::Waiter$new(color = "grey", fadeout = TRUE)
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
