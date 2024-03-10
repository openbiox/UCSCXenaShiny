# # quick module
# # callModule(server.combo.sg.pancan.analysis, "combo.sg.pancan.analysis")
observeEvent(req(input$navbar=="TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)"),{
  callModule(server.modules_pancan_dist, "module_gene_pancan_dist")
}, once = TRUE)  

# observeEvent(req(input$navbar=="TCGA+GTEx: Molecular Profile Distribution (Tumor VS Normal)"),{
#   callModule(server.modules_cancer_dist, "modules_cancer_dist")
# }, once = TRUE)  

observeEvent(req(input$navbar=="TCGA+GTEx: Molecular Profile Anatomy"),{
  callModule(server.modules_pancan_anatomy, "modules_pancan_anatomy")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecule-Molecule Correlation"),{
  callModule(server.modules_pancan_gene_cor, "modules_pancan_gene_cor")
}, once = TRUE)  


observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Immune Signature"),{
  callModule(server.modules_pancan_immune, "modules_pancan_immune")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Tumor Immune Infiltration"),{
  callModule(server.modules_pancan_til, "modules_pancan_til")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and TMB/Stemness/MSI (Radar Show)"),{
  callModule(server.modules_pancan_radar, "modules_pancan_radar")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Pathway Score"),{
  callModule(server.modules_pw_cor, "modules_pw_cor")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Association Between Molecular Profile and Gene Mutation"),{
  callModule(server.modules_pancan_mut, "modules_pancan_mut")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecular Profile Cox Regression Analysis"),{
  callModule(server.modules_pancan_unicox, "modules_pancan_unicox")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Molecular Profile Kaplan-Meier Analysis"),{
  callModule(server.modules_sur_plot, "modules_sur_plot")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Dimension Reduction Distribution"),{
  callModule(server.modules_dim_dist, "modules_dim_dist")
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
		callModule(server.modules_pancan_cor_o2o, "modules_pancan_cor_o2o")
		callModule(server.modules_pancan_cor_o2m, "modules_pancan_cor_o2m")
		callModule(server.modules_pancan_cor_m2o, "modules_pancan_cor_m2o")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Comparison Analysis"),{
		callModule(server.modules_pancan_comp_o2o, "modules_pancan_comp_o2o")
		callModule(server.modules_pancan_comp_o2m, "modules_pancan_comp_o2m")
		callModule(server.modules_pancan_comp_m2o, "modules_pancan_comp_m2o")
}, once = TRUE)  

observeEvent(req(input$navbar=="TCGA: Survival Analysis"),{
		callModule(server.modules_pancan_sur_o2o, "modules_pancan_sur_o2o")
		callModule(server.modules_pancan_sur_o2m, "modules_pancan_sur_o2m")
		callModule(server.modules_pancan_sur_m2o, "modules_pancan_sur_m2o")
}, once = TRUE)  



# PCAWG 
observeEvent(req(input$navbar=="PCAWG: Association Analysis"),{
	callModule(server.modules_pcawg_cor_o2o, "modules_pcawg_cor_o2o")
	callModule(server.modules_pcawg_cor_o2m, "modules_pcawg_cor_o2m")
	callModule(server.modules_pcawg_cor_m2o, "modules_pcawg_cor_m2o")
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Comparison Analysis"),{
	callModule(server.modules_pcawg_comp_o2o, "modules_pcawg_comp_o2o")
	callModule(server.modules_pcawg_comp_o2m, "modules_pcawg_comp_o2m")
	callModule(server.modules_pcawg_comp_m2o, "modules_pcawg_comp_m2o")
}, once = TRUE)  
observeEvent(req(input$navbar=="PCAWG: Survival Analysis"),{
	callModule(server.modules_pcawg_sur_o2o, "modules_pcawg_sur_o2o")
	callModule(server.modules_pcawg_sur_o2m, "modules_pcawg_sur_o2m")
	callModule(server.modules_pcawg_sur_m2o, "modules_pcawg_sur_m2o")
}, once = TRUE)  


# CCLE
observeEvent(req(input$navbar=="CCLE: Association Analysis"),{
	callModule(server.modules_ccle_cor_o2o, "modules_ccle_cor_o2o")
	callModule(server.modules_ccle_cor_m2o, "modules_ccle_cor_m2o")
}, once = TRUE) 

observeEvent(req(input$navbar=="CCLE: Comparison Analysis"),{
	callModule(server.modules_ccle_comp_o2o, "modules_ccle_comp_o2o")
	callModule(server.modules_ccle_comp_m2o, "modules_ccle_comp_m2o")
}, once = TRUE) 







# # identifier help
observeEvent(req(input$navbar=="TPC ID Query"),{
	callModule(server.modules_id_reference, "modules_id_reference")
}, once = TRUE) 




# # download data
observeEvent(req(input$navbar=="Based on TCGA/PCAWG/CCLE Analysis"),{
	callModule(server.modules_download_pancan, "modules_download_pancan")
}, once = TRUE) 

observeEvent(req(input$navbar=="Based on Repository Datasets"),{
	callModule(server.modules_download_dataset, "modules_download_dataset")
}, once = TRUE) 


# PharmacoGenomics
observeEvent(req(input$navbar %in% c(
  "Drugs-omics pairs Analysis",
  "Profiles Display: Features across different types",
  "Profiles Display: Profile of drug sensitivity",
  "Features database significant analysis",
  "Statistics and Annotations"
)),{
  message("Preprocessing drug omics data...")
  source(system.file("shinyapp/PGdata.R", package = "UCSCXenaShiny"))
  
  callModule(serverDrugOmicPair, "DrugOmicPair")
  callModule(serverFeatureAcrossType, "FeatureAcrossType")
  callModule(serverProfileDrugSens, "ProfileDrugSens")
  callModule(serverFeatureDatabaseSig, "FeatureDatabaseSig")
  callModule(serverStatAnno, "StatAnno")
  message("Done for loading data and modules.")
}, once = TRUE) 


callModule(server.home_search_box, "homepage_pancan_search")
callModule(server.home_daily_gene, "homepage_daily_gene")
