# quick module
# callModule(server.combo.sg.pancan.analysis, "combo.sg.pancan.analysis")
callModule(server.modules_pancan_dist, "module_gene_pancan_dist")
callModule(server.modules_cancer_dist, "modules_cancer_dist")
callModule(server.modules_pancan_anatomy, "modules_pancan_anatomy")
callModule(server.modules_pancan_gene_cor, "modules_pancan_gene_cor")
callModule(server.modules_pancan_unicox, "modules_pancan_unicox")
callModule(server.modules_pancan_immune, "modules_pancan_immune")
callModule(server.modules_pancan_til, "modules_pancan_til")
callModule(server.modules_pancan_radar, "modules_pancan_radar")
callModule(server.modules_pw_cor, "modules_pw_cor")
callModule(server.modules_pancan_mut, "modules_pancan_mut")
callModule(server.modules_sur_plot, "modules_sur_plot")
callModule(server.modules_dim_dist, "modules_dim_dist")

callModule(server.modules_ccle_dist, "modules_ccle_dist")
callModule(server.modules_ccle_genecor, "modules_ccle_genecor")
callModule(server.modules_ccle_drug_target_asso, "modules_ccle_drug_target_asso")
callModule(server.modules_ccle_drug_response_diff, "modules_ccle_drug_response_diff")

callModule(server.modules_pcawg_dist, "modules_pcawg_dist")
callModule(server.modules_pcawg_unicox, "modules_pcawg_unicox")
callModule(server.modules_pcawg_gene_cor, "modules_pcawg_gene_cor")
callModule(server.modules_pcawg_sur_plot, "modules_pcawg_sur_plot")




# # TCGA pancan
callModule(server.modules_pancan_cor_o2o, "modules_pancan_cor_o2o")
callModule(server.modules_pancan_cor_o2m, "modules_pancan_cor_o2m")
callModule(server.modules_pancan_cor_m2o, "modules_pancan_cor_m2o")

callModule(server.modules_pancan_comp_o2o, "modules_pancan_comp_o2o")
callModule(server.modules_pancan_comp_o2m, "modules_pancan_comp_o2m")
callModule(server.modules_pancan_comp_m2o, "modules_pancan_comp_m2o")

callModule(server.modules_pancan_sur_o2o, "modules_pancan_sur_o2o")
callModule(server.modules_pancan_sur_o2m, "modules_pancan_sur_o2m")
callModule(server.modules_pancan_sur_m2o, "modules_pancan_sur_m2o")


# # PCAWG 
callModule(server.modules_pcawg_cor_o2o, "modules_pcawg_cor_o2o")
callModule(server.modules_pcawg_cor_o2m, "modules_pcawg_cor_o2m")
callModule(server.modules_pcawg_cor_m2o, "modules_pcawg_cor_m2o")

callModule(server.modules_pcawg_comp_o2o, "modules_pcawg_comp_o2o")
callModule(server.modules_pcawg_comp_o2m, "modules_pcawg_comp_o2m")
callModule(server.modules_pcawg_comp_m2o, "modules_pcawg_comp_m2o")

callModule(server.modules_pcawg_sur_o2o, "modules_pcawg_sur_o2o")
callModule(server.modules_pcawg_sur_o2m, "modules_pcawg_sur_o2m")
callModule(server.modules_pcawg_sur_m2o, "modules_pcawg_sur_m2o")


# # CCLE

callModule(server.modules_ccle_cor_o2o, "modules_ccle_cor_o2o")
callModule(server.modules_ccle_cor_m2o, "modules_ccle_cor_m2o")

callModule(server.modules_ccle_comp_o2o, "modules_ccle_comp_o2o")
callModule(server.modules_ccle_comp_m2o, "modules_ccle_comp_m2o")




# # identifier help
callModule(server.modules_id_reference, "modules_id_reference")

# # download data
callModule(server.modules_download_pancan, "modules_download_pancan")
callModule(server.modules_download_dataset, "modules_download_dataset")

# PharmacoGenomics analysis
callModule(serverDrugOmicPair, "DrugOmicPair")
callModule(serverFeatureAcrossType, "FeatureAcrossType")
callModule(serverProfileDrugSens, "ProfileDrugSens")
callModule(serverFeatureDatabaseSig, "FeatureDatabaseSig")
callModule(serverStatAnno, "StatAnno")