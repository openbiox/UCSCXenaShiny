data("XenaData", package = "UCSCXenaTools", envir = environment())
xena_table <- XenaData[, c(
  "XenaDatasets", "XenaHostNames", "XenaCohorts",
  "SampleCount", "DataSubtype", "Label", "Unit"
)]
xena_table$SampleCount <- as.integer(xena_table$SampleCount)
colnames(xena_table)[c(1:3)] <- c("Dataset ID", "Hub", "Cohort")

# Used in TCGA survival module
TCGA_datasets <- xena_table %>%
  dplyr::filter(Hub == "tcgaHub") %>%
  dplyr::select("Cohort") %>%
  unique() %>%
  dplyr::mutate(
    id = stringr::str_match(Cohort, "\\((\\w+?)\\)")[, 2],
    des = stringr::str_match(Cohort, "(.*)\\s+\\(")[, 2]
  ) %>%
  dplyr::arrange(id)

# Used in genecor and pancan-search-cancer module script
tcga_cancer_choices <- c(
  "SKCM", "THCA", "SARC", "PRAD", "PCPG", "PAAD", "HNSC", "ESCA",
  "COAD", "CESC", "BRCA", "TGCT", "KIRP", "KIRC", "LAML", "READ",
  "OV", "LUAD", "LIHC", "UCEC", "GBM", "LGG", "UCS", "THYM", "STAD",
  "DLBC", "LUSC", "MESO", "KICH", "UVM", "BLCA", "CHOL", "ACC"
)

TCGA_cli_merged <- dplyr::full_join(
  load_data("tcga_clinical"),
  load_data("tcga_surv"),
  by = "sample"
)

pancan_identifiers <- readRDS(
  system.file(
    "extdata", "pancan_identifier_list.rds",
    package = "UCSCXenaShiny"
  )
)
all_preload_identifiers <- c("NONE", unique(as.character(unlist(pancan_identifiers))))
tryCatch(
  load_data("transcript_identifier"),
  error = function(e) {
    stop("Load data failed, please run load_data('transcript_identifier') by hand before restarting the Shiny.")
  }
)

phenotype_datasets <- UCSCXenaTools::XenaData %>%
  dplyr::filter(Type == "clinicalMatrix") %>%
  dplyr::pull(XenaDatasets)


themes_list <- list(
  "cowplot" = cowplot::theme_cowplot(),
  "Light" = theme_light(),
  "Minimal" = theme_minimal(),
  "Classic" = theme_classic(),
  "Gray" = theme_gray(),
  "half_open" = cowplot::theme_half_open(),
  "minimal_grid" = cowplot::theme_minimal_grid()
)


## 通路基因
PW_meta <- load_data("tcga_PW_meta")
PW_meta <- PW_meta %>% 
  dplyr::arrange(Name) %>%
  dplyr::mutate(size = purrr::map_int(Gene, function(x){
    x_ids = strsplit(x, "/", fixed = TRUE)[[1]]
    length(x_ids)
  }), .before = 5) %>% 
  dplyr::mutate(display = paste0(Name, " (", size, ")"), .before = 6)

msigdbr_types <- data.frame(
  gs_cat = c("H","C1", "C2", "C2", "C2", "C2", "C2", "C2", "C2", 
             "C3", "C3", "C3", "C3", "C4", "C4", "C5", "C5", 
             "C5", "C5", "C6", "C7", "C7", "C8"),
  gs_subcat = c("","", "CGP", "CP", "CP:BIOCARTA", "CP:KEGG", 
                "CP:PID", "CP:REACTOME", "CP:WIKIPATHWAYS", 
                "MIR:MIRDB", "MIR:MIR_Legacy", "TFT:GTRD", 
                "TFT:TFT_Legacy", "CGN", "CM", "GO:BP", 
                "GO:CC", "GO:MF", "HPO", "", "IMMUNESIGDB", 
                "VAX", "")
)
msigdbr_types = msigdbr_types %>% 
  dplyr::mutate(gs_subcat2 = ifelse(gs_subcat=="",gs_cat,gs_subcat)) %>% 
  dplyr::mutate(gs_subcat_label = paste0(gs_cat, "--",gs_subcat2))

#   gs_cat   gs_subcat  gs_subcat2 gs_subcat_label
# 1      H                       H            H--H
# 2     C1                      C1          C1--C1
# 3     C2         CGP         CGP         C2--CGP
# 4     C2          CP          CP          C2--CP








## TCGA/PCAWG/CCLE value & id for general analysis
general_value_id = UCSCXenaShiny:::query_general_id()
# id
tcga_id_option = general_value_id[["id"]][[1]]
pcawg_id_option = general_value_id[["id"]][[2]]
ccle_id_option = general_value_id[["id"]][[3]]
# value
tcga_value_option = general_value_id[["value"]][[1]]
tcga_index_value = tcga_value_option[["Tumor index"]]
tcga_immune_value = tcga_value_option[["Immune Infiltration"]]
tcga_pathway_value = tcga_value_option[["Pathway activity"]]
tcga_phenotype_value = tcga_value_option[["Phenotype data"]]

pcawg_value_option = general_value_id[["value"]][[2]]
pcawg_index_value = pcawg_value_option[["Tumor index"]]
pcawg_immune_value = pcawg_value_option[["Immune Infiltration"]]
pcawg_pathway_value = pcawg_value_option[["Pathway activity"]]
pcawg_phenotype_value = pcawg_value_option[["Phenotype data"]]

ccle_value_option = general_value_id[["value"]][[3]]
ccle_index_value = ccle_value_option[["Tumor index"]]
ccle_phenotype_value = ccle_value_option[["Phenotype data"]]


TIL_signatures = lapply(tcga_id_option$`Immune Infiltration`, function(x) {
  x$all
}) %>% reshape2::melt() %>% 
  dplyr::mutate(x = paste0(value,"_",L1)) %>%
  dplyr::pull(x)



# Help → ID reference
tcga_id_referrence = load_data("pancan_identifier_help")
pcawg_id_referrence = load_data("pcawg_identifier")
ccle_id_referrence = load_data("ccle_identifier")



code_types = list("NT"= "NT (normal tissue)",
                  "TP"= "TP (primary tumor)",
                  "TR"= "TR (recurrent tumor)",
                  "TB"= "TB (blood derived tumor)",
                  "TAP"="TAP (additional primary)",
                  "TM"= "TM (metastatic tumor)",
                  "TAM"="TAM (additional metastatic)")

# CCLE tissues for drug analysis
# "ALL" means all tissues
ccle_drug_related_tissues <- c(
  "ALL", "prostate", "central_nervous_system", "urinary_tract", "haematopoietic_and_lymphoid_tissue",
  "kidney", "thyroid", "soft_tissue", "skin", "salivary_gland",
  "ovary", "lung", "bone", "endometrium", "pancreas", "breast",
  "large_intestine", "upper_aerodigestive_tract", "autonomic_ganglia",
  "stomach", "liver", "biliary_tract", "pleura", "oesophagus"
)

# Data summary
Data_hubs_number <- length(unique(xena_table$Hub))
Cohorts_number <- length(unique(xena_table$Cohort))
Datasets_number <- length(unique(xena_table$`Dataset ID`))
Samples_number <- "~2,000,000"
Primary_sites_number <- "~37"
Data_subtypes_number <- "~45"
Xena_summary <- dplyr::group_by(xena_table, Hub) %>%
  dplyr::summarise(
    n_cohort = length(unique(.data$Cohort)),
    n_dataset = length(unique(.data$`Dataset ID`)), .groups = "drop"
  )

# PCAWG project info
pcawg_items = sort(unique(pcawg_info_fine$Project)) #30
dcc_project_code_choices <- c(
  "BLCA-US", "BRCA-US", "OV-AU", "PAEN-AU", "PRAD-CA", "PRAD-US", "RECA-EU", "SKCM-US", "STAD-US",
  "THCA-US", "KIRP-US", "LIHC-US", "PRAD-UK", "LIRI-JP", "PBCA-DE", "CESC-US", "PACA-AU", "PACA-CA",
  "LAML-KR", "COAD-US", "ESAD-UK", "LINC-JP", "LICA-FR", "CLLE-ES", "HNSC-US", "EOPC-DE", "BRCA-UK",
  "BOCA-UK", "MALY-DE", "CMDI-UK", "BRCA-EU", "ORCA-IN", "BTCA-SG", "SARC-US", "KICH-US", "MELA-AU",
  "DLBC-US", "GACA-CN", "PAEN-IT", "GBM-US", "KIRC-US", "LAML-US", "LGG-US", "LUAD-US", "LUSC-US",
  "OV-US", "READ-US", "UCEC-US"
)

# Global color
mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))

## PharmacoGenomics ----
#source(system.file("shinyapp/PGdata.R", package = "UCSCXenaShiny"), local = PGdata <- new.env(), echo = FALSE)

appdata_path = path.expand(file.path(getOption("xena.cacheDir"), "appdata.RData"))
message("Saving data to ", appdata_path)
save.image(file = appdata_path)

