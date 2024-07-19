meta_raw <- load_data("tcga_clinical") %>%
  dplyr::distinct()

meta_data <- meta_raw[, c(1, 3:5, 7:8, 10)]
colnames(meta_data) <- c(
  "Sample", "Cancer", "Age", "Gender",
  "Stage_ajcc", "Stage_clinical",
  "Grade"
)

if (TRUE) {
  # AJCC stage
  meta_data$Stage_ajcc[!grepl("Stage", meta_data$Stage_ajcc)] <- NA
  meta_data$Stage_ajcc[meta_data$Stage_ajcc %in% c("Stage 0", "Stage X")] <- NA
  meta_data$Stage_ajcc <- gsub("[ABC]", "", meta_data$Stage_ajcc)
  # Clinical stage
  meta_data$Stage_clinical[!grepl("Stage", meta_data$Stage_clinical)] <- NA
  meta_data$Stage_clinical <- gsub("[ABC12]", "", meta_data$Stage_clinical)
  meta_data$Stage_clinical[meta_data$Stage_clinical == "Stage IS"] <- "Stage I"
  # histological grade
  meta_data$Grade[!meta_data$Grade %in% paste0("G", 1:4)] <- NA

  tcga_clinical_fine <- meta_data %>%
    dplyr::mutate(Code = substr(.data$Sample, 14, 15), .before = 4) %>%
    dplyr::mutate(Code = case_when(
      Code == "01" ~ "TP", # Primary Solid Tumor
      Code == "02" ~ "TR", # Recurrent Solid Tumor
      Code == "03" ~ "TB", # Primary Blood Derived Cancer - Peripheral Blood
      Code == "05" ~ "TAP", # Additional - New Primary
      Code == "06" ~ "TM", # Metastatic
      Code == "07" ~ "TAM", # Additional Metastatic
      Code == "11" ~ "NT" # Solid Tissue Normal
    ), .before = 4)
  head(meta_data)
}

head(tcga_clinical_fine)




pcawg_info_fine <- pcawg_info[, c(
  "icgc_specimen_id", "dcc_project_code",
  "donor_age_at_diagnosis", "donor_sex", "type2"
)]
colnames(pcawg_info_fine) <- c("Sample", "Project", "Age", "Gender", "Type")
dim(pcawg_info_fine)
# [1] 6231    5

## 去重
pcawg_meta <- pcawg_info_fine %>% dplyr::distinct()
# [1] 6139    5

pcawg_gene <- get_pcawg_gene_value("TP53")
pcawg_fusion <- get_pcawg_fusion_value("PTEN")
pcawg_mi <- get_pcawg_miRNA_value("hsa-let-7a-2-3p")
pcawg_pro <- get_pcawg_promoter_value("prmtr.1")
pcawg_apobec <- get_pcawg_APOBEC_mutagenesis_value("A3A_or_A3B")

pcawg_meta_stat <- pcawg_meta %>%
  dplyr::mutate(mRNA = ifelse(Sample %in% names(pcawg_gene$data), 1, 0)) %>%
  dplyr::mutate(fusion = ifelse(Sample %in% names(pcawg_fusion$data), 1, 0)) %>%
  dplyr::mutate(miRNA = ifelse(Sample %in% names(pcawg_mi$data), 1, 0)) %>%
  dplyr::mutate(promoter = ifelse(Sample %in% names(pcawg_pro$data), 1, 0)) %>%
  dplyr::mutate(apobec = ifelse(Sample %in% names(pcawg_pro$data), 1, 0))

# pcawg_meta_stat = pcawg_meta_stat %>%
#   dplyr::group_by(Project) %>%
#   dplyr::summarise(all = n(),
#                    mRNA = sum(mRNA),
#                    fusion = sum(fusion),
#                    miRNA = sum(miRNA),
#                    promoter = sum(promoter),
#                    apobec = sum(apobec))
pcawg_meta_stat <- pcawg_meta_stat[rowSums(pcawg_meta_stat[, 6:10]) != 0, ] # 1615

pcawg_info_fine <- pcawg_meta %>%
  dplyr::filter(Sample %in% pcawg_meta_stat$Sample)
length(unique(pcawg_info_fine$Project))
# [1] 30
dim(pcawg_info_fine)
# 1523

# pcawg_meta_stat %>%
#   reshape2::melt("Project") %>%
#   ggplot(aes(x = Project, y=value, fill=variable)) +
#   geom_col(position = "dodge") +
#   coord_flip()

ccle_info_fine <- ccle_info %>%
  dplyr::select(CCLE_name, Site_Primary, Gender, Histology, Type)
colnames(ccle_info_fine)[1] <- "Sample"
ccle_info_fine <- tibble(ccle_info_fine)





usethis::use_data(ccle_info_fine, overwrite = TRUE)

# document()
