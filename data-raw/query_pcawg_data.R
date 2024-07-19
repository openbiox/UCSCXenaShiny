# Query PCAWG phenotype data----------------------
# Access data: 2021-04-30
# Update: 2021-05-06

## Variable code:
# pheno_pcawg_specimen$dcc_project_code #project type info
# pheno_pcawg_specimen$OS # survival status 1 deceased 0 alive
# pheno_pcawg_specimen$OS.time # follow up time (months)
# pheno_pcawg_specimen$donor #donor info

# projects <- c(
#   "BLCA-US", "BRCA-US", "OV-AU", "PAEN-AU", "PRAD-CA", "PRAD-US", "RECA-EU", "SKCM-US", "STAD-US",
#   "THCA-US", "KIRP-US", "LIHC-US", "PRAD-UK", "LIRI-JP", "PBCA-DE", "CESC-US", "PACA-AU", "PACA-CA",
#   "LAML-KR", "COAD-US", "ESAD-UK", "LINC-JP", "LICA-FR", "CLLE-ES", "HNSC-US", "EOPC-DE", "BRCA-UK",
#   "BOCA-UK", "MALY-DE", "CMDI-UK", "BRCA-EU", "ORCA-IN", "BTCA-SG", "SARC-US", "KICH-US", "MELA-AU",
#   "DLBC-US", "GACA-CN", "PAEN-IT", "GBM-US", "KIRC-US", "LAML-US", "LGG-US", "LUAD-US", "LUSC-US",
#   "OV-US", "READ-US", "UCEC-US"
# )

# Load R package
library("UCSCXenaTools")

# Generate dataset(s) information
dataset_query <- structure(
  list(
    hosts = c(
      "https://pcawg.xenahubs.net",
      "https://pcawg.xenahubs.net",
      "https://pcawg.xenahubs.net",
      "https://pcawg.xenahubs.net",
      "https://pcawg.xenahubs.net"
    ),
    datasets = c(
      "sp_specimen_type",
      "project_code_sp",
      "survival_sp",
      "pcawg_specimen_histology_August2016_v9",
      "pcawg_donor_clinical_August2016_v9_sp"
    ),
    url = c(
      `https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/sp_specimen_type",
      `https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/project_code_sp",
      `https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/survival_sp",
      `https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/pcawg_specimen_histology_August2016_v9",
      `https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/pcawg_donor_clinical_August2016_v9_sp"
    ),
    browse = c(
      "https://xenabrowser.net/datapages/?dataset=sp_specimen_type&host=https://pcawg.xenahubs.net",
      "https://xenabrowser.net/datapages/?dataset=project_code_sp&host=https://pcawg.xenahubs.net",
      "https://xenabrowser.net/datapages/?dataset=survival_sp&host=https://pcawg.xenahubs.net",
      "https://xenabrowser.net/datapages/?dataset=pcawg_specimen_histology_August2016_v9&host=https://pcawg.xenahubs.net",
      "https://xenabrowser.net/datapages/?dataset=pcawg_donor_clinical_August2016_v9_sp&host=https://pcawg.xenahubs.net"
    )
  ),
  row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame")
)

# Download dataset(s)
dl <- XenaDownload(dataset_query,
  destdir = "data-raw/", # At default, download to working directory
  download_probeMap = TRUE,
  trans_slash = TRUE
)

# Load dataset(s) into R
datasets <- XenaPrepare(dl)
# Check data
datasets

names(datasets)

sp_specimen_type <- datasets[["sp_specimen_type"]]
project_code_sp <- datasets[["project_code_sp"]]
survival_sp <- datasets[["survival_sp"]]
pcawg_specimen_histology_August2016_v9 <- datasets[["pcawg_specimen_histology_August2016_v9"]]
pcawg_donor_clinical_August2016_v9_sp <- datasets[["pcawg_donor_clinical_August2016_v9_sp"]]

pheno_pcawg_specimen <- sp_specimen_type %>%
  left_join(project_code_sp, by = "icgc_specimen_id") %>%
  left_join(survival_sp, by = c("icgc_specimen_id" = "xena_sample")) %>%
  left_join(pcawg_specimen_histology_August2016_v9, by = "icgc_specimen_id") %>%
  left_join(pcawg_donor_clinical_August2016_v9_sp, by = c("icgc_specimen_id" = "xena_sample"))

pheno_pcawg_specimen$OS <- ifelse(pheno_pcawg_specimen$"_EVENT" == "deceased", 1, 0) #
pheno_pcawg_specimen$OS.time <- pheno_pcawg_specimen$"_TIME_TO_EVENT"
pheno_pcawg_specimen$donor <- pheno_pcawg_specimen$"_PATIENT"
pheno_pcawg_specimen$type2 <- ifelse(stringr::str_detect(pheno_pcawg_specimen$dcc_specimen_type, pattern = "Normal"), "normal", "tumor")

pcawg_info <- pheno_pcawg_specimen %>%
  dplyr::select(-c(
    "donor_vital_status.x", "donor_survival_time.x",
    "donor_interval_of_last_followup.x",
    "_EVENT", "_TIME_TO_EVENT", "_PATIENT",
    "donor_vital_status.y", "donor_survival_time.y",
    "donor_interval_of_last_followup.y", "level_of_cellularity"
  )) %>%
  dplyr::select(icgc_specimen_id, donor, dplyr::everything())

attr(pcawg_info, "data_source") <- "https://xenabrowser.net/datapages/?cohort=PCAWG%20(specimen%20centric)&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443"

usethis::use_data(pcawg_info, overwrite = TRUE)

#-Query PCAWG purity and ploidy data----------
# Access data: 2021-05-07
# Load R package
library("UCSCXenaTools")

# Generate dataset(s) information
dataset_query <- structure(list(hosts = "https://pcawg.xenahubs.net", datasets = "consensus.20170217.purity.ploidy_sp", url = c(`https://pcawg.xenahubs.net` = "https://pcawg.xenahubs.net/download/consensus.20170217.purity.ploidy_sp"), browse = "https://xenabrowser.net/datapages/?dataset=consensus.20170217.purity.ploidy_sp&host=https://pcawg.xenahubs.net"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))

# Download dataset(s)
dl <- XenaDownload(dataset_query,
  destdir = "./", # At default, download to working directory
  download_probeMap = TRUE,
  trans_slash = TRUE
)

# Load dataset(s) into R
datasets <- XenaPrepare(dl)
# Check data
datasets

pcawg_purity <- datasets %>% rename(icgc_specimen_id = samplename)
attr(pcawg_purity, "data_source") <- "https://xenabrowser.net/datapages/?cohort=PCAWG%20(specimen%20centric)&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443"

usethis::use_data(pcawg_purity, overwrite = TRUE)
