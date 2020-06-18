# cohort: TCGA TARGET GTEx
library(UCSCXenaTools)
toil_info <- XenaGenerate(subset = XenaDatasets == "TcgaTargetGTEX_phenotype.txt") %>%
  XenaQuery() %>%
  XenaDownload() %>%
  XenaPrepare() %>%
  as.data.frame()

toil_surv <- XenaGenerate(subset = XenaDatasets == "TCGA_survival_data") %>%
  XenaQuery() %>%
  XenaDownload() %>%
  XenaPrepare() %>%
  as.data.frame()

#-------------immune sig data--------------------------
#access date: 2020-06-14
#from https://gdc.cancer.gov/about-data/publications/panimmune
# require(data.table)
# immune_sig <- data.table::fread("data-raw/Scores_160_Signatures.tsv", data.table = F)
# #test = immune_sig[1:5,1:5]
# immune_sig = immune_sig %>% tibble::column_to_rownames("V1")

#------------TMB (tumor mutation burden)-------------------
#access date: 2020-06-14
#from https://gdc.cancer.gov/about-data/publications/panimmune
# require(data.table)
# tmb_data <- data.table::fread("data-raw/mutation-load_updated.txt", data.table = F)
# names(tmb_data)[c(4,5)] <- c("Silent_per_Mb","Non_silent_per_Mb")

usethis::use_data(toil_info, overwrite = TRUE)
usethis::use_data(toil_surv, overwrite = TRUE)
usethis::use_data(immune_sig, overwrite = TRUE)
usethis::use_data(tmb_data, overwrite = TRUE)
