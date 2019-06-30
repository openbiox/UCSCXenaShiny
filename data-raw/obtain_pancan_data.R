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

usethis::use_data(toil_info, overwrite = TRUE)
usethis::use_data(toil_surv, overwrite = TRUE)
