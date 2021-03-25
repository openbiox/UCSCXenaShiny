# 清理目前所有包内置的数据集
# 所有的数据集都要标注数据源和相关引用信息
# 大/不常使用的数据文件统一上传到 https://zenodo.org/
# 全部的文件都以 .rds 格式保存
# sed -i "" "s/stemness_data_RNA/tcga_stemness/g" `grep "stemness_data_RNA" -rl R/*`

library(UCSCXenaTools)
library(dplyr)

pan_xe <- XenaData %>% 
  filter(XenaHostNames == "pancanAtlasHub") %>% 
  XenaGenerate() 

datasets(pan_xe)
sel_data <- datasets(pan_xe)[c(1, 3, 5)]
r <- purrr::map(
  sel_data,
  ~UCSCXenaTools::fetch_dataset_identifiers(hosts(pan_xe), .)
)

names(r) <- c("miRNA", "gene", "protein")
r <- lapply(r, function(x) {
  setdiff(x, "sampleID")
})

saveRDS(r, file = "inst/extdata/pancan_identifier_list.rds")

# cohort: TCGA TARGET GTEx
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
attr(toil_surv, "data_source") <- "DOI:https://doi.org/10.1016/j.cell.2018.02.052"
tcga_surv <- toil_surv

#-------------immune sig data--------------------------
# access date: 2020-06-14
# from https://gdc.cancer.gov/about-data/publications/panimmune
require(data.table)
immune_sig <- data.table::fread("data-raw/Scores_160_Signatures.tsv", data.table = F)
# test = immune_sig[1:5,1:5]
immune_sig <- immune_sig %>% tibble::column_to_rownames("V1")
tcga_pan_immune_signature <- immune_sig
attr(tcga_pan_immune_signature, "data_source") <- "DOI:https://doi.org/10.1016/j.immuni.2018.03.023"

#------------TMB (tumor mutation burden)-------------------
# access date: 2020-06-14
# from https://gdc.cancer.gov/about-data/publications/panimmune
require(data.table)
tmb_data <- data.table::fread("data-raw/mutation-load_updated.txt", data.table = F)
names(tmb_data)[c(4, 5)] <- c("Silent_per_Mb", "Non_silent_per_Mb")
tcga_tmb <- tmb_data
attr(tcga_tmb, "data_source") <- "DOI:https://doi.org/10.1016/j.immuni.2018.03.023"

#-----------Stemness----------------------------------------
# access date:2020-06-17
# from https://pancanatlas.xenahubs.net/download/StemnessScores_DNAmeth_20170210.tsv.gz
stemness_data <- data.table::fread("data-raw/StemnessScores_RNAexp_20170127.2.tsv", data.table = F)
stemness_data <- stemness_data %>%
  tibble::column_to_rownames(var = "sample") %>%
  t() %>%
  as.data.frame()
stemness_data_RNA <- stemness_data %>%
  tibble::rownames_to_column(var = "sample") %>%
  dplyr::mutate(sample = stringr::str_replace_all(sample, "\\.", "-"))

download.file("https://tcga-pancan-atlas-hub.s3.us-east-1.amazonaws.com/latest/StemnessScores_DNAmeth_20170210.tsv.gz",
  destfile = "data-raw/StemnessScores_MethyBased.tsv.gz"
)
stemness_data_methy <- readr::read_tsv("data-raw/StemnessScores_MethyBased.tsv.gz")

# 有一些重复的样本名
stemness_data_methy <- stemness_data_methy %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("TCGA")) %>%
  tidyr::pivot_wider(names_from = "sample", values_from = "value", values_fill = NA)
colnames(stemness_data_methy)[1] <- "sample"

tcga_stemness <- dplyr::full_join(stemness_data_RNA, stemness_data_methy, by = "sample")
head(tcga_stemness)
attr(tcga_stemness, "data_source") <- "https://pancanatlas.xenahubs.net"

#--------purity and ploidy data-----------------------------
# access date:2020-06-17
# from https://gdc.cancer.gov/about-data/publications/PanCanStemness-2018
## genome instability
gi_data <- data.table::fread("data-raw/Purity_Ploidy_All_Samples_9_28_16.tsv", data.table = F)
gi_data <- gi_data %>%
  dplyr::select(c(3, 5, 6, 7, 9, 10))
gi_data <- gi_data %>%
  dplyr::select(sample, purity, ploidy, Genome_doublings = `Genome doublings`, Cancer_DNA_fraction = `Cancer DNA fraction`, Subclonal_genome_fraction = `Subclonal genome fraction`) %>%
  dplyr::mutate(sample = stringr::str_sub(sample, 1, 15))

tcga_genome_instability <- gi_data
attr(tcga_genome_instability, "data_source") <- "DOI:https://doi.org/10.1016/j.cell.2018.03.034"

#-------purity data----------------------------------------
# access date:2020-06-18
# from https://www.nature.com/articles/ncomms9971#Sec14
library(readxl)
purity_data <- read_excel("data-raw/41467_2015_BFncomms9971_MOESM1236_ESM.xlsx", skip = 3)
purity_data <- purity_data %>%
  dplyr::select(c(1:7)) %>%
  dplyr::rename(sample = "Sample ID", cancer_type = "Cancer type") %>%
  dplyr::mutate(sample = stringr::str_sub(sample, 1, 15))

tcga_purity <- purity_data
tcga_purity$ESTIMATE <- as.numeric(tcga_purity$ESTIMATE)
tcga_purity$ABSOLUTE <- as.numeric(tcga_purity$ABSOLUTE)
tcga_purity$LUMP <- as.numeric(tcga_purity$LUMP)
tcga_purity$CPE <- as.numeric(tcga_purity$CPE)
attr(tcga_purity, "data_source") <- "DOI:https://doi.org/10.1038/ncomms9971"

#-------anatomy visualization--------------------------------
# refer to FigureYa78gganatogram
TCGA.organ <- data.table::fread("data-raw/TCGA_organ.txt", data.table = F)
TCGA.organ <- TCGA.organ[, -3]

#-------ccle phenotype--------------------------------------
download.file("https://data.broadinstitute.org/ccle_legacy_data/cell_line_annotations/CCLE_sample_info_file_2012-10-18.txt", destfile = "./data-raw/ccle_pheno.txt", method = "curl")
ccle_info <- data.table::fread("./data-raw/ccle_pheno.txt", data.table = F)
table(ccle_info$Histology)
table(ccle_info$`Site Primary`)
ccle_info %>% mutate(Type = ifelse(`Hist Subtype1` == "NS", Histology, `Hist Subtype1`)) -> ccle_info
names(ccle_info) <- c("CCLE_name", "Cell_line_primary_name", "Cell_line_aliases", "Gender", "Site_Primary", "Histology", "Hist_Subtype1", "Notes", "Source", "Expression_arrays", "SNP_arrays", "Oncomap", "Hybrid_Capture_Sequencing", "Type")
attr(ccle_info, "data_source") <- "https://data.broadinstitute.org/ccle_legacy_data/cell_line_annotations/"

#---- code to prepare `TCGAsubtype` dataset goes here
download.file(
  "https://pancanatlas.xenahubs.net/download/TCGASubtype.20170308.tsv.gz",
  "data-raw/TCGAsubtype.tsv.gz"
)

download.file(
  "https://pancanatlas.xenahubs.net/download/Subtype_Immune_Model_Based.txt.gz",
  "data-raw/immuSubtype.tsv.gz"
)

d1 <- data.table::fread("data-raw/TCGAsubtype.tsv.gz")
d2 <- data.table::fread("data-raw/immuSubtype.tsv.gz")

TCGAsubtype <- merge(d1, d2, by.x = "sampleID", by.y = "sample", all = TRUE)
TCGAsubtype <- as.data.frame(TCGAsubtype)
tcga_subtypes <- TCGAsubtype
attr(tcga_subtypes, "data_source") <- "https://pancanatlas.xenahubs.net/"

# TCGA clinical data (survival is excluded, use toil_surv instead.) -----
download.file(
  "https://tcga-pancan-atlas-hub.s3.us-east-1.amazonaws.com/latest/Survival_SupplementalTable_S1_20171025_xena_sp.gz",
  "data-raw/tcga_clinical.gz"
)
tcga_clinical <- readr::read_tsv("data-raw/tcga_clinical.gz", guess_max = 3000)
tcga_clinical <- tcga_clinical[, c(-c(13, 16, 17, 23, 26:34))]
colnames(tcga_clinical)[c(2:3)] <- c("patient", "type")
attr(tcga_clinical, "data_source") <- "DOI:https://doi.org/10.1016/j.cell.2018.02.052"

# TCGA TIL --------
tcga_TIL <- readr::read_csv("data-raw/infiltration_estimation_for_tcga.csv")
attr(tcga_TIL, "data_source") <- "http://timer.cistrome.org/"

# TCGA 染色体变异 ---------
tcga_chr_alteration <- readxl::read_excel("data-raw/TCGA 染色体变异数据.xlsx", skip = 1)
attr(tcga_chr_alteration, "data_source") <- "DOI:https://doi.org/10.1016/j.ccell.2018.03.007"

# pancan MSI -----------
pancan_MSI <- readxl::read_excel("data-raw/ds_PO.17.00073-1.xlsx")
pancan_MSI <- pancan_MSI[, 1:3]
colnames(pancan_MSI) <- c("case_id", "cancer_type", "MANTIS_Score")
attr(pancan_MSI, "data_source") <- "DOI:https://doi.org/10.1200/PO.17.00073"

# tcga MSI ------
tcga_MSI <- readr::read_csv("data-raw/ncomms15180-s2.csv")
attr(tcga_MSI, "data_source") <- "DOI:https://doi.org/10.1038/ncomms15180"

# ccle ABSOLUTE result -----
ccle_absolute <- readxl::read_excel("data-raw/CCLE-ABSOLUTE结果-10.1038:s41586-020-03133-3.xlsx")
attr(ccle_absolute, "data_source") <- "DOI:https://doi.org/10.1038/s41586-020-03133-3"

# 清理问题编码
toil_info$`_primary_site` <- stringi::stri_enc_toascii(toil_info$`_primary_site`)

usethis::use_data(ccle_absolute, overwrite = TRUE)
usethis::use_data(tcga_MSI, overwrite = TRUE)
usethis::use_data(pancan_MSI, overwrite = TRUE)
usethis::use_data(tcga_chr_alteration, overwrite = TRUE)
usethis::use_data(tcga_TIL, overwrite = TRUE)
usethis::use_data(tcga_clinical, overwrite = TRUE)
usethis::use_data(tcga_subtypes, overwrite = TRUE)
usethis::use_data(tcga_subtypes, overwrite = TRUE)
usethis::use_data(toil_info, overwrite = TRUE)
usethis::use_data(tcga_surv, overwrite = TRUE)
usethis::use_data(tcga_pan_immune_signature, overwrite = TRUE)
usethis::use_data(tcga_tmb, overwrite = TRUE)
usethis::use_data(tcga_stemness, overwrite = TRUE)
usethis::use_data(tcga_genome_instability, overwrite = TRUE)
usethis::use_data(tcga_purity, overwrite = TRUE)
usethis::use_data(TCGA.organ, overwrite = TRUE)
usethis::use_data(ccle_info, overwrite = TRUE)

# 如果是要存储在远端的数据，将其从 data/ 移入 data-remote/ 中
dir.create("data-remote")
system("cd data && mv pancan_MSI* tcga_MSI* tcga_chr_* tcga_stemness* tcga_tmb* tcga_pan_immune* tcga_TIL* ../data-remote")
