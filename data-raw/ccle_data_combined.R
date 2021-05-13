library(tidyverse)

CCLE_expr <- data.table::fread("./data-raw/ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502.gct")

CCLE_expr <- CCLE_expr %>%
  distinct(Description, .keep_all = T) %>%
  column_to_rownames(var = "Description") %>%
  select(-c("Name"))

# import selected resistant genes:
geneids <- read.table(
  file = "./data-raw/ccle/cisplatin_resistant_list.txt",
  sep = "\t", header = T,
  colClasses = "character"
)
geneids <- geneids$Genes
geneids <- geneids[is.element(geneids, rownames(CCLE_expr))]
head(geneids)

# quantile normalize the microarray data among all different cell lines
# ref: http://www.bio-info-trainee.com/2043.html
CCLE_mat <- preprocessCore::normalize.quantiles(as.matrix(CCLE_expr), copy = TRUE)
colnames(CCLE_mat) <- colnames(CCLE_expr)
rownames(CCLE_mat) <- rownames(CCLE_expr)

# only keep the cell lines with both expression data and IC50 values
CCLE_sampleinfo <- data.table::fread("./data-raw/ccle/CCLE_sample_info_file_2012-10-18.txt")
CCLE_drug <- data.table::fread("./data-raw/ccle/CCLE_NP24.2009_Drug_data_2015.02.24.csv") %>%
  left_join(CCLE_sampleinfo, by = c("CCLE Cell Line Name" = "CCLE name"))

iOrd <- intersect(colnames(CCLE_mat), CCLE_drug$`CCLE Cell Line Name`)
CCLE_mat <- CCLE_mat[, iOrd]
# cells ids in gdsc-IC50 table and gdsc_mat expresssion matrix are now in the same order.
CCLE_drug <- filter(CCLE_drug, `CCLE Cell Line Name` %in% iOrd)
# expression of selected genes
CCLE_mat.sel <- CCLE_mat[geneids, ]

# Extract slope variable from ccle datasets
CCLE_GNF_IC50 <- data.table::fread("./data-raw/ccle/CCLE_GNF_data_090613_IC50 Table.txt", header = TRUE) %>%
  select(c("GNF_REG_ID", "compound name"))
CCLE_GNF_NP26 <- data.table::fread("./data-raw/ccle/CCLE_GNF_data_090613_NP-26 only.txt", header = TRUE) %>%
  select(c("GNF_REG_ID", "Cell line name", "SLOPE")) %>%
  group_by(GNF_REG_ID, `Cell line name`) %>%
  summarise(Slope = mean(SLOPE, na.rm = T)) %>% na.omit()
CCLE_GNF <- left_join(CCLE_GNF_NP26, CCLE_GNF_IC50)
CCLE_drug_c <- left_join(CCLE_drug, CCLE_GNF[,2:4], by = c("Primary Cell Line Name" = "Cell line name", "Compound" = "compound name")) %>%
  rename(EC50 = `EC50 (uM)`, IC50 = `IC50 (uM)`)


# 3. Using hgsc data to compute the partial correlation of drug-gene pairs
CCLE_drug_mat <- pivot_wider(CCLE_drug[, c("CCLE Cell Line Name", "Compound", "IC50 (uM)")],
                             names_from = "Compound", values_from = "IC50 (uM)", values_fill = NA
)

########################### 把核心数据保留
ccle_expr_and_drug_response <- list(
  expr = CCLE_mat,
  drug_ic50 = CCLE_drug_mat %>%
    tibble::column_to_rownames("CCLE Cell Line Name") %>%
    as.matrix(),
  drug_info = unique(CCLE_drug_c[, c("CCLE Cell Line Name", "Site Primary", "Compound", "Target", "Slope", "IC50", "EC50", "Amax", "ActArea")]) %>%
    as.data.frame()
)

str(ccle_expr_and_drug_response, max.level = 1)
save(ccle_expr_and_drug_response, file = "data-raw/ccle/ccle_expr_and_drug_response_advanced.rda")

#=====================================================================================#

ccle_data <- ccle_expr_and_drug_response







