library(tidyverse)
library(readxl)

# only keep the cell lines with both expression data and IC50 values
CCLE_sampleinfo <- data.table::fread("https://data.broadinstitute.org/ccle_legacy_data/cell_line_annotations/CCLE_sample_info_file_2012-10-18.txt")
CCLE_drug <- data.table::fread("https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_NP24.2009_Drug_data_2015.02.24.csv") %>%
  left_join(CCLE_sampleinfo, by = c("CCLE Cell Line Name" = "CCLE name"))

# Extract slope variable from ccle datasets
# https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_GNF_data_090613.xls

CCLE_GNF_IC50 <- readxl::read_excel("./data-raw/CCLE_GNF_data_090613.xls", sheet = 1) %>%
  select(c("GNF_REG_ID", "compound name"))
CCLE_GNF_NP26 <- readxl::read_excel("./data-raw/CCLE_GNF_data_090613.xls", sheet = 2, skip = 1) %>%
  select(c("GNF_REG_ID", "Cell line name", "SLOPE")) %>%
  group_by(GNF_REG_ID, `Cell line name`) %>%
  summarise(Slope = mean(SLOPE, na.rm = T)) %>% # 提取平均的 slope
  na.omit()

CCLE_GNF <- left_join(CCLE_GNF_NP26, CCLE_GNF_IC50)
CCLE_drug_c <- left_join(CCLE_drug, CCLE_GNF[, 2:4],
                         by = c("Primary Cell Line Name" = "Cell line name", 
                                "Compound" = "compound name")) %>%
  rename(EC50 = `EC50 (uM)`, IC50 = `IC50 (uM)`)

ccle_drug_response_extend = unique(
  CCLE_drug_c[, c("CCLE Cell Line Name", "Site Primary", 
                  "Compound", "Target",
                  "Doses (uM)", "Activity Data (median)", # 有必要可以使用这2个数据重构Slope
                  "Slope", 
                  "IC50", "EC50", "Amax", "ActArea")]) %>%
  as.data.frame()

save(ccle_drug_response_extend, file = "data-raw/ccle_drug_response_extend.rda")
