library(shiny)
library(devtools)
library(tidyverse)
load_all()

# runApp("inst/shinyapp/", launch.browser = T)

pancan_identifiers <- readRDS(
      system.file(
          "extdata", "pancan_identifier_list.rds",
          package = "UCSCXenaShiny"
      )
)
transcript_ids = load_data("transcript_identifier")
gene_pc = data.table::fread("../Gene_basic_annotation_gencode_v23.csv") %>%
    as.data.frame() %>% 
    dplyr::filter(gene_type == "protein_coding") %>% 
    dplyr::pull(Symbol)

## L1: ID levele-1
## L2: ID levele-2
## L3: ID levele-3

# TCGA ID data
## L1: Molecule
tcga_id_1_1 = data.frame(
    L1 = "Molecule",
    L2 = "Gene",
    L3 = pancan_identifiers$gene
)
tcga_id_1_1 = tcga_id_1_1  %>% 
    dplyr::filter(L3 %in% gene_pc)


# tcga_id_referrence$id_molecule$id_gene$Level3
tmp = load_data("pancan_identifier_help")[["id_molecule"]]$id_gene$Level3



tcga_id_1_2 = data.frame(
    L1 = "Molecule",
    L2 = "Transcript",
    L3 = transcript_ids
)

tcga_id_1_3 = data.frame(
    L1 = "Molecule",
    L2 = "Protein",
    L3 = pancan_identifiers$protein
)

tcga_id_1_4 = data.frame(
    L1 = "Molecule",
    L2 = "miRNA",
    L3 = pancan_identifiers$miRNA
)

tcga_id_1 = do.call(rbind,
    list(tcga_id_1_1, tcga_id_1_2, tcga_id_1_3, tcga_id_1_4))
table(tcga_id_1$L2)
# Gene      miRNA    Protein Transcript 
# 17295        743        258     197045



## L2: Index
tcga_purity = load_data("tcga_purity") %>% 
    dplyr::rename("Sample"="sample") %>% 
    dplyr::select(-cancer_type)
tcga_purity = tcga_purity %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(across(everything(),  ~ mean(.x, na.rm = T)))  %>% 
    as.data.frame() %>% 
    dplyr::mutate(across(-Sample, ~ ifelse(is.nan(.x),NA,.x)))

tcga_id_2_1 = data.frame(
    L1 = "Index",
    L2 = "Purity",
    L3 = colnames(tcga_purity)[-1]
)
colnames(tcga_purity)[-1] = paste(
    tcga_id_2_1$L1, tcga_id_2_1$L2, tcga_id_2_1$L3, sep = "+")

tcga_stem = load_data("tcga_stemness") %>% 
    dplyr::rename("Sample"="sample")
tcga_id_2_2 = data.frame(
    L1 = "Index",
    L2 = "Stem",
    L3 = colnames(tcga_stem)[-1]
)
colnames(tcga_stem)[-1] = paste(
    tcga_id_2_2$L1, tcga_id_2_2$L2, tcga_id_2_2$L3, sep = "+")

tcga_tmb = load_data("tcga_tmb") %>% 
    dplyr::rename("Sample"="Tumor_Sample_ID") %>% 
    dplyr::select(-Cohort, -Patient_ID)
tcga_id_2_3 = data.frame(
    L1 = "Index",
    L2 = "TMB",
    L3 = colnames(tcga_tmb)[-1]
)
colnames(tcga_tmb)[-1] = paste(
    tcga_id_2_3$L1, tcga_id_2_3$L2, tcga_id_2_3$L3, sep = "+")

tcga_msi = load_data("tcga_gtex") %>%
    dplyr::rename("Sample"="sample") %>%
    dplyr::mutate(Barcode = stringr::str_sub(.data$Sample, 1, 12)) %>%
    dplyr::select('Barcode', 'Sample') %>%
    dplyr::inner_join(load_data("tcga_MSI"), by = "Barcode") %>% 
    dplyr::select(-Barcode, -Cancer_type,-MSI_category_nb_from_TCGA_consortium)

tcga_id_2_4 = data.frame(
    L1 = "Index",
    L2 = "MSI",
    L3 = colnames(tcga_msi)[-1]
)
colnames(tcga_msi)[-1] = paste(
    tcga_id_2_4$L1, tcga_id_2_4$L2, tcga_id_2_4$L3, sep = "+")

tcga_gi = load_data("tcga_genome_instability") %>% 
    dplyr::rename("Sample"="sample") %>% as.data.frame()
tcga_gi = tcga_gi %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(across(everything(),  ~ mean(.x, na.rm = T))) %>% 
    as.data.frame() %>% 
    dplyr::mutate(across(-Sample, ~ ifelse(is.nan(.x),NA,.x)))

tcga_id_2_5 = data.frame(
    L1 = "Index",
    L2 = "GI",
    L3 = colnames(tcga_gi)[-1]
)


tcga_id_2 = do.call(rbind, 
    list(tcga_id_2_1, tcga_id_2_2, tcga_id_2_3,
         tcga_id_2_4, tcga_id_2_5))

tcga_index = tcga_purity %>% 
    dplyr::full_join(tcga_stem) %>% 
    dplyr::full_join(tcga_tmb) %>% 
    dplyr::full_join(tcga_msi) %>% 
    dplyr::full_join(tcga_gi)


## L3 Immune
tcga_TIL <- load_data("tcga_TIL")
colnames(tcga_TIL)[1] <- "Sample"

tcga_CIB = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("CIBERSORT")) %>% 
    dplyr::rename_with(~ gsub("_CIBERSORT", "", .x))
tcga_id_3_1 = data.frame(
    L1 = "Immune",
    L2 = "CIB",
    L3 = colnames(tcga_CIB)[-1]
)
colnames(tcga_CIB)[-1] = paste(
    tcga_id_3_1$L1, tcga_id_3_1$L2, tcga_id_3_1$L3, sep = "+")


tcga_CIB.ABS = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("CIBERSORT-ABS")) %>% 
    dplyr::rename_with(~ gsub("_CIBERSORT-ABS", "", .x))
tcga_id_3_2 = data.frame(
    L1 = "Immune",
    L2 = "CIB.ABS",
    L3 = colnames(tcga_CIB.ABS)[-1]
)
colnames(tcga_CIB.ABS)[-1] = paste(
    tcga_id_3_2$L1, tcga_id_3_2$L2, tcga_id_3_2$L3, sep = "+")

tcga_EPIC = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("EPIC")) %>% 
    dplyr::rename_with(~ gsub("_EPIC", "", .x))
tcga_id_3_3 = data.frame(
    L1 = "Immune",
    L2 = "EPIC",
    L3 = colnames(tcga_EPIC)[-1]
)
colnames(tcga_EPIC)[-1] = paste(
    tcga_id_3_3$L1, tcga_id_3_3$L2, tcga_id_3_3$L3, sep = "+")

tcga_MCP = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("MCPCOUNTER")) %>% 
    dplyr::rename_with(~ gsub("_MCPCOUNTER", "", .x))
tcga_id_3_4 = data.frame(
    L1 = "Immune",
    L2 = "MCP",
    L3 = colnames(tcga_MCP)[-1]
)
colnames(tcga_MCP)[-1] = paste(
    tcga_id_3_4$L1, tcga_id_3_4$L2, tcga_id_3_4$L3, sep = "+")

tcga_quant = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("QUANTISEQ")) %>% 
    dplyr::rename_with(~ gsub("_QUANTISEQ", "", .x))
tcga_id_3_5 = data.frame(
    L1 = "Immune",
    L2 = "Quant",
    L3 = colnames(tcga_quant)[-1]
)
colnames(tcga_quant)[-1] = paste(
    tcga_id_3_5$L1, tcga_id_3_5$L2, tcga_id_3_5$L3, sep = "+")

tcga_timer = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("TIMER")) %>% 
    dplyr::rename_with(~ gsub("_TIMER", "", .x))
tcga_id_3_6 = data.frame(
    L1 = "Immune",
    L2 = "TIMER",
    L3 = colnames(tcga_timer)[-1]
)
colnames(tcga_timer)[-1] = paste(
    tcga_id_3_6$L1, tcga_id_3_6$L2, tcga_id_3_6$L3, sep = "+")

tcga_xcell = tcga_TIL %>% 
    dplyr::select("Sample", ends_with("XCELL")) %>% 
    dplyr::rename_with(~ gsub("_XCELL", "", .x))
tcga_id_3_7 = data.frame(
    L1 = "Immune",
    L2 = "XCELL",
    L3 = colnames(tcga_xcell)[-1]
)
colnames(tcga_xcell)[-1] = paste(
    tcga_id_3_7$L1, tcga_id_3_7$L2, tcga_id_3_7$L3, sep = "+")


tcga_id_3 = do.call(rbind, 
    list(tcga_id_3_1, tcga_id_3_2, tcga_id_3_3,
         tcga_id_3_4, tcga_id_3_5, tcga_id_3_6, tcga_id_3_7))
table(tcga_id_3$L2)
# CIB CIB.ABS    EPIC     MCP   Quant   TIMER   XCELL 
#  22      22       8      11      11       6      39 
tcga_immune = tcga_CIB %>% 
    dplyr::full_join(tcga_CIB.ABS) %>% 
    dplyr::full_join(tcga_EPIC) %>% 
    dplyr::full_join(tcga_MCP) %>% 
    dplyr::full_join(tcga_quant) %>% 
    dplyr::full_join(tcga_timer) %>% 
    dplyr::full_join(tcga_xcell)
dim(tcga_immune)
# [1] 11070   120


## Pathway
tcga_PW = load_data("tcga_PW") %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Sample")

tcga_hm = tcga_PW %>% 
    dplyr::select("Sample", starts_with("HALLMARK")) %>% 
    dplyr::rename_with(~ gsub("HALLMARK_", "", .x))
tcga_id_4_1 = data.frame(
    L1 = "Pathway",
    L2 = "HM",
    L3 = colnames(tcga_hm)[-1]
)
colnames(tcga_hm)[-1] = paste(
    tcga_id_4_1$L1, tcga_id_4_1$L2, tcga_id_4_1$L3, sep = "+")

tcga_kegg = tcga_PW %>% 
    dplyr::select("Sample", starts_with("KEGG")) %>% 
    dplyr::rename_with(~ gsub("KEGG_", "", .x))
tcga_id_4_2 = data.frame(
    L1 = "Pathway",
    L2 = "KEGG",
    L3 = colnames(tcga_kegg)[-1]
)
colnames(tcga_kegg)[-1] = paste(
    tcga_id_4_2$L1, tcga_id_4_2$L2, tcga_id_4_2$L3, sep = "+")

tcga_iobr = tcga_PW %>% 
    dplyr::select("Sample", starts_with("IOBR")) %>% 
    dplyr::rename_with(~ gsub("IOBR_", "", .x))
tcga_id_4_3 = data.frame(
    L1 = "Pathway",
    L2 = "IOBR",
    L3 = colnames(tcga_iobr)[-1]
)
colnames(tcga_iobr)[-1] = paste(
    tcga_id_4_3$L1, tcga_id_4_3$L2, tcga_id_4_3$L3, sep = "+")


tcga_id_4 = do.call(rbind, 
    list(tcga_id_4_1, tcga_id_4_2, tcga_id_4_3))
table(tcga_id_4$L2)
#   HM IOBR KEGG 
#   50  264  186 

tcga_pathway = tcga_hm %>% 
    dplyr::full_join(tcga_kegg) %>% 
    dplyr::full_join(tcga_iobr)
dim(tcga_pathway)
# [1] 10496   501


# Phenotype
tcga_phe = tcga_clinical_fine
tcga_id_5 = data.frame(
    L1 = "Phenotype",
    L2 = "Clinical",
    L3 = colnames(tcga_clinical_fine)[-1]
)
colnames(tcga_phe)[-1] = paste(
    tcga_id_5$L1, tcga_id_5$L2, tcga_id_5$L3, sep = "+")


tcga_value_nonomics = tcga_phe %>% 
    dplyr::left_join(tcga_index) %>% 
    dplyr::left_join(tcga_immune) %>% 
    dplyr::left_join(tcga_pathway)

tcga_value_nonomics[1:4,1:4]

save(tcga_value_nonomics, file = "../v2_tcga_value_nonomics.rda")

tcga_id_nonomics = do.call(rbind, 
    list(tcga_id_2, tcga_id_3,
         tcga_id_4, tcga_id_5)
)
tcga_id_omics = tcga_id_1
tcga_id = rbind(tcga_id_nonomics, tcga_id_omics) %>% tibble()
save(tcga_id, file = "../v2_tcga_id.rda")


######### PCAWG
pcawg_id_referrence = load_data("pcawg_identifier")

pcawg_id_1_1 = data.frame(
    L1 = "Molecule",
    L2 = "Gene",
    L3 = sort(pcawg_id_referrence$id_gene$Level3)
)
pcawg_id_1_1 = pcawg_id_1_1 %>% 
    dplyr::filter(L3 %in% gene_pc)

pcawg_id_1_2 = data.frame(
    L1 = "Molecule",
    L2 = "Promoter",
    L3 = sort(pcawg_id_referrence$id_pro$Level3)
)

pcawg_id_1_3 = data.frame(
    L1 = "Molecule",
    L2 = "Fusion",
    L3 = sort(pcawg_id_referrence$id_fusion$Level3)
)
pcawg_id_1_3 = pcawg_id_1_3 %>% 
    dplyr::filter(L3 %in% gene_pc)

pcawg_id_1_4 = data.frame(
    L1 = "Molecule",
    L2 = "miRNA",
    L3 = sort(pcawg_id_referrence$id_mi$Level3)
)

pcawg_id_1_5 = data.frame(
    L1 = "Molecule",
    L2 = "Muta",
    L3 = sort(pcawg_id_referrence$id_maf$Level3)
)

pcawg_id_1 = do.call(rbind,
    list(pcawg_id_1_1, pcawg_id_1_2, pcawg_id_1_3, pcawg_id_1_4, pcawg_id_1_5))
table(pcawg_id_1$L2)
#   Fusion     Gene    miRNA     Muta Promoter 
#     3919    19189     5806       16    45087 

## Index
pcawg_purity = load_data("pcawg_purity") %>% 
    .[,c("icgc_specimen_id", "purity", "ploidy", "purity_conf_mad", "wgd_status", "wgd_uncertain")] %>% 
    dplyr::filter(.data$icgc_specimen_id %in% pcawg_info_fine$Sample) %>% 
    dplyr::rename("Sample"="icgc_specimen_id")
pcawg_index = pcawg_purity
pcawg_id_2 = data.frame(
    L1 = "Index",
    L2 = "Purity",
    L3 = colnames(pcawg_index)[-1]
)
colnames(pcawg_index)[-1] = paste(
    pcawg_id_2$L1, pcawg_id_2$L2, pcawg_id_2$L3, sep = "+")


## Immune
pcawg_TIL <- load_data("pcawg_TIL")
colnames(pcawg_TIL)[1] <- "Sample"

pcawg_CIB = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("CIBERSORT")) %>% 
    dplyr::rename_with(~ gsub("_CIBERSORT", "", .x))
pcawg_id_3_1 = data.frame(
    L1 = "Immune",
    L2 = "CIB",
    L3 = colnames(pcawg_CIB)[-1]
)
colnames(pcawg_CIB)[-1] = paste(
    pcawg_id_3_1$L1, pcawg_id_3_1$L2, pcawg_id_3_1$L3, sep = "+")


pcawg_CIB.ABS = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("CIBERSORT-ABS")) %>% 
    dplyr::rename_with(~ gsub("_CIBERSORT-ABS", "", .x))
pcawg_id_3_2 = data.frame(
    L1 = "Immune",
    L2 = "CIB.ABS",
    L3 = colnames(pcawg_CIB.ABS)[-1]
)
colnames(pcawg_CIB.ABS)[-1] = paste(
    pcawg_id_3_2$L1, pcawg_id_3_2$L2, pcawg_id_3_2$L3, sep = "+")

pcawg_EPIC = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("EPIC")) %>% 
    dplyr::rename_with(~ gsub("_EPIC", "", .x))
pcawg_id_3_3 = data.frame(
    L1 = "Immune",
    L2 = "EPIC",
    L3 = colnames(pcawg_EPIC)[-1]
)
colnames(pcawg_EPIC)[-1] = paste(
    pcawg_id_3_3$L1, pcawg_id_3_3$L2, pcawg_id_3_3$L3, sep = "+")

pcawg_MCP = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("MCPCOUNTER")) %>% 
    dplyr::rename_with(~ gsub("_MCPCOUNTER", "", .x))
pcawg_id_3_4 = data.frame(
    L1 = "Immune",
    L2 = "MCP",
    L3 = colnames(pcawg_MCP)[-1]
)
colnames(pcawg_MCP)[-1] = paste(
    pcawg_id_3_4$L1, pcawg_id_3_4$L2, pcawg_id_3_4$L3, sep = "+")

pcawg_quant = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("QUANTISEQ")) %>% 
    dplyr::rename_with(~ gsub("_QUANTISEQ", "", .x))
pcawg_id_3_5 = data.frame(
    L1 = "Immune",
    L2 = "Quant",
    L3 = colnames(pcawg_quant)[-1]
)
colnames(pcawg_quant)[-1] = paste(
    pcawg_id_3_5$L1, pcawg_id_3_5$L2, pcawg_id_3_5$L3, sep = "+")

pcawg_timer = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("TIMER")) %>% 
    dplyr::rename_with(~ gsub("_TIMER", "", .x))
pcawg_id_3_6 = data.frame(
    L1 = "Immune",
    L2 = "TIMER",
    L3 = colnames(pcawg_timer)[-1]
)
colnames(pcawg_timer)[-1] = paste(
    pcawg_id_3_6$L1, pcawg_id_3_6$L2, pcawg_id_3_6$L3, sep = "+")

pcawg_xcell = pcawg_TIL %>% 
    dplyr::select("Sample", ends_with("XCELL")) %>% 
    dplyr::rename_with(~ gsub("_XCELL", "", .x))
pcawg_id_3_7 = data.frame(
    L1 = "Immune",
    L2 = "XCELL",
    L3 = colnames(pcawg_xcell)[-1]
)
colnames(pcawg_xcell)[-1] = paste(
    pcawg_id_3_7$L1, pcawg_id_3_7$L2, pcawg_id_3_7$L3, sep = "+")


pcawg_id_3 = do.call(rbind, 
    list(pcawg_id_3_1, pcawg_id_3_2, pcawg_id_3_3,
         pcawg_id_3_4, pcawg_id_3_5, pcawg_id_3_6, pcawg_id_3_7))
table(pcawg_id_3$L2)
# CIB CIB.ABS    EPIC     MCP   Quant   TIMER   XCELL 
#  22      22       8      11      11       6      39 

pcawg_immune = pcawg_CIB %>% 
    dplyr::full_join(pcawg_CIB.ABS) %>% 
    dplyr::full_join(pcawg_EPIC) %>% 
    dplyr::full_join(pcawg_MCP) %>% 
    dplyr::full_join(pcawg_quant) %>% 
    dplyr::full_join(pcawg_timer) %>% 
    dplyr::full_join(pcawg_xcell)
dim(pcawg_immune)
# [1] 1466   120


## Pathway
pcawg_PW = load_data("pcawg_PW") %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Sample")

pcawg_hm = pcawg_PW %>% 
    dplyr::select("Sample", starts_with("HALLMARK")) %>% 
    dplyr::rename_with(~ gsub("HALLMARK_", "", .x))
pcawg_id_4_1 = data.frame(
    L1 = "Pathway",
    L2 = "HM",
    L3 = colnames(pcawg_hm)[-1]
)
colnames(pcawg_hm)[-1] = paste(
    pcawg_id_4_1$L1, pcawg_id_4_1$L2, pcawg_id_4_1$L3, sep = "+")

pcawg_kegg = pcawg_PW %>% 
    dplyr::select("Sample", starts_with("KEGG")) %>% 
    dplyr::rename_with(~ gsub("KEGG_", "", .x))
pcawg_id_4_2 = data.frame(
    L1 = "Pathway",
    L2 = "KEGG",
    L3 = colnames(pcawg_kegg)[-1]
)
colnames(pcawg_kegg)[-1] = paste(
    pcawg_id_4_2$L1, pcawg_id_4_2$L2, pcawg_id_4_2$L3, sep = "+")

pcawg_iobr = pcawg_PW %>% 
    dplyr::select("Sample", starts_with("IOBR")) %>% 
    dplyr::rename_with(~ gsub("IOBR_", "", .x))
pcawg_id_4_3 = data.frame(
    L1 = "Pathway",
    L2 = "IOBR",
    L3 = colnames(pcawg_iobr)[-1]
)
colnames(pcawg_iobr)[-1] = paste(
    pcawg_id_4_3$L1, pcawg_id_4_3$L2, pcawg_id_4_3$L3, sep = "+")


pcawg_id_4 = do.call(rbind, 
    list(pcawg_id_4_1, pcawg_id_4_2, pcawg_id_4_3))
table(pcawg_id_4$L2)
#   HM IOBR KEGG 
#   50  264  186 

pcawg_pathway = pcawg_hm %>% 
    dplyr::full_join(pcawg_kegg) %>% 
    dplyr::full_join(pcawg_iobr)
dim(pcawg_pathway)
# [1] 1466   501


## Phenotype
pcawg_phe = pcawg_info_fine %>% 
    .[,c("Sample","Age", "Gender","Type")]
pcawg_id_5 = data.frame(
    L1 = "Phenotype",
    L2 = "Clinical",
    L3 = colnames(pcawg_phe)[-1]
)
colnames(pcawg_phe)[-1] = paste(
    pcawg_id_5$L1, pcawg_id_5$L2, pcawg_id_5$L3, sep = "+")


pcawg_value_nonomics = pcawg_phe %>% 
    dplyr::left_join(pcawg_index) %>% 
    dplyr::left_join(pcawg_immune) %>% 
    dplyr::left_join(pcawg_pathway)
save(pcawg_value_nonomics, file = "../v2_pcawg_value_nonomics.rda")

pcawg_id_nonomics = do.call(rbind, 
    list(pcawg_id_2, pcawg_id_3,
         pcawg_id_4, pcawg_id_5)
)
pcawg_id_omics = pcawg_id_1
pcawg_id = rbind(pcawg_id_nonomics, pcawg_id_omics) %>% tibble()
save(pcawg_id, file = "../v2_pcawg_id.rda")


## CCLE
ccle_id_referrence = load_data("ccle_identifier")

ccle_id_1_1 = data.frame(
    L1 = "Molecule",
    L2 = "Gene",
    L3 = sort(ccle_id_referrence$id_gene$Level3)
)
ccle_id_1_1 = ccle_id_1_1 %>% 
    dplyr::filter(L3 %in% gene_pc)

ccle_id_1_2 = data.frame(
    L1 = "Molecule",
    L2 = "Protein",
    L3 = sort(ccle_id_referrence$id_pro$Level3)
)

ccle_id_1_3 = data.frame(
    L1 = "Molecule",
    L2 = "CNV",
    L3 = sort(ccle_id_referrence$id_cnv$Level3)
)
ccle_id_1_3 = ccle_id_1_3 %>% 
    dplyr::filter(L3 %in% gene_pc)

ccle_id_1_4 = data.frame(
    L1 = "Molecule",
    L2 = "Muta",
    L3 = sort(ccle_id_referrence$id_mut$Level3)
)
ccle_id_1_4 = ccle_id_1_4 %>% 
    dplyr::filter(L3 %in% gene_pc)


ccle_id_1 = do.call(rbind,
    list(ccle_id_1_1, ccle_id_1_2))
table(ccle_id_1$L2)
#   Gene Protein 
#  19189     214 

## Index
ccle_purity = load_data("ccle_absolute") %>% 
    .[,c("Cell Line", "Purity", "Ploidy", "Genome Doublings", "Lineage")] %>% 
    dplyr::rename("Sample"="Cell Line") %>%
    dplyr::filter(.data$Sample %in% ccle_info_fine$Sample)
ccle_index = ccle_purity
ccle_id_2 = data.frame(
    L1 = "Index",
    L2 = "Purity",
    L3 = colnames(ccle_index)[-1]
)
colnames(ccle_index)[-1] = paste(
    ccle_id_2$L1, ccle_id_2$L2, ccle_id_2$L3, sep = "+")

## Phenotype
ccle_phe = ccle_info_fine %>% 
    .[,c("Sample","Gender","Histology","Type")]
ccle_id_5 = data.frame(
    L1 = "Phenotype",
    L2 = "Clinical",
    L3 = colnames(ccle_phe)[-1]
)
colnames(ccle_phe)[-1] = paste(
    ccle_id_5$L1, ccle_id_5$L2, ccle_id_5$L3, sep = "+")


ccle_value_nonomics = ccle_phe %>% 
    dplyr::left_join(ccle_index)
save(ccle_value_nonomics, file = "../v2_ccle_value_nonomics.rda")

ccle_id_nonomics = do.call(rbind, 
    list(ccle_id_2, ccle_id_5)
)
ccle_id_omics = ccle_id_1
ccle_id = rbind(ccle_id_nonomics, ccle_id_omics) %>% tibble()
save(ccle_id, file = "../v2_ccle_id.rda")


