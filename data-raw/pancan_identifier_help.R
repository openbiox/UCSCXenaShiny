library(tidyverse)

### molecular profile----
## gene
# https://toil-xena-hub.s3.us-east-1.amazonaws.com/download/probeMap%2Fgencode.v23.annotation.gene.probemap
id_gene <- data.table::fread("ID_meta/probeMap_gencode.v23.annotation.gene.probemap")
id_gene <- id_gene %>%
  dplyr::mutate(Level2 = "mRNA Expression", .before = 1) %>%
  dplyr::rename(Level3 = gene, Ensembl = id) %>%
  dplyr::select(Level2, Level3, everything())
head(id_gene)

## transcript
# https://toil-xena-hub.s3.us-east-1.amazonaws.com/download/probeMap%2Fgencode.v23.annotation.transcript.probemap
id_trans <- data.table::fread("ID_meta/probeMap_gencode.v23.annotation.transcript.probemap")
id_trans <- id_trans %>%
  dplyr::mutate(Level2 = "Transcript Expression", .before = 1) %>%
  dplyr::mutate(Level3 = str_split(id, "[.]", simplify = T)[, 1]) %>%
  dplyr::rename(Ensembl = id, Symbol = gene) %>%
  dplyr::select(Level2, Level3, everything())
head(id_trans)


## Methylation
# 450: https://gdc-hub.s3.us-east-1.amazonaws.com/download/illuminaMethyl450_hg38_GDC
id_M450 <- data.table::fread("ID_meta/illuminaMethyl450_hg38_GDC")
colnames(id_M450)[1] <- "id"
id_M450 <- id_M450 %>%
  dplyr::mutate(Level2 = "DNA Methylation", .before = 1) %>%
  dplyr::rename(CpG = id) %>%
  tidyr::separate_rows("gene", sep = ",") %>%
  dplyr::rename(Level3 = gene) %>%
  dplyr::select(Level2, Level3, CpG, everything())
head(id_M450)


id_M27K <- data.table::fread("ID_meta/illuminaMethyl27_hg38_GDC")
colnames(id_M27K)[1] <- "id"
id_M27K <- id_M27K %>%
  dplyr::mutate(Level2 = "DNA Methylation", .before = 1) %>%
  dplyr::rename(CpG = id) %>%
  tidyr::separate_rows("gene", sep = ",") %>%
  dplyr::rename(Level3 = gene) %>%
  dplyr::select(Level2, Level3, CpG, everything())
head(id_M27K)


## Protein
# https://xenabrowser.net/datapages/?dataset=TCGA-RPPA-pancan-clean.xena&host=https://pancanatlas.xenahubs.net
id_pro <- data.table::fread("ID_meta/TCGA-RPPA-ID.txt")
id_pro <- id_pro %>%
  dplyr::mutate(Level2 = "Protein Expression", .before = 1) %>%
  dplyr::rename(Level3 = id)
head(id_pro)


## miRNA
# https://xenabrowser.net/datapages/?dataset=pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena&host=https://pancanatlas.xenahubs.net
id_mi <- data.table::fread("ID_meta/pancanMiRs.txt")
id_mi <- id_mi %>%
  dplyr::mutate(Level2 = "miRNA Expression", .before = 1) %>%
  dplyr::rename(Level3 = id)
head(id_mi)


## mutate
# https://tcga-pancan-atlas-hub.s3.us-east-1.amazonaws.com/download/probeMap%2Fhugo_gencode_good_hg19_V24lift37_probemap
id_mut <- data.table::fread("ID_meta/probeMap_hugo_gencode_good_hg19_V24lift37_probemap")[, "id"]
id_mut <- id_mut %>%
  dplyr::mutate(Level2 = "Mutation status", .before = 1) %>%
  dplyr::rename(Level3 = id)
head(id_mut)


## CNV
# https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_data_by_genes&host=https://tcga.xenahubs.net
id_cn <- data.table::fread("ID_meta/Gistic2_CN.txt", sep = "\n")
id_cn <- id_cn %>%
  dplyr::mutate(Level2 = "Copy Number Variation", .before = 1) %>%
  dplyr::rename(Level3 = id)
head(id_cn)


id_molecule.list <- list(
  id_gene = id_gene,
  id_pro = id_pro,
  id_mut = id_mut,
  id_cn = id_cn,
  id_trans = id_trans,
  id_M450 = id_M450,
  id_M27K = id_M27K,
  id_mi = id_mi
)







### tumor index
library(UCSCXenaShiny)
tumor_index_list <- list(
  tcga_purity = load_data("tcga_purity"),
  tcga_stemness = load_data("tcga_stemness"),
  tcga_tmb = load_data("tcga_tmb"),
  tcga_msi = load_data("tcga_MSI"),
  tcga_genome_instability = load_data("tcga_genome_instability")
)
colnames(tumor_index_list$tcga_tmb)[3] <- "sample"
tumor_index_list$tcga_msi <- tcga_gtex %>%
  dplyr::mutate(Barcode = stringr::str_sub(sample, 1, 12)) %>%
  dplyr::select(Barcode, sample) %>%
  dplyr::inner_join(tumor_index_list$tcga_msi, by = "Barcode")

id_tumor_index <- lapply(tumor_index_list, colnames) %>%
  reshape2::melt() %>%
  dplyr::select(L1, value) %>%
  dplyr::rename(Level2 = L1, Level3 = value) %>%
  dplyr::filter(!Level3 %in% c("sample", "cancer_type", "Barcode", "Cancer_type", "Cohort", "Patient_ID"))


id_tumor_index.list <- split(id_tumor_index, id_tumor_index$Level2)
id_tumor_index.list$tcga_genome_instability$Level2 <- "Genome Instability"
id_tumor_index.list$tcga_msi$Level2 <- "Microsatellite Instability"
id_tumor_index.list$tcga_purity$Level2 <- "Tumor Purity"
id_tumor_index.list$tcga_stemness$Level2 <- "Tumor Stemness"
id_tumor_index.list$tcga_tmb$Level2 <- "Tumor Mutation Burden"
# 行名重置
id_tumor_index.list <- lapply(id_tumor_index.list, function(x) {
  x <- x %>% tibble::remove_rownames()
})



### immune infiltration
tcga_TIL <- load_data("tcga_TIL")
TIL_signatures <- colnames(tcga_TIL)[-1]
TIL_meta <- strsplit(TIL_signatures, "_") %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  dplyr::rename("method" = "V2", "celltype" = "V1") %>%
  dplyr::select(method, celltype) %>%
  dplyr::rename(Level2 = method, Level3 = celltype)
head(TIL_meta)

id_TIL.list <- split(TIL_meta, TIL_meta$Level2)
# 行名重置
id_TIL.list <- lapply(id_TIL.list, function(x) {
  x <- x %>% tibble::remove_rownames()
})



### pathway score
PW_meta <- load_data("tcga_PW_meta")
PW_meta <- PW_meta %>%
  dplyr::arrange(Name) %>%
  dplyr::mutate(size = purrr::map_int(Gene, function(x) {
    x_ids <- strsplit(x, "/", fixed = TRUE)[[1]]
    length(x_ids)
  }), .before = 5) %>%
  dplyr::mutate(display = paste0(Name, " (", size, ")"), .before = 6)
PW_meta <- PW_meta %>%
  dplyr::select(Type, Name, Origin, size) %>%
  dplyr::rename(Level2 = Type, Level3 = Name)

id_PW.list <- split(PW_meta, PW_meta$Level2)
id_PW.list <- lapply(id_PW.list, function(x) {
  x <- x %>% tibble::remove_rownames()
})


### 合并上述4类数据
id_merge <- list(
  id_molecule.list, id_tumor_index.list,
  id_TIL.list, id_PW.list
)

names(id_merge) <- c("id_molecule", "id_tumor_index", "id_TIL", "id_PW")
save(id_merge, file = "pancan_identifier_help.rda")








library(tidyverse)


# tcga_ids = load_data("pancan_identifier_help")
# names(tcga_ids)
# # [1] "id_molecule"    "id_tumor_index" "id_TIL"         "id_PW"
# names(tcga_ids$id_molecule)
# # [1] "id_gene"  "id_pro"   "id_mut"   "id_cn"    "id_trans" "id_M450"  "id_M27K"  "id_mi"

pcawg_identifier <- list()

# gene
gene_ref <- data.table::fread("id_ref/gencode.v19.annotation.gene.probemap")
head(gene_ref)

gene_ref <- gene_ref %>%
  dplyr::mutate(Level2 = "mRNA Expression", .before = 1) %>%
  dplyr::rename(Level3 = gene) %>%
  dplyr::rename("Ensembl" = "#id") %>%
  dplyr::select(Level2, Level3, dplyr::everything())

# promoter
promoter_ref <- data.table::fread("id_ref/expressedPromoterCoordinates.probeMap")

promoter_ref <- promoter_ref %>%
  dplyr::mutate(Level2 = "Promoter activity", .before = 1) %>%
  dplyr::rename(Level3 = id)



# fusion
fusion <- data.table::fread("id_ref/gene_fusion_ids.txt", header = FALSE)

fusion_ref <- gene_ref %>%
  dplyr::mutate(Ensembl = str_split(Ensembl, "[.]", simplify = T)[, 1]) %>%
  dplyr::filter(Ensembl %in% fusion$V1) %>%
  dplyr::mutate(Level2 = "Gene fusion") %>%
  dplyr::select(Level2, Level3, Ensembl)


# miRNA
miRNA <- data.table::fread("id_ref/miRNA_ids.txt", header = FALSE)
miRNA_ref <- miRNA %>%
  dplyr::mutate(Level2 = "miRNA expression", .before = 1) %>%
  dplyr::rename(Level3 = V1)



# MAF
maf <- data.table::fread("id_ref/MAF.txt", header = FALSE)
maf_ref <- maf %>%
  dplyr::mutate(Level2 = "APOBEC mutagenesis", .before = 1) %>%
  dplyr::rename(Level3 = V1)



pcawg_identifier <- list(
  id_gene = gene_ref,
  id_pro = promoter_ref,
  id_fusion = fusion_ref,
  id_mi = miRNA_ref,
  id_maf = maf_ref
)

save(pcawg_identifier, file = "./id_ref/pcawg_identifier.rda")







# gene
gene_ref <- data.table::fread("./id_ref2/probeMap_gencode.v19.annotation.gene.probemap")

gene_ref <- gene_ref %>%
  dplyr::mutate(Level2 = "mRNA Expression", .before = 1) %>%
  dplyr::rename(Level3 = gene) %>%
  dplyr::rename("Ensembl" = "#id") %>%
  dplyr::select(Level2, Level3, dplyr::everything())


# protein

protein_ref <- data.table::fread("id_ref2/ids_protein.txt", header = FALSE)
protein_ref <- protein_ref %>%
  dplyr::mutate(Level2 = "Protein expression", .before = 1) %>%
  dplyr::rename(Level3 = V1)



# CNV
cnv_ref <- data.table::fread("id_ref2/CCLE_copynumber_byGene_2013-12-03.txt.gz")
cnv_ref <- cnv_ref %>%
  dplyr::select(SYMBOL) %>%
  dplyr::mutate(Level2 = "Copy Number Variation", .before = 1) %>%
  dplyr::rename(Level3 = SYMBOL)

# SNP
snp_raw <- data.table::fread("id_ref2/CCLE_DepMap_18Q2_maf_20180502.txt")
head(snp_raw)

snp_ref <- data.frame(Level3 = na.omit(unique(snp_raw$Hugo_Symbol))) %>%
  dplyr::mutate(Level2 = "Mutation status", .before = 1)


ccle_identifier <- list(
  id_gene = gene_ref,
  id_pro = protein_ref,
  id_cnv = cnv_ref,
  id_mut = snp_ref
)

save(ccle_identifier, file = "./id_ref2/ccle_identifier.rda")
