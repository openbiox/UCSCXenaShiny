#' @describeIn get_pancan_value Fetch copy number value from CCLE dataset
#' @export
get_ccle_cn_value <- function(identifier) {
  host <- "publicHub"
  dataset <- "ccle/CCLE_copynumber_byGene_2013-12-03"

  res <- check_exist_data(identifier, dataset, host)
  if (res$ok) {
    data <- res$data
  } else {
    data <- get_pancan_value(identifier, dataset = dataset, host = host)
    save_data(data, identifier, dataset, host)
  }

  unit <- "log(copy number/2)"
  report_dataset_info(dataset)
  res <- list(data = data, unit = unit)
  res
}




#' @param norm normalization method
#'
#' @describeIn get_pancan_value Fetch gene expression value from CCLE dataset
#' @export
get_ccle_gene_value <- function(identifier, norm = c("rpkm","nc")) {
  host <- "publicHub"
  if(norm=="rpkm"){
    dataset <- "ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502"
  } else {
    dataset <- "ccle/CCLE_DepMap_18Q2_RNAseq_reads_20180502.log2"
  }
  res <- check_exist_data(identifier, dataset, host)
  if (res$ok) {
    expression <- res$data
  } else {
    expression <- get_pancan_value(identifier, dataset = dataset, host = host)
    save_data(expression, identifier, dataset, host)
  }

  if(norm=="rpkm"){
    unit <- "RPKM"
  } else {
    unit <- "log2(count+1)"
  }
  
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}


#' @describeIn get_pancan_value Fetch gene protein expression value from CCLE dataset
#' @export
get_ccle_protein_value <- function(identifier) {
  ## NOTE: Only ~200 proteins available, so many identifiers will return NAs
  host <- "publicHub"
  dataset <- "ccle/CCLE_RPPA_20180123"

  res <- check_exist_data(identifier, dataset, host)
  if (res$ok) {
    expression <- res$data
  } else {
    expression <- get_pancan_value(identifier, dataset = dataset, host = host)
    save_data(expression, identifier, dataset, host)
  }

  unit <- "norm_value"
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}

## .p_dataset_field("https://ucscpublic.xenahubs.net", "ccle/CCLE_RPPA_20180123")
## NOTE: this is different from .all_pancan_proteins
.all_ccle_proteins <- c(
  "14-3-3_beta", "14-3-3_epsilon_Caution", "14-3-3_zeta", "4E-BP1",
  "4E-BP1_pS65", "4E-BP1_pT37_T46", "4E-BP1_pT70", "53BP1", "A-Raf_pS299_Caution",
  "ACC1_Caution", "ACC_pS79", "ACVRL1_Caution", "ADAR1", "AMPK_alpha_Caution",
  "AMPK_pT172", "AR", "ASNS", "ATM", "Acetyl-a-Tubulin (Lys40)_Caution",
  "Akt", "Akt_pS473", "Akt_pT308", "Annexin 1", "Annexin_VII",
  "B-Raf_Caution", "B-Raf_pS445", "BRCA2_Caution", "Bad_pS112",
  "Bak_Caution", "Bap1 c-4", "Bax", "Bcl-2", "Bcl-xL", "Beclin_Caution",
  "Bid_Caution", "Bim(CST2933)", "Bim(EP1036)", "C-Raf(BD610151)_Caution",
  "C-Raf(MP05-739)", "C-Raf_pS338", "CD20 _Caution", "CD31", "CD49b",
  "CDK1", "Caspase-7_cleavedD198_Caution", "Caspase-8_Caution",
  "Caveolin-1", "Chk1_Caution", "Chk1_pS345_Caution", "Chk2", "Chk2_pT68_Caution",
  "Claudin-7", "Collagen_VI", "Cyclin_B1", "Cyclin_D1", "Cyclin_E1",
  "Cyclin_E2_Caution", "DJ-1", "Di-Ras3_Caution", "Dvl3", "E-Cadherin",
  "EGFR", "EGFR_pY1068_Caution", "EGFR_pY1173", "ER-alpha", "ER-alpha_pS118",
  "ERCC1_Caution", "ERK2_Caution", "ETS-1", "FASN", "FOXO3a_Caution",
  "FOXO3a_pS318_S321_Caution", "FRA1_Caution", "Fibronectin", "FoxM1",
  "G6PD", "GAPDH_Caution", "GATA3", "GSK-3-BETA_Caution", "GSK3-alpha-beta",
  "GSK3-alpha-beta_pS21_S9", "GSK3_pS9", "Gab2", "HER2", "HER2_pY1248_Caution",
  "HER3", "HER3_pY1289_Caution", "HSP70_Caution", "Heregulin",
  "IGFBP2", "INPP4B", "IRS1", "JAK2", "JNK2_Caution", "JNK_pT183_Y185",
  "Ku80_Caution", "Lck", "MAPK_pT202_Y204", "MDM2_pS166", "MDMX_MDM4(BetIHC-00108)_Caution",
  "MEK1", "MEK1_pS217_S221", "MIG-6", "MSH2", "MSH6_Caution", "MYH11",
  "Mre11_Caution", "Myosin IIa pS1943", "N-Cadherin", "N-Ras",
  "NDRG1_pT346", "NF-kB-p65_pS536_Caution", "NF2_Caution", "Notch1",
  "P-Cadherin_Caution", "PAI-1", "PARP_cleaved_Caution", "PCNA_Caution",
  "PDCD4_Caution", "PDK1", "PDK1_pS241", "PEA15", "PEA15_pS116",
  "PI3K-p110-alpha_Caution", "PI3K-p85", "PKC-alpha", "PKC-alpha_pS657_Caution",
  "PKC-delta_pS664", "PKC-pan_BetaII_pS660", "PR", "PRAS40_pT246",
  "PRDX1", "PREX1", "PTEN", "Paxillin_Caution", "Porin", "RAD51",
  "RBM15", "RSK1-2-3_Caution", "Rab25", "Rad50", "Raptor", "Rb_Caution",
  "Rb_pS807_S811", "Rictor_Caution", "Rictor_pT1135", "S6_pS235_S236",
  "S6_pS240_S244", "SCD1", "SETD2_Caution", "SF2", "SHP-2_pY542_Caution",
  "STAT3_Caution", "STAT3_pY705", "STAT5-alpha", "Shc_pY317", "Smac_Caution",
  "Smad1", "Smad3", "Smad4", "Snail_Caution", "Src", "Src_pY416_Caution",
  "Src_pY527", "Stathmin", "Syk", "TAZ", "TFRC", "TIGAR", "TSC1_Caution",
  "TTF1", "Transglutaminase", "Tuberin", "Tuberin_pT1462", "VAV1_Caution",
  "VEGFR2", "VHL_Caution", "XBP1_Caution", "XRCC1_Caution", "YAP_Caution",
  "YAP_pS127_Caution", "YB-1", "YB-1_pS102", "alpha-Catenin", "beta Actin_Caution",
  "beta-Catenin", "beta-Catenin_pT41_S45", "c-Jun_pS73", "c-Kit",
  "c-Met_Caution", "c-Met_pY1235", "c-Myc_Caution", "cIAP_Caution",
  "eEF2K", "eEF2_Caution", "eIF4E", "eIF4G_Caution", "mTOR", "mTOR_pS2448_Caution",
  "p14 Arf(BetA300-340A)_Caution", "p21", "p27", "p27_pT157_Caution",
  "p27_pT198", "p38 alpha MAPK", "p38_MAPK", "p38_pT180_Y182",
  "p53_Caution", "p62 Lck ligand_Caution", "p70S6K", "p70S6K_pT389",
  "p90RSK_Caution", "p90RSK_pT359_S363_Caution", "p90RSK_pT573_Caution"
)

# NOTE this result is different from get_pancan_mutation_status!
# It is maf like format, needs further processing.
# See ?read.maf to get nonsync mutation
#' @describeIn get_pancan_value Fetch gene mutation info from CCLE dataset
#' @export
get_ccle_mutation_status <- function(identifier) {
  if (utils::packageVersion("UCSCXenaTools") < "1.3.2") {
    stop("You need to update 'UCSCXenaTools' (>=1.3.2).", call. = FALSE)
  }

  host <- "https://ucscpublic.xenahubs.net"
  dataset <- "ccle/CCLE_DepMap_18Q2_maf_20180502"
  report_dataset_info(dataset)

  res <- check_exist_data(identifier, dataset, host)
  if (res$ok) {
    data <- res$data
  } else {
    query_list <- UCSCXenaTools::fetch_sparse_values(host, dataset, identifier)
    data <- as.data.frame(query_list$rows)
    data <- dplyr::full_join(
      dplyr::tibble(
        sampleID = query_list$samples
      ),
      data,
      by = "sampleID"
    )
    save_data(data, identifier, dataset, host)
  }

  report_dataset_info(dataset)
  data
}
