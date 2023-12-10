#' Show Available Hosts
#'
#' @return hosts
#' @export
#'
#' @examples
#' available_hosts()
available_hosts <- function() {
  .xena_hosts <- "UCSCXenaTools" %:::% ".xena_hosts"
  .xena_hosts %>% as.character()
}


#' Fetch Identifier Value from Pan-cancer Dataset
#'
#' Identifier includes gene/probe etc.
#'
#' @param identifier a length-1 character representing a gene symbol, ensembl gene id, or probe id.
#' Gene symbol is highly recommended.
#' @param subtype a length-1 chracter representing a regular expression for matching
#' `DataSubtype` column of [UCSCXenaTools::XenaData].
#' @param dataset a length-1 chracter representing a regular expression for matching
#' `XenaDatasets` of [UCSCXenaTools::XenaData].
#' @param host a character vector representing host name(s), e.g. "toilHub".
#' @param samples a character vector representing samples want to be returned.
#' @param ... other parameters.
#' @return a named vector or `list`.
#'
#' @examples
#' \dontrun{
#' # Fetch TP53 expression value from pan-cancer dataset
#' t1 <- get_pancan_value("TP53",
#'   dataset = "TcgaTargetGtex_rsem_isoform_tpm",
#'   host = "toilHub"
#' )
#' t2 <- get_pancan_gene_value("TP53")
#' t3 <- get_pancan_protein_value("AKT")
#' t4 <- get_pancan_mutation_status("TP53")
#' t5 <- get_pancan_cn_value("TP53")
#' }
#'
#' @export
#' @describeIn get_pancan_value Fetch identifier value from pan-cancer dataset
get_pancan_value <- function(identifier, subtype = NULL, dataset = NULL, host = available_hosts(),
                             samples = NULL, ...) {
  stopifnot(length(identifier) == 1, !all(is.null(subtype), is.null(dataset)))
  if (!requireNamespace("UCSCXenaTools", quietly = TRUE)) {
    stop("Package 'UCSCXenaTools' is not installed.")
  }
  if (!"UCSCXenaTools" %in% .packages()) {
    attachNamespace("UCSCXenaTools")
  }
  host <- match.arg(host, choices = available_hosts(), several.ok = TRUE)

  data <- UCSCXenaTools::XenaData
  if (!is.null(subtype)) {
    data <- data %>%
      dplyr::filter(XenaHostNames %in% host, grepl(subtype, DataSubtype))
  }
  if (!is.null(dataset)) {
    data <- data %>%
      dplyr::filter(XenaHostNames %in% host, grepl(dataset, XenaDatasets))
  }

  if (nrow(data) == 0) {
    stop("No dataset is determined by input")
  }

  if (nrow(data) > 1) {
    data <- dplyr::filter(data, .data$XenaDatasets == dataset)
  }

  res <- try_query_value(data[["XenaHosts"]], data[["XenaDatasets"]],
    identifiers = identifier, samples = samples,
    check = FALSE, use_probeMap = TRUE, ...
  )
  res2 <- res[1, ]
  names(res2) <- colnames(res)
  res2
}

## try solving internet connection problem
try_query_value <- function(host, dataset,
                            identifiers, samples,
                            check = FALSE, use_probeMap = TRUE,
                            rule_out = NULL,
                            aggr = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100"),
                            max_try = 5L) {
  aggr = match.arg(aggr)
  Sys.sleep(0.05)
  
  tryCatch(
    {
      message("Try querying data #", abs(max_try - 6L))
      
      if (aggr != "NA") {
        # Designed for methylation data
        # be careful!
        message("Fetching all ids, take patience...")
        #ids = UCSCXenaTools::fetch_dataset_identifiers(host, dataset)
        ## Download probeMap time-cost
        # xe = UCSCXenaTools::XenaQueryProbeMap(UCSCXenaTools::XenaGenerate(subset = XenaDatasets == dataset))
        # xd = UCSCXenaTools::XenaPrepare(UCSCXenaTools::XenaDownload(xe), col_names = FALSE)[, c(1, 2)]
        # xd = tidyr::separate_rows(xd, "X2", sep = ",")
        
        ## Use the prepared ID referrence data
        id_ref = load_data("pancan_identifier_help")[["id_molecule"]]
        if(grepl("450", dataset)){
          xd = id_ref[["id_M450"]]
        } else {
          xd = id_ref[["id_M27K"]]
        }
        # xd = ifelse(grepl("450", dataset), id_ref[["id_M450"]], id_ref[["id_M27K"]])
        xd = xd %>%
          dplyr::select("CpG", "Level3") %>% 
          dplyr::rename("X1" = "CpG", "X2" = "Level3")
        
        xd = dplyr::filter(xd, .data$X2 %in% identifiers)
        
        if (!is.null(rule_out)) {
          xd = dplyr::filter(xd, !.data$X1 %in% rule_out)  # X2 → X1
        }
        ids = xd$X1
        
        d = UCSCXenaTools::fetch_dense_values(host, dataset,
                                              identifiers = ids, samples = samples,
                                              check = FALSE, use_probeMap = FALSE
        )
        f = switch(aggr,
                   mean = function(x) mean(x, na.rm = TRUE),
                   Q0 = function(x) quantile(x, 0, na.rm = TRUE),
                   Q25 = function(x) quantile(x, 0.25, na.rm = TRUE),
                   Q50 = function(x) quantile(x, 0.5, na.rm = TRUE),
                   Q75 = function(x) quantile(x, 0.75, na.rm = TRUE),
                   Q100 = function(x) quantile(x, 1, na.rm = TRUE))
        
        z = apply(d, 2, f)
        matrix(z, nrow = 1, dimnames = list("aggr_methy_value", names(z)))
      } else {
        UCSCXenaTools::fetch_dense_values(host, dataset,
                                          identifiers = identifiers, samples = samples,
                                          check = check, use_probeMap = use_probeMap
        )
      }
      
    },
    error = function(e) {
      if (max_try == 1) {
        warning("Tried 5 times but failed, please check URL or your internet connection or try it later!", immediate. = TRUE)
        return(NULL)
      } else {
        try_query_value(host, dataset,
          identifiers, samples,
          check = check, use_probeMap = use_probeMap,
          max_try = max_try - 1L,
          rule_out = rule_out,
          aggr = aggr
        )
      }
    }
  )
}


# Toil/TCGA ---------------------------------------------------------------

#' @describeIn get_pancan_value Fetch gene expression value from pan-cancer dataset
#' @export
get_pancan_gene_value <- function(identifier, norm = c("tpm","fpkm","nc")) {
  host <- "toilHub"
  
  norm <- match.arg(norm)
  dataset <- switch(norm, 
                   "tpm"="TcgaTargetGtex_rsem_gene_tpm",
                   "fpkm"="TcgaTargetGtex_rsem_gene_fpkm",
                   "nc"="TcgaTargetGtex_RSEM_Hugo_norm_count")
  # dataset <- "TcgaTargetGtex_rsem_gene_tpm"

  expression <- get_data(dataset, identifier, host)
  unit <- switch(norm,
                  "tpm"="log2(tpm+0.001)",
                  "fpkm"="log2(fpkm+0.001)",
                  "nc" = "log2(norm_count+1)")
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}


#' @describeIn get_pancan_value Fetch gene transcript expression value from pan-cancer dataset
#' @export
get_pancan_transcript_value <- function(identifier, norm = c("tpm", "fpkm", "isopct")) {
  # ENST00000000233
  id <- identifier
  host <- "toilHub"

  norm = match.arg(norm)
  
  dataset = switch(norm, 
                   "tpm"="TcgaTargetGtex_rsem_isoform_tpm",
                   "fpkm"="TcgaTargetGtex_RSEM_isoform_fpkm",
                   "isopct"="TcgaTargetGtex_rsem_isopct")
  
  res_p <- check_exist_data("mp", dataset, host)
  if (res_p$ok) {
    ids <- res_p$data
  } else {
    ids <- UCSCXenaTools::fetch_dataset_identifiers("https://toil.xenahubs.net", dataset)
    names(ids) <- sub("\\.[0-9]+", "", ids)
    save_data(ids, "mp", dataset, host)
  }

  identifier <- as.character(ids[identifier])
  if (is.na(identifier)) identifier <- id # roll back

  expression <- get_data(dataset, identifier, host)
  unit <- switch(norm,
                 "tpm"="log2(tpm+0.001)",
                 "fpkm"="log2(fpkm+0.001)",
                 "isopct" = "IsoPct")
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch protein expression value from pan-cancer dataset
#' @export
get_pancan_protein_value <- function(identifier) {
  ## NOTE: Only ~200 proteins available, so many identifiers will return NAs
  host <- "pancanAtlasHub"
  dataset <- "TCGA-RPPA-pancan-clean.xena"

  expression <- get_data(dataset, identifier, host)

  unit <- "norm_value"
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}

## .p_dataset_field("https://pancanatlas.xenahubs.net", "TCGA-RPPA-pancan-clean.xena")
.all_pancan_proteins <- c(
  "ACC1", "ACC_pS79", "ACETYLATUBULINLYS40", "ACVRL1", "ADAR1",
  "AKT", "AKT_pS473", "AKT_pT308", "ALPHACATENIN", "AMPKALPHA",
  "AMPKALPHA_pT172", "ANNEXIN1", "ANNEXINVII", "AR", "ARAF", "ARAF_pS299",
  "ARID1A", "ASNS", "ATM", "AXL", "BAD_pS112", "BAK", "BAP1C4",
  "BAX", "BCL2", "BCL2A1", "BCLXL", "BECLIN", "BETACATENIN", "BID",
  "BIM", "BRAF", "BRAF_pS445", "BRCA2", "BRD4", "CA9", "CABL",
  "CASPASE3", "CASPASE7CLEAVEDD198", "CASPASE8", "CASPASE9", "CAVEOLIN1",
  "CD20", "CD26", "CD31", "CD49B", "CDK1", "CDK1_pY15", "CHK1",
  "CHK1_pS296", "CHK1_pS345", "CHK2", "CHK2_pT68", "CHROMOGRANINANTERM",
  "CIAP", "CJUN_pS73", "CK5", "CKIT", "CLAUDIN7", "CMET", "CMET_pY1235",
  "CMYC", "COG3", "COLLAGENVI", "COMPLEXIISUBUNIT30", "CRAF", "CRAF_pS338",
  "CTLA4", "CYCLINB1", "CYCLIND1", "CYCLINE1", "CYCLINE2", "DIRAS3",
  "DJ1", "DUSP4", "DVL3", "E2F1", "ECADHERIN", "EEF2", "EEF2K",
  "EGFR", "EGFR_pY1068", "EGFR_pY1173", "EIF4E", "EIF4G", "ENY2",
  "EPPK1", "ERALPHA", "ERALPHA_pS118", "ERCC1", "ERCC5", "ERK2",
  "ETS1", "EZH2", "FASN", "FIBRONECTIN", "FOXM1", "FOXO3A", "FOXO3A_pS318S321",
  "G6PD", "GAB2", "GAPDH", "GATA3", "GATA6", "GCN5L2", "GSK3ALPHABETA",
  "GSK3ALPHABETA_pS21S9", "GSK3_pS9", "GYGGLYCOGENIN1", "GYS",
  "GYS_pS641", "HER2", "HER2_pY1248", "HER3", "HER3_pY1289", "HEREGULIN",
  "HIF1ALPHA", "HSP70", "IGF1R_pY1135Y1136", "IGFBP2", "INPP4B",
  "IRF1", "IRS1", "JAB1", "JAK2", "JNK2", "JNK_pT183Y185", "KEAP1",
  "KU80", "LCK", "LCN2A", "LDHA", "LDHB", "LKB1", "MACC1", "MAPK_pT202Y204",
  "MEK1", "MEK1_pS217S221", "MIG6", "MITOCHONDRIA", "MRE11", "MSH2",
  "MSH6", "MTOR", "MTOR_pS2448", "MYH11", "MYOSINIIA", "MYOSINIIA_pS1943",
  "NAPSINA", "NCADHERIN", "NDRG1_pT346", "NF2", "NFKBP65_pS536",
  "NOTCH1", "NRAS", "NRF2", "OXPHOSCOMPLEXVSUBUNITB", "P16INK4A",
  "P21", "P27", "P27_pT157", "P27_pT198", "P38MAPK", "P38_pT180Y182",
  "P53", "P62LCKLIGAND", "P63", "P70S6K1", "P70S6K_pT389", "P90RSK",
  "P90RSK_pT359S363", "PAI1", "PARP1", "PARPAB3", "PARPCLEAVED",
  "PAXILLIN", "PCADHERIN", "PCNA", "PDCD1", "PDCD4", "PDK1", "PDK1_pS241",
  "PDL1", "PEA15", "PEA15_pS116", "PI3KP110ALPHA", "PI3KP85", "PKCALPHA",
  "PKCALPHA_pS657", "PKCDELTA_pS664", "PKCPANBETAII_pS660", "PKM2",
  "PR", "PRAS40_pT246", "PRDX1", "PREX1", "PTEN", "PYGB", "PYGBAB2",
  "PYGL", "PYGM", "RAB11", "RAB25", "RAD50", "RAD51", "RAPTOR",
  "RB", "RBM15", "RB_pS807S811", "RET_pY905", "RICTOR", "RICTOR_pT1135",
  "S6", "S6_pS235S236", "S6_pS240S244", "SCD1", "SETD2", "SF2",
  "SHC_pY317", "SHP2_pY542", "SLC1A5", "SMAC", "SMAD1", "SMAD3",
  "SMAD4", "SNAIL", "SRC", "SRC_pY416", "SRC_pY527", "STAT3_pY705",
  "STAT5ALPHA", "STATHMIN", "SYK", "SYNAPTOPHYSIN", "TAZ", "TFRC",
  "THYMIDILATESYNTHASE", "TIGAR", "TRANSGLUTAMINASE", "TSC1", "TTF1",
  "TUBERIN", "TUBERIN_pT1462", "VEGFR2", "X1433BETA", "X1433EPSILON",
  "X1433ZETA", "X4EBP1", "X4EBP1_pS65", "X4EBP1_pT37T46", "X4EBP1_pT70",
  "X53BP1", "XBP1", "XRCC1", "YAP", "YAP_pS127", "YB1", "YB1_pS102"
)

#' @describeIn get_pancan_value Fetch mutation status value from pan-cancer dataset
#' @export
get_pancan_mutation_status <- function(identifier) {
  if (utils::packageVersion("UCSCXenaTools") < "1.3.2") {
    stop("You need to update 'UCSCXenaTools' (>=1.3.2).", call. = FALSE)
  }

  host <- "pancanAtlasHub"
  dataset <- "mc3.v0.2.8.PUBLIC.nonsilentGene.xena"
  report_dataset_info(dataset)

  data <- get_data(dataset, identifier, host)

  return(data)
}

#' @param gistic2 if `TRUE` (default), use GISTIC2 data.
#' @param use_thresholded_data if `TRUE`, use GISTIC2-thresholded value.
#' @describeIn get_pancan_value Fetch gene copy number value from pan-cancer dataset processed by GISTIC 2.0
#' @export
get_pancan_cn_value <- function(identifier, gistic2 = TRUE, use_thresholded_data = FALSE) {
  
  
  
  if (gistic2 & use_thresholded_data) {
    host <- "tcgaHub"
    dataset <- "TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes"
    unit <- "-2,-1,0,1,2: 2 copy del,1 copy del,no change,amplification,high-amplification"
  } else if (gistic2 & !use_thresholded_data) {
    host <- "tcgaHub"
    dataset <- "TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_data_by_genes"
    unit <- "Gistic2 copy number"
  } else if (!gistic2){
    host  <-  "pancanAtlasHub"
    dataset <- "broad.mit.edu_PANCAN_Genome_Wide_SNP_6_whitelisted.gene.xena"
    unit <- "log(tumor/normal)"
  }

  data <- get_data(dataset, identifier, host)
  report_dataset_info(dataset)
  res <- list(data = data, unit = unit)
  res
}


#' @describeIn get_pancan_value Fetch gene expression value from CCLE dataset
#' @param type methylation type, one of "450K" and "27K".
#' for function `get_pcawg_promoter_value`, it can be one of
#' "raw", "relative", "outlier".
#' @param rule_out methylation sites to rule out before analyzing.
#' @param aggr apporaches to aggregate the methylation data, default is 'NA',
#' in such case, a mean value is obtained for gene-level methylation.
#' Allowed value is one of `c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100")`.
#' Here, `Q50` is median.
#' @export
get_pancan_methylation_value <- function(identifier, type = c("450K", "27K"),
                                         rule_out = NULL,
                                         aggr = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100")) {
  type <- match.arg(type)
  aggr = match.arg(aggr)

  if (type == "450K") {
    # host <- "pancanAtlasHub"
    # dataset <- "jhu-usc.edu_PANCAN_HumanMethylation450.betaValue_whitelisted.tsv.synapse_download_5096262.xena"
    host <- "gdcHub"
    dataset <- "GDC-PANCAN.methylation450.tsv"
  } else {
    # host <- "tcgaHub"
    # dataset <- "TCGA.PANCAN.sampleMap/HumanMethylation27"
    host <- "gdcHub"
    dataset <- "GDC-PANCAN.methylation27.tsv"
  }

  data <- get_data(dataset, identifier, host, rule_out = rule_out, aggr = aggr)
  ## 去重，优先取No.16位字符为A的情况
  data = sort(data)
  names(data) = substr(names(data),1,15)
  data = data[!duplicated(names(data))]

  unit <- "beta value"
  report_dataset_info(dataset)
  res <- list(data = data, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch miRNA expression value from pan-cancer dataset
#' @export
get_pancan_miRNA_value <- function(identifier) {
  host <- "pancanAtlasHub"
  dataset <- "pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena"

  expression <- get_data(dataset, identifier, host)

  unit <- "log2(norm_value+1)"
  report_dataset_info(dataset)
  res <- list(expression = expression, unit = unit)
  res
}

# Utils -------------------------------------------------------------------

report_dataset_info <- function(dataset) {
  msg <- paste0(
    "More info about dataset please run following commands:\n",
    "  library(UCSCXenaTools)\n",
    "  XenaGenerate(subset = XenaDatasets == \"", dataset, "\") %>% XenaBrowse()"
  )
  message(msg)
}

check_file <- function(id, dataset, host, ...) {
  f <- file.path(
    get_cache_dir(),
    # paste0(
    #   host, "_",
    #   gsub("[/]", "_", dataset),
    #   "_", id, ".rds"
    # ),
    paste0(digest::digest(list(id, dataset, host, ...)), ".rds")
  )
  return(f)
}

check_exist_data <- function(id, dataset, host, ...) {
  f <- check_file(id, dataset, host, ...)
  if (file.exists(f)) {
    message("Reading cache data ", f)
    data <- tryCatch(
      readRDS(f),
      error = function(e) {
        message("Read cache data failed.")
        return(NULL)
      }
    )
    if (!is.null(data)) {
      return(list(
        ok = TRUE,
        data = data
      ))
    } else {
      return(
        list(
          ok = FALSE,
          data = NULL
        )
      )
    }
  } else {
    return(
      list(
        ok = FALSE,
        data = NULL
      )
    )
  }
}

save_data <- function(data, id, dataset, host, ...) {
  f <- check_file(id, dataset, host, ...)
  if (!dir.exists(dirname(f))) {
    dir.create(dirname(f), recursive = TRUE)
  }

  message("Saving data to file ", f)
  
  tryCatch(
    saveRDS(data, file = f),
    error = function(e) {
      message("Save data to cache directory failed.")
    }
  )
}

get_data <- function(dataset, identifier, host = NULL, ...) {
  stopifnot(length(dataset) == 1)

  if (is.null(host)) {
    host <- UCSCXenaTools::XenaData %>%
      dplyr::filter(.data$XenaDatasets == dataset) %>%
      dplyr::pull(.data$XenaHostNames)
  }
  res <- check_exist_data(identifier, dataset, host, ...)
  if (res$ok) {
    value <- res$data
  } else {
    value <- get_pancan_value(identifier, dataset = dataset, host = host, ...)
    label <- UCSCXenaTools::XenaData %>%
      dplyr::filter(.data$XenaDatasets == dataset) %>%
      dplyr::pull(.data$DataSubtype)
    if (!is.null(value)) attr(value, "label") <- label
    save_data(value, identifier, dataset, host, ...)
  }
  value
}
