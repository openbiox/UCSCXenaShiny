# PCAWG -------------------------------------------------------------------

# Not all gene data available
# query_pancan_value("KRAS", database = "pcawg")
# query_pancan_value("hsa-let-7c-3p", database = "pcawg", data_type = "miRNA")
# query_pancan_value("hsa-let-7c-3p", database = "pcawg", data_type = "miRNA", norm_method = "UQ")
# query_pancan_value("ENSG00000000419", database = "pcawg", data_type = "fusion") # gene symbol also work
# query_pancan_value("tCa_MutLoad_MinEstimate", database = "pcawg", data_type = "APOBEC")

#' @describeIn get_pancan_value Fetch specimen-level gene expression value from PCAWG cohort
#' @export
get_pcawg_gene_value <- function(identifier) {
  host <- "pcawgHub"
  dataset <- "tophat_star_fpkm_uq.v2_aliquot_gl.sp.log"
  
  expression <- get_data(dataset, identifier, host)
  unit <- "log2(fpkm-uq+0.001)"
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch specimen-level gene fusion value from PCAWG cohort
#' @export
get_pcawg_fusion_value <- function(identifier) {
  host <- "pcawgHub"
  dataset <- "pcawg3_fusions_PKU_EBI.gene_centric.sp.xena"
  
  expression <- get_data(dataset, identifier, host)
  unit <- "binary fusion call, 1 fusion, 0 otherwise"
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch specimen-level miRNA value from PCAWG cohort
#' @export
get_pcawg_miRNA_value <- function(identifier, norm_method = c("TMM", "UQ")) {
  host <- "pcawgHub"
  norm_method <- match.arg(norm_method)
  
  if (norm_method == "TMM") {
    dataset <- "x3t2m1.mature.TMM.mirna.matrix.log"
    unit <- "log2(cpm-TMM+0.1)"
  } else {
   dataset <- "x3t2m1.mature.UQ.mirna.matrix.log" 
   unit <- "log2(cpm-uq+0.1)"
  }
  
  expression <- get_data(dataset, identifier, host)
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}


#' @describeIn get_pancan_value Fetch specimen-level gene fusion value from PCAWG cohort
#' @export
get_pcawg_APOBEC_mutagenesis_value <- function(identifier = c(
  "tCa_MutLoad_MinEstimate", "APOBECtCa_enrich",
  "A3A_or_A3B", "APOBEC_tCa_enrich_quartile", "APOBECrtCa_enrich", 
  "APOBECytCa_enrich", "APOBECytCa_enrich-APOBECrtCa_enrich", 
  "BH_Fisher_p-value_tCa", "ntca+tgan", "rtCa_to_G+rtCa_to_T", 
  "rtca+tgay",  "tCa_to_G+tCa_to_T", 
  "ytCa_rtCa_BH_Fisher_p-value", "ytCa_rtCa_Fisher_p-value", "ytCa_to_G+ytCa_to_T", 
  "ytca+tgar")) {
  
  identifier <- match.arg(identifier)
  host <- "pcawgHub"
  dataset <- "MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp"
  
  data <- get_data(dataset, identifier, host)
  report_dataset_info(dataset)
  data
}

## 突变数据直接用表格展示??
