# PCAWG -------------------------------------------------------------------

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
