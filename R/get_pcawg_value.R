#' @describeIn get_pancan_value Fetch specimen-level gene expression value from PCAWG cohort
#' @export
get_pcawg_gene_value <- function(identifier) {
  host <- "pcawgHub"
  dataset <- "tophat_star_fpkm_uq.v2_aliquot_gl.sp.log"

  expression <- get_data(dataset, identifier, host, use_probeMap = FALSE)
  unit <- "log2(fpkm-uq+0.001)"
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch specimen-level miRNA expression value from PCAWG cohort
#' @export
get_pcawg_miRNA_value <- function(identifier, norm = c("TMM", "UQ")) {
  host <- "pcawgHub"
  norm <- match.arg(norm)

  if (norm == "TMM") {
    dataset <- "x3t2m1.mature.TMM.mirna.matrix.log"
    unit <- "log2(cpm-TMM+0.1)"
  } else {
    dataset <- "x3t2m1.mature.UQ.mirna.matrix.log"
    unit <- "log2(cpm-UQ+0.1)"
  }

  expression <- get_data(dataset, identifier, host, use_probeMap = FALSE)
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch specimen-level gene fusion value from PCAWG cohort
#' @export
get_pcawg_fusion_value <- function(identifier) {
  host <- "pcawgHub"
  dataset <- "pcawg3_fusions_PKU_EBI.gene_centric.sp.xena"

  expression <- get_data(dataset, identifier, host, use_probeMap = FALSE)
  unit <- "fusion status (1: detected, 0: not detected)"
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch specimen-level gene promoter activity value from PCAWG cohort
#' @export
get_pcawg_promoter_value <- function(identifier, type = c("relative", "raw", "outlier")) {
  host_name <- "pcawgHub"
  host_url <- "https://pcawg.xenahubs.net"
  type <- match.arg(type)

  if (type == "raw") {
    dataset <- "rawPromoterActivity.sp"
    unit <- "raw promoter activity"
  } else if (type == "relative") {
    dataset <- "relativePromoterActivity.sp"
    unit <- "portion of transcription activity of the gene driven by the promoter"
  } else {
    dataset <- "promoterCentricTable_0.2_1.0.sp"
    unit <- "-1 (low expression), 0 (normal), 1 (high expression)"
  }

  res <- check_exist_data(identifier, dataset, host_name)
  if (res$ok) {
    expression <- res$data
  } else {
    if (any(!startsWith(identifier, "prmtr"))) {
      map <- load_data("pcawg_promoter_id")
      id_map <- map[names(map) %in% identifier]
      ids_to_query <- unique(c(as.character(id_map), identifier[startsWith(identifier, "prmtr")]))
    } else {
      ids_to_query <- identifier
    }

    if (length(ids_to_query) == 0) {
      return(NULL)
    }

    # Use URL for direct fetch
    query_list <- UCSCXenaTools::fetch_dense_values(host_url, dataset, ids_to_query, use_probeMap = FALSE)
    expression <- as.data.frame(t(query_list))
    if (length(ids_to_query) == 1) {
      expression <- expression %>%
        tibble::rownames_to_column("sampleID") %>%
        dplyr::rename(data = 2)
    }
    save_data(expression, identifier, dataset, host_name)
  }

  report_dataset_info(dataset)
  if (is.data.frame(expression) && "data" %in% colnames(expression)) {
    res_data <- expression$data
    names(res_data) <- expression$sampleID
    res <- list(data = res_data, unit = unit)
  } else {
    res <- list(data = expression, unit = unit)
  }
  res
}

#' @describeIn get_pancan_value Fetch gene fusion value from PCAWG cohort
#' @export
get_pcawg_APOBEC_mutagenesis_value <- function(identifier = c(
                                                 "tCa_MutLoad_MinEstimate", "APOBECtCa_enrich",
                                                 "A3A_or_A3B", "APOBEC_tCa_enrich_quartile", "APOBECrtCa_enrich",
                                                 "APOBECytCa_enrich", "APOBECytCa_enrich-APOBECrtCa_enrich",
                                                 "BH_Fisher_p-value_tCa", "ntca+tgan", "rtCa_to_G+rtCa_to_T",
                                                 "rtca+tgay", "tCa_to_G+tCa_to_T",
                                                 "ytCa_rtCa_BH_Fisher_p-value", "ytCa_rtCa_Fisher_p-value", "ytCa_to_G+ytCa_to_T",
                                                 "ytca+tgar"
                                               )) {
  identifier <- match.arg(identifier)
  host <- "pcawgHub"
  dataset <- "MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp"

  expression <- get_data(dataset, identifier, host, use_probeMap = FALSE)
  unit <- ""
  report_dataset_info(dataset)
  res <- list(data = expression, unit = unit)
  res
}

#' @describeIn get_pancan_value Fetch gene mutation info from PCAWG cohort
#' @export
get_pcawg_mutation_status <- function(identifier) {
  if (utils::packageVersion("UCSCXenaTools") < "1.3.2") {
    stop("You need to update 'UCSCXenaTools' (>=1.3.2).", call. = FALSE)
  }

  host_name <- "pcawgHub"
  host_url <- "https://pcawg.xenahubs.net"
  dataset <- "October_2016_whitelist_2583.snv_mnv_indel.maf.coding.xena"
  report_dataset_info(dataset)

  res <- check_exist_data(identifier, dataset, host_name)
  if (res$ok) {
    data <- res$data
  } else {
    # Use URL for direct fetch
    query_list <- UCSCXenaTools::fetch_sparse_values(host_url, dataset, identifier)
    data <- as.data.frame(query_list$rows)
    data <- dplyr::full_join(
      dplyr::tibble(
        sampleID = query_list$samples
      ),
      data,
      by = "sampleID"
    )
    save_data(data, identifier, dataset, host_name)
  }

  report_dataset_info(dataset)
  data
}

#' @describeIn get_pancan_value Fetch gene copy number value from PCAWG cohort
#' @export
get_pcawg_cn_value <- function(identifier) {
  host_name <- "pcawgHub"
  host_url <- "https://pcawg.xenahubs.net"
  dataset <- "20170119_final_consensus_copynumber_sp"

  res <- check_exist_data(identifier, dataset, host_name)
  if (res$ok) {
    data <- res$data
  } else {
    # Use URL for direct fetch
    query_list <- UCSCXenaTools::fetch_dense_values(host_url, dataset, identifier, use_probeMap = FALSE)
    data <- as.data.frame(t(query_list))
    data <- data %>%
      tibble::rownames_to_column("sampleID")
    if (length(identifier) == 1) {
      colnames(data)[2] <- "data"
    }
    save_data(data, identifier, dataset, host_name)
  }

  report_dataset_info(dataset)
  if (length(identifier) == 1) {
    res_data <- data$data
    names(res_data) <- data$sampleID
    res <- list(data = res_data, unit = "")
  } else {
    res <- list(data = data, unit = "")
  }
  res
}
