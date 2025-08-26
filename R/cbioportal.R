#' cBioPortal Data Access Functions
#'
#' Functions to access and process data from cBioPortal
#'
#' @name cbioportal
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
NULL

#' Get available studies from cBioPortal
#'
#' @return data.frame with study information
#' @export
get_cbioportal_studies <- function() {
  if (!requireNamespace("cBioPortalData", quietly = TRUE)) {
    stop("Package 'cBioPortalData' is required for cBioPortal functionality")
  }
  
  tryCatch({
    cbio <- cBioPortalData::cBioPortal()
    studies <- cBioPortalData::getStudies(cbio)
    return(studies)
  }, error = function(e) {
    warning("Failed to connect to cBioPortal: ", e$message)
    return(data.frame())
  })
}

#' Get study data from cBioPortal
#'
#' @param study_id Character, the study identifier
#' @return MultiAssayExperiment object with study data
#' @export
get_cbioportal_study_data <- function(study_id) {
  if (!requireNamespace("cBioPortalData", quietly = TRUE)) {
    stop("Package 'cBioPortalData' is required for cBioPortal functionality")
  }
  
  tryCatch({
    study_data <- cBioPortalData::cBioDataPack(study_id)
    return(study_data)
  }, error = function(e) {
    warning("Failed to fetch study data for ", study_id, ": ", e$message)
    return(NULL)
  })
}

#' Extract molecular data from cBioPortal study
#'
#' @param study_data MultiAssayExperiment object from cBioPortal
#' @param data_type Character, type of molecular data to extract
#' @return data.frame with molecular data in long format
#' @export
extract_cbioportal_molecular_data <- function(study_data, data_type = "geneExp") {
  if (is.null(study_data)) {
    return(data.frame())
  }
  
  tryCatch({
    experiments <- MultiAssayExperiment::experiments(study_data)
    
    # Find matching experiment
    exp_names <- names(experiments)
    matching_exp <- grep(data_type, exp_names, ignore.case = TRUE, value = TRUE)
    
    if (length(matching_exp) == 0) {
      warning("No matching experiment found for data type: ", data_type)
      return(data.frame())
    }
    
    # Extract data from the first matching experiment
    exp_data <- experiments[[matching_exp[1]]]
    
    # Convert to data.frame
    if (is(exp_data, "SummarizedExperiment")) {
      assay_data <- SummarizedExperiment::assay(exp_data)
      
      # Convert to long format
      df <- as.data.frame(assay_data) %>%
        tibble::rownames_to_column("id") %>%
        tidyr::pivot_longer(cols = -"id", names_to = "Sample", values_to = "value")
      
      return(df)
    }
    
    return(data.frame())
  }, error = function(e) {
    warning("Failed to extract molecular data: ", e$message)
    return(data.frame())
  })
}

#' Extract clinical data from cBioPortal study
#'
#' @param study_data MultiAssayExperiment object from cBioPortal
#' @return data.frame with clinical data
#' @export
extract_cbioportal_clinical_data <- function(study_data) {
  if (is.null(study_data)) {
    return(data.frame())
  }
  
  tryCatch({
    clinical_data <- MultiAssayExperiment::colData(study_data)
    df <- as.data.frame(clinical_data) %>%
      tibble::rownames_to_column("Sample")
    return(df)
  }, error = function(e) {
    warning("Failed to extract clinical data: ", e$message)
    return(data.frame())
  })
}

#' Get available data types for a study
#'
#' @param study_data MultiAssayExperiment object from cBioPortal
#' @return character vector of available data types
#' @export
get_cbioportal_data_types <- function(study_data) {
  if (is.null(study_data)) {
    return(character(0))
  }
  
  tryCatch({
    experiments <- MultiAssayExperiment::experiments(study_data)
    return(names(experiments))
  }, error = function(e) {
    warning("Failed to get data types: ", e$message)
    return(character(0))
  })
}

#' Get cBioPortal gene expression value
#'
#' @param identifier Character, gene symbol
#' @param study_id Character, cBioPortal study identifier  
#' @return list with expression data and unit
#' @export
get_cbioportal_gene_value <- function(identifier, study_id) {
  if (!requireNamespace("cBioPortalData", quietly = TRUE)) {
    stop("Package 'cBioPortalData' is required for cBioPortal functionality")
  }
  
  tryCatch({
    # Get study data
    study_data <- get_cbioportal_study_data(study_id)
    if (is.null(study_data)) {
      return(list(data = data.frame(), unit = ""))
    }
    
    # Extract gene expression data
    molecular_data <- extract_cbioportal_molecular_data(study_data, "geneExp")
    
    # Filter for specific gene
    gene_data <- molecular_data[molecular_data$id == identifier, ]
    
    unit <- "mRNA expression (log2)"
    return(list(expression = gene_data, unit = unit))
  }, error = function(e) {
    warning("Failed to get cBioPortal gene value for ", identifier, ": ", e$message)
    return(list(expression = data.frame(), unit = ""))
  })
}