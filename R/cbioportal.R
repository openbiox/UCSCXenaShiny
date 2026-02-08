#' cBioPortal Data Access Functions
#'
#' Functions to access and process data from cBioPortal using cbioportalR
#'
#' @name cbioportal
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
NULL

#' Get available studies from cBioPortal
#'
#' @param base_url The database URL to query. If NULL, uses "public" (www.cbioportal.org)
#' @return data.frame with study information
#' @export
get_cbioportal_studies <- function(base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality. Install with: install.packages('cbioportalR')")
  }
  
  tryCatch({
    # Use public instance if no URL specified
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    # Initialize the cBioPortal database connection
    # This is required before making API calls
    suppressMessages({
      cbioportalR::set_cbioportal_db(db = base_url)
    })
    
    # Get studies from cBioPortal
    studies <- cbioportalR::available_studies(base_url = base_url)
    
    # Convert to base data.frame for compatibility
    return(as.data.frame(studies))
  }, error = function(e) {
    warning("Failed to connect to cBioPortal: ", e$message)
    return(data.frame())
  })
}

#' Get study metadata from cBioPortal
#'
#' @param study_id Character, the study identifier
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return data.frame with study metadata
#' @export
get_cbioportal_study_info <- function(study_id, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    study_info <- cbioportalR::get_study_info(study_id = study_id, base_url = base_url)
    return(as.data.frame(study_info))
  }, error = function(e) {
    warning("Failed to fetch study info for ", study_id, ": ", e$message)
    return(data.frame())
  })
}

#' Get available molecular profiles for a study
#'
#' @param study_id Character, the study identifier
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return data.frame with available molecular profiles
#' @export
get_cbioportal_profiles <- function(study_id, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    profiles <- cbioportalR::available_profiles(study_id = study_id, base_url = base_url)
    return(as.data.frame(profiles))
  }, error = function(e) {
    warning("Failed to fetch profiles for ", study_id, ": ", e$message)
    return(data.frame())
  })
}

#' Get molecular data for genes in a study
#'
#' @param study_id Character, the study identifier
#' @param genes Character vector of gene symbols
#' @param molecular_profile_id Character, molecular profile ID. If NULL, tries to guess from study_id
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return data.frame with molecular data in long format
#' @export
get_cbioportal_molecular_data <- function(study_id, genes, molecular_profile_id = NULL, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    # If no molecular profile specified, try to find RNA seq profile
    if (is.null(molecular_profile_id)) {
      profiles <- get_cbioportal_profiles(study_id, base_url)
      
      # Look for RNA expression profiles
      rna_profiles <- profiles[grepl("rna|mrna|expression", profiles$name, ignore.case = TRUE) & 
                               profiles$datatype %in% c("Z-SCORE", "CONTINUOUS"), ]
      
      if (nrow(rna_profiles) == 0) {
        warning("No RNA expression profile found for study: ", study_id)
        return(data.frame())
      }
      
      molecular_profile_id <- rna_profiles$molecularProfileId[1]
    }
    
    # Get sample list for the study
    sample_list_id <- paste0(study_id, "_all")
    
    # Build API query URL
    url_path <- paste0(
      "molecular-profiles/", molecular_profile_id,
      "/molecular-data?sampleListId=", sample_list_id
    )
    
    # Add genes as query parameters
    if (!is.null(genes) && length(genes) > 0) {
      # Need to get entrez IDs for genes
      gene_info <- cbioportalR::get_entrez_id(hugo_symbol = genes, base_url = base_url)
      entrez_ids <- gene_info$entrezGeneId
      
      if (length(entrez_ids) > 0) {
        url_path <- paste0(url_path, "&entrezGeneId=", paste(entrez_ids, collapse = "&entrezGeneId="))
      }
    }
    
    # Query the API
    result <- cbioportalR::cbp_api(url_path = url_path, base_url = base_url)
    
    # Parse results
    if (length(result$content) > 0) {
      df <- purrr::map_dfr(result$content, ~ tibble::as_tibble(.x))
      
      # Add hugo symbols if not present
      if (!"hugoGeneSymbol" %in% names(df) && "entrezGeneId" %in% names(df)) {
        gene_mapping <- cbioportalR::get_hugo_symbol(
          entrez_id = unique(df$entrezGeneId), 
          base_url = base_url
        )
        df <- merge(df, gene_mapping[, c("entrezGeneId", "hugoGeneSymbol")], 
                   by = "entrezGeneId", all.x = TRUE)
      }
      
      # Rename columns for consistency
      if ("hugoGeneSymbol" %in% names(df)) {
        names(df)[names(df) == "hugoGeneSymbol"] <- "gene"
      }
      if ("sampleId" %in% names(df)) {
        names(df)[names(df) == "sampleId"] <- "sample"
      }
      
      return(as.data.frame(df))
    }
    
    return(data.frame())
  }, error = function(e) {
    warning("Failed to fetch molecular data for ", study_id, ": ", e$message)
    return(data.frame())
  })
}

#' Extract clinical data from cBioPortal study
#'
#' @param study_id Character, the study identifier
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return data.frame with clinical data
#' @export
get_cbioportal_clinical_data <- function(study_id, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    clinical_data <- cbioportalR::get_clinical_by_study(
      study_id = study_id, 
      base_url = base_url
    )
    
    return(as.data.frame(clinical_data))
  }, error = function(e) {
    warning("Failed to extract clinical data for ", study_id, ": ", e$message)
    return(data.frame())
  })
}

#' Get gene expression value for a specific gene in a study
#'
#' @param identifier Character, gene symbol
#' @param study_id Character, cBioPortal study identifier  
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return list with expression data and unit
#' @export
get_cbioportal_gene_value <- function(identifier, study_id, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    # Get molecular data for the gene
    molecular_data <- get_cbioportal_molecular_data(
      study_id = study_id,
      genes = identifier,
      base_url = base_url
    )
    
    # Filter for specific gene if column exists
    if (nrow(molecular_data) > 0 && "gene" %in% names(molecular_data)) {
      gene_data <- molecular_data[molecular_data$gene == identifier, ]
    } else {
      gene_data <- molecular_data
    }
    
    # Determine unit based on profile type
    profiles <- get_cbioportal_profiles(study_id, base_url)
    rna_profile <- profiles[grepl("rna|expression", profiles$name, ignore.case = TRUE), ][1, ]
    
    unit <- if (grepl("z-score", rna_profile$name, ignore.case = TRUE)) {
      "mRNA expression z-scores"
    } else {
      "mRNA expression"
    }
    
    return(list(expression = gene_data, unit = unit))
  }, error = function(e) {
    warning("Failed to get cBioPortal gene value for ", identifier, ": ", e$message)
    return(list(expression = data.frame(), unit = ""))
  })
}

#' Get correlation data for two genes in a study
#'
#' @param gene_x Character, first gene symbol
#' @param gene_y Character, second gene symbol
#' @param study_id Character, cBioPortal study identifier
#' @param base_url The database URL to query. If NULL, uses "public"
#' @return list with correlation data and statistics
#' @export
get_cbioportal_gene_correlation <- function(gene_x, gene_y, study_id, base_url = NULL) {
  if (!requireNamespace("cbioportalR", quietly = TRUE)) {
    stop("Package 'cbioportalR' is required for cBioPortal functionality")
  }
  
  tryCatch({
    if (is.null(base_url)) {
      base_url <- "public"
    }
    
    # Get molecular data for both genes
    molecular_data <- get_cbioportal_molecular_data(
      study_id = study_id,
      genes = c(gene_x, gene_y),
      base_url = base_url
    )
    
    if (nrow(molecular_data) == 0 || !"gene" %in% names(molecular_data)) {
      warning("No data found for genes: ", paste(gene_x, gene_y, sep = ", "))
      return(list(data = data.frame(), correlation = NULL))
    }
    
    # Filter data for each gene
    gene_x_data <- molecular_data[molecular_data$gene == gene_x, ]
    gene_y_data <- molecular_data[molecular_data$gene == gene_y, ]
    
    if (nrow(gene_x_data) == 0) {
      warning("Gene ", gene_x, " not found in the data")
      return(list(data = data.frame(), correlation = NULL))
    }
    
    if (nrow(gene_y_data) == 0) {
      warning("Gene ", gene_y, " not found in the data")
      return(list(data = data.frame(), correlation = NULL))
    }
    
    # Merge data for correlation
    correlation_data <- merge(
      gene_x_data[, c("sample", "value")],
      gene_y_data[, c("sample", "value")],
      by = "sample",
      suffixes = c("_x", "_y")
    )
    
    # Remove NA values
    correlation_data <- correlation_data[complete.cases(correlation_data), ]
    
    if (nrow(correlation_data) < 3) {
      warning("Insufficient data points for correlation analysis")
      return(list(data = correlation_data, correlation = NULL))
    }
    
    # Calculate correlation
    cor_result <- cor.test(correlation_data$value_x, correlation_data$value_y)
    
    return(list(
      data = correlation_data,
      correlation = cor_result,
      gene_x = gene_x,
      gene_y = gene_y
    ))
  }, error = function(e) {
    warning("Failed to calculate correlation: ", e$message)
    return(list(data = data.frame(), correlation = NULL))
  })
}
