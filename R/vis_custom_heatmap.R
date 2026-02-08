#' Parse Custom Sample Groups
#'
#' Parses a custom group definition string into a structured format.
#'
#' @param group_text Character string with group definitions. Format should be:
#'   "GroupName: sample1,sample2,sample3" with one group per line.
#' @param available_samples Optional character vector of available sample names
#'   to filter against. If provided, only samples in this vector will be included.
#'
#' @return A data frame with columns 'sample' and 'group', or NULL if parsing fails
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' group_text <- "Group1: TCGA-A1-A0SD,TCGA-A1-A0SE
#' Group2: TCGA-A1-A0SF,TCGA-A1-A0SG"
#' groups <- parse_custom_groups(group_text)
#' 
#' # With available samples filter
#' available <- c("TCGA-A1-A0SD", "TCGA-A1-A0SE", "TCGA-A1-A0SF")
#' groups <- parse_custom_groups(group_text, available_samples = available)
#' }
parse_custom_groups <- function(group_text, available_samples = NULL) {
  # Validate input
  if (is.null(group_text) || !is.character(group_text) || nchar(group_text) == 0) {
    warning("Empty or invalid group_text provided")
    return(NULL)
  }
  
  tryCatch({
    # Split by line and remove empty lines
    group_lines <- strsplit(group_text, "\n")[[1]]
    group_lines <- group_lines[nzchar(group_lines)]
    
    if (length(group_lines) == 0) {
      warning("No valid group lines found")
      return(NULL)
    }
    
    # Parse each line
    group_list <- list()
    for (line in group_lines) {
      if (grepl(":", line)) {
        parts <- strsplit(line, ":")[[1]]
        if (length(parts) >= 2) {
          group_name <- trimws(parts[1])
          samples <- trimws(strsplit(parts[2], ",")[[1]])
          
          # Filter to available samples if provided
          if (!is.null(available_samples)) {
            samples <- samples[samples %in% available_samples]
          }
          
          if (length(samples) > 0) {
            group_list[[group_name]] <- samples
          }
        }
      }
    }
    
    if (length(group_list) == 0) {
      warning("No valid groups found after parsing")
      return(NULL)
    }
    
    # Convert to data frame
    group_df <- purrr::map2_dfr(group_list, names(group_list), function(samples, group_name) {
      data.frame(sample = samples, group = group_name, stringsAsFactors = FALSE)
    })
    
    return(group_df)
    
  }, error = function(e) {
    warning("Error parsing custom groups: ", e$message)
    return(NULL)
  })
}


#' Prepare Heatmap Data
#'
#' Prepares molecular data in long format for heatmap visualization,
#' optionally merging with sample group information.
#'
#' @param data A data frame in long format with columns: sample, gene (or feature), value
#' @param groups Optional data frame with columns 'sample' and 'group' for sample grouping
#' @param max_features Maximum number of features to include (default: 500)
#'
#' @return A data frame in long format ready for heatmap generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic data preparation
#' data <- data.frame(
#'   sample = rep(c("S1", "S2", "S3"), each = 3),
#'   gene = rep(c("TP53", "KRAS", "PTEN"), 3),
#'   value = rnorm(9)
#' )
#' prepared <- prepare_heatmap_data(data)
#' 
#' # With grouping
#' groups <- data.frame(
#'   sample = c("S1", "S2", "S3"),
#'   group = c("A", "A", "B")
#' )
#' prepared <- prepare_heatmap_data(data, groups = groups)
#' }
prepare_heatmap_data <- function(data, groups = NULL, max_features = 500) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  required_cols <- c("sample", "gene", "value")
  if (!all(required_cols %in% colnames(data))) {
    stop("data must have columns: sample, gene, value")
  }
  
  # Remove NA values
  data <- data %>% dplyr::filter(!is.na(value))
  
  if (nrow(data) == 0) {
    stop("No valid data after removing NA values")
  }
  
  # Limit features if needed
  unique_features <- unique(data$gene)
  if (length(unique_features) > max_features) {
    features_to_keep <- unique_features[1:max_features]
    data <- data %>% dplyr::filter(gene %in% features_to_keep)
    message("Limited to ", max_features, " features")
  }
  
  # Merge with groups if provided
  if (!is.null(groups)) {
    if (!is.data.frame(groups)) {
      stop("groups must be a data frame")
    }
    if (!all(c("sample", "group") %in% colnames(groups))) {
      stop("groups must have columns: sample, group")
    }
    
    data <- data %>%
      dplyr::left_join(groups, by = "sample") %>%
      dplyr::mutate(group = ifelse(is.na(group), "Ungrouped", group))
  }
  
  return(data)
}


#' Generate Custom Grouped Heatmap
#'
#' Creates a heatmap visualization using tidyHeatmap with optional sample grouping.
#'
#' @param data A data frame in long format with columns: sample, gene, value, and optionally group
#' @param cluster_rows Logical, whether to cluster rows (features) (default: TRUE)
#' @param cluster_columns Logical, whether to cluster columns (samples) (default: TRUE)
#' @param clustering_method Clustering method: "complete", "average", "single", etc. (default: "complete")
#' @param show_row_names Logical, whether to show feature names (default: TRUE)
#' @param show_column_names Logical, whether to show sample names (default: FALSE)
#' @param color_palette Color palette name: "viridis", "plasma", "RdYlBu", etc. (default: "RdYlBu")
#' @param scale Scaling method: "none", "row", "column" (default: "row")
#'
#' @return A heatmap object (tidyHeatmap/ComplexHeatmap)
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic heatmap
#' data <- data.frame(
#'   sample = rep(paste0("S", 1:10), each = 5),
#'   gene = rep(paste0("Gene", 1:5), 10),
#'   value = rnorm(50)
#' )
#' hm <- generate_custom_heatmap(data)
#' 
#' # With grouping
#' data$group <- rep(c("A", "B"), each = 25)
#' hm <- generate_custom_heatmap(data, color_palette = "viridis")
#' }
generate_custom_heatmap <- function(data,
                                   cluster_rows = TRUE,
                                   cluster_columns = TRUE,
                                   clustering_method = "complete",
                                   show_row_names = TRUE,
                                   show_column_names = FALSE,
                                   color_palette = "RdYlBu",
                                   scale = "row") {
  # Check tidyHeatmap availability
  if (!requireNamespace("tidyHeatmap", quietly = TRUE)) {
    stop("tidyHeatmap package is required but not installed. Install with: install.packages('tidyHeatmap')")
  }
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  required_cols <- c("sample", "gene", "value")
  if (!all(required_cols %in% colnames(data))) {
    stop("data must have columns: sample, gene, value")
  }
  
  if (nrow(data) == 0) {
    stop("data is empty")
  }
  
  # Check if grouping is present
  has_groups <- "group" %in% colnames(data)
  
  tryCatch({
    # Create base heatmap
    p <- data %>%
      tidyHeatmap::heatmap(
        .row = gene,
        .column = sample,
        .value = value,
        scale = scale,
        clustering_method = clustering_method,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        show_row_names = show_row_names,
        show_column_names = show_column_names,
        palette_value = color_palette
      )
    
    # Add group annotation if groups are defined
    if (has_groups) {
      p <- p %>% tidyHeatmap::annotation_tile(group)
    }
    
    return(p)
    
  }, error = function(e) {
    stop("Error generating heatmap: ", e$message)
  })
}


#' Create Custom Grouped Heatmap (Wrapper Function)
#'
#' High-level wrapper function that combines data preparation and heatmap generation.
#' This is the main entry point for creating custom grouped heatmaps.
#'
#' @param data A data frame in long format with columns: sample, gene (or feature), value
#' @param groups Optional data frame with columns 'sample' and 'group', or character string
#'   with group definitions (format: "GroupName: sample1,sample2")
#' @param max_features Maximum number of features to include (default: 500)
#' @param cluster_rows Logical, whether to cluster rows (default: TRUE)
#' @param cluster_columns Logical, whether to cluster columns (default: TRUE)
#' @param clustering_method Clustering method (default: "complete")
#' @param show_row_names Logical, show feature names (default: TRUE)
#' @param show_column_names Logical, show sample names (default: FALSE)
#' @param color_palette Color palette (default: "RdYlBu")
#' @param scale Scaling method (default: "row")
#'
#' @return A heatmap object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   sample = rep(paste0("Sample", 1:20), each = 10),
#'   gene = rep(paste0("Gene", 1:10), 20),
#'   value = rnorm(200, mean = 5, sd = 2)
#' )
#' 
#' # Basic heatmap without grouping
#' hm <- vis_custom_heatmap(data)
#' 
#' # With custom groups as string
#' group_text <- "Group1: Sample1,Sample2,Sample3,Sample4,Sample5
#' Group2: Sample6,Sample7,Sample8,Sample9,Sample10"
#' hm <- vis_custom_heatmap(data, groups = group_text)
#' 
#' # With groups as data frame
#' groups_df <- data.frame(
#'   sample = paste0("Sample", 1:20),
#'   group = rep(c("A", "B", "C", "D"), each = 5)
#' )
#' hm <- vis_custom_heatmap(data, groups = groups_df, color_palette = "viridis")
#' }
vis_custom_heatmap <- function(data,
                              groups = NULL,
                              max_features = 500,
                              cluster_rows = TRUE,
                              cluster_columns = TRUE,
                              clustering_method = "complete",
                              show_row_names = TRUE,
                              show_column_names = FALSE,
                              color_palette = "RdYlBu",
                              scale = "row") {
  
  # Parse groups if provided as character string
  if (!is.null(groups) && is.character(groups)) {
    available_samples <- unique(data$sample)
    groups <- parse_custom_groups(groups, available_samples = available_samples)
  }
  
  # Prepare data
  prepared_data <- prepare_heatmap_data(data, groups = groups, max_features = max_features)
  
  # Generate heatmap
  heatmap <- generate_custom_heatmap(
    data = prepared_data,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    clustering_method = clustering_method,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    color_palette = color_palette,
    scale = scale
  )
  
  return(heatmap)
}
