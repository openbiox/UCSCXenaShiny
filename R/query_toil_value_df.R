#' Obtain ToilHub Info for Single Gene
#'
#' @inheritParams get_pancan_value
#' @importFrom utils data
#' @return a `tibble`
#' @export
#'
#' @examples
#' \dontrun{
#' t <- query_toil_value_df()
#' t
#' }
query_toil_value_df <- function(identifier = "TP53") {
  df <- get_pancan_gene_value(identifier = identifier)
  data("toil_info", package = "UCSCXenaShiny", envir = environment())
  toil_info <- toil_info %>%
    dplyr::left_join(dplyr::tibble(
      sample = names(df$expression),
      expression = df$expression
    ), by = "sample") %>%
    dplyr::filter(!is.na(expression)) %>%
    dplyr::rename(
      primary_site = `_primary_site`,
      sample_type = `_sample_type`,
      gender = `_gender`,
      study = `_study`
    ) %>%
    dplyr::select(sample, primary_site, detailed_category, sample_type, gender, study, expression) %>%
    dplyr::mutate(unit = df$unit) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(sample_type %in% c(
      "Primary Tumor", "Normal Tissue", "Primary Solid Tumor", "Solid Tissue Normal",
      "Metastatic", "Cell Line", "Primary Blood Derived Cancer - Peripheral Blood",
      "Primary Blood Derived Cancer - Bone Marrow", "Recurrent Blood Derived Cancer - Bone Marrow"
    )) %>%
    # Only keep samples with count > 100
    dplyr::mutate(
      sample_type = factor(sample_type, levels = c(
        "Primary Tumor", "Normal Tissue", "Primary Solid Tumor", "Solid Tissue Normal",
        "Metastatic", "Cell Line", "Primary Blood Derived Cancer - Peripheral Blood",
        "Primary Blood Derived Cancer - Bone Marrow", "Recurrent Blood Derived Cancer - Bone Marrow"
      ))
    )

  toil_info
}

utils::globalVariables(
  c(
    "_primary_site",
    "_sample_type",
    "_gender",
    "_study",
    "primary_site",
    "sample_type",
    "gender",
    "study",
    "expression",
    "detailed_category"
  )
)
