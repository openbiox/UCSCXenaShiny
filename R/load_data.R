#' Load Dataset Provided by This Package
#'
#' Load data from builtin or Zenodo.
#' Option `xena.zenodoDir` can be used to set default path for storing
#' extra data from Zenodo, e.g., `options(xena.zenodoDir = "/home/xxx/dataset")`.
#'
#' @param name a dataset name. Could be one of
#' 
#' **Builtin datasets**:
#'   - `ccle_absolute`: CCLE ABSOLUTE result.
#'   - `ccle_info`: CCLE information.
#'   - `pcawg_info`: PCAWG information.
#'   - `pcawg_purity`: PCAWG tumor purity, ploidy and WGD data.
#'   - `tcga_clinical`: TCGA clinical data.
#'   - `tcga_genome_instability`: TCGA genome instability data.
#'   - `tcga_gtex`: TCGA and GTEX sample info.
#'   - `tcga_purity`: TCGA tumor purity data.
#'   - `tcga_subtypes`: TCGA subtypes data.
#'   - `tcga_surv`: TCGA survival data.
#'   - `TCGA.organ`: TCGA organ data.
#'   - `toil_info`: Toil hub information.
#'
#' **Remote datasets stored in [Zenodo](https://zenodo.org/record/5548587/)**:
#'   - `pcawg_promoter_id`: PCAWG promoter identifiers.
#'   - `transcript_identifier`: Common transcript identifiers.
#'   - `ccle_expr_and_drug_response`: CCLE expression and drug response data.
#'   - `ccle_drug_response_extend`: CCLE drug response extended data.
#'   - `pancan_MSI`: Pan-cancer MSI data.
#'   - `tcga_chr_alteration`: TCGA chromosome alteration data.
#'   - `tcga_MSI`: TCGA MSI data.
#'   - `tcga_pan_immune_signature`: TCGA pan-cancer immune signature.
#'   - `tcga_stemness`: TCGA tumor stemness data.
#'   - `tcga_TIL`: TCGA TIL data.
#'   - `tcga_tmb`: TCGA TMB data.
#'   - `tcga_armcalls`: TCGA arm alteration calls and Aneuploidy data.
#'   - `tcga_dna_repair`: TCGA DNA repair data.
#'   - `pancancer_conserved_immune_subtype`: Pan-cancer conserved immune subtypes.
#'   
#' @return a dataset, typically a `data.frame`.
#' @export
#'
#' @examples
#' data1 <- load_data("tcga_surv")
#' data1
#' \donttest{
#' data2 <- load_data("tcga_armcalls")
#' data2
#' }
load_data <- function(name) {
  stopifnot(length(name) == 1)
  name2 <- paste0(name, ".rda")
  data_path <- file.path(get_zenodo_dir(), name2)

  # builtin datasets
  available_datasets <- c(
    "ccle_absolute", "ccle_info",
    "tcga_clinical", "tcga_genome_instability",
    "tcga_gtex", "tcga_purity",
    "tcga_subtypes", "tcga_surv", "TCGA.organ",
    "toil_info",
    "pcawg_info", "pcawg_purity"
  )
  if (name %in% available_datasets) {
    # The data is builtin
    data(list = name, package = "UCSCXenaShiny", envir = environment())
  } else {
    if (!file.exists(data_path)) {
      # Download it to inst/extdata from zenodo
      # Then load it
      data_url <- file.path("https://zenodo.org/record/5548587/files", name2)
      message("Loading data from remote: ", data_url, ", please wait...")
      name <- FALSE
      tryCatch(
        {
          download.file(data_url, data_path)
          message("Data has been saved to ", data_path)
        },
        error = function(e) {
          message("Data load failed, please check your input and the internet.\n NULL will be returned.")
          if (file.exists(data_path)) unlink(data_path, recursive = TRUE, force = TRUE)
          name <<- TRUE
        }
      )
      if (isTRUE(name)) {
        return(invisible(NULL))
      }
    }
    tryCatch(
      load(data_path, envir = environment()),
      error = function(e) {
        message("Data load failed, probably due to broken download file, please try again.\n This time NULL will be returned.")
        if (file.exists(data_path)) unlink(data_path, recursive = TRUE, force = TRUE)
        name <<- TRUE
      }
    )
    if (isTRUE(name)) {
      return(invisible(NULL))
    }
  }

  return(get(setdiff(ls(), c("name2", "name", "data_path", "data_url", "available_datasets"))))
}
