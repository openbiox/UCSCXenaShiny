#' Load Dataset Provided by This Package
#'
#' Load data from builtin or Zenodo.
#' Option `xena.zenodoDir` can be used to set default path for storing
#' extra data from Zenodo, e.g., `options(xena.zenodoDir = "/home/xxx/dataset")`.
#'
#' @param name dataset name, can be one of
#' `r paste(sub(".rda", "", unique(dir(system.file(c("data", "data-remote"), package = "UCSCXenaShiny")))), collapse="\n")`
#' @return a dataset, typically a `data.frame`.
#' @export
#'
#' @examples
#' load_data("tcga_surv")
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
      data_url <- file.path("https://zenodo.org/record/4751161/files", name2)
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
