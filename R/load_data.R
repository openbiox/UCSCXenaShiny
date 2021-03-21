#' Load Dataset Provided by This Package
#' 
#' Load data from builtin or zenodo.
#'
#' @param name dataset name, can be one of
#' `r paste(sub(".rda", "", dir(system.file(c("data", "data-remote"), package = "UCSCXenaShiny"))), collapse="\n")`
#'
#' @return a dataset, typically a `data.frame`.
#' @export
#'
#' @examples
#' load_data("tcga_surv")
load_data <- function(name) {
  name2 <- paste0(name, ".rda")
  data_path <- file.path(system.file("data", package = "UCSCXenaShiny"), name2)
  if (file.exists(data_path)) {
    data(list = name, package = "UCSCXenaShiny", envir = environment())
    return(get(setdiff(ls(), c("name2", "name", "data_path", "data_url"))))
  } else {
    data_url <- file.path("https://zenodo.org/record/4625640/files", name2)
    message("Loading data from remote: ", data_url, ", please wait...")
    download.file(data_url, data_path)
    load_data(name)
  }
}
