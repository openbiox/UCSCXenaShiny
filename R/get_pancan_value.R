#' Show available hosts
#'
#' @return hosts
#' @export
#'
#' @examples
#' available_hosts()
available_hosts <- function() {
  .xena_hosts <- "UCSCXenaTools" %:::% ".xena_hosts"
  .xena_hosts %>% as.character()
}


#' Fetch identifier value from pan-cancer dataset
#'
#' Identifier includes gene/probe etc.
#'
#' @param identifier a length-1 character representing a gene symbol, ensembl gene id, or probe id.
#' Gene symbol is highly recommended.
#' @param subtype a length-1 chracter representing a regular expression for matching
#' `DataSubtype` column of [UCSCXenaTools::XenaData].
#' @param dataset a length-1 chracter representing a regular expression for matching
#' `XenaDatasets` of [UCSCXenaTools::XenaData].
#' @param host a length-1 character representing host name, e.g. "toilHub".
#' @param samples a character vector representing samples want to be returned.
#'
#' @return a named vector or `list`
#'
#' @examples
#' \donttest{
#' # Fetch TP53 expression value from pan-cancer dataset
#' t1 <- get_pancan_value("TP53",
#'   dataset = "TcgaTargetGtex_rsem_isoform_tpm",
#'   host = "toilHub"
#' )
#' t2 <- get_pancan_gene_value("TP53")
#' }
#' 
#' @export
#' @describeIn get_pancan_value Fetch identifier value from pan-cancer dataset
get_pancan_value <- function(identifier, subtype = NULL, dataset = NULL, host = available_hosts(),
                             samples = NULL) {
  stopifnot(length(identifier) == 1, !all(is.null(subtype), is.null(dataset)))
  if (!requireNamespace("UCSCXenaTools", quietly = TRUE)) {
    stop("Package 'UCSCXenaTools' is not installed.")
  }
  if (!"UCSCXenaTools" %in% .packages()) {
    attachNamespace("UCSCXenaTools")
  }
  host <- match.arg(host)

  data <- UCSCXenaTools::XenaData
  if (!is.null(subtype)) {
    data <- data %>%
      dplyr::filter(XenaHostNames == host, grepl(subtype, DataSubtype))
  }
  if (!is.null(dataset)) {
    data <- data %>%
      dplyr::filter(XenaHostNames == host, grepl(dataset, XenaDatasets))
  }

  if (nrow(data) != 1) {
    stop("No dataset or more than one dataset is determined by input")
  }

  res <- UCSCXenaTools::fetch_dense_values(data[["XenaHosts"]], data[["XenaDatasets"]],
    identifiers = identifier, samples = samples,
    check = FALSE, use_probeMap = TRUE
  )
  res2 <- res[1, ]
  names(res2) <- colnames(res)
  res2
}

#' @describeIn get_pancan_value Fetch gene expression value from pan-cancer dataset
#' @export
get_pancan_gene_value <- function(identifier, host = "toilHub") {
  host <- match.arg(host)
  if (host == "toilHub") {
    dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
    expression <- get_pancan_value(identifier, dataset = dataset, host = host)
    unit <- "log2(tpm+0.001)"
  }
  msg <- paste0(
    "More info about dataset please run following commands:\n",
    "  library(UCSCXenaTools)\n",
    "  XenaGenerate(subset = XenaDatasets == \"", dataset, "\") %>% XenaBrowse()"
  )
  message(msg)
  res <- list(expression = expression, unit = unit)
  res
}


utils::globalVariables(
  c(
    "XenaHostNames",
    "DataSubtype",
    "XenaDatasets"
  )
)
