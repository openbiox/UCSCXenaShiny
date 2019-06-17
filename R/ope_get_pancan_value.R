# 获取某特征（基因、蛋白等）在Pancan中的表达量

available_hosts = function() {
  UCSCXenaTools:::.xena_hosts %>% as.character()
}

get_pancan_value <- function(identifier, subtype=NULL, dataset=NULL, host=available_hosts(), 
                             samples = NULL) {
  stopifnot(length(identifier) == 1)
  if (!requireNamespace("UCSCXenaTools", quietly = TRUE)) 
    stop("Package 'UCSCXenaTools' is not installed.")
  attachNamespace("UCSCXenaTools")
  host = match.arg(host)
  
  data = UCSCXenaTools::XenaData
  if (!is.null(subtype)) {
    data = data %>%
      dplyr::filter(XenaHostNames == host, grepl(subtype, DataSubtype))
  }
  if (!is.null(dataset)) {
    data = data %>% 
      dplyr::filter(XenaHostNames == host, grepl(dataset, XenaDatasets))
  }
  
  if (nrow(data) != 1) {
    stop("More than one dataset is determined by input")
  }
  
  res = fetch_dense_values(data[["XenaHosts"]], data[["XenaDatasets"]], 
                           identifiers = identifier, samples = samples,
                           check = FALSE, use_probeMap = TRUE)
  res2 = res
  names(res2) = colnames(res)
  res2
}

get_pancan_gene_value <- function(identifier, host = "toilHub") {
  host = match.arg(host)
  if (host == "toilHub") {
    dataset = "TcgaTargetGtex_rsem_isoform_tpm"
    expression = get_pancan_value(identifier, dataset = dataset, host = host)
    unit = "log2(tpm+0.001)"
  }
  msg = paste0("More info about dataset please run following commands:\n",
               "  library(UCSCXenaTools)\n",
               "  XenaGenerate(subset = XenaDatasets == \"", dataset, "\") %>% XenaBrowse()")
  message(msg)
  res = list(expression=expression, unit=unit)
  res
}
