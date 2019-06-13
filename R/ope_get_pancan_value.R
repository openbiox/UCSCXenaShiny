# 获取某特征（基因、蛋白等）在Pancan中的表达量

available_hosts = function() {
  UCSCXenaTools:::.xena_hosts %>% as.character()
}

get_pancan_value <- function(identifier, subtype=NULL, dataset=NULL, host=available_hosts(), 
                             samples = NULL) {
  stopifnot(length(identifier) == 1)
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
  as.numeric(res)
}