# 获取某特征（基因、蛋白等）在Pancan中的表达量

get_pancan_value <- function(identifier, data_subtype, host=available_hosts(), ) {
  host = match.arg(host)
  data_filter = XenaData %>%
    dplyr::filter(XenaHostNames == host, DataSubtype == data_subtype)
  host
  
  fetch_dense_values(host, dataset, identifiers = NULL, samples = NULL,
                     check = TRUE, use_probeMap = FALSE)
}

available_hosts = function() {
  UCSCXenaTools:::.xena_hosts %>% as.character()
}
