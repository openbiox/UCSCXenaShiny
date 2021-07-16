query_custom_feature_value <- function(id) {
  # 为简单起见，暂不支持 genomic signature 组合多个特征值特性
  message("Reading custom feature data from temp directory.")
  fData <- readRDS(file.path(tempdir(), "custom_feature_data.rds"))
  colnames(fData)[1] <- "sample"
  fData <- fData %>%
    dplyr::filter(.data$sample %in% id)
  value <- fData[, -1]
  names(value) <- colnames(fData)[-1]
  value
}