#' Visualize Identifier-identifier Correlation
#'
#' @param id1 the first molecule identifier.
#' @param id2 the second molecule identifier.
#' @param dataset1 the dataset to obtain `id1`.
#' @param dataset2 the dataset to obtain `id2`.
#' @export
vis_identifier_cor <- function(dataset1, id1, dataset2, id2, samples = NULL, line_color = "blue", ...) {
  stopifnot(length(id1) == 1, length(id2) == 1)
  
  # res <- check_exist_data(identifier, dataset, host)
  # if (res$ok) {
  #   expression <- res$data
  # } else {
  #   expression <- get_pancan_value(identifier, dataset = dataset, host = host)
  #   save_data(data, identifier, dataset, host)
  # }
  
  id1_value <- get_pancan_value(id1, dataset = dataset1, samples = samples)
  id2_value <- get_pancan_value(id2, dataset = dataset2, samples = samples)
  
  df <- dplyr::inner_join(
    dplyr::tibble(
      name = names(id1_value),
      x = as.numeric(id1_value)
    ),
    dplyr::tibble(
      name = names(id2_value),
      Y = as.numeric(id2_value)
    ),
    by = "name"
  )
  
  colnames(df) <- c("sample", id1, id2)
  eval(parse(text = "library(ggpubr)"))
  p <- do.call("ggscatter", list(
    data = df,
    x = id1, y = id2,
    add = "reg.line",
    add.params = list(color = line_color, fill = "lightgray"),
    cor.coef = TRUE, ...
  ))
  p
}
