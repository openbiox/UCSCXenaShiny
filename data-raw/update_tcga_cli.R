# https://zenodo.org/record/6326542#.Y4sP5ZxBzvQ
# https://doi.org/10.1101/2021.11.01.466731
devtools::load_all()

fl <- list.files("~/Downloads/TCGA_RNASeq", pattern = "rds", full.names = TRUE)
rf <- dplyr::tibble()

for (i in fl) {
  x <- readRDS(i)
  x2 <- colData(x)
  x2 <- as.data.frame(x2[, c("Samples", "BatchId_mda")])
  x2 <- x2 |>
    dplyr::as_tibble() |>
    dplyr::rename(sample = Samples, BatchId = BatchId_mda) |>
    dplyr::mutate(sample = substr(sample, 1, 15))
  rf <- dplyr::bind_rows(rf, x2)
}

tcga_clinical <- tcga_clinical |>
  dplyr::left_join(rf, by = "sample")

usethis::use_data(tcga_clinical, overwrite = TRUE)
