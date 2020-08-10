## code to prepare `TCGAsubtype` dataset goes here
download.file("https://pancanatlas.xenahubs.net/download/TCGASubtype.20170308.tsv.gz",
              "data-raw/TCGAsubtype.tsv.gz")

download.file("https://pancanatlas.xenahubs.net/download/Subtype_Immune_Model_Based.txt.gz",
              "data-raw/immuSubtype.tsv.gz")

d1 = data.table::fread("data-raw/TCGAsubtype.tsv.gz")
d2 = data.table::fread("data-raw/immuSubtype.tsv.gz")

TCGAsubtype = merge(d1, d2, by.x = "sampleID", by.y = "sample", all = TRUE)
TCGAsubtype = as.data.frame(TCGAsubtype)
usethis::use_data(TCGAsubtype, overwrite = TRUE)
