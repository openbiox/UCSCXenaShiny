## how to document datasets: you need to specify @docType and @name; do not
## forget NULL in the end

#' Number of datasets in each cohort
#' @docType data
#' @name dat_datasets
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("dat_datasets")
NULL

#' Number of samples in each cohort
#' @docType data
#' @name dat_samples
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("dat_samples")
NULL

#' Summary info of UCSC Xena
#' @docType data
#' @name XenaInfo
#' @format A [list]
#' @source Generate from data-raw
#' @examples
#' data("XenaInfo")
NULL

#' Phenotype info of CCLE database
#' @docType data
#' @name ccle_info
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("ccle_info")
NULL

#' Toil Hub: TCGA TARGET GTEX selected phenotypes
#' @docType data
#' @name toil_info
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("toil_info")
NULL


#' Toil Hub: TCGA survival data
#' @docType data
#' @name toil_surv
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("toil_surv")
NULL

#' Toil Hub: TCGA survival data from paper
#' @docType data
#' @name cliMat
#' @format A [data.frame]
#' @source Generate from paper
#' @examples
#' data("tcga_clinicalMatrix")
NULL

#' Toil Hub: merged TCGA GTEX selected phenotypes
#' @docType data
#' @name tcga_gtex
#' @format A [data.frame]
#' @examples
#' data("tcga_gtex_sampleinfo")
NULL

#' TCGA: immune signature data
#' @docType data
#' @name  immune_sig
#' @format A [data.frame]
#' @examples
#' data("immune_sig")
#' @source  <https://gdc.cancer.gov/about-data/publications/panimmune>
NULL

#' TCGA: TMB (tumor mutation burden) data
#' @docType data
#' @name  tmb_data
#' @format A [data.frame]
#' @examples
#' data("tmb_data")
#' @source <https://gdc.cancer.gov/about-data/publications/panimmune>
NULL

#' TCGA: Stemness data
#' @docType data
#' @name  stemness_data_RNA
#' @format A [data.frame]
#' @examples
#' data("stemness_data_RNA")
#' @source <https://pancanatlas.xenahubs.net/download/StemnessScores_DNAmeth_20170210.tsv.gz>
NULL

#' TCGA: genome instability data
#' @docType data
#' @name  gi_data
#' @format A [data.frame]
#' @examples
#' data("gi_data")
#' @source <https://gdc.cancer.gov/about-data/publications/PanCanStemness-2018>
NULL

#' TCGA: purity data
#' @docType data
#' @name  purity_data
#' @format A [data.frame]
#' @examples
#' data("purity_data")
#' @source <https://www.nature.com/articles/ncomms9971#Sec14>
NULL

#' TCGA: organ data
#' @docType data
#' @name  TCGA.organ
#' @format A [data.frame]
#' @examples
#' data("TCGA.organ")
NULL

#' TCGA subtype data
#' @docType data
#' @name  TCGAsubtype
#' @format A [data.frame]
#' @source UCSC Xena.
#' @examples
#' data("TCGAsubtype")
NULL
