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

#' Toil Hub: merged TCGA GTEX selected phenotypes
#' @docType data
#' @name tcga_gtex
#' @format A [data.frame]
#' @examples
#' data("tcga_gtex_sampleinfo")
NULL
