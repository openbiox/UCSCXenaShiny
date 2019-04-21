## how to document datasets: you need to specify @docType and @name; do not
## forget NULL in the end

#' Number of datasets in each cohort
#' @docType data
#' @name dat_datasets
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples data(dat_datasets)
NULL

#' Number of samples in each cohort
#' @docType data
#' @name dat_samples
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples data(dat_samples)
NULL

#' Summary info of UCSC Xena
#' @docType data
#' @name XenaInfo
#' @format A [list]
#' @source Generate from data-raw
#' @examples data(XenaInfo)
NULL
