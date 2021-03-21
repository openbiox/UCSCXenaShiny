## how to document datasets: you need to specify @docType and @name; do not
## forget NULL in the end

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
#' @name  tcga_pan_tcga_pan_immune_signaturenature
#' @format A [data.frame]
#' @examples
#' data("tcga_pan_tcga_pan_immune_signaturenature")
#' @source  <https://gdc.cancer.gov/about-data/publications/panimmune>
NULL

#' TCGA: TMB (tumor mutation burden) data
#' @docType data
#' @name  tcga_tmb
#' @format A [data.frame]
#' @examples
#' data("tcga_tmb")
#' @source <https://gdc.cancer.gov/about-data/publications/panimmune>
NULL

#' TCGA: Stemness data
#' @docType data
#' @name  tcga_stemness
#' @format A [data.frame]
#' @examples
#' data("tcga_stemness")
#' @source <https://pancanatlas.xenahubs.net>
NULL

#' TCGA: genome instability data
#' @docType data
#' @name  tcga_genome_instability
#' @format A [data.frame]
#' @examples
#' data("tcga_genome_instability")
#' @source <https://gdc.cancer.gov/about-data/publications/PanCanStemness-2018>
NULL

#' TCGA: purity data
#' @docType data
#' @name  tcga_purity
#' @format A [data.frame]
#' @examples
#' data("tcga_purity")
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
#' @name  tcga_subtypes
#' @format A [data.frame]
#' @source UCSC Xena.
#' @examples
#' data("tcga_subtypes")
NULL
