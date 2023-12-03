## how to document datasets: you need to specify @docType and @name; do not
## forget NULL in the end

#' Purity Data of PCAWG
#' @docType data
#' @name pcawg_purity
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("pcawg_purity")
NULL

#' Phenotype Info of CCLE Database
#' @docType data
#' @name ccle_info
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("ccle_info")
NULL

#' Cleaned Phenotype Info of CCLE Database for grouping
#' @docType data
#' @name ccle_info_fine
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("ccle_info_fine")
NULL


#' Phenotype Info of PCAWG Database
#' @docType data
#' @name pcawg_info
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("pcawg_info")
NULL


#' Cleaned Phenotype Info of PCAWG Database for grouping
#' @docType data
#' @name pcawg_info_fine
#' @format A `data.frame`
#' @source UCSC Xena.
#' @examples
#' data("pcawg_info_fine")
NULL



#' ABSOLUTE Result of CCLE Database
#' @docType data
#' @name ccle_absolute
#' @format A `data.frame`
#' @source see "data_source" attribute.
#' @examples
#' data("ccle_absolute")
NULL

#' Toil Hub: TCGA TARGET GTEX Selected Phenotype
#' @docType data
#' @name toil_info
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("toil_info")
NULL


#' Toil Hub: TCGA Clinical Data
#'
#' See `tcga_surv` for TCGA survival data.
#' @docType data
#' @name tcga_clinical
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("tcga_clinical")
NULL


#' Toil Hub: Cleaned TCGA Clinical Data for grouping
#'
#' See `tcga_surv` for TCGA survival data.
#' @docType data
#' @name tcga_clinical_fine
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("tcga_clinical_fine")
NULL


#' Toil Hub: TCGA Survival Data
#' @docType data
#' @name tcga_surv
#' @format A [data.frame]
#' @source Generate from data-raw
#' @examples
#' data("tcga_surv")
NULL


#' Toil Hub: Merged TCGA GTEx Selected Phenotype
#' @docType data
#' @name tcga_gtex
#' @format A [data.frame]
#' @examples
#' data("tcga_gtex")
NULL

#' TCGA: TMB (Tumor Mutation Burden) Data
#' @docType data
#' @name  tcga_tmb
#' @format A [data.frame]
#' @examples
#' data("tcga_tmb")
#' @source <https://gdc.cancer.gov/about-data/publications/panimmune>
NULL


#' TCGA: Genome Instability Data
#' @docType data
#' @name  tcga_genome_instability
#' @format A [data.frame]
#' @examples
#' data("tcga_genome_instability")
#' @source <https://gdc.cancer.gov/about-data/publications/PanCanStemness-2018>
NULL

#' TCGA: Purity Data
#' @docType data
#' @name  tcga_purity
#' @format A [data.frame]
#' @examples
#' data("tcga_purity")
#' @source <https://www.nature.com/articles/ncomms9971#Sec14>
NULL

#' TCGA: Organ Data
#' @docType data
#' @name  TCGA.organ
#' @format A [data.frame]
#' @examples
#' data("TCGA.organ")
NULL

#' TCGA Subtype Data
#' @docType data
#' @name  tcga_subtypes
#' @format A [data.frame]
#' @source UCSC Xena.
#' @examples
#' data("tcga_subtypes")
NULL
