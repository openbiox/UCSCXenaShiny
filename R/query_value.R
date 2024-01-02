#' Get Molecule or Signature Data Values from Dense (Genomic) Matrix Dataset of UCSC Xena Data Hubs
#'
#' @param dataset a UCSC Xena dataset in dense matrix format (rows are features
#' (e.g., gene, cell line) and columns are samples).
#' @param molecule a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' **NOTE**, when a signature is specified, a space must exist in the input.
#' @param host a UCSC Xena host, default is `NULL`, auto-detect from the dataset.
#'
#' @return a named vector.
#' @export
#'
#' @examples
#' # What does dense matrix mean?
#' table(UCSCXenaTools::XenaData$Type)
#' # It is a the UCSC Xena dataset with "Type" equals to "genomicMatrix"
#' \dontrun{
#' dataset <- "ccle/CCLE_copynumber_byGene_2013-12-03"
#' x <- query_molecule_value(dataset, "TP53")
#' head(x)
#'
#' signature <- "TP53 + 2*KRAS - 1.3*PTEN" # a space must exist in the string
#' y <- query_molecule_value(dataset, signature)
#' head(y)
#' }
query_molecule_value <- function(dataset, molecule, host = NULL) {
  has_signature <- grepl(" ", molecule)
  if (has_signature) {
    fm <- parse(text = molecule)
    ids <- all.vars(fm)
    message("Querying multiple identifiers at the same time for genomic signature...")
    message("IDs include ", paste(ids, collapse = ", "))
    tryCatch(
      {
        values <- purrr::map(ids, ~ get_data(dataset, ., host = host))
        df <- as.data.frame(values %>% purrr::set_names(ids))
        colnames(df) <- ids
        sig_values <- eval(fm, envir = df)
        names(sig_values) <- rownames(df)
        0L
      },
      error = function(e) {
        warning("Query and evaluate failed, bad IDs or formula or data values.", immediate. = TRUE)
        return(NA)
      }
    ) -> return_value
    if (is.na(return_value)) sig_values <- NA
    return(sig_values)
  } else {
    get_data(dataset, molecule, host = host)
  }
}


#' A default setting for pan-cancer studies
#' @export
.opt_pancan = list(
  # Toil
  toil_mRNA = list(norm = "tpm"),
  toil_transcript = list(),
  toil_protein = list(),
  toil_mutation = list(),
  toil_cnv = list(gistic2 = TRUE, use_thresholded_data = FALSE),
  toil_methylation = list(type = "450K", rule_out = NULL, aggr = "NA"),
  toil_miRNA = list(),
  # PCAWG
  pcawg_mRNA = list(),
  pcawg_fusion = list(),
  pcawg_miRNA = list(norm = "TMM"),
  pcawg_promoter = list(type = "relative"),
  pcawg_APOBEC = list(),
  # CCLE
  ccle_mRNA = list(norm = "rpkm"),
  ccle_protein = list(),
  ccle_mutation = list(),
  ccle_cnv = list()
)




#' Query Single Identifier or Signature Value from Pan-cancer Database
#'
#' @param molecule a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param data_type data type. Can be one of "mRNA", "transcript", "protein",
#' "mutation", "cnv", "methylation", "miRNA".
#' @param database database, either 'toil' for TCGA TARGET GTEx, or 'ccle' for
#' CCLE.
#' @param reset_id if not `NULL`, set the specified variable at parent frame to "Signature".
#' @param opt_pancan other extra parameters passing to the underlying functions.
#' @return a list.
#' @export

#' @details
#' `query_pancan_value()` provide convenient interface to download multi-omics
#' data from 3 databases by specifying one or several canonical datasets. It is 
#' derived from `query_pancan_value()` and support query for genomic signature.
#' To query comprehensive datasets that UCSCXena supports, users can check 
#' `UCSCXenaTools::XenaData` and use `get_pancan_value()` directly.
#' 
#'  Option `opt_pancan` is a nested list and allow to adjust the downloading details. 
#'  For now, only `cnv(toil)`,`methylation(toil)`,`miRNA(toil)`,`miRNA(pcawg)`,`promoter(pcawg)` 
#'  support optional parameters. The default set is `.opt_pancan` and we check meanings of sublist(parameters) 
#'  through the following relationship. 
#'  
#' @section "toil" database: 
#'  1. mRNA--`get_pancan_gene_value()`
#'  2. transcript--`get_pancan_transcript_value()`
#'  3. protein--`get_pancan_protein_value()`
#'  4. mutation--`get_pancan_mutation_status()`
#'  5. cnv--`get_pancan_cn_value()`
#'  6. methylation--`get_pancan_methylation_value()`
#'  7. miRNA--`get_pancan_miRNA_value()`
#' @section "ccle" database: 
#'  1. mRNA--`get_ccle_gene_value()`
#'  2. protein--`get_ccle_protein_value()`
#'  3. mutation--`get_ccle_mutation_status()`
#'  4. cnv--`get_ccle_cn_value()`
#' @section "pcawg" database: 
#'  1. mRNA--`get_pcawg_gene_value()`
#'  2. miRNA--`get_pcawg_miRNA_value()`
#'  3. promoter--`get_pcawg_promoter_value()`
#'  4. fusion--`get_pcawg_fusion_value()`
#'  5. APOBEC--`get_pcawg_APOBEC_mutagenesis_value()`
#' 
#' @examples
#' \dontrun{
#' query_pancan_value("KRAS")
#' query_pancan_value("KRAS", database = "ccle")
#' query_pancan_value("KRAS", database = "pcawg")
#' 
#' 
#' query_pancan_value("ENSG00000000419",
#'   database = "pcawg",
#'   data_type = "fusion"
#' ) # gene symbol also work
#' 
#' .opt_pancan
#' 
#' opt_pancan = list(toil_cnv = list(use_thresholded_data = FALSE))
#' query_pancan_value("PTEN",data_type = "cnv", database = "toil", opt_pancan = opt_pancan)
#' 
#' 
#' opt_pancan = list(toil_methylation = list(type = "450K",rule_out = "cg21115430", aggr = "Q25"))
#' query_pancan_value("PTEN",data_type = "methylation", database = "toil", opt_pancan = opt_pancan)
#' 
#' }
query_pancan_value <- function(molecule,
                               data_type = c(
                                 "mRNA", "transcript", "protein", "mutation",
                                 "cnv",  "methylation", "miRNA",
                                 "fusion", "promoter", "APOBEC"
                               ),
                               database = c("toil", "ccle", "pcawg"),
                               reset_id = NULL, opt_pancan = .opt_pancan) {
  data_type <- match.arg(data_type)
  database <- match.arg(database)

  # molecule = "TP53 + 2*KRAS - 1.3*PTEN"
  has_signature <- grepl(" ", molecule)
  if (has_signature) {
    fm <- parse(text = molecule)
    ids <- all.vars(fm)
    message("Querying multiple identifiers at the same time for genomic signature...")
    message("IDs include ", paste(ids, collapse = ", "))
    tryCatch(
      {
        values <- purrr::map(ids, function(x) {
          query_value(x, data_type, database, opt_pancan)
        })
        unit <- if (is.list(values[[1]]) && length(values[[1]]) > 1) values[[1]][[2]] else NULL
        if (is.null(unit)) {
          df <- as.data.frame(values %>% purrr::set_names(ids))
        } else {
          df <- purrr::reduce(purrr::map(values, ~ as.data.frame(.[[1]])), mbind) %>% magrittr::set_colnames(ids)
        }
        colnames(df) <- ids
        sig_values <- eval(fm, envir = df)
        names(sig_values) <- rownames(df)
        0L
      },
      error = function(e) {
        warning("Query and evaluate failed, bad IDs or formula or data values.", immediate. = TRUE)
        return(NA)
      }
    ) -> return_value
    if (is.na(return_value)) sig_values <- NA
    if (!exists("unit")) unit <- NULL

    if (!is.null(reset_id)) {
      assign(reset_id, "Signature", envir = parent.frame())
    }

    if (is.null(unit)) {
      return(sig_values)
    } else {
      return(
        list(
          value = sig_values,
          unit = unit
        )
      )
    }
  } else {
    query_value(molecule, data_type, database, opt_pancan)
  }
}


query_value <- function(identifier,
                        data_type = c(
                          "mRNA", "transcript", "protein",
                          "mutation", "cnv",
                          "methylation", "miRNA",
                          "fusion", "promoter", "APOBEC"
                        ),
                        database = c("toil", "ccle", "pcawg"),
                        opt_pancan = .opt_pancan) {
  database <- match.arg(database)
  data_type <- match.arg(data_type)

  if (database == "toil") {
    f <- switch(data_type,
      mRNA = get_pancan_gene_value,
      transcript = get_pancan_transcript_value,
      protein = get_pancan_protein_value,
      mutation = get_pancan_mutation_status,
      cnv = get_pancan_cn_value,
      methylation = get_pancan_methylation_value,
      miRNA = get_pancan_miRNA_value,
      stop("Please choose one of c('mRNA','transcript','protein','mutation','cnv','methylation','miRNA') for Toil database!")
    )
  } else if (database == "ccle") {
    f <- switch(data_type,
      mRNA = get_ccle_gene_value,
      protein = get_ccle_protein_value,
      mutation = get_ccle_mutation_status,
      cnv = get_ccle_cn_value,
      stop("Please choose one of c('mRNA','protein','mutation','cnv') for Toil database!")

    )
  } else if (database == "pcawg") {
    f <- switch(data_type,
      mRNA = get_pcawg_gene_value,
      fusion = get_pcawg_fusion_value,
      miRNA = get_pcawg_miRNA_value,
      promoter = get_pcawg_promoter_value,
      APOBEC = get_pcawg_APOBEC_mutagenesis_value,
      stop("Please choose one of c('mRNA','fusion','miRNA','promoter','APOBEC') for Toil database!")
    )
  }

  do.call(f, c(identifier, opt_pancan[[paste0(database,"_",data_type)]]))
}
