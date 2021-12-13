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

#' Query Single Identifier or Signature Value from Pan-cancer Database
#'
#' @param molecule a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param data_type data type. Can be one of "mRNA", "transcript", "protein",
#' "mutation", "cnv" (-2, -1, 0, 1, 2), "cnv_gistic2", "methylation", "miRNA".
#' @param database database, either 'toil' for TCGA TARGET GTEx, or 'ccle' for
#' CCLE.
#' @param reset_id if not `NULL`, set the specified variable at parent frame to "Signature".
#' @param ... other extra parameters passing to the underlying functions.
#' @return a list.
#' @export
#' @examples
#' \dontrun{
#' query_pancan_value("KRAS")
#' query_pancan_value("KRAS", database = "ccle")
#' query_pancan_value("KRAS", database = "pcawg")
#' query_pancan_value("hsa-let-7c-3p",
#'   database = "pcawg",
#'   data_type = "miRNA"
#' )
#' query_pancan_value("hsa-let-7c-3p",
#'   database = "pcawg",
#'   data_type = "miRNA", norm_method = "UQ"
#' )
#' query_pancan_value("ENSG00000000419",
#'   database = "pcawg",
#'   data_type = "fusion"
#' ) # gene symbol also work
#' query_pancan_value("tCa_MutLoad_MinEstimate",
#'   database = "pcawg", data_type = "APOBEC"
#' )
#' query_pancan_value("prmtr.10000",
#'   database = "pcawg", data_type = "promoter"
#' )
#' }
query_pancan_value <- function(molecule,
                               data_type = c(
                                 "mRNA", "transcript", "protein", "mutation",
                                 "cnv", "cnv_gistic2",
                                 "methylation", "miRNA",
                                 "fusion", "promoter", "APOBEC"
                               ),
                               database = c("toil", "ccle", "pcawg"),
                               reset_id = NULL, ...) {
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
          query_value(x, data_type, database, ...)
        })
        unit <- if (is.list(values[[1]]) && length(values[[1]]) > 1) values[[1]][[2]] else NULL
        if (is.null(unit)) {
          df <- as.data.frame(values %>% purrr::set_names(ids))
        } else {
          df <- as.data.frame(purrr::map(values, ~ .[[1]]) %>% purrr::set_names(ids))
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
    query_value(molecule, data_type, database, ...)
  }
}


query_value <- function(identifier,
                        data_type = c(
                          "mRNA", "transcript", "protein",
                          "mutation", "cnv", "cnv_gistic2",
                          "methylation", "miRNA",
                          "fusion", "promoter", "APOBEC"
                        ),
                        database = c("toil", "ccle", "pcawg"),
                        ...) {
  database <- match.arg(database)
  data_type <- match.arg(data_type)

  if (database == "toil") {
    f <- switch(data_type,
      mRNA = get_pancan_gene_value,
      transcript = get_pancan_transcript_value,
      protein = get_pancan_protein_value,
      mutation = get_pancan_mutation_status,
      cnv = get_pancan_cn_value,
      cnv_gistic2 = get_pancan_cn_value,
      methylation = get_pancan_methylation_value,
      miRNA = get_pancan_miRNA_value,
      stop("Not support for database 'toil (TCGA)'!")
    )
  } else if (database == "ccle") {
    f <- switch(data_type,
      mRNA = get_ccle_gene_value,
      protein = get_ccle_protein_value,
      mutation = get_ccle_mutation_status,
      cnv = get_ccle_cn_value,
      stop("Not support for database 'ccle'!")
    )
  } else if (database == "pcawg") {
    f <- switch(data_type,
      mRNA = get_pcawg_gene_value,
      fusion = get_pcawg_fusion_value,
      miRNA = get_pcawg_miRNA_value,
      promoter = get_pcawg_promoter_value,
      APOBEC = get_pcawg_APOBEC_mutagenesis_value,
      stop("Not support for database 'ccle'!")
    )
  }
  if (data_type == "cnv_gistic2") {
    f(identifier, use_thresholded_data = FALSE)
  } else {
    f(identifier, ...)
  }
}
