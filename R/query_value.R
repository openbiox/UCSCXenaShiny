#' Query Single Identifier Value from Pan-cancer Database
#'
#' @param identifier a ID, e.g. gene symbol.
#' @param data_type data type.
#' @param database database, either 'toil' for TCGA TARGET GTEx, or 'ccle' for
#' CCLE.
#'
#' @return a list.
#' @export
query_value <- function(identifier,
                        data_type = c(
                          "gene", "transcript", "protein",
                          "mutation", "cnv", "methylation"
                        ),
                        database = c("toil", "ccle")) {
  database <- match.arg(database)
  data_type <- match.arg(data_type)

  if (database == "toil") {
    f <- switch(data_type,
      gene = get_pancan_gene_value,
      transcript = get_pancan_transcript_value,
      protein = get_pancan_protein_value,
      mutation = get_pancan_mutation_status,
      cnv = get_pancan_cn_value,
      methylation = get_pancan_methylation_value
    )
  } else {
    f <- switch(data_type,
      gene = get_ccle_gene_value,
      transcript = stop("Not support for database 'ccle'!"),
      protein = get_ccle_protein_value,
      mutation = get_ccle_mutation_status,
      cnv = get_ccle_cn_value,
      methylation = stop("Not support for database 'ccle'!")
    )
  }
  f(identifier)
}
