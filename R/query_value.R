#' Query Single Identifier Value from Pan-cancer Database
#'
#' @param identifier a ID, e.g. gene symbol.
#' @param data_type data type. Can be one of "mRNA", "transcript", "protein",
#' "mutation", "cnv" (-2, -1, 0, 1, 2), "cnv_gistic2", "methylation", "miRNA".
#' @param database database, either 'toil' for TCGA TARGET GTEx, or 'ccle' for
#' CCLE.
#'
#' @return a list.
#' @export
query_value <- function(identifier,
                        data_type = c(
                          "mRNA", "transcript", "protein",
                          "mutation", "cnv", "cnv_gistic2",
                          "methylation", "miRNA"
                        ),
                        database = c("toil", "ccle")) {
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
      miRNA = get_pancan_miRNA_value
    )
  } else {
    f <- switch(data_type,
      mRNA = get_ccle_gene_value,
      transcript = stop("Not support for database 'ccle'!"),
      protein = get_ccle_protein_value,
      mutation = get_ccle_mutation_status,
      cnv = get_ccle_cn_value,
      methylation = stop("Not support for database 'ccle'!"),
      miRNA = stop("Not support for database 'ccle'!")
    )
  }
  if (data_type == "cnv_gistic2") {
    f(identifier, use_thresholded_data = FALSE)
  } else {
    f(identifier)
  }
}
