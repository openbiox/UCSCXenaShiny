# Query Single Identifier or Signature Value from Pan-cancer Database

Query Single Identifier or Signature Value from Pan-cancer Database

## Usage

``` r
query_pancan_value(
  molecule,
  data_type = c("mRNA", "transcript", "protein", "mutation", "cnv", "methylation",
    "miRNA", "fusion", "promoter", "APOBEC"),
  database = c("toil", "ccle", "pcawg"),
  reset_id = NULL,
  opt_pancan = .opt_pancan
)
```

## Arguments

- molecule:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- data_type:

  data type. Can be one of "mRNA", "transcript", "protein", "mutation",
  "cnv", "methylation", "miRNA".

- database:

  database, either 'toil' for TCGA TARGET GTEx, or 'ccle' for CCLE.

- reset_id:

  if not `NULL`, set the specified variable at parent frame to
  "Signature".

- opt_pancan:

  other extra parameters passing to the underlying functions.

## Value

a list.

## Details

`query_pancan_value()` provide convenient interface to download
multi-omics data from 3 databases by specifying one or several canonical
datasets. It is derived from `query_pancan_value()` and support query
for genomic signature. To query comprehensive datasets that UCSCXena
supports, users can check
[`UCSCXenaTools::XenaData`](https://docs.ropensci.org/UCSCXenaTools/reference/XenaData.html)
and use
[`get_pancan_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
directly.

Option `opt_pancan` is a nested list and allow to adjust the downloading
details. For now, only
`cnv(toil)`,`methylation(toil)`,`miRNA(toil)`,`miRNA(pcawg)`,`promoter(pcawg)`
support optional parameters. The default set is `.opt_pancan` and we
check meanings of sublist(parameters) through the following
relationship.

## "toil" database

1.  mRNA–[`get_pancan_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

2.  transcript–[`get_pancan_transcript_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

3.  protein–[`get_pancan_protein_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

4.  mutation–[`get_pancan_mutation_status()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

5.  cnv–[`get_pancan_cn_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

6.  methylation–[`get_pancan_methylation_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

7.  miRNA–[`get_pancan_miRNA_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

## "ccle" database

1.  mRNA–[`get_ccle_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

2.  protein–[`get_ccle_protein_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

3.  mutation–[`get_ccle_mutation_status()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

4.  cnv–[`get_ccle_cn_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

## "pcawg" database

1.  mRNA–[`get_pcawg_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

2.  miRNA–[`get_pcawg_miRNA_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

3.  promoter–[`get_pcawg_promoter_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

4.  fusion–[`get_pcawg_fusion_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

5.  APOBEC–[`get_pcawg_APOBEC_mutagenesis_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_pancan_value("KRAS")
query_pancan_value("KRAS", database = "ccle")
query_pancan_value("KRAS", database = "pcawg")


query_pancan_value("ENSG00000000419",
  database = "pcawg",
  data_type = "fusion"
) # gene symbol also work

.opt_pancan

opt_pancan <- list(toil_cnv = list(use_thresholded_data = FALSE))
query_pancan_value("PTEN", data_type = "cnv", database = "toil", opt_pancan = opt_pancan)


opt_pancan <- list(toil_methylation = list(type = "450K", rule_out = "cg21115430", aggr = "Q25"))
query_pancan_value("PTEN", data_type = "methylation", database = "toil", opt_pancan = opt_pancan)
} # }
```
