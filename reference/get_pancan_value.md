# Fetch Identifier Value from Pan-cancer Dataset

Identifier includes gene/probe etc.

## Usage

``` r
get_ccle_cn_value(identifier)

get_ccle_gene_value(identifier, norm = c("rpkm", "nc"))

get_ccle_protein_value(identifier)

get_ccle_mutation_status(identifier)

get_pancan_value(
  identifier,
  subtype = NULL,
  dataset = NULL,
  host = available_hosts(),
  samples = NULL,
  ...
)

get_pancan_gene_value(identifier, norm = c("tpm", "fpkm", "nc"))

get_pancan_transcript_value(identifier, norm = c("tpm", "fpkm", "isopct"))

get_pancan_protein_value(identifier)

get_pancan_mutation_status(identifier)

get_pancan_cn_value(identifier, gistic2 = TRUE, use_thresholded_data = FALSE)

get_pancan_methylation_value(
  identifier,
  type = c("450K", "27K"),
  rule_out = NULL,
  aggr = c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100")
)

get_pancan_miRNA_value(identifier)

get_pcawg_gene_value(identifier)

get_pcawg_fusion_value(identifier)

get_pcawg_promoter_value(identifier, type = c("raw", "relative", "outlier"))

get_pcawg_miRNA_value(identifier, norm = c("TMM", "UQ"))

get_pcawg_APOBEC_mutagenesis_value(
  identifier = c("tCa_MutLoad_MinEstimate", "APOBECtCa_enrich", "A3A_or_A3B",
    "APOBEC_tCa_enrich_quartile", "APOBECrtCa_enrich", "APOBECytCa_enrich",
    "APOBECytCa_enrich-APOBECrtCa_enrich", "BH_Fisher_p-value_tCa", "ntca+tgan",
    "rtCa_to_G+rtCa_to_T", "rtca+tgay", "tCa_to_G+tCa_to_T",
    "ytCa_rtCa_BH_Fisher_p-value", "ytCa_rtCa_Fisher_p-value", "ytCa_to_G+ytCa_to_T",
    "ytca+tgar")
)
```

## Arguments

- identifier:

  a length-1 character representing a gene symbol, ensembl gene id, or
  probe id. Gene symbol is highly recommended.

- norm:

  the normalization method.

- subtype:

  a length-1 chracter representing a regular expression for matching
  `DataSubtype` column of
  [UCSCXenaTools::XenaData](https://docs.ropensci.org/UCSCXenaTools/reference/XenaData.html).

- dataset:

  a length-1 chracter representing a regular expression for matching
  `XenaDatasets` of
  [UCSCXenaTools::XenaData](https://docs.ropensci.org/UCSCXenaTools/reference/XenaData.html).

- host:

  a character vector representing host name(s), e.g. "toilHub".

- samples:

  a character vector representing samples want to be returned.

- ...:

  other parameters.

- gistic2:

  if `TRUE` (default), use GISTIC2 data.

- use_thresholded_data:

  if `TRUE`, use GISTIC2-thresholded value.

- type:

  methylation type, one of "450K" and "27K". for function
  `get_pcawg_promoter_value`, it can be one of "raw", "relative",
  "outlier".

- rule_out:

  methylation sites to rule out before analyzing.

- aggr:

  apporaches to aggregate the methylation data, default is 'NA', in such
  case, a mean value is obtained for gene-level methylation. Allowed
  value is one of `c("NA", "mean", "Q0", "Q25", "Q50", "Q75", "Q100")`.
  Here, `Q50` is median.

## Value

a named vector or `list`.

## Functions

- `get_ccle_cn_value()`: Fetch copy number value from CCLE dataset

- `get_ccle_gene_value()`: Fetch gene expression value from CCLE dataset

- `get_ccle_protein_value()`: Fetch gene protein expression value from
  CCLE dataset

- `get_ccle_mutation_status()`: Fetch gene mutation info from CCLE
  dataset

- `get_pancan_value()`: Fetch identifier value from pan-cancer dataset

- `get_pancan_gene_value()`: Fetch gene expression value from pan-cancer
  dataset

- `get_pancan_transcript_value()`: Fetch gene transcript expression
  value from pan-cancer dataset

- `get_pancan_protein_value()`: Fetch protein expression value from
  pan-cancer dataset

- `get_pancan_mutation_status()`: Fetch mutation status value from
  pan-cancer dataset

- `get_pancan_cn_value()`: Fetch gene copy number value from pan-cancer
  dataset processed by GISTIC 2.0

- `get_pancan_methylation_value()`: Fetch gene expression value from
  CCLE dataset

- `get_pancan_miRNA_value()`: Fetch miRNA expression value from
  pan-cancer dataset

- `get_pcawg_gene_value()`: Fetch specimen-level gene expression value
  from PCAWG cohort

- `get_pcawg_fusion_value()`: Fetch specimen-level gene fusion value
  from PCAWG cohort

- `get_pcawg_promoter_value()`: Fetch specimen-level gene promoter
  activity value from PCAWG cohort

- `get_pcawg_miRNA_value()`: Fetch specimen-level miRNA value from PCAWG
  cohort

- `get_pcawg_APOBEC_mutagenesis_value()`: Fetch specimen-level gene
  fusion value from PCAWG cohort

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch TP53 expression value from pan-cancer dataset
t1 <- get_pancan_value("TP53",
  dataset = "TcgaTargetGtex_rsem_isoform_tpm",
  host = "toilHub"
)
t2 <- get_pancan_gene_value("TP53")
t3 <- get_pancan_protein_value("AKT")
t4 <- get_pancan_mutation_status("TP53")
t5 <- get_pancan_cn_value("TP53")
} # }
```
