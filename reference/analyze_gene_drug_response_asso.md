# Analyze Association between Gene (Signature) and Drug Response with CCLE Data

Analyze partial correlation of gene-drug association after controlling
for tissue average expression.

## Usage

``` r
analyze_gene_drug_response_asso(gene_list, combine = FALSE)
```

## Arguments

- gene_list:

  a gene symbol list.

- combine:

  if `TRUE`, combine the expression of gene list as a gene signature.

## Value

a `data.frame`

- If `combine` is `TRUE`, genes are combined as `signature`.

- `mean.diff` and `median.diff` indicate mean and median of normalized
  expression difference between High IC50 cells and Low IC50 cells. The
  cutoff between High and Low are median IC50.

## Examples

``` r
if (FALSE) { # \dontrun{
analyze_gene_drug_response_asso("TP53")
analyze_gene_drug_response_asso(c("TP53", "KRAS"))
analyze_gene_drug_response_asso(c("TP53", "KRAS"), combine = TRUE)

# Visualization
vis_gene_drug_response_asso("TP53")
} # }
```
