# Visualize Gene and Drug Response Difference with CCLE Data

See
[analyze_gene_drug_response_diff](https://openbiox.github.io/UCSCXenaShiny/reference/analyze_gene_drug_response_diff.md)
for examples.

## Usage

``` r
vis_gene_drug_response_diff(
  Gene = "TP53",
  tissue = "lung",
  Show.P.label = TRUE,
  Method = "wilcox.test",
  values = c("#DF2020", "#DDDF21"),
  alpha = 0.5
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- tissue:

  select cell line origin tissue.

- Show.P.label:

  `TRUE` or `FALSE` present p value with number or label `*`, `**`,
  `***` and `****`

- Method:

  default method is wilcox.test

- values:

  the color to fill tumor or normal

- alpha:

  set alpha for dots.

## Value

a `ggplot` object.
