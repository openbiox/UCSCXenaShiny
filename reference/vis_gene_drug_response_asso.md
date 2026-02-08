# Visualize Gene and Drug-Target Association with CCLE Data

See
[analyze_gene_drug_response_asso](https://openbiox.github.io/UCSCXenaShiny/reference/analyze_gene_drug_response_asso.md)
for examples.

## Usage

``` r
vis_gene_drug_response_asso(
  Gene = "TP53",
  x_axis_type = c("mean.diff", "median.diff"),
  output_form = c("plotly", "ggplot2")
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- x_axis_type:

  set the value type for X axis.

- output_form:

  `plotly` or `ggplot2`.

## Value

`plotly` or `ggplot2` object.
