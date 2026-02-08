# Visualize Correlation between Gene and TMB (Tumor Mutation Burden)

Visualize Correlation between Gene and TMB (Tumor Mutation Burden)

## Usage

``` r
vis_gene_tmb_cor(
  Gene = "TP53",
  cor_method = "spearman",
  data_type = "mRNA",
  Plot = "TRUE",
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- cor_method:

  correlation method

- data_type:

  choose gene profile type, including "mRNA", "transcript", "protein",
  "mutation", "cnv", "methylation", "miRNA".

- Plot:

  output the plot directly, default 'TRUE'

- opt_pancan:

  specify one dataset for some molercular profiles

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_gene_tmb_cor(Gene = "TP53")
} # }
```
