# Heatmap for Correlation between Gene and Immune Signatures

Heatmap for Correlation between Gene and Immune Signatures

## Usage

``` r
vis_gene_immune_cor(
  Gene = "TP53",
  cor_method = "spearman",
  data_type = "mRNA",
  Immune_sig_type = "Cibersort",
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

- Immune_sig_type:

  quantification method, default is "Cibersort"

- Plot:

  output the plot directly, default 'TRUE'

- opt_pancan:

  specify one dataset for some molercular profiles

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_gene_immune_cor(Gene = "TP53")
} # }
```
