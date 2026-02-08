# Heatmap for Correlation between Gene and Tumor Immune Infiltration (TIL)

Heatmap for Correlation between Gene and Tumor Immune Infiltration (TIL)

## Usage

``` r
vis_gene_TIL_cor(
  Gene = "TP53",
  cor_method = "spearman",
  data_type = "mRNA",
  sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
    "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
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

- sig:

  Immune Signature, default: result from TIMER

- Plot:

  output the plot directly, default 'TRUE'

- opt_pancan:

  specify one dataset for some molercular profiles

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_gene_TIL_cor(Gene = "TP53")
} # }
```
