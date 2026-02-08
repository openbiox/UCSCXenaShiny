# Visualize Gene-Gene Correlation in TCGA

Visualize Gene-Gene Correlation in TCGA

## Usage

``` r
vis_gene_cor(
  Gene1 = "CSF1R",
  Gene2 = "JAK3",
  data_type1 = "mRNA",
  data_type2 = "mRNA",
  use_regline = TRUE,
  purity_adj = TRUE,
  alpha = 0.5,
  color = "#000000",
  filter_tumor = TRUE,
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene1:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- Gene2:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- data_type1:

  choose gene profile type for the first gene, including
  "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"

- data_type2:

  choose gene profile type for the second gene, including
  "mRNA","transcript","methylation","miRNA","protein","cnv_gistic2"

- use_regline:

  if `TRUE`, add regression line.

- purity_adj:

  whether performing partial correlation adjusted by purity

- alpha:

  dot alpha.

- color:

  dot color.

- filter_tumor:

  whether use tumor sample only, default `TRUE`

- opt_pancan:

  specify one dataset for some molercular profiles
