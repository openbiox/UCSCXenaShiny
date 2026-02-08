# Visualize Gene-Gene Correlation in a TCGA Cancer Type

Visualize Gene-Gene Correlation in a TCGA Cancer Type

## Usage

``` r
vis_gene_cor_cancer(
  Gene1 = "CSF1R",
  Gene2 = "JAK3",
  data_type1 = "mRNA",
  data_type2 = "mRNA",
  purity_adj = TRUE,
  cancer_choose = "GBM",
  use_regline = TRUE,
  cor_method = "spearman",
  use_all = FALSE,
  alpha = 0.5,
  color = "#000000",
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

- purity_adj:

  whether performing partial correlation adjusted by purity

- cancer_choose:

  TCGA cohort name, e.g. "ACC".

- use_regline:

  if `TRUE`, add regression line.

- cor_method:

  correlation method.

- use_all:

  use all sample, default `FALSE`.

- alpha:

  dot alpha.

- color:

  dot color.

- opt_pancan:

  specify one dataset for some molercular profiles
