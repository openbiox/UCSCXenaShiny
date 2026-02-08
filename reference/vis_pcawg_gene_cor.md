# Visualize Gene-Gene Correlation in TCGA

Visualize Gene-Gene Correlation in TCGA

## Usage

``` r
vis_pcawg_gene_cor(
  Gene1 = "CSF1R",
  Gene2 = "JAK3",
  data_type1 = "mRNA",
  data_type2 = "mRNA",
  cor_method = "spearman",
  purity_adj = TRUE,
  use_log_x = FALSE,
  use_log_y = FALSE,
  use_regline = TRUE,
  dcc_project_code_choose = "BLCA-US",
  use_all = FALSE,
  filter_tumor = TRUE,
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

- cor_method:

  correlation method

- purity_adj:

  whether performing partial correlation adjusted by purity

- use_log_x:

  if `TRUE`, log X values.

- use_log_y:

  if `TRUE`, log Y values.

- use_regline:

  if `TRUE`, add regression line.

- dcc_project_code_choose:

  select project code.

- use_all:

  use all sample, default `FALSE`.

- filter_tumor:

  whether use tumor sample only, default `TRUE`

- alpha:

  dot alpha.

- color:

  dot color.

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object
