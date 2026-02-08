# Visualize Correlation between Gene and Pathway signature Score

Visualize Correlation between Gene and Pathway signature Score

## Usage

``` r
vis_gene_pw_cor(
  Gene = "TP53",
  data_type = "mRNA",
  pw_name = "HALLMARK_ADIPOGENESIS",
  cancer_choose = "GBM",
  use_regline = TRUE,
  cor_method = "spearman",
  use_all = FALSE,
  alpha = 0.5,
  color = "#000000",
  filter_tumor = TRUE,
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature ("TP53 + 2 \* KRAS - 1.3 \* PTEN").

- data_type:

  choose gene profile type, including "mRNA", "transcript", "protein",
  "mutation", "cnv", "methylation", "miRNA".

- pw_name:

  the queried Pathway name, see the supported pathway from
  'load("toil_sig_score")'default is NULL

- cancer_choose:

  select cancer cohort(s)

- use_regline:

  if TRUE, add regression line.

- cor_method:

  select correlation coefficient (pearson/spearman)

- use_all:

  use all sample, default FALSE.

- alpha:

  dot alpha.

- color:

  dot color.

- filter_tumor:

  whether use tumor sample only, default TRUE

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object or dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
vis_gene_pw_cor(
  Gene = "TP53", data_type = "mRNA",
  pw_name = "HALLMARK_ADIPOGENESIS",
  cancer_choose = "BRCA"
)
} # }
```
