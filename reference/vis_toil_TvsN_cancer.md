# Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs Normal (TCGA & GTEx))

Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs Normal (TCGA &
GTEx))

## Usage

``` r
vis_toil_TvsN_cancer(
  Gene = "TP53",
  Mode = c("Violinplot", "Dotplot"),
  data_type = "mRNA",
  Show.P.value = FALSE,
  Show.P.label = FALSE,
  Method = "wilcox.test",
  values = c("#DF2020", "#DDDF21"),
  TCGA.only = FALSE,
  Cancer = "ACC",
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- Mode:

  "Boxplot" or "Violinplot" to represent data

- data_type:

  choose gene profile type, including "mRNA", "transcript", "protein",
  "mutation", "cnv", "methylation", "miRNA".

- Show.P.value:

  `TRUE` or `FALSE` whether to count P value

- Show.P.label:

  `TRUE` or `FALSE` present p value with number or label `*`, `**`,
  `***` and `****`

- Method:

  default method is wilcox.test

- values:

  the color to fill tumor or normal

- TCGA.only:

  include samples only from TCGA dataset

- Cancer:

  select cancer cohort(s).

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object.
