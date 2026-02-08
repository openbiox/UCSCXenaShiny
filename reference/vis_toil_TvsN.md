# Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))

Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))

## Usage

``` r
vis_toil_TvsN(
  Gene = "TP53",
  Mode = c("Boxplot", "Violinplot"),
  data_type = "mRNA",
  Show.P.value = TRUE,
  Show.P.label = TRUE,
  Method = c("wilcox.test", "t.test"),
  values = c("#DF2020", "#DDDF21"),
  TCGA.only = FALSE,
  draw_quantiles = c(0.25, 0.5, 0.75),
  trim = TRUE,
  include.Tumor.only = FALSE,
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

- draw_quantiles:

  draw quantiles for violinplot

- trim:

  whether trim the violin

- include.Tumor.only:

  if `TRUE`, include "UVM" and "MESO" these two types with matched
  normals samples.

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = FALSE, Show.P.label = FALSE)
p <- vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = FALSE, Show.P.label = FALSE)
} # }
```
