# Visualize molecular profile in PCAWG

Visualize molecular profile in PCAWG

## Usage

``` r
vis_pcawg_dist(
  Gene = "TP53",
  Mode = c("Boxplot", "Violinplot"),
  data_type = "mRNA",
  Show.P.value = TRUE,
  Show.P.label = TRUE,
  Method = c("wilcox.test", "t.test"),
  values = c("#DF2020", "#DDDF21"),
  draw_quantiles = c(0.25, 0.5, 0.75),
  trim = TRUE,
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

- draw_quantiles:

  draw quantiles for violinplot

- trim:

  whether trim the violin

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_pcawg_dist(Gene = "TP53")
} # }
```
