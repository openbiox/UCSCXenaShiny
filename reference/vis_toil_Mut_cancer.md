# Visualize molecular profile difference between mutation and wild status of queried gene in Single Cancer Type

Visualize molecular profile difference between mutation and wild status
of queried gene in Single Cancer Type

## Usage

``` r
vis_toil_Mut_cancer(
  mut_Gene = "TP53",
  Gene = NULL,
  data_type = NULL,
  Mode = c("Dotplot", "Violinplot"),
  Show.P.value = TRUE,
  Show.P.label = TRUE,
  Method = c("wilcox.test", "t.test"),
  values = c("#DF2020", "#DDDF21"),
  draw_quantiles = c(0.25, 0.5, 0.75),
  trim = TRUE,
  Cancer = "ACC",
  opt_pancan = .opt_pancan
)
```

## Arguments

- mut_Gene:

  the queried gene to determine grouping based on mutation and wild
  status

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- data_type:

  choose gene profile type, including "mRNA", "transcript",
  "methylation", "miRNA".

- Mode:

  choose one visualize mode to represent data

- Show.P.value:

  `TRUE` or `FALSE` whether to count P value

- Show.P.label:

  `TRUE` or `FALSE` present p value with number or label `*`, `**`,
  `***` and `****`

- Method:

  default method is wilcox.test

- values:

  the color to fill mutation or wild status

- draw_quantiles:

  draw quantiles for violinplot

- trim:

  whether to trim the violin

- Cancer:

  select cancer cohort(s).

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object or a tibble data.frame.
