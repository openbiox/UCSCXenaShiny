# Visualize Identifier-Identifier Correlation

NOTE: the dataset must be dense matrix in UCSC Xena data hubs.

## Usage

``` r
vis_identifier_cor(
  dataset1,
  id1,
  dataset2,
  id2,
  samples = NULL,
  use_ggstats = FALSE,
  use_simple_axis_label = TRUE,
  line_color = "blue",
  alpha = 0.5,
  ...
)
```

## Arguments

- dataset1:

  the dataset to obtain `id1`.

- id1:

  the first molecule identifier.

- dataset2:

  the dataset to obtain `id2`.

- id2:

  the second molecule identifier.

- samples:

  default is `NULL`, can be common sample names for two datasets.

- use_ggstats:

  if `TRUE`, use
  [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot) package
  for plotting.

- use_simple_axis_label:

  if `TRUE` (default), use simple axis labels. Otherwise, data subtype
  will be labeled.

- line_color:

  set the color for regression line.

- alpha:

  set the alpha for dots.

- ...:

  other parameters passing to
  [ggscatter](http://rpkgs.datanovia.com/ggpubr/reference/ggscatter.md).

## Value

a (gg)plot object.

## Examples

``` r
if (FALSE) { # \dontrun{
dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
id1 <- "TP53"
id2 <- "KRAS"
vis_identifier_cor(dataset, id1, dataset, id2)

samples <- c(
  "TCGA-D5-5538-01", "TCGA-VM-A8C8-01",
  "TCGA-ZN-A9VQ-01", "TCGA-EE-A17X-06",
  "TCGA-05-4420-01"
)
vis_identifier_cor(dataset, id1, dataset, id2, samples)

dataset1 <- "TCGA-BLCA.htseq_counts.tsv"
dataset2 <- "TCGA-BLCA.gistic.tsv"
id1 <- "TP53"
id2 <- "KRAS"
vis_identifier_cor(dataset1, id1, dataset2, id2)
} # }
```
