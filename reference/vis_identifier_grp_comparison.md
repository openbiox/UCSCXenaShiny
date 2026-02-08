# Visualize Comparison of an Molecule Identifier between Groups

NOTE: the dataset must be dense matrix in UCSC Xena data hubs.

## Usage

``` r
vis_identifier_grp_comparison(
  dataset = NULL,
  id = NULL,
  grp_df,
  samples = NULL,
  fun_type = c("betweenstats", "withinstats"),
  type = c("parametric", "nonparametric", "robust", "bayes"),
  pairwise.comparisons = TRUE,
  p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
    "none"),
  ggtheme = cowplot::theme_cowplot(),
  ...
)
```

## Arguments

- dataset:

  the dataset to obtain identifiers.

- id:

  the molecule identifier.

- grp_df:

  When `dataset` and `id` are all not `NULL`, it should be a
  `data.frame` with 2 or 3 columns.

  - The first column refers to sample ID.

  - The second column refers to groups indicated in axis X.

  - The third column is optional, which indicates facet variable. When
    any of `dataset` and `id` is `NULL`, it should be a `data.frame`
    with 3 or 4 columns.

  - The first column refers to sample ID.

  - The second column refers to values indicated in axis Y.

  - The third column refers to groups indicated in axis X.

  - The fourth column is optional, which indicates facet variable.

- samples:

  default is `NULL`, can be common sample names for two datasets.

- fun_type:

  select the function to compare groups.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- pairwise.comparisons:

  whether pairwise comparison

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

- ggtheme:

  A `{ggplot2}` theme. Default value is
  [`theme_ggstatsplot()`](https://indrajeetpatil.github.io/ggstatsplot/reference/theme_ggstatsplot.html).
  Any of the `{ggplot2}` themes (e.g.,
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)),
  or themes from extension packages are allowed (e.g.,
  `ggthemes::theme_fivethirtyeight()`, `hrbrthemes::theme_ipsum_ps()`,
  etc.). But note that sometimes these themes will remove some of the
  details that `{ggstatsplot}` plots typically contains. For example, if
  relevant,
  [`ggbetweenstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html)
  shows details about multiple comparison test as a label on the
  secondary Y-axis. Some themes (e.g.
  `ggthemes::theme_fivethirtyeight()`) will remove the secondary Y-axis
  and thus the details as well.

- ...:

  other parameters passing to
  [ggstatsplot::ggbetweenstats](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html)
  or
  [ggstatsplot::ggwithinstats](https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html).

## Value

a (gg)plot object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(UCSCXenaTools)
expr_dataset <- "TCGA.LUAD.sampleMap/HiSeqV2_percentile"
cli_dataset <- "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
id <- "TP53"
cli_df <- XenaGenerate(
  subset = XenaDatasets == "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
) %>%
  XenaQuery() %>%
  XenaDownload() %>%
  XenaPrepare()

# group data.frame with 2 columns
vis_identifier_grp_comparison(expr_dataset, id, cli_df[, c("sampleID", "gender")])
# group data.frame with 3 columns
vis_identifier_grp_comparison(
  expr_dataset, id,
  cli_df[, c("sampleID", "pathologic_M", "gender")] %>%
    dplyr::filter(pathologic_M %in% c("M0", "MX"))
)

# When not use the value of `identifier` from `dataset`
vis_identifier_grp_comparison(grp_df = cli_df[, c(1, 2, 71)])
vis_identifier_grp_comparison(grp_df = cli_df[, c(1, 2, 71, 111)])
} # }
```
