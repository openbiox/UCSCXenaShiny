# Visualize Correlation for Multiple Identifiers

NOTE: the dataset must be dense matrix in UCSC Xena data hubs.

## Usage

``` r
vis_identifier_multi_cor(
  dataset,
  ids,
  samples = NULL,
  matrix.type = c("full", "upper", "lower"),
  type = c("parametric", "nonparametric", "robust", "bayes"),
  partial = FALSE,
  sig.level = 0.05,
  p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
    "none"),
  color_low = "#E69F00",
  color_high = "#009E73",
  ...
)
```

## Arguments

- dataset:

  the dataset to obtain identifiers.

- ids:

  the molecule identifiers.

- samples:

  default is `NULL`, can be common sample names for two datasets.

- matrix.type:

  Character, `"upper"` (default), `"lower"`, or `"full"`, display full
  matrix, lower triangular or upper triangular matrix.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- partial:

  Can be `TRUE` for partial correlations. For Bayesian partial
  correlations, "full" instead of pseudo-Bayesian partial correlations
  (i.e., Bayesian correlation based on frequentist partialization) are
  returned.

- sig.level:

  Significance level (Default: `0.05`). If the *p*-value in *p*-value
  matrix is bigger than `sig.level`, then the corresponding correlation
  coefficient is regarded as insignificant and flagged as such in the
  plot.

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

- color_low:

  the color code for lower value mapping.

- color_high:

  the color code for higher value mapping.

- ...:

  other parameters passing to
  [ggstatsplot::ggcorrmat](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html).

## Value

a (gg)plot object.

## Examples

``` r
if (FALSE) { # \dontrun{
dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
ids <- c("TP53", "KRAS", "PTEN")
vis_identifier_multi_cor(dataset, ids)
} # }
```
