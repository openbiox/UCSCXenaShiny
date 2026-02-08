# TCGA Survival Analysis

- Firstly, get merged data of one molecular profile value and associated
  clinical data from TCGA Pan-Cancer dataset.

- Secondly, filter data as your wish.

- Finally, show K-M plot.

## Usage

``` r
tcga_surv_get(
  item,
  TCGA_cohort = "LUAD",
  profile = c("mRNA", "miRNA", "methylation", "transcript", "protein", "mutation", "cnv"),
  TCGA_cli_data = dplyr::full_join(load_data("tcga_clinical"), load_data("tcga_surv"), by
    = "sample"),
  opt_pancan = .opt_pancan
)

tcga_surv_plot(
  data,
  time = "time",
  status = "status",
  cutoff_mode = c("Auto", "Custom"),
  cutpoint = c(50, 50),
  cnv_type = c("Duplicated", "Normal", "Deleted"),
  profile = c("mRNA", "miRNA", "methylation", "transcript", "protein", "mutation", "cnv"),
  palette = "aaas",
  ...
)
```

## Arguments

- item:

  a molecular identifier, can be gene symbol (common cases), protein
  symbol, etc.

- TCGA_cohort:

  a TCGA cohort, e.g. "LUAD" (default), "LUSC", "ACC".

- profile:

  a molecular profile. Option can be one of "mRNA" (default), "miRNA",
  "methylation", "transcript", "protein", "mutation", "cnv".

- TCGA_cli_data:

  a `data.frame` containing TCGA clinical data. Default use pre-compiled
  TCGA clinical data in this package.

- opt_pancan:

  specify one dataset for some molercular profiles

- data:

  a subset of result from `tcga_surv_get()`.

- time:

  the column name for "time".

- status:

  the column name for "status".

- cutoff_mode:

  mode for grouping samples, can be "Auto" (default) or "Custom".

- cutpoint:

  cut point (in percent) for "Custom" mode, default is `c(50, 50)`.

- cnv_type:

  only used when profile is "cnv", can select from
  `c("Duplicated", "Normal", "Deleted")`.

- palette:

  color palette, can be "hue", "grey", "RdBu", "Blues", "npg", "aaas",
  etc. More see
  [`?survminer::ggsurvplot`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html).

- ...:

  other parameters passing to
  [`survminer::ggsurvplot`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html)

## Value

a `data.frame` or a plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. get data
data <- tcga_surv_get("TP53")
# 2. filter data (optional)

# 3. show K-M plot
tcga_surv_plot(data, time = "DSS.time", status = "DSS")
} # }
```
