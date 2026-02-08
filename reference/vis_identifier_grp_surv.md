# Visualize Identifier Group Survival Difference

NOTE: the dataset must be dense matrix in UCSC Xena data hubs.

## Usage

``` r
vis_identifier_grp_surv(
  dataset = NULL,
  id = NULL,
  surv_df,
  samples = NULL,
  cutoff_mode = c("Auto", "Custom", "None"),
  cutpoint = c(50, 50),
  palette = "aaas",
  ...
)
```

## Arguments

- dataset:

  the dataset to obtain identifiers.

- id:

  the molecule identifier.

- surv_df:

  a `data.frame`. The "time" should be in unit of "days".

  - If there are 3 columns, the names should be "sample", "time",
    "status".

  - If there are 4 columns, the names should be "sample", "value",
    "time", "status".

- samples:

  default is `NULL`, can be common sample names for two datasets.

- cutoff_mode:

  mode for grouping samples, can be "Auto" (default) or "Custom" or
  "None" (for groups have been prepared).

- cutpoint:

  cut point (in percent) for "Custom" mode, default is `c(50, 50)`.

- palette:

  color palette, can be "hue", "grey", "RdBu", "Blues", "npg", "aaas",
  etc. More see
  [`?survminer::ggsurvplot`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html).

- ...:

  other parameters passing to
  [`survminer::ggsurvplot`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html)

## Value

a (gg)plot object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(UCSCXenaTools)
expr_dataset <- "TCGA.LUAD.sampleMap/HiSeqV2_percentile"
cli_dataset <- "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
id <- "KRAS"
cli_df <- XenaGenerate(
  subset = XenaDatasets == "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
) %>%
  XenaQuery() %>%
  XenaDownload() %>%
  XenaPrepare()

# Use individual survival data
surv_df1 <- cli_df[, c("sampleID", "ABSOLUTE_Ploidy", "days_to_death", "vital_status")]
surv_df1$vital_status <- ifelse(surv_df1$vital_status == "DECEASED", 1, 0)
vis_identifier_grp_surv(surv_df = surv_df1)

# Use both dataset argument and vis_identifier_grp_surv(surv_df = surv_df1)
surv_df2 <- surv_df1[, c(1, 3, 4)]
vis_identifier_grp_surv(expr_dataset, id, surv_df = surv_df2)
vis_identifier_grp_surv(expr_dataset, id,
  surv_df = surv_df2,
  cutoff_mode = "Custom", cutpoint = c(25, 75)
)
} # }
```
