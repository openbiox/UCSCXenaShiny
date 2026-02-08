# Visualize cross-omics of one pathway among pan-cancers

Visualize cross-omics of one pathway among pan-cancers

## Usage

``` r
vis_pathway_cross_omics(
  pw = "HALLMARK_ADIPOGENESIS",
  tumor_projects = NULL,
  tumor_samples = NULL,
  pval_mrna = c(0.05, 0.01, 0.001),
  return_list = FALSE
)
```

## Arguments

- pw:

  pathway name

- tumor_projects:

  Select specific TCGA projects. Default NULL, indicating all TCGA
  projects.

- tumor_samples:

  Select specific tumor samples. Default NULL, indicating all tumor
  samples.

- pval_mrna:

  The P value thresholds

- return_list:

  TRUE returns a list including plot object and data. FALSE just returns
  plot.

## Value

funkyheatmap
