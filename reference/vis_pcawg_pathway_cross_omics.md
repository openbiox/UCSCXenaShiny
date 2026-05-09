# Visualize cross-omics of one pathway in PCAWG

Visualize cross-omics of one pathway in PCAWG

## Usage

``` r
vis_pcawg_pathway_cross_omics(
  pw = "HALLMARK_ADIPOGENESIS",
  tumor_projects = NULL,
  return_list = FALSE
)
```

## Arguments

- pw:

  pathway name

- tumor_projects:

  Select specific PCAWG projects. Default NULL, indicating all.

- return_list:

  TRUE returns a list including plot object and data. FALSE just returns
  plot.

## Value

funkyheatmap
