# Visualize cross-omics of one gene in PCAWG

Visualize cross-omics of one gene in PCAWG

## Usage

``` r
vis_pcawg_gene_cross_omics(
  gene = "TP53",
  tumor_projects = NULL,
  n_promoter = 0,
  add_mean_promoter = FALSE,
  promoter_type = c("relative", "raw", "outlier"),
  return_list = FALSE
)
```

## Arguments

- gene:

  a gene symbol identifier (e.g., "TP53")

- tumor_projects:

  Select specific PCAWG projects. Default NULL, indicating all.

- n_promoter:

  number of promoters to show.

- add_mean_promoter:

  whether to add median promoter activity.

- promoter_type:

  one of "relative", "raw", "outlier".

- return_list:

  TRUE returns a list including plot object and data. FALSE just returns
  plot.

## Value

funkyheatmap
