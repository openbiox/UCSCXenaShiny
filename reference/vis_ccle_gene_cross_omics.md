# Visualize cross-omics of one gene in CCLE

Visualize cross-omics of one gene in CCLE

## Usage

``` r
vis_ccle_gene_cross_omics(
  gene = "TP53",
  tumor_projects = NULL,
  n_protein = 0,
  add_mean_protein = FALSE,
  return_list = FALSE
)
```

## Arguments

- gene:

  a gene symbol identifier (e.g., "TP53")

- tumor_projects:

  Select specific CCLE tissues. Default NULL, indicating all.

- n_protein:

  number of antibodies to show.

- add_mean_protein:

  whether to add median protein expression.

- return_list:

  TRUE returns a list including plot object and data. FALSE just returns
  plot.

## Value

funkyheatmap
