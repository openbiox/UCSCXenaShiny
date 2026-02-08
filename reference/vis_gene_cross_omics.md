# Visualize cross-omics of one gene among pan-cancers

Visualize cross-omics of one gene among pan-cancers

## Usage

``` r
vis_gene_cross_omics(
  gene = "TP53",
  tumor_projects = NULL,
  tumor_samples = NULL,
  n_trans = 5,
  n_methy = 5,
  seed = 42,
  add_mean_trans = TRUE,
  add_mean_methy = TRUE,
  pval_mrna = c(0.05, 0.01, 0.001),
  return_list = FALSE
)
```

## Arguments

- gene:

  a gene symbol identifier (e.g., "TP53")

- tumor_projects:

  Select specific TCGA projects. Default NULL, indicating all TCGA
  projects.

- tumor_samples:

  Select specific tumor samples. Default NULL, indicating all tumor
  samples.

- n_trans:

  The number of sampling transcripts or specific transcript identifiers.

- n_methy:

  The number of sampling CpG sites or specific CpG identifiers.

- seed:

  The seed of sampling.

- add_mean_trans:

  Add overall column to display the mean values of all gene's
  transcripts.

- add_mean_methy:

  Add overall column to display the mean values of all gene's cpg sites.

- pval_mrna:

  The P value thresholds

- return_list:

  TRUE returns a list including plot object and data. FALSE just returns
  plot.

## Value

funkyheatmap
