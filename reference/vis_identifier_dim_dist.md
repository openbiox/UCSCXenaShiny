# Visualize the distribution difference of samples after Molecule Identifier dimensionality reduction analysis

NOTE: the dataset must be dense matrix in UCSC Xena data hubs.

## Usage

``` r
vis_identifier_dim_dist(
  dataset = NULL,
  ids = NULL,
  grp_df,
  samples = NULL,
  return.data = FALSE,
  DR_method = c("PCA", "UMAP", "tSNE"),
  add_margin = NULL,
  palette = "Set1"
)
```

## Arguments

- dataset:

  the dataset to obtain identifiers.

- ids:

  the molecule identifiers.

- grp_df:

  When `dataset` and `id` are all not `NULL`, it should be a
  `data.frame` with 2 columns.

  - The first column refers to sample ID.

  - The second column refers to groups indicated in axis X.

- samples:

  default is `NULL`, can be common sample names for two datasets.

- return.data:

  whether to reture the raw meta/matrix data (list) instead of plot

- DR_method:

  the dimensionality reduction method

- add_margin:

  the marginal plot (NULL, "density", "boxplot")

- palette:

  the color setting of RColorBrewer

## Value

a `ggplot` object.

## Examples

``` r
# vis_identifier_dim_dist(expr_dataset, ids, grp_df, DR_method="PCA")
```
