# Visualize the distribution difference of samples after dimensionality reduction analysis

Visualize the distribution difference of samples after dimensionality
reduction analysis

## Usage

``` r
vis_dim_dist(
  ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
  data_type = "mRNA",
  group_info = NULL,
  DR_method = c("PCA", "UMAP", "tSNE"),
  palette = "Set1",
  add_margin = NULL,
  opt_pancan = .opt_pancan
)
```

## Arguments

- ids:

  molecular identifiers (\>=3)

- data_type:

  molecular types, refer to query_pancan_value() function

- group_info:

  two-column grouping information with names 'Sample','Group'

- DR_method:

  the dimensionality reduction method

- palette:

  the color setting of RColorBrewer

- add_margin:

  the marginal plot (NULL, "density", "boxplot")

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a ggplot object or rawdata list

## Examples

``` r
if (FALSE) { # \dontrun{
group_info <- tcga_clinical_fine %>%
  dplyr::filter(Cancer == "BRCA") %>%
  dplyr::select(Sample, Code) %>%
  dplyr::rename(Group = Code)

vis_dim_dist(
  ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
  group_info = group_info
)
} # }
```
