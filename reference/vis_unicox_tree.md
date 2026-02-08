# Visualize Single Gene Univariable Cox Result from Toil Data Hub

Visualize Single Gene Univariable Cox Result from Toil Data Hub

## Usage

``` r
vis_unicox_tree(
  Gene = "TP53",
  measure = "OS",
  data_type = "mRNA",
  use_optimal_cutoff = FALSE,
  values = c("grey", "#E31A1C", "#377DB8"),
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- measure:

  a survival measure, e.g. "OS".

- data_type:

  choose gene profile type, including
  "mRNA","transcript","methylation","miRNA","protein","mutation","cnv"

- use_optimal_cutoff:

  use `surv_cutpoint` from survminer package for thresholding samples in
  each cancer type.

- values:

  the color to fill tumor or normal

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object

## Examples

``` r
if (FALSE) { # \dontrun{
p <- vis_unicox_tree(Gene = "TP53")
} # }
```
