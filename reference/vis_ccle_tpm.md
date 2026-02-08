# Visualize CCLE Gene Expression

Visualize CCLE Gene Expression

## Usage

``` r
vis_ccle_tpm(
  Gene = "TP53",
  data_type = "mRNA",
  use_log = FALSE,
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- data_type:

  support genomic profile for CCLE, currently "mRNA", "protein","cnv"
  are supported

- use_log:

  if `TRUE`, log values.

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object
