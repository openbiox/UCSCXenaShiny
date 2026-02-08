# Visualize Single Gene Expression in Anatomy Location

Visualize Single Gene Expression in Anatomy Location

## Usage

``` r
vis_pancan_anatomy(
  Gene = "TP53",
  Gender = c("Female", "Male"),
  data_type = "mRNA",
  option = "D",
  opt_pancan = .opt_pancan
)
```

## Arguments

- Gene:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- Gender:

  a string, "Female" (default) or "Male".

- data_type:

  choose gene profile type, including
  "mRNA","transcript","methylation","miRNA","protein","cnv"

- option:

  A character string indicating the color map option to use. Eight
  options are available:

  - `"magma"` (or `"A"`)

  - `"inferno"` (or `"B"`)

  - `"plasma"` (or `"C"`)

  - `"viridis"` (or `"D"`)

  - `"cividis"` (or `"E"`)

  - `"rocket"` (or `"F"`)

  - `"mako"` (or `"G"`)

  - `"turbo"` (or `"H"`)

- opt_pancan:

  specify one dataset for some molercular profiles

## Value

a `ggplot` object
