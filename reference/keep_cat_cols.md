# Keep Only Columns Used for Sample Selection

Keep Only Columns Used for Sample Selection

## Usage

``` r
keep_cat_cols(x, keep_sam_cols = TRUE, return_idx = TRUE)
```

## Arguments

- x:

  a `data.frame` with many columns.

- keep_sam_cols:

  if `TRUE` (default), keep columns with pattern 'sample', 'patient',
  etc.

- return_idx:

  if `TRUE` (default), return index of 5 (at most) columns, it is useful
  in Shiny.

## Value

a `data.frame` or a `list`.
