# Run partial correlation

Run partial correlation

## Usage

``` r
ezcor_partial_cor(
  data = NULL,
  split = FALSE,
  split_var = NULL,
  var1 = NULL,
  var2 = NULL,
  var3 = NULL,
  cor_method = "pearson",
  sig_label = TRUE,
  ...
)
```

## Arguments

- data:

  a `data.frame` containing variables

- split:

  whether perform correlation grouped by a variable, default is 'FALSE'

- split_var:

  a `character`, the group variable

- var1:

  a `character`, the first variable in correlation

- var2:

  a `character`, the second variable in correlation

- var3:

  a `character` or `character vector`, the third variable in correlation

- cor_method:

  method="pearson" is the default value. The alternatives to be passed
  to cor are "spearman" and "kendall"

- sig_label:

  whether add symbal of significance. P \< 0.001,"***"; P \< 0.01,"**";
  P \< 0.05,"*"; P \>=0.05,""

- ...:

  other arguments passed to methods

## Value

a `data.frame`

## See also

[`ppcor::pcor.test()`](https://rdrr.io/pkg/ppcor/man/pcor.test.html)
which this function wraps.

## Author

Yi Xiong
