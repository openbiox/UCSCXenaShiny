# Run correlation between two variables in a batch mode and support group by a variable

Run correlation between two variables in a batch mode and support group
by a variable

## Usage

``` r
ezcor_batch(
  data,
  var1,
  var2,
  split = FALSE,
  split_var = NULL,
  cor_method = "pearson",
  adjust_method = "none",
  use = "complete",
  sig_label = TRUE,
  parallel = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  a `data.frame` containing variables

- var1:

  a character, the first variable in correlation

- var2:

  a character, the second variable in correlation

- split:

  whether perform correlation grouped by a variable, default is 'FALSE'

- split_var:

  a `character`, the group variable

- cor_method:

  method="pearson" is the default value. The alternatives to be passed
  to cor are "spearman" and "kendall"

- adjust_method:

  What adjustment for multiple tests should be used? ("holm",
  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

- use:

  use="pairwise" will do pairwise deletion of cases. use="complete" will
  select just complete cases

- sig_label:

  whether add symbal of significance. P \< 0.001,`***`; P \< 0.01,`**`;
  P \< 0.05,`*`; P \>=0.05,""

- parallel:

  if `TRUE`, do parallel computation by **furrr** package.

- verbose:

  if `TRUE`, print extra info.

## Value

a `data.frame`

## Author

Yi Xiong, Shixiang Wang
