# Run Correlation between Two Variables and Support Group by a Variable

Run Correlation between Two Variables and Support Group by a Variable

## Usage

``` r
ezcor(
  data = NULL,
  split = FALSE,
  split_var = NULL,
  var1 = NULL,
  var2 = NULL,
  cor_method = "pearson",
  adjust_method = "none",
  use = "complete",
  sig_label = TRUE,
  verbose = TRUE
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

  a character, the first variable in correlation

- var2:

  a character, the second variable in correlation

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

- verbose:

  if `TRUE`, print extra info.

## Value

a `data.frame`

## Author

Yi Xiong
