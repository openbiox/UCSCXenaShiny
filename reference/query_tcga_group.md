# Group TPC samples by build-in or custom phenotype and support filtering or merging operations

Group TPC samples by build-in or custom phenotype and support filtering
or merging operations

## Usage

``` r
query_tcga_group(
  database = c("toil", "pcawg", "ccle"),
  cancer = NULL,
  custom = NULL,
  group = "Gender",
  filter_by = NULL,
  filter_id = NULL,
  merge_by = NULL,
  merge_quantile = FALSE,
  return_all = FALSE
)
```

## Arguments

- database:

  one of c("toil","pcawg","ccle")

- cancer:

  select cancer cohort(s)

- custom:

  upload custom phenotype data

- group:

  target group names

- filter_by:

  filter samples by one or multiple criterion

- filter_id:

  directly filter samples by provided sample ids

- merge_by:

  merge the target group for main categories

- merge_quantile:

  whether to merge numerical variable by percentiles

- return_all:

  return the all phenotype data

## Value

a list object with grouping samples and statistics

## Examples

``` r
if (FALSE) { # \dontrun{
query_tcga_group(group = "Age")

query_tcga_group(
  cancer = "BRCA",
  group = "Stage_ajcc"
)

query_tcga_group(
  cancer = "BRCA",
  group = "Stage_ajcc",
  filter_by = list(
    c("Code", c("TP"), "+"),
    c("Stage_ajcc", c(NA), "-")
  )
)

query_tcga_group(
  cancer = "BRCA",
  group = "Stage_ajcc",
  filter_by = list(
    c("Age", c(0.5), "%>")
  )
)

query_tcga_group(
  cancer = "BRCA",
  group = "Stage_ajcc",
  filter_by = list(
    c("Age", c(60), ">")
  )
)

query_tcga_group(
  cancer = "BRCA",
  group = "Stage_ajcc",
  merge_by = list(
    "Early" = c("Stage I"),
    "Late" = c("Stage II", "Stage III", "Stage IV")
  )
)

query_tcga_group(
  cancer = "BRCA",
  group = "Age",
  merge_by = list(
    "Young" = c(20, 60),
    "Old" = c(60, NA)
  )
)

query_tcga_group(
  cancer = "BRCA",
  group = "Age",
  merge_quantile = TRUE,
  merge_by = list(
    "Young" = c(0, 0.5),
    "Old" = c(0.5, 1)
  )
)
} # }
```
