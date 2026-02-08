# Download data for shiny general analysis

Download data for shiny general analysis

## Usage

``` r
query_general_value(
  L1,
  L2,
  L3,
  database = c("toil", "pcawg", "ccle"),
  tpc_value_nonomics = NULL,
  opt_pancan = NULL,
  custom_metadata = NULL
)
```

## Arguments

- L1:

  level 1 main datatype

- L2:

  level 2 sub datatype

- L3:

  level 3 identifier

- database:

  one of c("toil","pcawg","ccle")

- tpc_value_nonomics:

  non-omics matrix data of one database

- opt_pancan:

  molecular datasets parameters

- custom_metadata:

  user customized metadata

## Examples

``` r
if (FALSE) { # \dontrun{
general_value_id <- UCSCXenaShiny:::query_general_id()
tcga_value_option <- general_value_id[["value"]][[1]]
tcga_index_value <- tcga_value_option[["Tumor index"]]
tcga_immune_value <- tcga_value_option[["Immune Infiltration"]]
tcga_pathway_value <- tcga_value_option[["Pathway activity"]]
tcga_phenotype_value <- tcga_value_option[["Phenotype data"]]

clinical_phe = tcga_phenotype_value[["Clinical Phenotype"]]
x_data = UCSCXenaShiny:::query_general_value(
           "Molecular profile", "mRNA Expression", "TP53", "toil",
           tcga_index_value, tcga_immune_value, tcga_pathway_value,
           clinical_phe)

y_data = UCSCXenaShiny:::query_general_value(
           "Immune Infiltration", "CIBERSORT", "Monocyte", "toil",
           tcga_index_value, tcga_immune_value, tcga_pathway_value,
           clinical_phe)
} # }
```
