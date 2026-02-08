# Changelog

## UCSCXenaShiny 2.2.0

CRAN release: 2025-08-01

- Adapted `interleave()`.
- Added gene and pathway cross-omics analysis functions/modules.
- Added
  [`app_run2()`](https://openbiox.github.io/UCSCXenaShiny/reference/app_run2.md)
  function for custom app start with lightweight modules.
- Fixed [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) parameter for KM
  plot
- Improved the UI of Custom TPC Modules.
- Removed a mistake by Yi Xiong for setting problematic threshold
  parameter in unicox analysis.

## UCSCXenaShiny 2.1.0

CRAN release: 2024-05-15

- Handled error raised due to internet issue in CRAN check.
- Optimized data preloading of Shiny application.
- Dockerfile updated.

## UCSCXenaShiny 2.0.1

- Fixed some minor bugs in Shiny app.
- Updated README about Docker usage.
- Fixed an issue in
  [`vis_identifier_grp_comparison()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_grp_comparison.md)
  ([\#317](https://github.com/openbiox/UCSCXenaShiny/issues/317)).

## UCSCXenaShiny 2.0.0

CRAN release: 2024-03-14

See the [UCSCXenaShiny v2
Book](https://lishensuo.github.io/UCSCXenaShiny_Book/index.html) for a
comprehensive guidance.

### New Features

#### Datasets

- `load_data("tcga_PW")`: ssGSEA scores of HALLMARK, KEGG, IOBR terms
  for TCGA samples.
- `load_data("tcga_PW_meta")`: metadata annotation for HALLMARK, KEGG,
  IOBR terms.
- `load_data("pcawg_TIL")`: PCAWG TIL data.
- `load_data("pcawg_PW")`: ssGSEA scores of HALLMARK, KEGG, IOBR terms
  for PCAWG samples.
- and more.

#### R Package Functions

- `.opt_pancan` : Default setting for alternative TPC datasets.
- [`mol_quick_analysis()`](https://openbiox.github.io/UCSCXenaShiny/reference/mol_quick_analysis.md):
  Quick molecule analysis and report generation based on TCGA dataset.
- [`query_tcga_group()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_tcga_group.md):
  Group TPC samples by build-in or custom phenotype and support
  filtering or merging operations.
- [`vis_dim_dist()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_dim_dist.md):
  Visualize the distribution difference of TCGA samples after
  dimensionality reduction analysis.
- [`vis_identifier_dim_dist()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_dim_dist.md):
  Visualize the distribution difference of samples after Molecule
  Identifier dimensionality reduction analysis.
- [`vis_toil_Mut()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_Mut.md):
  Visualize molecular profile difference between mutation and wild
  status of queried gene.
- [`vis_toil_Mut_cancer()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_Mut_cancer.md):
  Visualize molecular profile difference between mutation and wild
  status of queried gene in Single Cancer Type

#### Shiny application

- Homepage

  - Added slicker gallery to display page summary;
  - Added report generation for TCGA pan-cancer exploration.

- General Dataset Analysis

  - Added one general dimensionality reduction analysis module.

- Quick TPC Analysis

  - Added one module for association analysis between molecule and
    pathway;
  - Added one module for association analysis between molecule and
    mutation;
  - Added one module for dimensionality reduction analysis.

- Personalized Analysis

  - Designed personalized TPC analysis pipelines for based on 3 methods
    and 3 modes.

- Download

  - Added two modules for exact subset of integrated TPC data and
    UCSCXena datasets.

### Enhancements

- Supported getting more flexible methylation value.

``` r
UCSCXenaShiny::get_pancan_methylation_value(
  "RCAN2",
  rule_out = c("cg21115430", "cg19452802"), 
  aggr = "Q75"
)
```

- Supported installing the package from r-universe
  (<https://openbiox.r-universe.dev/UCSCXenaShiny>).

- Supported alternative molecular profiling datasets for quick and
  personalized TPC analysis.

### Bug Fixes

- Merged data with unequal size in pan-cancer data query with a gene
  signature
  ([\#283](https://github.com/openbiox/UCSCXenaShiny/issues/283)), the
  fix also enhance the sample names match.

Test code:

``` r
vis_gene_tmb_cor("`ZFAT-AS1` + `SNORD116-1` + SPATA31D1", data_type = "methylation")
```

## UCSCXenaShiny 1.1.11

- Fixed check warning in pan-cancer radar plot.

## UCSCXenaShiny 1.1.10

CRAN release: 2023-02-28

- Fixed check issue due to internet access
  ([\#253](https://github.com/openbiox/UCSCXenaShiny/issues/253)).

## UCSCXenaShiny 1.1.9

CRAN release: 2022-12-12

- Added cancer type control for PCAWG survival analysis.
- Added TCGA batch id from MDA.

## UCSCXenaShiny 1.1.8

CRAN release: 2022-06-07

- Fixed issue to set mislabelled color in pcawg pan-cancer analysis
  ([\#247](https://github.com/openbiox/UCSCXenaShiny/issues/247), Thanks
  to Tangjian Li).
- Fixed issue in querying gene signature in general analysis page
  ([\#244](https://github.com/openbiox/UCSCXenaShiny/issues/244)).
- Fixed issue “Radar plot “stemness” does not plot the “ACC” tumor type”
  ([\#242](https://github.com/openbiox/UCSCXenaShiny/issues/242)).

## UCSCXenaShiny 1.1.7

CRAN release: 2022-04-13

- Added option `include.Tumor.only` to control if include type
- Set default theme if `flatly` not available.
- Added example to generate radar plot, close
  [\#239](https://github.com/openbiox/UCSCXenaShiny/issues/239)

## UCSCXenaShiny 1.1.6

- Added description of extra datasets.

## UCSCXenaShiny 1.1.5

CRAN release: 2022-01-15

- Fixed survival KM plot output issue due to `ggsave()` failure in
  General Analysis page.
  ([\#230](https://github.com/openbiox/UCSCXenaShiny/issues/230))

## UCSCXenaShiny 1.1.4

CRAN release: 2021-12-13

- Fixed the colnames being changed by
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) when
  querying a symbol with unvalid R name. (Related to
  [\#234](https://github.com/openbiox/UCSCXenaShiny/issues/234))
- Added more informative error for scatter plot in General Analysis tab.
  ([\#233](https://github.com/openbiox/UCSCXenaShiny/issues/233))
- Reversed default color setting for groups in survival analysis to fit
  conventional color grouping (in Xena).
  ([\#232](https://github.com/openbiox/UCSCXenaShiny/issues/232), thanks
  to feedback from [Enrique](https://github.com/quiquemedina))
- Supported known science palette and custom colors for survival
  analysis in Quick PanCan Analysis tab.

## UCSCXenaShiny 1.1.3

CRAN release: 2021-11-29

- Data check.
  ([\#225](https://github.com/openbiox/UCSCXenaShiny/issues/225))
- Fixed survival KM plot output issue due to `ggsave()` failure.
  ([\#230](https://github.com/openbiox/UCSCXenaShiny/issues/230))

## UCSCXenaShiny 1.1.2

CRAN release: 2021-11-17

- Fixed value query for gene signature with `purrr` lambda function.
- Updated dataset doc.
- Uploaded zenodo link.
- Uploaded this tool to conda forge, the user can install it from conda
  now. For more details, please read the README file.

## UCSCXenaShiny 1.1.1

CRAN release: 2021-07-30

- Updated citation.
- Fixed the data loading bug due to function scope problem
  ([\#222](https://github.com/openbiox/UCSCXenaShiny/issues/222)).

## UCSCXenaShiny 1.1.0

CRAN release: 2021-07-16

- Supported uploading data files for analysis.
- Improved user experience.
- Added more contents in README.
- Corrected LICENSE (this package is built on the top of code with GPLv3
  LICENCE).
- Added docker image for UCSCXenaShiny
  <https://hub.docker.com/r/shixiangwang/ucscxenashiny>.

## UCSCXenaShiny 1.0.1

CRAN release: 2021-06-23

- Added a package doc site link in usage navbar list.
- Fixed bug of PCAWG survival analysis
  ([\#209](https://github.com/openbiox/UCSCXenaShiny/issues/209)).

## UCSCXenaShiny 1.0.0

CRAN release: 2021-05-16

This the first stable and formal version of `UCSCXenaShiny`. We have
refactored the whole R package and corresponding Shiny for service.

Enjoy yourself.
