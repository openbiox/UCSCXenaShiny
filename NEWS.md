# UCSCXenaShiny 2.0.0

See the [UCSCXenaShiny v2 Book](https://lishensuo.github.io/UCSCXenaShiny_Book/index.html) for a comprehensive guidance.

## New Features

### Datasets

- `load_data("tcga_PW")`: ssGSEA scores of HALLMARK, KEGG, IOBR terms for TCGA samples.
- `load_data("tcga_PW_meta")`: metadata annotation for HALLMARK, KEGG, IOBR terms.
- `load_data("pcawg_TIL")`: PCAWG TIL data.
- `load_data("pcawg_PW")`: ssGSEA scores of HALLMARK, KEGG, IOBR terms for PCAWG samples.
- and more.

### R Package Functions

- `.opt_pancan` : Default setting for alternative TPC datasets.
- `mol_quick_analysis()`: Quick molecule analysis and report generation based on TCGA dataset.
- `query_tcga_group()`: Group TPC samples by build-in or custom phenotype and support filtering or merging operations.
- `vis_dim_dist()`: Visualize the distribution difference of TCGA samples after dimension reduction analysis.
- `vis_identifier_dim_dist()`: Visualize the distribution difference of samples after Molecule Identifier dimension reduction analysis.
- `vis_toil_Mut()`: Visualize molecular profile difference between mutation and wild status of queried gene.
- `vis_toil_Mut_cancer()`: Visualize molecular profile difference between mutation and wild status of queried gene in Single Cancer Type

### Shiny application

- Homepage
  - Added slicker gallery to display page summary;
  - Added report generation for TCGA pan-cancer exploration.
  
- General Dataset  Analysis

  - Added one general dimension reduction analysis module.

- Quick TPC Analysis

  - Added one module for association analysis between molecule and pathway;
  - Added one module for association analysis between molecule and mutation;
  - Added one module for dimension reduction analysis.

- Personalized Analysis

  - Designed personalized TPC analysis pipelines for based on 3 methods and 3 modes.

- Download

  - Added two modules for exact subset of integrated TPC data and UCSCXena datasets.

## Enhancements

- Supported getting more flexible methylation value.

```r
UCSCXenaShiny::get_pancan_methylation_value(
  "RCAN2",
  rule_out = c("cg21115430", "cg19452802"), 
  aggr = "Q75"
)
```

- Supported installing the package from r-universe (<https://openbiox.r-universe.dev/UCSCXenaShiny>).

- Supported alternative molecular profiling datasets for quick and personalized TPC analysis.

## Bug Fixes

- Merged data with unequal size in pan-cancer data query with a gene signature (#283), the fix also enhance the sample names match.

Test code:

```r
vis_gene_tmb_cor("`ZFAT-AS1` + `SNORD116-1` + SPATA31D1", data_type = "methylation")
```

# UCSCXenaShiny 1.1.11

- Fixed check warning in pan-cancer radar plot.

# UCSCXenaShiny 1.1.10

- Fixed check issue due to internet access (#253).

# UCSCXenaShiny 1.1.9

- Added cancer type control for PCAWG survival analysis.
- Added TCGA batch id from MDA.

# UCSCXenaShiny 1.1.8

- Fixed issue to set mislabelled color in pcawg pan-cancer analysis (#247, Thanks to Tangjian Li).
- Fixed issue in querying gene signature in general analysis page (#244).
- Fixed issue "Radar plot “stemness” does not plot the “ACC” tumor type" (#242).

# UCSCXenaShiny 1.1.7

- Added option `include.Tumor.only` to control if include type 
- Set default theme if `flatly` not available.
- Added example to generate radar plot, close #239

# UCSCXenaShiny 1.1.6

- Added description of extra datasets.

# UCSCXenaShiny 1.1.5

- Fixed survival KM plot output issue due to `ggsave()` failure in General Analysis page. (#230)

# UCSCXenaShiny 1.1.4

- Fixed the colnames being changed by `as.data.frame()` when querying a symbol with unvalid R name. (Related to #234)
- Added more informative error for scatter plot in General Analysis tab. (#233)
- Reversed default color setting for groups in survival analysis to fit conventional color grouping (in Xena).
(#232, thanks to feedback from [Enrique](https://github.com/quiquemedina))
- Supported known science palette and custom colors for survival analysis in Quick PanCan Analysis tab.

# UCSCXenaShiny 1.1.3

- Data check. (#225)
- Fixed survival KM plot output issue due to `ggsave()` failure. (#230)

# UCSCXenaShiny 1.1.2

- Fixed value query for gene signature with `purrr` lambda function.
- Updated dataset doc.
- Uploaded zenodo link.
- Uploaded this tool to conda forge, the user can install it from conda now.
For more details, please read the README file.

# UCSCXenaShiny 1.1.1

- Updated citation.
- Fixed the data loading bug due to function scope problem (#222).

# UCSCXenaShiny 1.1.0

- Supported uploading data files for analysis.
- Improved user experience.
- Added more contents in README.
- Corrected LICENSE (this package is built on the top of code with GPLv3 LICENCE).
- Added docker image for UCSCXenaShiny <https://hub.docker.com/r/shixiangwang/ucscxenashiny>.

# UCSCXenaShiny 1.0.1

- Added a package doc site link in usage navbar list.
- Fixed bug of PCAWG survival analysis (#209).

# UCSCXenaShiny 1.0.0

This the first stable and formal version of `UCSCXenaShiny`. We have refactored
the whole R package and corresponding Shiny for service.

Enjoy yourself.