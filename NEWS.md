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