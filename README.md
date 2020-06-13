
<!-- README.md is generated from README.Rmd. Please edit that file -->

# XenaShiny <img src="https://github.com/openbiox/openbiox-wiki/blob/master/static/img/logo-long.png" align="right" width="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/UCSCXenaShiny)](https://cran.r-project.org/package=UCSCXenaShiny)
[![](http://cranlogs.r-pkg.org/badges/grand-total/UCSCXenaShiny?color=orange)](https://cran.r-project.org/package=UCSCXenaShiny)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of **UCSCXenaShiny** is to provide a web app for downloading,
analyzing and visulizing datasets from [UCSC
Xena](https://xenabrowser.net/datapages/), which is a collection of
UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and
others. Databases are normalized so they can be combined, linked,
filtered, explored and downloaded.

## Installation

You can install stable release of **UCSCXenaShiny** from CRAN with:

``` r
install.packages("UCSCXenaShiny")
```

You can install the development version of **UCSCXenaShiny** from Github
with:

``` r
remotes::install_github("openbiox/XenaShiny")
```

## Usage

First load package:

``` r
library(UCSCXenaShiny)
```

Then run the following command in console:

``` r
app_run()
```

## Developer

  - Shixiang Wang
  - Yi Xiong
  - Longfei Zhao
  - Kai Gu
  - Yin Li
  - Fei Zhao

## LICENSE

MIT © [Openbiox](https://github.com/openbiox)
