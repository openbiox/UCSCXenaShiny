
<!-- README.md is generated from README.Rmd. Please edit that file -->

# XenaShiny <img src="https://github.com/openbiox/openbiox-wiki/blob/master/static/img/logo-long.png" align="right" width="200"/>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![emoji-log](https://cdn.rawgit.com/ahmadawais/stuff/ca97874/emoji-log/non-flat-round.svg)](https://github.com/ahmadawais/Emoji-Log/)
[![Travis build
status](https://travis-ci.org/openbiox/XenaShiny.svg?branch=master)](https://travis-ci.org/openbiox/XenaShiny)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/openbiox/XenaShiny?branch=master&svg=true)](https://ci.appveyor.com/project/openbiox/XenaShiny)
[![Coverage
status](https://codecov.io/gh/openbiox/XenaShiny/branch/master/graph/badge.svg)](https://codecov.io/github/openbiox/XenaShiny?branch=master)

The goal of **UCSCXenaShiny** is to provide a web app for downloading,
analyzing and visulizing datasets from [UCSC
Xena](https://xenabrowser.net/datapages/), which is a collection of
UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and
others. Databases are normalized so they can be combined, linked,
filtered, explored and downloaded.

## Installation

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
