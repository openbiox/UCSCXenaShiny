# UCSCXenaShiny <img src="https://github.com/openbiox/openbiox-wiki/blob/master/static/img/logo-long.png" align="right" width="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/UCSCXenaShiny)](https://cran.r-project.org/package=UCSCXenaShiny)
[![](http://cranlogs.r-pkg.org/badges/grand-total/UCSCXenaShiny?color=orange)](https://cran.r-project.org/package=UCSCXenaShiny)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![R-CMD-check](https://github.com/openbiox/UCSCXenaShiny/workflows/R-CMD-check/badge.svg)

The goal of **UCSCXenaShiny** is to provide a web app for downloading,
analyzing and visualizing datasets from [UCSC
Xena](https://xenabrowser.net/datapages/), which is a collection of
UCSC-hosted public databases such as TCGA, ICGC, TARGET, GTEx, CCLE, and
others. Databases are normalized so they can be combined, linked,
filtered, explored and downloaded.

Please cite the following article when using `UCSCXenaShiny`:

Wang, S.; Xiong, Y.; Gu, K.; Zhao, L.; Li, Y.; Zhao, F.; Li, X.; Liu, X. UCSCXenaShiny: An R Package for Exploring and Analyzing UCSC Xena Public Datasets in Web Browser. Preprints 2020, 2020070179 (doi: 10.20944/preprints202007.0179.v1).

## :cloud: Use on cloud

If you don't want to install R and packages locally, or you have no programming experience, try using this tool on Hiplot platform: <https://shiny.hiplot.com.cn/ucsc-xena-shiny/>.

## :arrow\_double\_down: Installation

You can install stable release of **UCSCXenaShiny** from CRAN with:

``` r
install.packages("UCSCXenaShiny")
```

You can install the development version of **UCSCXenaShiny** from Github
with:

``` r
remotes::install_github("openbiox/XenaShiny")
```

Or Gitee (for Chinese users):

```r
remotes::install_git("https://gitee.com/XenaShiny/UCSCXenaShiny")
```

## :beginner: Usage

First load package:

``` r
library(UCSCXenaShiny)
```

Then run the following command in console:

``` r
app_run()
```

For advanced users, useful datasets/functions provided by this package are described in vignette.

## :writing\_hand: Author

  - [Shixiang Wang](https://github.com/ShixiangWang)
  - [Yi Xiong](https://github.com/Byronxy)
  - [Longfei Zhao](https://github.com/longfei8533)
  - [Kai Gu](https://github.com/kaigu1990)
  - [Yin Li](https://github.com/yinlisssss)
  - [Fei Zhao](https://github.com/fei0810)

## :page\_with\_curl: LICENSE

MIT © [Openbiox](https://github.com/openbiox)
