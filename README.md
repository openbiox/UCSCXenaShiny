# UCSCXenaShiny <img src="https://raw.githubusercontent.com/openbiox/wiki/master/static/img/logo-long.png" align="right" width="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/UCSCXenaShiny)](https://cran.r-project.org/package=UCSCXenaShiny)
[![](https://cranlogs.r-pkg.org/badges/grand-total/UCSCXenaShiny?color=orange)](https://cran.r-project.org/package=UCSCXenaShiny)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![R-CMD-check](https://github.com/openbiox/UCSCXenaShiny/workflows/R-CMD-check/badge.svg)
[![Lines Of
Code](https://tokei.rs/b1/github/openbiox/UCSCXenaShiny?category=code)](https://github.com/openbiox/UCSCXenaShiny/)

**UCSCXenaShiny** is an R package for interactively exploring UCSC Xena.
It is mainly designed to provide a web app (built on the top of [`{shiny}`](https://shiny.rstudio.com/) framework and [`{UCSCXenaTools}`](https://github.com/ropensci/UCSCXenaTools/) package) for downloading,
analyzing and visualizing datasets from [UCSC
Xena](https://xenabrowser.net/datapages/).

Please cite the following article when you used **UCSCXenaShiny** in your study:

---------------

Wang, S.; Xiong, Y.; Gu, K.; Zhao, L.; Li, Y.; Zhao, F.; Li, X.; Liu, X. UCSCXenaShiny: An R Package for Exploring and Analyzing UCSC Xena Public Datasets in Web Browser. Preprints 2020, 2020070179 (doi: 10.20944/preprints202007.0179.v1).

---------------

## :cloud: Use on cloud

If you don't want to install R and packages locally, or you have no programming experience, try using this tool on Hiplot platform: <https://shiny.hiplot.com.cn/ucsc-xena-shiny/>.

## :package: Use with Docker

<img alt="Docker Image Version (latest by date)" src="https://img.shields.io/docker/v/shixiangwang/ucscxenashiny?color=blue"> <img alt="Docker Image Size (latest by date)" src="https://img.shields.io/docker/image-size/shixiangwang/ucscxenashiny"> <img alt="Docker Pulls" src="https://img.shields.io/docker/pulls/shixiangwang/ucscxenashiny">

UCSCXenaShiny has corresponding docker image at <https://hub.docker.com/r/shixiangwang/ucscxenashiny/>, you can install it with:

```bash
docker pull shixiangwang/ucscxenashiny
```

All builds can be found at <https://hub.docker.com/r/shixiangwang/ucscxenashiny/builds/>.

Then run the docker image with:

```bash
docker run -d --name xenashiny -p 3838:3838 shixiangwang/ucscxenashiny
```

Now you should find the Shiny when you open URL `http://127.0.0.1:3838` with your web browser.
If you deploy the docker in a public (cloud) Linux server, change `127.0.0.1` to the host IP.

You can manage the deployed container with the following commands:

```bash
# Stop the container
docker stop xenashiny
# Start the container
docker start xenashiny
```

## :arrow_double_down: Installation

You can install stable release of **UCSCXenaShiny** from CRAN with:

```r
install.packages("UCSCXenaShiny")
```

You can install the development version of **UCSCXenaShiny** from Github
with:

```r
remotes::install_github("openbiox/XenaShiny")
```

Or Gitee (for Chinese users):

```r
remotes::install_git("https://gitee.com/XenaShiny/UCSCXenaShiny")
```

## :beginner: Usage

First load package:

```r
library(UCSCXenaShiny)
```

Start Shiny in your R console (ignore this if you just want to use functions in this package):

```r
app_run()
# At default, the Shiny is running under client mode
# It means the data queried from remote UCSC Xena server will
# be saved to temporary directory determined by R
# If you frequently use this tool or deploy this tool as a web service for multiple users
# It is recommended to run it with 'server' mode
# i.e.,
#
# app_run("server")
```

If you want deploy UCSC Xena Shiny with Shiny Server, please copy `App.R` and `www/` directory under [`shinyapp`](https://github.com/openbiox/UCSCXenaShiny/tree/master/inst/shinyapp).
`xena.runMode` on the top of `App.R` is recommended to set as `"server"` instead of `"client"` (default).

For advanced users, examples for illustrating useful functions to obtain and analyze data are described in vignette.

All exported data and functions are organized at [here](https://openbiox.github.io/UCSCXenaShiny/reference/index.html).

`xena.cacheDir` and `xena.zenodoDir` are two options to control where to store data.

e.g.,

```r
options(xena.cacheDir = "/home/xxx/xena_data")
```

Option `xena.runMode` can be used to control the way how the Shiny works.
It can be 'client' or 'server'. You can directly set it in `app_run()`.

## :movie_camera: Videos

- Home page: <https://www.bilibili.com/video/bv1R84y1F7WT>.
- Repository page: <https://www.bilibili.com/video/bv1Mq4y1J73o>.
- General Analysis page: <https://www.bilibili.com/video/bv1p64y1U7iU>.
- Quick PanCan Analysis page: <https://www.bilibili.com/video/bv1o64y1y7jW>.
- Features:
  - Genomic Signature: <https://www.bilibili.com/video/bv1zB4y1c7eB>.

## :hammer_and_wrench: Troubleshooting

- `ERROR: dependencies ‘gmp’, ‘Rmpfr’ are not available for package ‘PMCMRplus’` or `ERROR: dependency ‘pairwiseComparisons’ is not available for package ‘ggstatsplot’` on Linux when installing `ggstatsplot` package.

It seems that your operating system lacks `gmp` and `Rmpfr` development libraries.

If you use Ubuntu, you can install these dependencies:

```
sudo apt-get install libgmp3-dev
sudo apt-get install libmpfr-dev
```

## :writing_hand: Author

  - [Shixiang Wang](https://github.com/ShixiangWang)
  - [Yi Xiong](https://github.com/Byronxy)
  - [Longfei Zhao](https://github.com/longfei8533)
  - [Kai Gu](https://github.com/kaigu1990)
  - [Yin Li](https://github.com/yinlisssss)
  - [Fei Zhao](https://github.com/fei0810)

## :page_with_curl: LICENSE

GPLv3 © [Openbiox](https://github.com/openbiox)
