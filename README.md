# UCSCXenaShiny <img src="https://raw.githubusercontent.com/openbiox/wiki/master/static/img/logo-long.png" align="right" width="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/UCSCXenaShiny)](https://cran.r-project.org/package=UCSCXenaShiny)
[![](https://cranlogs.r-pkg.org/badges/grand-total/UCSCXenaShiny?color=orange)](https://cran.r-project.org/package=UCSCXenaShiny)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![R-CMD-check](https://github.com/openbiox/UCSCXenaShiny/workflows/R-CMD-check/badge.svg)
[![Lines Of
Code](https://tokei.rs/b1/github/openbiox/UCSCXenaShiny?category=code)](https://github.com/openbiox/UCSCXenaShiny/)
[![check in Biotreasury](https://img.shields.io/badge/Biotreasury-collected-brightgreen)](https://biotreasury.rjmart.cn/#/tool?id=61144)
[![Gitter](https://badges.gitter.im/ShixiangWang/community.svg)](https://gitter.im/ShixiangWang/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

**UCSCXenaShiny** is an R package for interactively exploring UCSC Xena.
It is mainly designed to provide a web app (built on the top of [`{shiny}`](https://shiny.rstudio.com/) framework and [`{UCSCXenaTools}`](https://github.com/ropensci/UCSCXenaTools/) package) for downloading,
analyzing and visualizing datasets from [UCSC
Xena](https://xenabrowser.net/datapages/).

![Alt](https://repobeats.axiom.co/api/embed/2f1f0c9a7a5c3c710febb3ee3d1e26f249d5a3ed.svg "Repobeats analytics image")

Please cite the following article when you used **UCSCXenaShiny** in your study:

---------------

Shixiang Wang<sup>\#</sup>, Yi Xiong<sup>\#</sup>, Longfei Zhao<sup>\#</sup>, Kai Gu<sup>\#</sup>, Yin Li, Fei Zhao, Jianfeng Li, Mingjie Wang, Haitao Wang, Ziyu Tao, Tao Wu, Yichao Zheng, Xuejun Li, Xue-Song Liu, UCSCXenaShiny: An R/CRAN Package for Interactive Analysis of UCSC Xena Data, Bioinformatics, 2021;, btab561, https://doi.org/10.1093/bioinformatics/btab561.

---------------

## :cloud: Use on cloud

If you don't want to install R and packages locally, or you have no programming experience, try using this tool on `Hiplot-academic` platform (<https://shiny.hiplot-academic.com/ucsc-xena-shiny/>) or `shinyapps.io` (<https://shixiangwang.shinyapps.io/ucscxenashiny/>).

## :snake: Use with Conda

| Name | Downloads | Version | Platforms |
| --- | --- | --- | --- |
| [![Conda Recipe](https://img.shields.io/badge/recipe-r--ucscxenashiny-green.svg)](https://github.com/conda-forge/r-ucscxenashiny-feedstock) | [![Conda Downloads](https://img.shields.io/conda/dn/conda-forge/r-ucscxenashiny.svg)](https://github.com/conda-forge/r-ucscxenashiny-feedstock) | [![Conda Version](https://img.shields.io/conda/vn/conda-forge/r-ucscxenashiny.svg)](https://github.com/conda-forge/r-ucscxenashiny-feedstock) | [![Conda Platforms](https://img.shields.io/conda/pn/conda-forge/r-ucscxenashiny.svg)](https://github.com/conda-forge/r-ucscxenashiny-feedstock) |

Install from `conda-forge` channel with:

```bash
conda install -c conda-forge r-ucscxenashiny
```

It is possible to list all of the versions of `r-ucscxenashiny` available on your platform with:

```bash
conda search r-ucscxenashiny --channel conda-forge
```

## :package: Use with Docker

<img alt="Docker Image Version (latest by date)" src="https://img.shields.io/docker/v/shixiangwang/ucscxenashiny?color=blue"> <img alt="Docker Image Size (latest by date)" src="https://img.shields.io/docker/image-size/shixiangwang/ucscxenashiny"> <img alt="Docker Pulls" src="https://img.shields.io/docker/pulls/shixiangwang/ucscxenashiny">

UCSCXenaShiny has corresponding docker image at <https://hub.docker.com/r/shixiangwang/ucscxenashiny/>, you can install the latest version with:

```bash
docker pull shixiangwang/ucscxenashiny
```

All versions can be found at <https://hub.docker.com/r/shixiangwang/ucscxenashiny/tags/>.
To use a specified version (e.g., `v1.0.2`), run the following command to install:

```bash
docker pull shixiangwang/ucscxenashiny:v1.0.2
```

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

## :arrow_double_down: Manual installation

You can install stable release of **UCSCXenaShiny** from CRAN with:

```r
install.packages("UCSCXenaShiny")
```

You can install the development version of **UCSCXenaShiny** from Github
with:

```r
remotes::install_github("openbiox/UCSCXenaShiny")
```

Or Gitee (for Chinese users):

```r
remotes::install_git("https://gitee.com/XenaShiny/UCSCXenaShiny")
```

Other dependent R packages specific to the Shiny application will be automatically installed when you start with `app_run()` command. If you failed to install **UCSCXenaShiny**, please check if the following system dependencies have been properly installed or see [**Troubleshooting**](#hammer_and_wrench-troubleshooting) section for specific installation issues.

### System dependencies installation

When you use Windows/MacOS, please skip reading this sub-section.

As Linux distributions are very diverse,  here we only test the installation of **UCSCXenaShiny** on common used Ubuntu/CentOS. If you are using other Linux distributions, you need to solve the system dependencies installation problems yourself when you encounter R package installation errors. However, the installation of system dependencies on Ubuntu/CentOS could be very good references.

> Please note all commands below are execuated with `root`.

**Ubuntu**:

```bash
apt update -y && apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev \
	libgmp3-dev libmpfr-dev
```

**CentOS**:

```bash
yum update -y && yum install -y libcurl-devel openssl-devel libxml2-devel \
	gmp-devel mpfr-devel libjpeg-devel cairo-devel
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
  - Upload custom data for general analysis: <https://www.bilibili.com/video/BV1pf4y1j7eP>.
  Please note that upload files must in the format of csv/tsv (gz compression is supported) and
  files must have extension tsv/txt/csv/gz.

## :hammer_and_wrench: Troubleshooting

1. `ERROR: dependencies ‘gmp’, ‘Rmpfr’ are not available for package ‘PMCMRplus’` or `ERROR: dependency ‘pairwiseComparisons’ is not available for package ‘ggstatsplot’`.

   **Your operating system lacks `gmp` and `Rmpfr` development libraries.**
   
   ```bash
   # Ubuntu
   apt install -y libgmp3-dev libmpfr-dev
   # CentOS
   yum install -y gmp-devel mpfr-devel
   ```
   
2. `installation of package ‘gridtext’ had non-zero exit status` with error info `grid-renderer.h:61:94: error: no matching function for call to ‘Rcpp::Vector<10, Rcpp::PreserveStorage>::Vector(int, bool&, const GraphicsContext&)’`.
   
    **You have an older C++ version which cannot support C++11 features. This error seems only happen on CentOS. Install a newer C++ and set it as default compiler for R would fix this problem.** 
    
    ```bash
    yum install centos-release-scl
    yum install devtoolset-9
    # If you use your non-root account to install packages, 
    # change /root to /home/your_id in the following command
    mkdir -p /root/.R
    vi /root/.R/Makevars 
    ```
    
     **Append content to the openning file.**

    ```bash
    CXX11=/opt/rh/devtoolset-9/root/usr/bin/g++ -std=c++11
    ```


3. `installation of package ‘nloptr’ had non-zero exit status` with error info `libtool: link: ERROR: no information for variable 'AR' cru`.

   **The latest version of `nloptr` can only support R>=4.0. When you are using R3.6 or below would have this issue. So install an older version in R console can fix this.**

   ```R
   packageurl <- "https://cran.r-project.org/src/contrib/Archive/nloptr/nloptr_1.2.1.tar.gz"
   
   install.packages(packageurl, repos=NULL, type="source")
   ```

   > Reference: https://stackoverflow.com/questions/62900525/install-lme4-from-cran-on-ubuntu

4. `package ‘pacman’ is not available` or similar.

   **Install it by hand in R console.**

   ```R
   install.packages("pacman")
   ```

5. `there is no package called ‘shinythemes’` or similar.

   **Install it by hand in R cosole.**

   ```R
   install.packages("shinythemes")
   ```

6. Install package `gganatogram` failed or similar.

   **Install it by hand in R cosole.**

   ```R
   if (!requireNamespace("gganatogram")) {
      library(remotes)
      tryCatch(
         remotes::install_github("jespermaag/gganatogram"),
         error = function(e) {
            remotes::install_git("https://gitee.com/XenaShiny/gganatogram")
         }
      )
   }
   ```

7. Install package `ggradar` failed or similar.

   **Install it by hand in R cosole.**

   ```R
   if (!requireNamespace("ggradar")) {
      library(remotes)
      tryCatch(
         remotes::install_github("ricardo-bion/ggradar"),
         error = function(e) {
            remotes::install_git("https://gitee.com/XenaShiny/ggradar")
         }
      )
   }
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
