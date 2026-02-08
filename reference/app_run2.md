# Run UCSC Xena Shiny App with specifc content

Run UCSC Xena Shiny App with specifc content

## Usage

``` r
app_run2(
  runMode = "client",
  port = getOption("shiny.port"),
  content = c("a", "s", "q", "p", "d")
)
```

## Arguments

- runMode:

  default is 'client' for personal user, set it to 'server' for running
  on server.

- port:

  The TCP port that the application should listen on. If the `port` is
  not specified, and the `shiny.port` option is set (with
  `options(shiny.port = XX)`), then that port will be used. Otherwise,
  use a random port between 3000:8000, excluding ports that are blocked
  by Google Chrome for being considered unsafe: 3659, 4045, 5060, 5061,
  6000, 6566, 6665:6669 and 6697. Up to twenty random ports will be
  tried.

- content:

  Modules to enable.

  - `a`: all modules

  - `s`: only loading basic modules;

  - `q`: add tpc (TCGA, PCAWG, CCLE) modules

  - `p`: add tpc pipelines

  - `d`: add pharmcogenomics modules

## Examples

``` r
if (FALSE) { # \dontrun{
app_run2(content = "s")
} # }
```
