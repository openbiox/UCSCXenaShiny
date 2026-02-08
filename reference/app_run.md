# Run UCSC Xena Shiny App

Run UCSC Xena Shiny App

## Usage

``` r
app_run(runMode = "client", port = getOption("shiny.port"))
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

## Examples

``` r
if (FALSE) { # \dontrun{
app_run()
} # }
```
