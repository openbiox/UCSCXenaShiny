#' Keep Only Columns Used for Sample Selection
#'
#' @param x a `data.frame` with many columns.
#' @param keep_sam_cols if `TRUE` (default), keep columns with
#' pattern 'sample', 'patient', etc.
#' @param return_idx if `TRUE` (default), return index of 5 (at most) columns, it is useful in Shiny.
#'
#' @return a `data.frame` or a `list`.
#' @export
keep_cat_cols <- function(x, keep_sam_cols = TRUE, return_idx = TRUE) {
  stopifnot(is.data.frame(x))
  x <- as.data.frame(x)
  samp_pass <- grepl("sample|patient|donor|specimen|submitter", colnames(x), ignore.case = TRUE)
  pass_col <- function(x) {
    if (length(x) < 100) {
      length(unique(x)) < length(x) / 1.5
    } else {
      length(unique(x)) < length(x) / 5
    }
  }
  cols_pass <- sapply(x, pass_col)

  if (keep_sam_cols) {
    x <- x[, samp_pass | cols_pass, drop = FALSE]
  } else {
    x <- x[, cols_pass, drop = FALSE]
  }

  if (return_idx) {
    samp_pass <- grepl("sample|patient|donor|specimen|submitter", colnames(x), ignore.case = TRUE)
    s <- if (any(samp_pass)) which(samp_pass)[1] else c()
    nn <- sum(!samp_pass)
    s <- c(s, sample(which(!samp_pass), min(5, nn) - length(s)))
    return(list(data = x, idx = s))
  } else {
    x
  }
}

get_run_mode <- function() {
  getOption("xena.runMode", default = "client")
}

get_cache_dir <- function() {
  path <- getOption("xena.cacheDir", default = file.path(tempdir(), "UCSCXenaShiny"))
  # message("Path for storing data is ", path, ". You can set it with options(xena.cacheDir = xxx)")
  path
}

get_zenodo_dir <- function() {
  path <- getOption("xena.zenodoDir", default = system.file("extdata", package = "UCSCXenaShiny"))
  # message("Path for storing zenodo extra data is ", path, ". You can set it with options(xena.zenodoDir = xxx)")
  path
}

rm_cache_dir <- function() {
  unlink(get_cache_dir(), recursive = TRUE)
}

# https://stackoverflow.com/questions/6029743/merge-or-combine-by-rownames
mbind <- function(...) {
  Reduce( function(x,y){cbind(x,y[match(row.names(x),row.names(y)),])}, list(...) )
}
