#' Pipe Operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

`%:::%` <- function(pkg, fun, inherits = TRUE) {
  get(fun,
    envir = asNamespace(pkg),
    inherits = inherits
  )
}
