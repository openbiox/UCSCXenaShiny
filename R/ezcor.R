#' Run correlation between two variables and support group by a variable
#'
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character, the first variable in correlation
#' @param var2 a `character, the second variable in correlation
#' @param cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param adjust_method What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#' @param use	use="pairwise" will do pairwise deletion of cases. use="complete" will select just complete cases
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"**"; P < 0.05,"*"; P >=0.05,""
#' @param verbose if `TRUE`, print extra info.

#' @import psych
#' @import purrr
#' @importFrom purrr set_names
#'
#' @return a `data.frame`
#' @author Yi Xiong
#' @export
#' @examples
#' data("exprSet", package = "ezcor", envir = environment())
#' g1 <- colnames(exprSet)[3]
#' g2 <- colnames(exprSet)[4]
#' res <- ezcor::ezcor(data= exprSet,
#'                    split = TRUE,
#'                    split_var = "tissue",
#'                    var1 = g1,
#'                    var2 = g2,
#'                    cor_method = "spearman",
#'                    adjust_method = "none",
#'                    sig_label = TRUE,
#'                    verbose = TRUE)

ezcor <- function(data = NULL,
                  split = FALSE,
                  split_var = NULL,
                  var1 = NULL,
                  var2 = NULL,
                  cor_method = "pearson",
                  adjust_method = "none",
                  use = "complete",
                  sig_label = TRUE,
                  verbose = TRUE) {
  stopifnot(is.data.frame(data))
  ss <- data
  if (!var1 %in% colnames(ss)) {
    stop("the first variable is unavailable in the dataset!")
  }
  if (!var2 %in% colnames(ss)) {
    stop("the second variable is unavailable in the dataset!")
  }
  if (length(var1) != 1) {
    stop("only one element is needed in the first variable!")
  }
  if (length(var2) != 1) {
    stop("only one element is needed in the second variable!")
  }
  if (split == TRUE) {
    if (!split_var %in% colnames(ss)) {
      stop("split variable is unavailable in the dataset!")
    }
    #index
    n = which(colnames(ss) %in% split_var)
    sss <- with(ss, split(ss, ss[, n]))
    s <- names(sss)
    ##calculate correlation
    cor2var <- purrr::map(s, purrr::safely(function(x) {
      #x = s[1]
      sss_sub <- sss[[x]]
      dd <-
        psych::corr.test(
          as.numeric(sss_sub[, var1]),
          as.numeric(sss_sub[, var2]),
          method = cor_method,
          adjust = adjust_method,
          use =  use
        )
      #dd <- stats::cor.test(as.numeric(sss_can[,var1]),as.numeric(sss_can[,var2]), type = cor_method)
      ddd <-
        data.frame(
          cor = dd$r,
          p.value = dd$p,
          method = cor_method,
          adjust = adjust_method,
          v1 = var1,
          v2 = var2 ,
          stringsAsFactors = F
        )
      ddd$group <- x
      return(ddd)
    })) %>% purrr::set_names(s)

    cor2var <- cor2var %>%
      purrr::map( ~ .x$result) %>%
      purrr::compact()
    cor2var_df <- do.call(rbind.data.frame, cor2var)

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(
        cor2var_df$p.value < 0.05,
        ifelse(
          cor2var_df$p.value < 0.001,
          "***",
          ifelse(cor2var_df$p.value < 0.01, "**", "*")
        ),
        ""
      )
    }
    return(cor2var_df)
  }

  if (split == FALSE) {
    sss <- ss
    dd <-
      psych::corr.test(
        as.numeric(sss[, var1]),
        as.numeric(sss[, var2]),
        method = cor_method,
        adjust = adjust_method,
        use =  use
      )
    ddd <-
      data.frame(
        cor = dd$r,
        p.value = dd$p,
        method = cor_method,
        adjust = adjust_method,
        v1 = var1,
        v2 = var2,
        stringsAsFactors = F
      )

    cor2var_df <- ddd

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(
        cor2var_df$p.value < 0.05,
        ifelse(
          cor2var_df$p.value < 0.001,
          "***",
          ifelse(cor2var_df$p.value < 0.01, "**", "*")
        ),
        ""
      )

    }
    return(cor2var_df)
  }
}

#' Run correlation between two variables in a batch mode and support group by a variable
#'
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `vector` containing variables in a batch mode
#' @param cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param adjust_method What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"\\**"; P < 0.05,"*"; P >=0.05,""
#' @param use	use="pairwise" will do pairwise deletion of cases. use="complete" will select just complete cases
#' @param parallel if `TRUE`, do parallel computation by **furrr** package.
#' @param verbose if `TRUE`, print extra info. If `parallel` is `TRUE`,
#' set `verbose` to `FALSE` may speed up.
#'
#' @import dplyr
#' @import psych
#' @import purrr
#' @importFrom purrr set_names
#'
#' @return a `data.frame`
#' @author Yi Xiong, Shixiang Wang
#' @export
#'
#' @examples
#' data("exprSet", package = "ezcor", envir = environment())
#' target_variable = colnames(exprSet)[3]
#' genelist <- colnames(exprSet)
#' genelist <-  setdiff(genelist,c("patient","tissue",target_variable))
#' res <- ezcor::ezcor_batch(data = exprSet,
#'                           var1 = target_variable,
#'                           var2 = genelist,
#'                           split = TRUE,
#'                           split_var = "tissue",
#'                           cor_method = "pearson",
#'                           adjust_method = "none",
#'                           use = "complete",
#'                           sig_label = TRUE,
#'                           parallel = FALSE,
#'                           verbose = TRUE)

ezcor_batch <- function(data,
                        var1,
                        var2,
                        split = FALSE,
                        split_var = NULL,
                        cor_method = "pearson",
                        adjust_method = "none",
                        use = "complete",
                        sig_label = TRUE,
                        parallel = FALSE,
                        verbose = FALSE) {
  stopifnot(is.data.frame(data))

  ss <- data

  if (!var1 %in% colnames(ss)) {
    stop("the first variable is unavailable in the dataset!")
  }
  if (!var2 %in% colnames(ss)) {
    stop("the second variable is unavailable in the dataset!")
  }
  if (length(var1) != 1) {
    stop("only one element is needed in the var1 variable!")
  }
  if (split == TRUE) {
    if (!split_var %in% colnames(ss)) {
      stop("split variable is unavailable in the dataset!")
    }
    all_cols <- unique(c(var1, var2, split_var))
    #ss <- data
    ss <- ss[, all_cols]
    if (parallel) {
      if (!requireNamespace("furrr")) {
        stop("Please install 'furrr' package firstly!")
      }
      if (length(var2) < 200) {
        if (verbose)
          message("Warning: variable < 200, parallel computation is not recommended!")
      }

      if (!requireNamespace("furrr")) {
        stop("Please install 'furrr' package firstly!")
      }
      oplan <- future::plan()
      future::plan("multiprocess")
      on.exit(future::plan(oplan), add = TRUE)
      res <- furrr::future_map(
        var2,
        ezcor_caller,
        data = ss,
        split_var = split_var,
        split = split,
        var1 = var1,
        cor_method = cor_method,
        adjust_method = adjust_method,
        use = use,
        sig_label = sig_label,
        verbose = verbose
      )
    } else {
      res <- purrr::map(
        var2,
        ezcor_caller,
        data = ss,
        split_var = split_var,
        split = split,
        var1 = var1,
        cor_method = cor_method,
        adjust_method = adjust_method,
        use = use,
        sig_label = sig_label,
        verbose = verbose
      ) %>% purrr::set_names(var2)
    }
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }
  else{
    all_cols <- unique(c(var1, var2))
    ss <- ss[, all_cols]
    res <- purrr::map(
      var2,
      ezcor_caller,
      data = ss,
      split_var = split_var,
      split = split,
      var1 = var1,
      cor_method = cor_method,
      adjust_method = adjust_method,
      use = use,
      sig_label = sig_label,
      verbose = verbose
    ) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }

}

ezcor_caller <- function(var2,
                         data,
                         split_var = split_var,
                         split = split,
                         var1 = var1,
                         cor_method = cor_method,
                         adjust_method = adjust_method,
                         use = use,
                         sig_label = sig_label,
                         verbose = verbose) {
  ezcor(
    data = data,
    split_var = split_var,
    split = split,
    var1 = var1,
    var2 = var2,
    cor_method = cor_method,
    adjust_method = adjust_method,
    use = use,
    sig_label = sig_label,
    verbose = verbose
  )
}
