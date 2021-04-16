#' Analyze Association between Gene (Signature) and Drug Response
#'
#' @param gene_list a gene symbol list.
#' @param combine if `TRUE`, combine the expression of gene list as
#' a gene signature.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' \dontrun{
#' analyze_gene_drug_response("TP53")
#' analyze_gene_drug_response(c("TP53", "KRAS"))
#' analyze_gene_drug_response(c("TP53", "KRAS"), combine = TRUE)
#' }
analyze_gene_drug_response <- function(gene_list, combine = FALSE) {
  stopifnot(length(gene_list) > 0)
  on.exit(invisible(gc()))

  ccle_data <- load_data("ccle_expr_and_drug_response")

  if (any(gene_list %in% rownames(ccle_data$expr))) {
    expr <- ccle_data$expr[gene_list, , drop = FALSE]
  } else {
    stop("None of your input genes exists in CCLE data.")
  }
  
  if (combine) {
    expr <- t(apply(expr, 2, gm_mean))
    rownames(expr) <- "signature"
  }

  drug_ic50 <- ccle_data$drug_ic50
  drug_info <- ccle_data$drug_info
  tissues <- unique(drug_info[, c("CCLE Cell Line Name", "Site Primary")])
  expr <- expr[, tissues[["CCLE Cell Line Name"]], drop = FALSE]
  tissues <- tissues[["Site Primary"]]

  rm(ccle_data)

  drugCor <- c()
  for (i in 1:nrow(expr)) {
    gene.exp <- expr[i, ]
    tissues.mean <- aggregate(gene.exp, by = list(tissues), mean)
    rownames(tissues.mean) <- tissues.mean[, 1]
    controls <- tissues.mean[tissues, 2] # control of partial correlation

    for (j in 1:ncol(drug_ic50)) {
      d.IC50 <- drug_ic50[, j]
      # Spearman correlation coefficient, significance and number of cells have IC50 and expression
      # for pair of gene i and drug j
      #
      # partial correlation -- variance-covariance (mat); Spearman's cor (s);Remove NAs(na.rm=T)
      dg.cor <- pcor_test(d.IC50, gene.exp, controls,
        use = "mat", method = "s", na.rm = TRUE
      )
      drugCor <- rbind(drugCor, c(i, j, dg.cor$estimate, dg.cor$p.value, dg.cor$n))
    }
  }

  drugCor <- as.data.frame(drugCor)
  drugCor[, 1] <- rownames(expr)[drugCor[, 1]]
  drugCor[, 2] <- colnames(drug_ic50)[drugCor[, 2]]
  names(drugCor) <- c("genes", "drugs", "cor", "p.value", "num_of_cells")

  drugCor <- dplyr::left_join(drugCor,
    unique(drug_info[, c("Compound", "Target")]),
    by = c("drugs" = "Compound")
  )
  drugCor$fdr <- stats::p.adjust(drugCor$p.value, method = "fdr")

  # Normalized expression of imported gene set (normalized in the same tissue)
  CCLE_mat.sel.n <- expr
  tissues <- unique(tissues)
  for (i in tissues) {
    cells <- unique(drug_info[["CCLE Cell Line Name"]][drug_info[["Site Primary"]] %in% i])
    if (length(cells) == 1) {
      CCLE_mat.sel.n[, cells] <- 0
    } else {
      cells.n <- apply(CCLE_mat.sel.n[, cells, drop = FALSE], 1, Zscore)
      cells.n <- t(cells.n)
      CCLE_mat.sel.n[, cells] <- cells.n
    }
  }

  # Compute the expression diff between high IC50 cells and low IC50 cells for each tissue separately:
  for (i in 1:nrow(drugCor)) {
    g <- drugCor[i, 1]
    d <- drugCor[i, 2]
    d.IC50 <- drug_ic50[, d]
    z.diff <- c()
    for (j in tissues) {
      cells <- unique(drug_info[["CCLE Cell Line Name"]][drug_info[["Site Primary"]] %in% j])
      d.IC50.tissue <- d.IC50[cells]
      if (length(d.IC50.tissue) >= 5) { # at least 5 cell lines in the tissue
        d.IC50.tissue <- sort(d.IC50.tissue)
        l <- length(d.IC50.tissue)
        l <- floor(l / 2) # 按中位分的
        highIC50.cells <- head(names(d.IC50.tissue), l)
        lowIC50.cells <- tail(names(d.IC50.tissue), l)
        z.diff <- rbind(
          z.diff,
          mean(CCLE_mat.sel.n[g, highIC50.cells],
            na.rm = TRUE
          ) - mean(CCLE_mat.sel.n[g, lowIC50.cells],
            na.rm = TRUE
          )
        )
      }
    }
    drugCor[i, 8] <- mean(z.diff)
    drugCor[i, 9] <- median(z.diff)
  }
  colnames(drugCor)[8:9] <- c("mean.diff", "median.diff")

  
  drugCor %>% 
    dplyr::arrange(.data$p.value, .data$fdr)
}

# Functions ---------------------------------------------------------------

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

# Z-score normalization
Zscore <- function(x) {
  y <- (x - mean(x)) / sd(x)
  return(y)
}

pcor_test <- function(x, y, z, use = "mat", method = "p", na.rm = TRUE) {
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  #
  # use: There are two methods to calculate the partial correlation coefficient.
  # 	 One is by using variance-covariance matrix ("mat") and the other is by using recursive formula ("rec").
  # 	 Default is "mat".
  #
  # method: There are three ways to calculate the correlation coefficient,
  # 	    which are Pearson's ("p"), Spearman's ("s"), and Kendall's ("k") methods.
  # 	    The last two methods which are Spearman's and Kendall's coefficient are based on the non-parametric analysis.
  # 	    Default is "p".
  #
  # na.rm: If na.rm is T, then all the missing samples are deleted from the whole dataset, which is (x,y,z).
  #        If not, the missing samples will be removed just when the correlation coefficient is calculated.
  # 	   However, the number of samples for the p-value is the number of samples after removing
  # 	   all the missing samples from the whole dataset.
  # 	   Default is "T".

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)

  if (use == "mat") {
    p.use <- "Var-Cov matrix"
    pcor <- pcor.mat(x, y, z, method = method, na.rm = na.rm)
  } else if (use == "rec") {
    p.use <- "Recursive formula"
    pcor <- pcor.rec(x, y, z, method = method, na.rm = na.rm)
  } else {
    stop("\'use\' should be either \"rec\" or \"mat\"!\n")
  }

  # print the method
  if (gregexpr("p", method)[[1]][1] == 1) {
    p.method <- "Pearson"
  } else if (gregexpr("s", method)[[1]][1] == 1) {
    p.method <- "Spearman"
  } else if (gregexpr("k", method)[[1]][1] == 1) {
    p.method <- "Kendall"
  } else {
    stop("\'method\' should be \"pearson\" or \"spearman\" or \"kendall\"!\n")
  }

  # sample number
  n <- dim(na.omit(data.frame(x, y, z)))[1]

  # given variables' number
  gn <- dim(z)[2]

  # p-value
  if (p.method == "Kendall") {
    statistic <- pcor / sqrt(2 * (2 * (n - gn) + 5) / (9 * (n - gn) * (n - 1 - gn)))
    p.value <- 2 * pnorm(-abs(statistic))
  } else {
    statistic <- pcor * sqrt((n - 2 - gn) / (1 - pcor^2))
    p.value <- 2 * pnorm(-abs(statistic))
  }

  data.frame(estimate = pcor, p.value = p.value, statistic = statistic, n = n, gn = gn, Method = p.method, Use = p.use)
}

# By using var-cov matrix
pcor.mat <- function(x, y, z, method = "p", na.rm = T) {
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)

  if (dim(z)[2] == 0) {
    stop("There should be given data\n")
  }

  data <- data.frame(x, y, z)

  if (na.rm == T) {
    data <- na.omit(data)
  }

  xdata <- na.omit(data.frame(data[, c(1, 2)]))
  Sxx <- cov(xdata, xdata, m = method)

  xzdata <- na.omit(data)
  xdata <- data.frame(xzdata[, c(1, 2)])
  zdata <- data.frame(xzdata[, -c(1, 2)])
  Sxz <- cov(xdata, zdata, m = method)

  zdata <- na.omit(data.frame(data[, -c(1, 2)]))
  Szz <- cov(zdata, zdata, m = method)

  # is Szz positive definite?
  zz.ev <- eigen(Szz)$values
  if (min(zz.ev)[1] < 0) {
    stop("\'Szz\' is not positive definite!\n")
  }

  # partial correlation
  Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)

  rxx.z <- cov2cor(Sxx.z)[1, 2]

  rxx.z
}

# By using recursive formula
pcor.rec <- function(x, y, z, method = "p", na.rm = T) {
  #

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)

  if (dim(z)[2] == 0) {
    stop("There should be given data\n")
  }

  data <- data.frame(x, y, z)

  if (na.rm == T) {
    data <- na.omit(data)
  }

  # recursive formula
  if (dim(z)[2] == 1) {
    tdata <- na.omit(data.frame(data[, 1], data[, 2]))
    rxy <- cor(tdata[, 1], tdata[, 2], m = method)

    tdata <- na.omit(data.frame(data[, 1], data[, -c(1, 2)]))
    rxz <- cor(tdata[, 1], tdata[, 2], m = method)

    tdata <- na.omit(data.frame(data[, 2], data[, -c(1, 2)]))
    ryz <- cor(tdata[, 1], tdata[, 2], m = method)

    rxy.z <- (rxy - rxz * ryz) / (sqrt(1 - rxz^2) * sqrt(1 - ryz^2))

    return(rxy.z)
  } else {
    x <- c(data[, 1])
    y <- c(data[, 2])
    z0 <- c(data[, 3])
    zc <- as.data.frame(data[, -c(1, 2, 3)])

    rxy.zc <- pcor.rec(x, y, zc, method = method, na.rm = na.rm)
    rxz0.zc <- pcor.rec(x, z0, zc, method = method, na.rm = na.rm)
    ryz0.zc <- pcor.rec(y, z0, zc, method = method, na.rm = na.rm)

    rxy.z <- (rxy.zc - rxz0.zc * ryz0.zc) / (sqrt(1 - rxz0.zc^2) * sqrt(1 - ryz0.zc^2))
    return(rxy.z)
  }
}
