#' Analyze Association between Gene (Signature) and Drug Response with CCLE Data
#'
#' Analyze partial correlation of gene-drug association after
#' controlling for tissue average expression.
#'
#' @param gene_list a gene symbol list.
#' @param combine if `TRUE`, combine the expression of gene list as
#' a gene signature.
#'
#' @return a `data.frame`
#' - If `combine` is `TRUE`, genes are combined as `signature`.
#' - `mean.diff` and `median.diff` indicate mean and median of
#' normalized expression difference between High IC50 cells and Low IC50 cells.
#' The cutoff between High and Low are median IC50.
#' @export
#'
#' @examples
#' \dontrun{
#' analyze_gene_drug_response_asso("TP53")
#' analyze_gene_drug_response_asso(c("TP53", "KRAS"))
#' analyze_gene_drug_response_asso(c("TP53", "KRAS"), combine = TRUE)
#'
#' # Visualization
#' vis_gene_drug_response_asso("TP53")
#' }
analyze_gene_drug_response_asso <- function(gene_list, combine = FALSE) {
  stopifnot(length(gene_list) > 0)
  on.exit(invisible(gc()))

  if (any(grepl(" ", gene_list))) {
    stop("Space is detected in your input, it's invalid.\nIf you want to use genomic signature feature, please input a gene list.")
  }

  ccle_data <- load_data("ccle_expr_and_drug_response")

  if (is.null(ccle_data)) {
    stop("Data load failed, try again?")
  }

  if (any(gene_list %in% rownames(ccle_data$expr))) {
    expr <- ccle_data$expr[gene_list, , drop = FALSE]
  } else {
    stop("None of your input genes exists in CCLE data.")
  }

  if (combine && length(gene_list) > 1) {
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
    tissues.mean <- stats::aggregate(gene.exp, by = list(tissues), mean)
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
  names(drugCor) <- c("genes", "drugs", "cor", "p.value", "num_of_cell_lines")

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
    dplyr::arrange(.data$p.value, .data$fdr) %>%
    dplyr::mutate_if(is.numeric, ~ round(., 3))
}

#' Analyze Difference of Drug Response (IC50 Value (uM)) between Gene (Signature) High and Low Expression with CCLE Data
#'
#' @inheritParams analyze_gene_drug_response_asso
#' @param drug a drug name. Check examples.
#' @param tissue a tissue name. Check examples.
#' @param cutpoint cut point (in percent) for High and Low group, default is `c(50, 50)`.
#'
#' @return a `data.frame`.
#' @export
#'
#' @examples
#' tissue_list <- c(
#'   "prostate", "central_nervous_system", "urinary_tract", "haematopoietic_and_lymphoid_tissue",
#'   "kidney", "thyroid", "soft_tissue", "skin", "salivary_gland",
#'   "ovary", "lung", "bone", "endometrium", "pancreas", "breast",
#'   "large_intestine", "upper_aerodigestive_tract", "autonomic_ganglia",
#'   "stomach", "liver", "biliary_tract", "pleura", "oesophagus"
#' )
#'
#' drug_list <- c(
#'   "AEW541", "Nilotinib", "17-AAG", "PHA-665752", "Lapatinib",
#'   "Nutlin-3", "AZD0530", "PF2341066", "L-685458", "ZD-6474", "Panobinostat",
#'   "Sorafenib", "Irinotecan", "Topotecan", "LBW242", "PD-0325901",
#'   "PD-0332991", "Paclitaxel", "AZD6244", "PLX4720", "RAF265", "TAE684",
#'   "TKI258", "Erlotinib"
#' )
#'
#' target_list <- c(
#'   "IGF1R", "ABL", "HSP90", "c-MET", "EGFR", "MDM2", "GS", "HDAC",
#'   "RTK", "TOP1", "XIAP", "MEK", "CDK4", "TUBB1", "RAF", "ALK", "FGFR"
#' )
#' \dontrun{
#' analyze_gene_drug_response_diff("TP53")
#' analyze_gene_drug_response_diff(c("TP53", "KRAS"), drug = "AEW541")
#' analyze_gene_drug_response_diff(c("TP53", "KRAS"),
#'   tissue = "kidney",
#'   combine = TRUE
#' )
#'
#' # Visualization
#' vis_gene_drug_response_diff("TP53")
#' }
analyze_gene_drug_response_diff <- function(gene_list,
                                            drug = "ALL",
                                            tissue = "ALL",
                                            combine = FALSE,
                                            cutpoint = c(50, 50)) {
  stopifnot(length(gene_list) > 0, length(cutpoint) > 0)
  on.exit(invisible(gc()))
  # 将基因表达按阈值分为高低两组，
  # 然后比较它们的 IC50 差异
  # 这里只要得到表达高低 2 组的 IC50 值即可

  tissue <- unique(tissue)
  drug <- unique(drug)
  if (any(grepl(" ", gene_list))) {
    stop("Space is detected in your input, it's invalid.\nIf you want to use genomic signature feature, please input a gene list.")
  }

  ccle_data <- load_data("ccle_expr_and_drug_response")

  if (any(gene_list %in% rownames(ccle_data$expr))) {
    expr <- ccle_data$expr[gene_list, , drop = FALSE]
  } else {
    stop("None of your input genes exists in CCLE data.")
  }

  if (combine && length(gene_list) > 1) {
    expr <- t(apply(expr, 2, gm_mean))
    rownames(expr) <- paste0("signature (", paste(gene_list, collapse = "&"), ")")
  }

  drug_ic50 <- ccle_data$drug_ic50
  drug_info <- ccle_data$drug_info
  rm(ccle_data)

  df <- expr %>%
    as.data.frame() %>%
    tibble::rownames_to_column("genes") %>%
    tidyr::pivot_longer(-"genes", names_to = "ccle_name", values_to = "expr") %>%
    dplyr::inner_join(
      unique(drug_info[, c("CCLE Cell Line Name", "Site Primary", "Compound", "Target")]),
      by = c("ccle_name" = "CCLE Cell Line Name")
    ) %>%
    dplyr::inner_join(
      drug_ic50 %>%
        as.data.frame() %>%
        tibble::rownames_to_column("ccle_name") %>%
        tidyr::pivot_longer(-"ccle_name", names_to = "drug", values_to = "IC50"),
      by = c("ccle_name" = "ccle_name", "Compound" = "drug")
    ) %>%
    dplyr::mutate(drug_target = paste(.data$Compound, "->", .data$Target)) %>%
    dplyr::select(-c("Target"))
  colnames(df)[1:6] <- c("genes", "ccle_name", "expression", "tissue", "drug", "IC50")

  if (!"ALL" %in% tissue) {
    df <- dplyr::filter(df, .data$tissue %in% .env$tissue)
  }

  if (!"ALL" %in% drug) {
    df <- dplyr::filter(df, .data$drug %in% .env$drug)
  }

  if (length(cutpoint) == 1) {
    cutpoint <- c(cutpoint, cutpoint)
  }
  cutpoint <- cutpoint / 100

  # 分组要保证在不同的组织下进行（因为不同组织表达本身可能有差异）
  # 如果用户选定多组织或全部组织（就不对组织分组）
  if ("ALL" %in% tissue || length(tissue) > 1) {
    df <- df %>%
      dplyr::group_by(.data$genes, .data$drug_target)
  } else {
    df <- df %>%
      dplyr::group_by(.data$genes, .data$drug_target, .data$tissue)
  }
  df <- df %>%
    dplyr::mutate(number_of_cell_lines = dplyr::n()) %>%
    dplyr::filter(.data$number_of_cell_lines >= 3) %>%
    # at least 3 cell lines in a tissue
    dplyr::mutate(group = dplyr::case_when(
      dplyr::percent_rank(.data$expression) > cutpoint[2] ~ "High",
      dplyr::percent_rank(.data$expression) <= cutpoint[1] ~ "Low",
      TRUE ~ NA_character_
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$group)) %>%
    dplyr::mutate(drug_target = paste0(.data$drug_target, "\n(n = ", .data$number_of_cell_lines, ")")) %>%
    as.data.frame()

  df %>%
    dplyr::mutate_if(is.numeric, ~ round(., 3))
}

# Functions ---------------------------------------------------------------

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
  if (any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  if (zero.propagate) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }
}

# Z-score normalization
Zscore <- function(x) {
  y <- (x - mean(x)) / stats::sd(x)
  return(y)
}


# Source link: https://github.com/Katan5555/Macau_project_1/blob/master/FUNCTIONS/partial_correlation_functions.R
# LICENCE: GPLv3
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
  n <- dim(stats::na.omit(data.frame(x, y, z)))[1]

  # given variables' number
  gn <- dim(z)[2]

  # p-value
  if (p.method == "Kendall") {
    statistic <- pcor / sqrt(2 * (2 * (n - gn) + 5) / (9 * (n - gn) * (n - 1 - gn)))
    p.value <- 2 * stats::pnorm(-abs(statistic))
  } else {
    statistic <- pcor * sqrt((n - 2 - gn) / (1 - pcor^2))
    p.value <- 2 * stats::pnorm(-abs(statistic))
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
    data <- stats::na.omit(data)
  }

  xdata <- stats::na.omit(data.frame(data[, c(1, 2)]))
  Sxx <- stats::cov(xdata, xdata, m = method)

  xzdata <- stats::na.omit(data)
  xdata <- data.frame(xzdata[, c(1, 2)])
  zdata <- data.frame(xzdata[, -c(1, 2)])
  Sxz <- stats::cov(xdata, zdata, m = method)

  zdata <- stats::na.omit(data.frame(data[, -c(1, 2)]))
  Szz <- stats::cov(zdata, zdata, m = method)

  # is Szz positive definite?
  zz.ev <- eigen(Szz)$values
  if (min(zz.ev)[1] < 0) {
    stop("\'Szz\' is not positive definite!\n")
  }

  # partial correlation
  Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)

  rxx.z <- stats::cov2cor(Sxx.z)[1, 2]

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
    data <- stats::na.omit(data)
  }

  # recursive formula
  if (dim(z)[2] == 1) {
    tdata <- stats::na.omit(data.frame(data[, 1], data[, 2]))
    rxy <- stats::cor(tdata[, 1], tdata[, 2], m = method)

    tdata <- stats::na.omit(data.frame(data[, 1], data[, -c(1, 2)]))
    rxz <- stats::cor(tdata[, 1], tdata[, 2], m = method)

    tdata <- stats::na.omit(data.frame(data[, 2], data[, -c(1, 2)]))
    ryz <- stats::cor(tdata[, 1], tdata[, 2], m = method)

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
