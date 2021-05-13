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
#' analyze_gene_drug_response_asso2("TP53")
#' analyze_gene_drug_response_asso2("TP53", dep_var = "EC50")
#' analyze_gene_drug_response_asso2("TP53", dep_var = "Amax")
#' analyze_gene_drug_response_asso2("TP53", dep_var = "IC50", manova = TRUE)
#'
#' # Visualization
#' vis_gene_drug_response_asso("TP53")
#' }
analyze_gene_drug_response_asso2 <- function(gene_list, 
                                            dep_var = c("IC50","EC50","Amax","ActArea"),
                                            manova = FALSE,
                                            combine = FALSE) {
  dep_var <- match.arg(dep_var)
  stopifnot(length(gene_list) == 1)
  stopifnot(length(dep_var) == 1)
  on.exit(invisible(gc()))
  
  
  if (any(grepl(" ", gene_list))) {
    stop("Space is detected in your input, it's invalid.\nIf you want to use genomic signature feature, please input a gene list.")
  }
  
  # ccle_data <- load_data("ccle_expr_and_drug_response")
  load("./data-raw/ccle_expr_and_drug_response_advanced.rda")
  
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
  
  for (i in 1:nrow(drugCor)) {
    g <- drugCor[i, 1]
    d <- drugCor[i, 2]
    
    if (manova == TRUE) {
      # define dependent variable
      d.dep <- filter(drug_info, Compound == d)[,c("CCLE Cell Line Name", dep_var, "Slope", "Site Primary")] %>%
        rename(cellName = `CCLE Cell Line Name`, Tissue = `Site Primary`)
      # extract expr data.frame
      geneExpr <- as.data.frame(t(expr)) %>%
        rownames_to_column(var = "cellName")
      # Combine expr, dependent var and tissue
      d_dat <- d.dep %>%
        left_join(geneExpr, by = "cellName") %>% na.omit() %>%
        mutate(group = ifelse(!!sym(gene_list) > stats::median(!!sym(gene_list)), "high", "low"))
      
      if (nrow(d_dat) == 0) {
        drugCor[i, 8] <- NA
      }else{
        # manova(cbind(IC50, Slope) ~ TP53 + Tissue, data = d_dat)
        manova_fit <- manova(as.formula(paste0("cbind(", dep_var, ", Slope", ")", " ~ ", paste(c(gene_list, "Tissue"), collapse="+"))), data = d_dat)
        tryCatch({
          manova_res <- summary(manova_fit)
          drugCor[i, 8] <- manova_res$stats[gene_list,6]
        }, error=function(e){drugCor[i, 8] <- NA})
      }
      
    }else{
      # define dependent variable
      d.dep <- filter(drug_info, Compound == d)[,c("CCLE Cell Line Name", dep_var, "Site Primary")] %>%
        rename(cellName = `CCLE Cell Line Name`, Tissue = `Site Primary`)
      # extract expr data.frame
      geneExpr <- as.data.frame(t(expr)) %>%
        rownames_to_column(var = "cellName")
      # Combine expr, dependent var and tissue
      d_dat <- d.dep %>%
        left_join(geneExpr, by = "cellName") %>% na.omit() %>%
        mutate(group = ifelse(!!sym(gene_list) > stats::median(!!sym(gene_list)), "high", "low"))
      
      aov_fit <- aov(as.formula(paste(paste(dep_var, collapse = "+"), "~", paste(c(gene_list, "Tissue"), collapse="+"))), data = d_dat)
      aov_res <- summary(aov_fit)
      drugCor[i, 8] <- aov_res[[1]][gene_list,5]
    }
    
    effect <- group_by(d_dat, group) %>%
      summarise(Mean = mean(!!sym(dep_var)))
    # high vs. low group
    effect <- effect$Mean[1] / effect$Mean[2]
    drugCor[i, 9] <- effect
    
  }
  colnames(drugCor)[8:9] <- c("p.value(aov)", "Effect")

  drugCor %>%
    dplyr::mutate(`fdr(aov)` = stats::p.adjust(`p.value(aov)`, method = "fdr")) %>%
    dplyr::arrange(.data$p.value, .data$fdr) %>%
    dplyr::mutate_if(is.numeric, ~ round(., 3))
}
