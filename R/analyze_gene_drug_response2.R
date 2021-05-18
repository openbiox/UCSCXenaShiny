#' Another Function to Analyze Association between Gene (Signature)
#' and Drug Response with CCLE Data
#'
#' Analyze gene-drug association using ANOVA or MANOVA.
#'
#' @param gene_list a gene symbol list.
#' @param response_type one measure type for drug response.
#' @param combine if `TRUE`, combine the expression of gene list as
#' a gene signature.
#' @inheritParams analyze_gene_drug_response_diff
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
#' analyze_gene_drug_response_asso2(c("TP53", "KRAS"))
#' analyze_gene_drug_response_asso2(c("TP53", "KRAS"), combine = TRUE)
#' analyze_gene_drug_response_asso2("TP53", response_type = "EC50")
#' analyze_gene_drug_response_asso2("TP53", response_type = "Amax")
#' analyze_gene_drug_response_asso2("TP53", response_type = "IC50", manova = TRUE)
#'
#' # Visualization
#' vis_gene_drug_response_asso2("TP53")
#' }
#' @references Garnett, Mathew J., et al. "Systematic identification of genomic markers of drug sensitivity in cancer cells." Nature 483.7391 (2012): 570-575.
analyze_gene_drug_response_asso2 <- function(gene_list,
                                             response_type = c("IC50", "EC50", "Amax", "ActArea"),
                                             tissue = "ALL",
                                             manova = FALSE,
                                             combine = FALSE) {
  stopifnot(length(gene_list) > 0)
  response_type <- match.arg(response_type)
  on.exit(invisible(gc()))

  if (any(grepl(" ", gene_list))) {
    stop("Space is detected in your input, it's invalid.\nIf you want to use genomic signature feature, please input a gene list.")
  }

  ccle_data <- load_data("ccle_expr_and_drug_response")
  ccle_drug_e <- load_data("ccle_drug_response_extend")

  if (is.null(ccle_data)) {
    stop("Data 'ccle_expr_and_drug_response' load failed, try again?")
  }
  if (is.null(ccle_drug_e)) {
    stop("Data 'ccle_drug_response_extend' load failed, try again?")
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

  df <- expr %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-"rowname", names_to = "cell_line", values_to = "expr") %>%
    dplyr::left_join(ccle_drug_e, by = c("cell_line" = "CCLE Cell Line Name"))

  rm(ccle_data, ccle_drug_e)

  colnames(df)[1:8] <- c(
    "gene", "cell_line", "expression", "tissue",
    "drug", "target", "dose", "activity"
  )
  df <- df %>%
    dplyr::select(dplyr::all_of(c(colnames(df)[1:6], "Slope", response_type))) %>%
    dplyr::mutate(drug_target = paste(.data$drug, "->", .data$target)) %>%
    dplyr::select(-c("drug", "target"))
  colnames(df)[6] <- "response"

  if (!"ALL" %in% tissue) {
    df <- dplyr::filter(df, .data$tissue %in% .env$tissue)
  }

  # ANOVA or MANOVA
  anova_fit <- function(data, manova = FALSE) {
    pval <- tryCatch(
      {
        if (manova) {
          md <- stats::manova(cbind(response, Slope) ~ expression + tissue, data = data) %>%
            summary()
          md$stats["expression", 6]
        } else {
          md <- stats::aov(response ~ expression + tissue, data = data) %>%
            summary()
          md[[1]]["expression", 5]
        }
      },
      error = function(e) {
        NA
      }
    )

    effect <- data %>%
      dplyr::group_by(.data$tissue) %>%
      dplyr::mutate(number_of_cell_lines = dplyr::n()) %>%
      dplyr::filter(.data$number_of_cell_lines >= 3) %>%
      # at least 3 cell lines in a tissue
      dplyr::mutate(group = dplyr::case_when(
        dplyr::percent_rank(.data$expression) > 0.5 ~ "High",
        dplyr::percent_rank(.data$expression) <= 0.5 ~ "Low",
        TRUE ~ NA_character_
      )) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$group, .data$tissue) %>%
      dplyr::summarise(response = mean(response, na.rm = TRUE), .groups = "drop") %>%
      # 先求每个组织中的均值
      tidyr::pivot_wider(id_cols = "tissue", names_from = "group", values_from = "response") %>%
      dplyr::mutate(effect = .data$High / .data$Low) %>%
      # 再求高低组比值
      dplyr::pull("effect") %>%
      mean(na.rm = TRUE) # 再求均值

    list(pval = pval, effect = effect)
  }

  out <- df %>%
    dplyr::group_nest(.data$gene, .data$drug_target) %>%
    dplyr::mutate(mod = purrr::map(.data$data, anova_fit, manova = manova)) %>%
    dplyr::mutate(
      pval = purrr::map_dbl(.data$mod, "pval"),
      effect = purrr::map_dbl(.data$mod, "effect")
    ) %>%
    dplyr::select(-c("data", "mod")) %>%
    dplyr::group_by(.data$gene) %>%
    # 对每个基因的结果作校正
    dplyr::mutate(fdr = stats::p.adjust(.data$pval, method = "fdr")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~ round(., 3)) %>%
    dplyr::arrange(.data$pval, .data$fdr)

  out
}


#' Another Function to Visualize Gene and Drug-Target Association with CCLE Data
#'
#' See [analyze_gene_drug_response_asso2] for examples.
#'
#' @export
#' @inheritParams analyze_gene_drug_response_asso2
#' @param Gene a gene symbol (list), when it is a list, all
#' genes are combined as a signature.
#' @param output_form `plotly` or `ggplot2`.
#' @return `plotly` or `ggplot2` object.
vis_gene_drug_response_asso2 <- function(Gene = "TP53", response_type = c("IC50", "EC50", "Amax", "ActArea"),
                                         tissue = "ALL", manova = FALSE,
                                         output_form = c("plotly", "ggplot2")) {
  response_type <- match.arg(response_type)
  output_form <- match.arg(output_form)

  if (!requireNamespace("plotly")) install.packages("plotly")
  if (!requireNamespace("ggrepel")) install.packages("ggrepel")

  df <- analyze_gene_drug_response_asso2(
    Gene,
    response_type = response_type,
    tissue = tissue, manova = manova,
    combine = TRUE
  ) # Combine as a signature if more than 1 gene

  df$p_log <- -log10(df$pval)
  df$effect_log <- log10(df$effect)
  df$text <- paste(
    "Gene(s): ", paste(Gene, collapse = "/"),
    "<br>Drug-Target: ", df$drug_target,
    "<br>Effect: ", df$effect,
    "<br><i>P</i> value: ", df$pval,
    "<br>FDR: ", df$fdr
  )

  p <- ggplot(data = df, aes_string(
    x = "effect_log",
    y = "p_log",
    text = "text"
  )) +
    geom_point() +
    ggtitle(paste0(
      if (length(Gene) > 1) {
        paste0("Signature (", paste(Gene, collapse = "&"), ")")
      } else {
        Gene
      },
      " and Drug-Target Response Association"
    )) +
    labs(
      x = paste0("Gene ", response_type, " log10(effect)"),
      y = "-log10(P-value)"
    ) +
    cowplot::theme_cowplot() +
    scale_color_gradient2(low = scales::muted("blue"), high = scales::muted("red"), midpoint = 0) +
    theme(
      plot.title = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = -log10(0.05), linetype = 2, size = 0.5, alpha = 0.5) +
    geom_vline(xintercept = 0, color = "gray", size = 0.5, alpha = 0.5)
  if (output_form == "plotly") p <- plotly::ggplotly(p, tooltip = "text")
  return(p)
}
