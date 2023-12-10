#' Visualize Correlation between Gene and Pathway signature Score
#' @param Gene 	a molecular identifier (e.g., "TP53") or a formula
#' specifying genomic signature ("TP53 + 2 * KRAS - 1.3 * PTEN").
#' @param data_type choose gene profile type, including
#' "mRNA", "transcript", "protein", "mutation", "cnv", "methylation", "miRNA".
#' @param pw_name the queried Pathway name, see the supported pathway from 'load("toil_sig_score")'default is NULL
#' @param Cancer  select cancer cohort(s)
#' @param cor_method select correlation coefficient (pearson/spearman)
#' @param cor_cutoff a list with 2 elements names 'r' and 'p' to define significant correlations
#' @param plot output the plot directly (default) or raw data.frame
#' @param opt_pancan specify one dataset for some molercular profiles

#' @return a `ggplot` object or  dataframe
#' @examples
#' \dontrun{
#' p1_1 <- vis_gene_pw_cor(Gene = "TP53", data_type = "mRNA", Cancer = "ACC")
#' p1_1_data <- vis_gene_pw_cor(Gene = "CD4", data_type = "mRNA", Cancer = "ACC", plot = FALSE)
#' p1_2 <- vis_gene_pw_cor(Gene = "hsa-miR-1228-3p", data_type = "miRNA", Cancer = "ACC")
#'
#' p2 <- vis_gene_pw_cor(
#'   Gene = "TP53", data_type = "mRNA", Cancer = "ACC",
#'   pw_name = "KEGG_VEGF_SIGNALING_PATHWAY"
#' )
#'
#' p3 <- vis_gene_pw_cor(
#'   Gene = "TP53", data_type = "mRNA", Cancer = "Overall",
#'   pw_name = "KEGG_VEGF_SIGNALING_PATHWAY"
#' )
#' }

#' @export
#'
vis_gene_pw_cor <- function(Gene = "TP53", data_type = "mRNA",
                            pw_name = NULL, Cancer = "ACC",
                            cor_method = "pearson", cor_cutoff = list(r = 0.3, p = 0.01),
                            plot = TRUE, opt_pancan = .opt_pancan) {
  ## mode1**gene -- one cancer -- many pathways: bar plot(default)
  ## mode2**gene -- many cancers -- one pathway: lollipop plot
  ## mode3**gene -- one cancer -- one pathway: point plot

  ## step1
  # build-in 500 pathway signaure score(toil tcga)
  if (!file.exists(file.path(get_zenodo_dir(), "tcga_PW.rda"))) {
    print("This is the first download from zenodo, please wait a few minutes.")
  }
  if ((length(Cancer) > 1 | Cancer == "Overall") & is.null(pw_name)) {
    stop("You need provide specific pathway name when dealing with multiple cancers")
  }
  toil_sig_score <- load_data("tcga_PW")
  toil_sig_meta  <- load_data("tcga_PW_meta")
  # toil_sig_score <- pw_score$score # [1] 9807  500
  # toil_sig_meta <- pw_score$meta
  # colnames(toil_sig_meta) <- gsub("sig", "pw", colnames(toil_sig_meta))

  if (!is.null(pw_name)) {
    if (!(pw_name %in% toil_sig_meta$ID)) {
      stop("You need provide valid pathway name (see load_data('tcga_PW_meta'))")
    }
  }

  # gene expression
  t1 <- query_pancan_value(Gene, data_type = data_type, opt_pancan = opt_pancan)
  if (is.null(t1[[1]])) {
    warning("No data available", immediate. = TRUE)
    return(NULL)
  }
  if (is.list(t1)) t1 <- t1[[1]]
  if (all(is.na(t1))) {
    message("All NAs returned, return NULL instead.")
    return(NULL)
  }

  message(paste0("Get data value for ", Gene))
  s <- data.frame(sample = names(t1), values = t1)


  # select tcga tumor samples
  tcga_gtex <- load_data("tcga_gtex")
  if (length(Cancer) == 1 & all(Cancer == "Overall")) Cancer <- as.character(unique(tcga_gtex$tissue))
  tcga_sp <- tcga_gtex %>%
    dplyr::filter(.data$type2 == "tumor") %>%
    dplyr::filter(.data$tissue %in% Cancer) %>%
    dplyr::pull(sample) %>%
    as.character()


  ## step2
  ## mode1**gene -- spe tcga -- all sig: bar plot(default)
  if (is.null(pw_name) && length(Cancer)==1) {
    message("Mode-1: the significant correlated pathways to one specific cancer")
    res_pan <- toil_sig_score[rownames(toil_sig_score) %in% tcga_sp, ] %>%
      as.data.frame() %>% tibble::rownames_to_column("sample") %>% 
      tidyr::pivot_longer(!sample, names_to = "pw_name", values_to = "pw_score") %>%
      dplyr::left_join(s) %>%
      dplyr::mutate(gene = Gene, .before = 1) %>%
      dplyr::group_by(pw_name)
    res_pan <- switch(cor_method,
      pearson = res_pan %>%
        dplyr::summarise(
          cor = stats::cor.test(.data$pw_score, .data$values, method = "pearson")$estimate,
          pval = stats::cor.test(.data$pw_score, .data$values, method = "pearson")$p.value
        ),
      spearman = res_pan %>%
        dplyr::summarise(
          cor = stats::cor.test(.data$pw_score, .data$values, method = "spearman")$estimate,
          pval = stats::cor.test(.data$pw_score, .data$values, method = "spearman")$p.value
        )
    )
    res_pan <- res_pan %>%
      dplyr::ungroup() %>%
      dplyr::left_join(toil_sig_meta[,1:4],by=c("pw_name"="ID"))

    res_pan_sig <- res_pan %>%
      dplyr::filter(abs(.data$cor) >= cor_cutoff$r, .data$pval <= cor_cutoff$p) %>%
      dplyr::arrange(.data$cor) %>%
      dplyr::mutate(pw_name = factor(pw_name, levels = pw_name))
    if (nrow(res_pan_sig) == 0) stop("No significant correlation was detected.")
    if (nrow(res_pan_sig) >= 50) {
      message("The top 50 most significant correlations were used to plot.")
      res_pan_sig <- res_pan_sig %>%
        dplyr::slice_min(pval, n = 50)
    }

    if (!plot) {
      return(res_pan)
    } else {
      p <- ggplot(res_pan_sig, aes(x = .data$pw_name, y = .data$cor, fill = .data$Type)) +
        geom_col() +
        scale_fill_manual(values = c("HALLMARK" = "#7fc97f", "KEGG" = "#beaed4", "IOBR" = "#fdc086")) +
        scale_x_discrete(labels = res_pan_sig$Name) +
        ylab(paste0(stringr::str_to_title(cor_method), " correlation coefficient")) +
        xlab("") +
        coord_flip() +
        guides(fill = guide_legend(title = "")) +
        theme_bw() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "top",
          text = element_text(size = 15)
        )
      return(p)
    }
    ## mode2**gene -- pan tcga -- spe sig: lollipop plot
  } else if (length(Cancer) > 1 && !is.null(pw_name)) {
    message("Mode-2: one specific pathway correlated to multiple cancers")
    res_pan_spe <- toil_sig_score[rownames(toil_sig_score) %in% tcga_sp, pw_name, drop = F] %>%
      as.data.frame() %>% tibble::rownames_to_column("sample") %>% 
      tidyr::pivot_longer(!sample, names_to = "pw_name", values_to = "pw_score") %>%
      dplyr::left_join(s) %>%
      dplyr::mutate(gene = Gene, .before = 1) %>%
      dplyr::mutate(Cancer = tcga_gtex$tissue[match(sample, tcga_gtex$sample)], .before = 3)
    # to avoid NA values in some cancers
    cancer_stat <- table(na.omit(res_pan_spe)$Cancer)
    if (length(names(cancer_stat[cancer_stat < 5])) > 1) {
      if (length(setdiff(names(cancer_stat), names(cancer_stat[cancer_stat < 5]))) < 2) {
        stop("The molecule information was invalid in less than 2 cancers")
      } else {
        message(names(cancer_stat[cancer_stat < 5]), " less than 5 samples was/were discarded")
      }
    }

    res_pan_spe <- res_pan_spe %>%
      dplyr::filter(!Cancer %in% names(cancer_stat[cancer_stat < 5])) %>%
      dplyr::group_by(Cancer)
    res_pan_spe <- switch(cor_method,
      pearson = res_pan_spe %>%
        dplyr::summarise(
          cor = stats::cor.test(.data$pw_score, .data$values, method = "pearson")$estimate,
          pval = stats::cor.test(.data$pw_score, .data$values, method = "pearson")$p.value
        ),
      spearman = res_pan_spe %>%
        dplyr::summarise(
          cor = stats::cor.test(.data$pw_score, .data$values, method = "spearman")$estimate,
          pval = stats::cor.test(.data$pw_score, .data$values, method = "spearman")$p.value
        )
    )
    res_pan_spe <- res_pan_spe %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$cor) %>%
      dplyr::mutate(Cancer = factor(Cancer, levels = Cancer))
    if (!plot) {
      return(res_pan_spe)
    } else {
      p <- res_pan_spe %>%
        dplyr::mutate(cor_type = ifelse(.data$cor > 0, "Positive", "Negative")) %>%
        ggplot() +
        geom_segment(aes(x = Cancer, xend = Cancer, y = 0, yend = cor, color = .data$cor_type), linewidth = 2) +
        geom_point(aes(x = Cancer, y = cor), size = 3) +
        scale_color_manual(values = c("Positive" = "#fc8d59", "Negative" = "#91cf60")) +
        scale_size_continuous(range = c(1, 3)) +
        xlab("") +
        ylab("Pearson correlation coefficient") +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          text = element_text(size = 15)
        )
        return(p)
    }
    ## mode3**gene -- spe tcga -- spe sig: point plot
  } else if (length(Cancer) == 1 && !is.null(pw_name)) {
    message("Mode-3: one specific pathway correlated to one specific cancer")
    res_pan_spe <- toil_sig_score[rownames(toil_sig_score) %in% tcga_sp, pw_name, drop = F] %>%
      as.data.frame() %>% tibble::rownames_to_column("sample") %>% 
      tidyr::pivot_longer(!sample, names_to = "pw_name", values_to = "pw_score") %>%
      dplyr::left_join(s) %>%
      dplyr::mutate(gene = Gene, .before = 1) %>%
      dplyr::mutate(Cancer = tcga_gtex$tissue[match(sample, tcga_gtex$sample)], .before = 3) %>%
      tibble()
    if (nrow(na.omit(res_pan_spe)) < 5) {
      stop("The molecule information was invalid in less than 5 samples")
    }
    if (!plot) {
      return(res_pan_spe)
    } else {
      eval(parse(text = "library(ggpubr)"))
      p <- do.call("ggscatter", list(
        data = res_pan_spe,
        x = "values", y = "pw_score",
        xlab = "Gene expression (TPM)",
        ylab = "Pathway score(ssGSEA)",
        alpha = 0.5,
        add = "reg.line",
        add.params = list(color = "blue", fill = "lightgray"),
        cor.method = cor_method,
        cor.coef = TRUE
      )) +
        theme(text = element_text(size = 15))
    }
    return(p)
  } else {
    stop("Please select one analysis mode and check the right parameters.")
  }
}
