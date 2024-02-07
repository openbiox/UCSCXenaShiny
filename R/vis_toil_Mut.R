#' Visualize molecular profile difference between mutation and wild status of queried gene
#'
#' @param mut_Gene the queried gene to determine grouping based on mutation and wild status
#' @param Gene a molecular identifier (e.g., "TP53") or a formula specifying
#' genomic signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).
#' @param data_type choose gene profile type,
#' including "mRNA", "transcript", "methylation", "miRNA".
#' @param Mode choose one visualize mode to represent data
#' @param Show.P.value  `TRUE` or `FALSE` whether to count P value
#' @param Show.P.label `TRUE` or `FALSE` present p value with number or label `*`, `**`, `***` and `****`
#' @param Method default method is wilcox.test
#' @param values the color to fill mutation or wild status
#' @param draw_quantiles draw quantiles for violinplot
#' @param trim whether to trim the violin
#' @param opt_pancan specify one dataset for some molercular profiles

#' @return a `ggplot` object or a tibble data.frame

#'
#' @examples
#' \dontrun{
#' p <- vis_toil_Mut(mut_Gene = "TP53")
#' p <- vis_toil_Mut(mut_Gene = "TP53", Gene = "TNF")
#' p <- vis_toil_Mut(mut_Gene = "TP53", Gene = "hsa-let-7d-3p", data_type = "miRNA")
#' }
#' @export

vis_toil_Mut <- function(mut_Gene = "TP53", Gene = NULL, data_type = NULL,
                         Mode = c("Boxplot", "Violinplot"),
                         Show.P.value = TRUE, Show.P.label = TRUE,
                         Method = c("wilcox.test", "t.test"),
                         values = c("#DF2020", "#DDDF21"),
                         draw_quantiles = c(0.25, 0.5, 0.75),
                         trim = TRUE, opt_pancan=.opt_pancan) {
  Mode <- match.arg(Mode)
  Method <- match.arg(Method)

  # tumor samples grouping based on mutation
  mut_dat_raw <- query_pancan_value(mut_Gene, data_type = "mutation")
  if (all(is.na(mut_dat_raw))) {
    stop("For the gene(", mut_Gene, ") mutation, all NAs returned.")
  }
  tcga_gtex <- load_data("tcga_gtex")
  mut_dat <- mut_dat_raw %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::rename("mut" = ".") %>%
    dplyr::inner_join(tcga_gtex, .) %>%
    dplyr::filter(.data$type2 == "tumor") %>%
    dplyr::select("sample", "tissue", "mut")
  # affected molecular profile
  if (is.null(Gene)) Gene <- mut_Gene
  if (is.null(data_type)) data_type <- "mRNA"
  if (!data_type %in% c("mRNA", "miRNA", "transcript", "methylation")) {
    stop("data_type ", data_type, " does not support in this function!")
  }
  exp_dat_raw <- query_pancan_value(Gene, data_type = data_type, opt_pancan = opt_pancan)
  if (all(is.na(exp_dat_raw$expression))) {
    stop("For the gene(", Gene, ") profile, all NAs returned.")
  }
  unit <- exp_dat_raw$unit
  exp_dat <- exp_dat_raw$expression %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::rename("expression" = ".") %>%
    dplyr::inner_join(tcga_gtex, .) %>%
    dplyr::select("sample", "expression")

  # merge data
  merge_dat <- dplyr::inner_join(mut_dat, exp_dat) %>%
    dplyr::mutate(
      tissue = as.character(.data$tissue),
      mut = factor(.data$mut, levels = c(1, 0), labels  = c("Mutation", "Wild"))
    )
  group_stat <- table(merge_dat$tissue, merge_dat$mut)
  tcga_discard <- rownames(group_stat)[apply(group_stat, 1, min) < 3]
  if (length(tcga_discard) == length(group_stat)) {
    stop("No one valid cancer type has enough grouping samples (<3) for comparsion.")
  } else {
    message(
      paste(tcga_discard, collapse = " "), " was/were discarded due to less than ",
      3, " samples in one group."
    )
  }
  merge_dat <- merge_dat %>%
    dplyr::filter(!(.data$tissue %in% tcga_discard)) %>%
    dplyr::mutate(mut = paste0(.data$mut, "(", mut_Gene, ")"))


  # calculate p value significantce
  tmp_dat <- merge_dat %>%
    dplyr::group_by(.data$tissue, .data$mut) %>%
    dplyr::summarise(expression = mean(.data$expression)) %>%
    tidyr::pivot_wider(names_from = "mut", values_from = "expression")

  pv <- merge_dat %>%
    ggpubr::compare_means(expression ~ mut, data = ., method = Method, group.by = "tissue") %>%
    dplyr::select(c("tissue", "p", "p.signif", "p.adj")) %>%
    dplyr::mutate(Gene = Gene, .before = 1) %>%
    dplyr::arrange(p) %>%
    dplyr::inner_join(tmp_dat)

  # if (!plot) {
  #   return(pv)
  # }

  # visualization
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(merge_dat, aes_string(x = "tissue", y = "expression", fill = "mut")) +
      ggplot2::geom_boxplot()
  } else if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(
      merge_dat, aes_string(x = "tissue", y = "expression", fill = "mut")
    ) +
      geom_split_violin(
        draw_quantiles = draw_quantiles,
        trim = trim,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = TRUE,
        position = "identity"
      )
  }

  p <- p +
    ggplot2::xlab("") +
    ggplot2::ylab(if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")) +
    ggplot2::scale_fill_manual(values = values) +
    ggplot2::theme_set(ggplot2::theme_classic(base_size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = .5, vjust = .5)) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0, 0), legend.justification = c(0, 0)
    )

  if (Show.P.value == TRUE & Show.P.label == TRUE) {
    p <- p + ggplot2::geom_text(
      aes(
        x = .data$tissue,
        y = max(merge_dat$expression) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
    )
  }

  if (Show.P.value == TRUE & Show.P.label == FALSE) {
    p <- p + ggplot2::geom_text(
      aes(
        x = .data$tissue,
        y = max(merge_dat$expression) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
    )
  }

  return(p)
}




#' Visualize molecular profile difference between mutation and wild status of queried gene in Single Cancer Type
#' @inheritParams vis_toil_Mut
#' @param Cancer select cancer cohort(s).
#' @return a `ggplot` object or a tibble data.frame.
#' @export
#'
vis_toil_Mut_cancer <- function(mut_Gene = "TP53", Gene = NULL, data_type = NULL,
                                Mode = c("Dotplot", "Violinplot"),
                                Show.P.value = TRUE, Show.P.label = TRUE,
                                Method = c("wilcox.test", "t.test"),
                                values = c("#DF2020", "#DDDF21"),
                                draw_quantiles = c(0.25, 0.5, 0.75),
                                trim = TRUE, Cancer = "ACC", opt_pancan=.opt_pancan) {
  Mode <- match.arg(Mode)
  Method <- match.arg(Method)

  # tumor samples grouping based on mutation
  mut_dat_raw <- query_pancan_value(mut_Gene, data_type = "mutation")
  if (all(is.na(mut_dat_raw))) {
    stop("For the gene(", mut_Gene, ") mutation, all NAs returned.")
  }
  tcga_gtex <- load_data("tcga_gtex")
  mut_dat <- mut_dat_raw %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::rename("mut" = ".") %>%
    dplyr::inner_join(tcga_gtex, .) %>%
    dplyr::filter(.data$type2 == "tumor") %>%
    dplyr::select("sample", "tissue", "mut")
  # affected molecular profile
  if (is.null(Gene)) Gene <- mut_Gene
  if (is.null(data_type)) data_type <- "mRNA"
  if (!data_type %in% c("mRNA", "miRNA", "transcript", "methylation")) {
    stop("data_type ", data_type, " does not support in this function!")
  }
  exp_dat_raw <- query_pancan_value(Gene, data_type = data_type)
  if (all(is.na(exp_dat_raw$expression))) {
    stop("For the gene(", Gene, ") profile, all NAs returned.")
  }
  unit <- exp_dat_raw$unit
  exp_dat <- exp_dat_raw$expression %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample") %>%
    dplyr::rename("expression" = ".") %>%
    dplyr::inner_join(tcga_gtex, .) %>%
    dplyr::select("sample", "expression")

  # merge data
  merge_dat <- dplyr::inner_join(mut_dat, exp_dat) %>%
    dplyr::mutate(
      tissue = as.character(.data$tissue),
      mut = factor(.data$mut, levels = c(1, 0), labels  = c("Mutation", "Wild"))
    ) %>%
    dplyr::filter(.data$tissue == Cancer) %>%
    dplyr::mutate(mut = paste0(.data$mut, "(", mut_Gene, ")"))


  group_stat <- table(merge_dat$mut)
  if (min(group_stat) < 3) {
    stop("The cancer type does not has enough grouping samples (<3) for comparsion.")
  }


  # calculate p value significantce
  tmp_dat <- merge_dat %>%
    dplyr::group_by(.data$tissue, .data$mut) %>%
    dplyr::summarise(expression = mean(.data$expression)) %>%
    tidyr::pivot_wider(names_from = "mut", values_from = "expression")

  pv <- merge_dat %>%
    ggpubr::compare_means(expression ~ mut, data = ., method = Method, group.by = "tissue") %>%
    dplyr::select(c("tissue", "p", "p.signif")) %>%
    dplyr::mutate(Gene = Gene, .before = 1) %>%
    dplyr::arrange(p) %>%
    dplyr::inner_join(tmp_dat)

  # if (!plot) {
  #   return(pv)
  # }

  if (Mode == "Dotplot") {
    p <- ggpubr::ggdotplot(
      merge_dat,
      x = "mut", y = "expression", fill = "mut", color = "mut", size = 0.6, binwidth = 0.2
    )
  } else if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(
      merge_dat,
      aes_string(x = "mut", y = "expression", fill = "mut")
    ) +
      ggplot2::geom_violin(trim = FALSE) +
      ggplot2::geom_boxplot(width = 0.1, fill = "white")
  }
  p <- p +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(if (is.null(unit)) Gene else paste0(Gene, " (", unit, ")")) +
    ggplot2::ggtitle(paste0("TCGA: ", Cancer)) +
    theme_set(theme_classic(base_size = 20)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5, size = 20)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_manual(values = values) +
    ggplot2::scale_color_manual(values = values)

  if (Show.P.value == TRUE & Show.P.label == TRUE) {
    p <- p + ggplot2::geom_text(
      aes(
        x = 1.5,
        y = max(merge_dat$expression) * 1.1,
        label = .data$p.signif
      ),
      data = pv,
      inherit.aes = FALSE
    )
  }

  if (Show.P.value == TRUE & Show.P.label == FALSE) {
    p <- p + ggplot2::geom_text(
      aes(
        x = 1.5,
        y = max(merge_dat$expression) * 1.1,
        label = as.character(signif(.data$p, 2))
      ),
      data = pv,
      inherit.aes = FALSE
    )
  }
  return(p)
}
