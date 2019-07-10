#' Visualize single gene expression from toil data hub
#' @inheritParams ggpubr::ggboxplot
#' @param angle.x angle for x lab
#' @param ... other parameters passing to [ggpubr::ggboxplot()]
#' @return a `ggplot` object
#' @export
vis_toil_gene <- function(data, x = "primary_site",
                          y = "expression",
                          color = "sample_type",
                          palette = "jco",
                          xlab = "Primary site",
                          ylab = "Expression",
                          title = NULL,
                          facet.by = NULL,
                          angle.x = 45,
                          ...) {
  ggpubr::ggboxplot(data,
    x = x, y = y, color = color,
    palette = palette, xlab = xlab, ylab = ylab,
    title = title, facet.by = NULL, ...
  ) +
    ggpubr::rotate_x_text(angle = angle.x)
}

# Visualize Pan-cancer tpm (Tumor (TCGA) vs normal (GTEX))
#' Visualize single gene expression from toil data hub
#' @import ggplot2 dplyr tibble
#' @inheritParams ggpubr::compare_means
#' @param Gene Gene symbal for comparision
#' @param Mode Boxplot or Violinplot to represent data
#' @param Show.P.value TRUE or FALSE whether to count P value
#' @param Method default method is wilcox.test
#' @param Show.P.label TRUE or FALSE present p value with number or label '* ** *** ****'
#' @param values the color to fill tumor or normal
#' @return a `ggplot` object
#' @examples
#' \donttest{
#' vis_toil_TvsN(Gene = "TP53", Mode = "Violinplot", Show.P.value = F, Show.P.label = F)
#' vis_toil_TvsN(Gene = "TP53", Mode = "Boxplot", Show.P.value = F, Show.P.label = F)
#' }
#' @export
#'
vis_toil_TvsN <- function(Gene = "TP53", Mode = "Boxplot", Show.P.value = TRUE, Show.P.label = TRUE, Method = "wilcox.test", values = c("#DF2020", "#DDDF21")) {
  data("tcga_gtex_sampleinfo", package = "UCSCXenaShiny", envir = environment())
  t1 <- get_pancan_value(Gene, dataset = "TcgaTargetGtex_rsem_isoform_tpm", host = "toilHub")
  tcga_gtex <- tcga_gtex %>% dplyr::distinct(sample, .keep_all = TRUE)
  t2 <- t1 %>%
    as.data.frame() %>%
    dplyr::rename("tpm" = ".") %>%
    tibble::rownames_to_column(var = "sample") %>%
    dplyr::inner_join(tcga_gtex, by = "sample")
  tumorlist <- unique(tcga_gtex[tcga_gtex$type2 == "tumor", ]$tissue)
  normallist <- unique(tcga_gtex[tcga_gtex$type2 == "normal", ]$tissue)
  withoutNormal <- setdiff(tumorlist, normallist)
  tcga_gtex <- t2 %>% dplyr::select("tpm", "tissue", "type2", "sample")
  tcga_gtex$type2 <- factor(tcga_gtex$type2, levels = c("tumor", "normal"))
  tcga_gtex_withNormal <- tcga_gtex[!(tcga_gtex$tissue %in% withoutNormal), ]
  tcga_gtex_MESO <- tcga_gtex[tcga_gtex$tissue == "MESO", ]
  tcga_gtex_UVM <- tcga_gtex[tcga_gtex$tissue == "UVM", ]
  if (Show.P.value == FALSE) {
    Show.P.label <- FALSE
  }
  if (Show.P.value == TRUE) {
    message("Counting P value")
    pv <- tcga_gtex_withNormal %>%
      ggpubr::compare_means(tpm ~ type2, data = ., method = Method, group.by = "tissue")
    pv <- pv %>% select(tissue, p, p.signif, p.adj)
    message("Counting P value finished")
  }
  if (Mode == "Boxplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(tissue, tpm, fill = type2)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab(NULL) + ggplot2::ylab(paste0(Gene, " expression (TPM)")) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(),
        legend.position = c(0, 0), legend.justification = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = values)
    p <- p + ggplot2::geom_boxplot(data = tcga_gtex_MESO) +
      ggplot2::geom_boxplot(data = tcga_gtex_UVM)
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = pv$p.signif
      ),
      data = pv, inherit.aes = F
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(p, 2))
      ),
      data = pv, inherit.aes = F
      )
    }
    print(p)
  }
  if (Mode == "Violinplot") {
    p <- ggplot2::ggplot(tcga_gtex_withNormal, aes(x = tissue, y = tpm, fill = type2)) + 
      geom_split_violin(
        draw_quantiles = c(0.25, 0.5, 0.75), 
        trim = T, 
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = T,
        position = "identity"
      ) + 
      ggplot2::ylab(paste0(Gene, " expression (TPM)")) + xlab("") +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_set(theme_set(theme_classic(base_size = 20))) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5)) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::theme(
        legend.background = element_blank(), 
        legend.position = c(0, 0), legend.justification = c(0, 0)
      )

    p + geom_split_violin(
      data = tcga_gtex_MESO,
      mapping = aes(x = tissue, y = tpm, fill = type2),
      draw_quantiles = c(0.25, 0.5, 0.75),
      trim = T,
      linetype = "solid",
      color = "black",
      size = 0.2,
      na.rm = T,
      position = "identity"
    ) +
      geom_split_violin(
        data = tcga_gtex_UVM,
        mapping = aes(x = tissue, y = tpm, fill = type2),
        draw_quantiles = c(0.25, 0.5, 0.75),
        trim = T,
        linetype = "solid",
        color = "black",
        size = 0.2,
        na.rm = T,
        position = "identity"
      ) +
      ggplot2::scale_x_discrete(limits = levels(tcga_gtex$tissue))
    if (Show.P.value == TRUE & Show.P.label == TRUE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = pv$p.signif
      ),
      data = pv, inherit.aes = F
      )
    }
    if (Show.P.value == TRUE & Show.P.label == FALSE) {
      p <- p + ggplot2::geom_text(aes(tissue,
        y = max(tcga_gtex_withNormal$tpm) * 1.1,
        label = as.character(signif(p, 2))
      ),
      data = pv, inherit.aes = F
      )
    }
    print(p)
  }
}

utils::globalVariables(
  c(
    ".",
    "tissue",
    "tpm",
    "type2",
    "p.signif",
    "p.adj"
  )
)
