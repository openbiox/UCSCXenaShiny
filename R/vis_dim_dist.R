#' Visualize the distribution difference of samples after dimension reduction analysis
#'
#' @param ids molecular identifiers (>=3)
#' @param data_type molecular types, refer to query_pancan_value() fuuntion
#' @param cancer_type select cancer cohort(s)
#' @param Mode how to define group metadata
#' @param group_name the provided phenotype under 'Clinical' mode. We collected six phenotypes based on TGCA project.
#' ("age","gender","race","stage_ajcc","stage_clinical","histological_grade"). For age, the older and yunger groups are determined by the median value.
#' For other phenotypes (such as stage_ajcc), the primary phenotypes were substracted.
#' @param custom_meta the provided metadata under 'Custom' mode
#' @param TCGA.only whether to include GTEx normal samples under 'TumorNomral' mode
#' @param DR_method the dimension reduction method
#' @param palette the color setting of RColorBrewer
#' @param add_margin the marginal plot (NULL, "density", "boxplot")
#' @param return.data whether to reture the raw meta/matrix data (list) instead of plot
#'
#' @return  a `ggplot` object.
#'
#' @examples
#' \donttest{
#' # The examples work, but may take a long time to run.
#'
#' p <- vis_dim_dist(DR_method = "UMAP")
#' p <- vis_dim_dist(Mode = "Clinical", group_name = "age")
#'
#' pancan_identifiers <- readRDS(
#'   system.file(
#'     "extdata", "pancan_identifier_list.rds",
#'     package = "UCSCXenaShiny"
#'   )
#' )
#' set.seed(42)
#' ids <- sample(pancan_identifiers$miRNA, 20)
#' p <- vis_dim_dist(ids = ids, data_type = "miRNA")
#'
#' p <- vis_dim_dist(cancer_type = c("LUAD", "LIHC", "LUSC"), Mode = "Cancer")
#'
#' set.seed(42)
#' custom_meta <- tcga_gtex %>%
#'   dplyr::mutate(across(where(is.factor), as.character)) %>%
#'   dplyr::filter(tissue == "LUAD") %>%
#'   dplyr::filter(grepl("TCGA", sample)) %>%
#'   dplyr::sample_frac(0.8) %>%
#'   dplyr::mutate(group = sample(c("TA", "TB"), size = nrow(.), replace = TRUE)) %>%
#'   dplyr::select(sample, group)
#' p <- vis_dim_dist(cancer_type = c("LUAD"), Mode = "Custom", custom_meta = custom_meta)
#' }
#' @export
vis_dim_dist <- function(ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
                         data_type = "mRNA", cancer_type = "LIHC",
                         Mode = c("TumorNomral", "Clinical", "Cancer", "Custom"),
                         group_name = NULL, custom_meta = NULL, TCGA.only = TRUE,
                         return.data = FALSE,
                         DR_method = c("PCA", "UMAP", "tSNE"),
                         palette = "Set1", add_margin = NULL) {
  Mode <- match.arg(Mode)
  DR_method <- match.arg(DR_method)


  if (length(ids) < 3) {
    stop("The number of valid ids is less than three. Please inspect the input ids and data_type(?query_pancan_value)")
  }

  if (Mode == "Clinical" && (is.null(group_name) |
    !group_name %in% c(
      "age", "gender", "race", "stage_ajcc",
      "stage_clinical", "histological_grade"
    ))) {
    stop("Please provide supported phenotype.")
  }

  ## step1: download assays  data
  exp_raw <- purrr::map(ids, function(x) {
    # x = ids[1]
    data <- query_pancan_value(x, data_type = data_type)
    data <- data[[1]]
    data <- dplyr::tibble(sample = names(data), y = as.numeric(data))
    colnames(data)[2] <- x
    data
  }) %>% purrr::reduce(dplyr::full_join, by = "sample")


  ## step2: prepare raw matrix and metadata
  if (Mode == "Clinical") {
    meta_raw <- load_data("tcga_clinical") %>%
      dplyr::mutate(across(where(is.factor), as.character)) %>%
      dplyr::filter(sample %in% exp_raw$sample) %>%
      .[, 1:10]
    meta_raw <- meta_raw[!apply(meta_raw[, -1], 1, function(x) all(is.na(x))), ] # clinical feature all NA
    meta_raw <- meta_raw[!grepl("-11$", meta_raw$sample), ]
    colnames(meta_raw) <- c(
      "sample", "patient", "cancer", "age", "gender", "race",
      "stage_ajcc", "stage_clinical",
      "histological_type", "histological_grade"
    )
    # RACE
    meta_raw$race[!meta_raw$race %in% c("ASIAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE")] <- NA
    # AJCC stage
    meta_raw$stage_ajcc[!grepl("Stage", meta_raw$stage_ajcc)] <- NA
    meta_raw$stage_ajcc[meta_raw$stage_ajcc %in% c("Stage 0", "Stage X")] <- NA
    meta_raw$stage_ajcc <- gsub("[ABC]", "", meta_raw$stage_ajcc)
    # Clinical stage
    meta_raw$stage_clinical[!grepl("Stage", meta_raw$stage_clinical)] <- NA
    meta_raw$stage_clinical <- gsub("[ABC12]", "", meta_raw$stage_clinical)
    meta_raw$stage_clinical[meta_raw$stage_clinical == "Stage IS"] <- "Stage I"
    # histological grade
    meta_raw$histological_grade[!meta_raw$histological_grade %in% paste0("G", 1:4)] <- NA
  } else {
    meta_raw <- load_data("tcga_gtex") %>%
      dplyr::mutate(across(where(is.factor), as.character)) %>%
      dplyr::filter(sample %in% exp_raw$sample)
  }

  exp_raw <- exp_raw[match(meta_raw$sample, exp_raw$sample), ]
  meta_raw <- meta_raw %>%
    dplyr::mutate(id = seq(nrow(.)), .before = 1)
  exp_raw <- exp_raw %>%
    dplyr::mutate(id = seq(nrow(.)), .before = 1)

  ## step3: prepare fine matrix and metadata
  if (Mode == "TumorNomral") {
    group_levels <- c("tumor", "normal")
    meta_data <- meta_raw %>%
      dplyr::filter(.data$tissue %in% cancer_type) %>%
      dplyr::rename("group" = "type2") %>%
      dplyr::mutate(group = factor(group, levels = group_levels))
    if (TCGA.only) {
      meta_data <- meta_data %>% dplyr::filter(grepl("TCGA", sample))
    }
  } else if (Mode == "Clinical") {
    group_levels <- switch(group_name,
      "age" = c("younger", "older"),
      "gender" = c("MALE", "FEMALE"),
      "race" = c("WHITE", "ASIAN", "AMERICAN INDIAN OR ALASKA NATIVE"),
      "stage_ajcc" = c("Stage I", "Stage II", "Stage III", "Stage IV"),
      "stage_clinical" = c("Stage I", "Stage II", "Stage III", "Stage IV"),
      "histological_grade" = c("G1", "G2", "G3", "G4")
    )

    meta_data <- meta_raw[, c("id", "sample", "cancer", group_name)]
    colnames(meta_data)[4] <- "group"
    meta_data <- meta_data %>%
      dplyr::filter(.data$cancer %in% cancer_type) %>%
      dplyr::filter(!is.na(group))
    if (group_name == "age") {
      meta_data <- meta_data %>% dplyr::mutate(group = ifelse(group > median(group), "older", "younger"))
    }
    meta_data <- meta_data %>%
      dplyr::filter(group %in% group_levels) %>%
      dplyr::mutate(group = factor(group, levels = group_levels))
  } else if (Mode == "Cancer") {
    group_levels <- cancer_type
    meta_data <- meta_raw %>%
      dplyr::filter(.data$tissue %in% cancer_type) %>%
      dplyr::filter(.data$type2 == "tumor") %>%
      dplyr::mutate(tissue = factor(.data$tissue, levels = cancer_type)) %>%
      dplyr::mutate(group = .data$tissue)
  } else if (Mode == "Custom") {
    group_levels <- unique(custom_meta$group)
    meta_data <- meta_raw[, c("id", "sample")] %>%
      dplyr::inner_join(custom_meta) %>%
      dplyr::mutate(group = factor(group, levels = group_levels))
  }

  exp_data <- exp_raw[match(meta_data$id, exp_raw$id), ]
  ids_NAN <- colnames(exp_data[, -1:-2])[apply(exp_data[, -1:-2], 2, function(x) all(is.na(x)))]
  ids_SD0 <- colnames(exp_data[, -1:-2])[apply(exp_data[, -1:-2], 2, function(x) stats::sd(x) == 0)] %>% na.omit()
  ids_OK <- setdiff(ids, c(ids_NAN, ids_SD0))
  # message(paste0((length(ids_OK)/length(ids))*100, "%"), " of input ids were obtained")
  exp_data <- exp_data[, which(!colnames(exp_data) %in% c(ids_NAN, ids_SD0))]
  meta_data <- meta_data[, c("id", "sample", "group")]

  if (length(ids_OK) < 3) {
    stop("The number of valid ids is less than three. Please inspect the input ids and data_type(?query_pancan_value)")
  }

  if (length(unique(meta_data$group)) < 2) {
    stop("The number of valid group levels is less than two")
  }

  if (return.data) {
    return(list(meta = meta_data, exp = exp_data))
  }

  ## step4: choose dimension reduction methods
  if (DR_method == "PCA") {
    pca_obj <- prcomp(exp_data[, ids_OK], center = TRUE, scale = TRUE)
    res_dims <- pca_obj$x[, 1:2] %>%
      as.data.frame() %>%
      dplyr::rename("PC_1" = "PC1", "PC_2" = "PC2")
  } else if (DR_method == "tSNE") {
    set.seed(123)
    tsne_obj <- Rtsne::Rtsne(exp_data[, ids_OK])
    res_dims <- tsne_obj$Y %>%
      as.data.frame() %>%
      dplyr::rename("tSNE_1" = "V1", "tSNE_2" = "V2")
  } else if (DR_method == "UMAP") {
    umap_obj <- umap::umap(exp_data[, ids_OK])
    res_dims <- umap_obj$layout %>%
      as.data.frame() %>%
      dplyr::rename("UMAP_1" = "V1", "UMAP_2" = "V2")
  }
  res_dims <- cbind(res_dims, meta_data)


  ## Step5: ggplot scatter plot
  groups <- max(length(cancer_type), length(group_levels))

  if (groups > 6) {
    color <- grDevices::hcl(
      h = seq(15, 375, length = groups + 1),
      l = 65, c = 100
    )[seq(groups)]
    shape <- rep(16, groups)
  } else {
    color <- RColorBrewer::brewer.pal(n = 6, name = palette)[seq(groups)]
    shape <- c(15:20)[seq(groups)]
  }


  p <- ggplot2::ggplot(res_dims, aes_string(colnames(res_dims)[1], colnames(res_dims)[2], color = "group", shape = "group")) +
    ggplot2::geom_point() +
    ggplot2::stat_ellipse() +
    ggplot2::theme_classic(base_size = 20) +
    ggplot2::guides(
      color = guide_legend(title = NULL),
      shape = guide_legend(title = NULL)
    ) +
    ggplot2::theme(
      legend.background = element_blank(),
      legend.position = c(0, 0),
      legend.justification = c(0, 0)
    ) +
    ggplot2::scale_color_manual(values = color) +
    ggplot2::scale_shape_manual(values = shape)

  if (!is.null(add_margin)) {
    geom_type <- switch(add_margin,
      "density" = geom_density,
      "boxplot" = geom_boxplot,
      stop("Please choose one of density/boxplot marginal type")
    )

    p_right <- cowplot::axis_canvas(p, axis = "x") +
      geom_type(
        data = p$data, aes_string(x = colnames(p$data)[1], fill = "group"),
        alpha = 0.8, linewidth = 0.3
      ) +
      ggplot2::scale_fill_manual(values = color)
    p_top <- cowplot::axis_canvas(p, axis = "y", coord_flip = TRUE) +
      geom_type(
        data = p$data, aes_string(x = colnames(p$data)[2], fill = "group"),
        alpha = 0.8, linewidth = 0.3
      ) +
      coord_flip() +
      ggplot2::scale_fill_manual(values = color)
    p_tmp <- p %>%
      cowplot::insert_xaxis_grob(p_right, grid::unit(.2, "null"), position = "top") %>%
      cowplot::insert_yaxis_grob(p_top, grid::unit(.2, "null"), position = "right")
    p_tmp2 <- cowplot::ggdraw(p_tmp)
    p_tmp2$data <- p$data
    p <- p_tmp2
  }
  return(p)
}
