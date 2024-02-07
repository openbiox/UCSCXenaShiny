#' Visualize the distribution difference of samples after dimension reduction analysis
#'
#' @param ids molecular identifiers (>=3)
#' @param data_type molecular types, refer to query_pancan_value() function
#' @param group_info two-column grouping information with names 'Sample','Group'
#' @param DR_method the dimension reduction method
#' @param palette the color setting of RColorBrewer
#' @param add_margin the marginal plot (NULL, "density", "boxplot")
#' @param opt_pancan specify one dataset for some molercular profiles

#' @return a ggplot object or rawdata list
#' @export
#'
#' @examples
#' \dontrun{
#' group_info = tcga_clinical_fine %>% 
#'   dplyr::filter(Cancer=="BRCA") %>% 
#'   dplyr::select(Sample, Code) %>% 
#'   dplyr::rename(Group=Code)
#' 
#' vis_dim_dist(
#'   ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
#'   group_info = group_info
#' )
#' 
#' }
#' 
vis_dim_dist <- function(ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
                          data_type = "mRNA", 
                          group_info = NULL, 
                          DR_method = c("PCA", "UMAP", "tSNE"),
                          palette = "Set1", 
                          add_margin = NULL,
                          opt_pancan = .opt_pancan) {
  # Mode <- match.arg(Mode)
  DR_method <- match.arg(DR_method)
  
  if (length(ids) < 3) {
    stop("The number of valid ids is less than three. Please inspect the input ids and data_type(?query_pancan_value)")
  }
  
  exp_raw <- purrr::map(ids, function(x) {
    # x = ids[1]
    data <- query_pancan_value(x, data_type = data_type, opt_pancan=opt_pancan)
    data <- data[[1]]
    data <- dplyr::tibble(Sample = names(data), y = as.numeric(data))
    colnames(data)[2] <- x
    data
  }) %>% purrr::reduce(dplyr::full_join, by = "Sample")
  
  
  # meta_raw = query_tcga_group(...)$data
  if (is.null(group_info)) {
    stop("Please input valid grouping information for `group_info` parameter.")
  }
  if(!all(colnames(group_info) == c("Sample", "Group"))){
    stop("The group_info should have two colnames named `Sample` and `Group`.")
  }
  meta_raw = group_info
  meta_data = meta_raw %>% dplyr::filter(.data$Sample %in% exp_raw$Sample)

  if(nrow(meta_data)==0){
    stop("No intersected samples are detected for the group_info.")
  }
  if(length(unique(meta_data$Group))<2){
    stop("Less two valid groups are detected for the group_info.")
  }

  
  exp_data = exp_raw[match(meta_data$Sample, exp_raw$Sample), ]
  ids_NAN <- colnames(exp_data[, -1])[apply(exp_data[, -1], 2, function(x) all(is.na(x)))]
  ids_SD0 <- colnames(exp_data[, -1])[apply(exp_data[, -1], 2, function(x) stats::sd(x) == 0)] %>% na.omit()
  ids_OK <- setdiff(ids, c(ids_NAN, ids_SD0))
  # message(paste0((length(ids_OK)/length(ids))*100, "%"), " of input ids were obtained")
  exp_data <- exp_data[, which(!colnames(exp_data) %in% c(ids_NAN, ids_SD0))]

  
  if (DR_method == "PCA") {
    pca_obj <- prcomp(exp_data[, ids_OK], center = TRUE, scale = TRUE)
    res_dims <- pca_obj$x[, 1:2] %>%
      as.data.frame() %>%
      dplyr::rename("PC_1" = "PC1", "PC_2" = "PC2")
  } else if (DR_method == "tSNE") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      stop(
        "Package \"Rtsne\" must be installed to use this method.",
        call. = FALSE
      )
    }
    set.seed(123)
    tsne_obj <- Rtsne::Rtsne(exp_data[, ids_OK])
    res_dims <- tsne_obj$Y %>%
      as.data.frame() %>%
      dplyr::rename("tSNE_1" = "V1", "tSNE_2" = "V2")
  } else if (DR_method == "UMAP") {
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop(
        "Package \"umap\" must be installed to use this method.",
        call. = FALSE
      )
    }
    umap_obj <- umap::umap(exp_data[, ids_OK])
    res_dims <- umap_obj$layout %>%
      as.data.frame() %>%
      dplyr::rename("UMAP_1" = "V1", "UMAP_2" = "V2")
  }
  
  res_dims <- cbind(res_dims, meta_data) %>%
    dplyr::inner_join(exp_data)
  
  
  ## Step5: ggplot scatter plot


  group_levels = unique(res_dims$Group)


  if (length(group_levels) > 6) {
    colors <- grDevices::hcl(
      h = seq(15, 375, length = length(group_levels) + 1),
      l = 65, c = 100
    )[seq(length(group_levels))]
    shapes <- rep(16, length(group_levels))
  } else {
    colors <- RColorBrewer::brewer.pal(n = 6, name = palette)[seq(group_levels)]
    shapes <- c(15:20)[seq(group_levels)]
  }
  
  
  p <- ggplot2::ggplot(res_dims, aes_string(colnames(res_dims)[1], colnames(res_dims)[2], color = "Group", shape = "Group")) +
    ggplot2::geom_point() +
    ggplot2::stat_ellipse() +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::guides(
      color = guide_legend(title = NULL),
      shape = guide_legend(title = NULL)
    ) +
    ggplot2::theme(
      # legend.background = element_blank(),
      # legend.position = c(0, 0),
      # legend.justification = c(0, 0)
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_shape_manual(values = shapes)
  
  if (!is.null(add_margin)) {
    geom_type <- switch(add_margin,
                        "density" = geom_density,
                        "boxplot" = geom_boxplot,
                        stop("Please choose one of density/boxplot marginal type")
    )
    
    p_right <- cowplot::axis_canvas(p, axis = "x") +
      geom_type(
        data = p$data, aes_string(x = colnames(p$data)[1], fill = "Group"),
        alpha = 0.8, linewidth = 0.3
      ) +
      ggplot2::scale_fill_manual(values = colors)
    p_top <- cowplot::axis_canvas(p, axis = "y", coord_flip = TRUE) +
      geom_type(
        data = p$data, aes_string(x = colnames(p$data)[2], fill = "Group"),
        alpha = 0.8, linewidth = 0.3
      ) +
      coord_flip() +
      ggplot2::scale_fill_manual(values = colors)
    p_tmp <- p %>%
      cowplot::insert_xaxis_grob(p_right, grid::unit(.2, "null"), position = "top") %>%
      cowplot::insert_yaxis_grob(p_top, grid::unit(.2, "null"), position = "right")
    p_tmp2 <- cowplot::ggdraw(p_tmp)
    p_tmp2$data <- p$data
    p <- p_tmp2
  }
  return(p)
}