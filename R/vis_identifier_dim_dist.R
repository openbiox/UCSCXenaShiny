#' Visualize the distribution difference of samples after Molecule Identifier dimension reduction analysis
#'
#' NOTE: the dataset must be dense matrix in UCSC Xena data hubs.
#' 
#' 
#' @param dataset the dataset to obtain identifiers. 
#' @param ids the molecule identifiers. 
#' @param grp_df When `dataset` and `id` are all not `NULL`, it should be a `data.frame` with 2 columns.
#' - The first column refers to sample ID.
#' - The second column refers to groups indicated in axis X.
#' @param samples default is `NULL`, can be common sample names for two datasets.
#' @param return.data whether to reture the raw meta/matrix data (list) instead of plot
#' @param DR_method the dimension reduction method
#' @param add_margin the marginal plot (NULL, "density", "boxplot")
#' @param palette the color setting of RColorBrewer
#'
#' @return  a `ggplot` object.
#' @export
#'
#' @examples
#' \donttest{
#' library(UCSCXenaTools)
#' expr_dataset <- "TCGA.LUAD.sampleMap/HiSeqV2_percentile"
#' ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A")
#' 
#' cli_dataset <- "TCGA.LUAD.sampleMap/LUAD_clinicalMatrix"
#' cli_df <- XenaGenerate(
#'   subset = XenaDatasets == cli_dataset
#' ) %>%
#'   XenaQuery() %>%
#'   XenaDownload() %>%
#'   XenaPrepare()
#' grp_df = cli_df[, c("sampleID", "gender")]

#' vis_identifier_dim_dist(expr_dataset, ids, grp_df, DR_method="PCA")
#' }
#' @export
vis_identifier_dim_dist = function(dataset = NULL, ids = NULL, grp_df, samples = NULL,
								   return.data = FALSE,
								   DR_method = c("PCA", "UMAP", "tSNE"), add_margin = NULL,
								   palette = "Set1"){

	stopifnot(ncol(grp_df) > 1)
	DR_method <- match.arg(DR_method)
	colnames(grp_df)[1] <- "sample"

	if (!is.null(dataset) && !is.null(ids)) {
		df <- purrr::map(ids, function(x) {
		message("Querying data of identifier ", x, " from dataset ", dataset, " for DR")
		data <- if (dataset == "custom_feature_dataset") query_custom_feature_value(x) else
		  query_molecule_value(dataset, x)
		data <- dplyr::tibble(
		  sample = names(data),
		  y = as.numeric(data)
		)
		colnames(data)[2] <- x
		data
		}) %>%
		  purrr::reduce(dplyr::full_join, by = "sample")
		df <- dplyr::inner_join(grp_df, df, by = "sample")
	} else {
		message("Directly use 'grp_df' for comparison analysis.")
		df <- grp_df
		ids = colnames(grp_df)[-1:-2]
	}

	if (!is.null(samples)) {
	  df <- dplyr::filter(df, .data$sample %in% samples)
	}

	ids_NAN <- colnames(df[, ids])[apply(df[, ids], 2, function(x) all(is.na(x)))]
	ids_SD0 <- colnames(df[, ids])[apply(df[, ids], 2, function(x) stats::sd(x) == 0)] %>% na.omit()
	ids_OK <- setdiff(ids, c(ids_NAN, ids_SD0))

	if (length(ids_OK) < 3) {
	  stop("The number of valid ids is less than three. Please inspect the input ids and data_type(?query_pancan_value)")
	}

	if (return.data) {
	  return(list(meta = grp_df, exp = df))
	}

	if (DR_method == "PCA") {
	pca_obj <- prcomp(df[, ids_OK], center = TRUE, scale = TRUE)
	res_dims <- pca_obj$x[, 1:2] %>%
	  as.data.frame() %>%
	  dplyr::rename("PC_1" = "PC1", "PC_2" = "PC2")
	} else if (DR_method == "tSNE") {
	set.seed(123)
	tsne_obj <- Rtsne::Rtsne(df[, ids_OK])
	res_dims <- tsne_obj$Y %>%
	  as.data.frame() %>%
	  dplyr::rename("tSNE_1" = "V1", "tSNE_2" = "V2")
	} else if (DR_method == "UMAP") {
	umap_obj <- umap::umap(df[, ids_OK])
	res_dims <- umap_obj$layout %>%
	  as.data.frame() %>%
	  dplyr::rename("UMAP_1" = "V1", "UMAP_2" = "V2")
	}

	res_dims <- cbind(df[1:2], res_dims)

	groups <- length(unique(res_dims[,colnames(grp_df)[2],drop=TRUE]))
	if (groups < 2) {
	  stop("The number of valid group levels is less than two")
	}

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

	p <- ggplot2::ggplot(res_dims, 
		aes_string(colnames(res_dims)[3], colnames(res_dims)[4], color = colnames(res_dims)[2], shape = colnames(res_dims)[2])) +
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
	      data = p$data, aes_string(x = colnames(p$data)[3], fill = colnames(grp_df)[2]),
	      alpha = 0.8, linewidth = 0.3
	    ) +
	    ggplot2::scale_fill_manual(values = color)
	  p_top <- cowplot::axis_canvas(p, axis = "y", coord_flip = TRUE) +
	    geom_type(
	      data = p$data, aes_string(x = colnames(p$data)[4], fill = colnames(grp_df)[2]),
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






