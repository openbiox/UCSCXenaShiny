# Obtain path to individual server code parts ----------------------------
server_file <- function(x) {
  server_path <- system.file("shinyapp", "server",
    package = "UCSCXenaShiny", mustWork = TRUE
  )
  file.path(server_path, x)
}


# Set utility functions ---------------------------------------------------
QUERY_CACHE <- dplyr::tibble()
xe_query_url <- function(data, use_cache = TRUE) {
  if (use_cache) {
    if (nrow(QUERY_CACHE) == 0) {
      non_exist_idx <- !data$XenaDatasets %in% NULL
    } else {
      non_exist_idx <- !data$XenaDatasets %in% QUERY_CACHE$datasets
    }
    if (any(non_exist_idx)) {
      non_exist_query <- xe_query_url(data[non_exist_idx, , drop = FALSE], use_cache = FALSE)
      QUERY_CACHE <<- dplyr::bind_rows(
        QUERY_CACHE,
        non_exist_query
      )
    }

    xe_query <- dplyr::filter(QUERY_CACHE, QUERY_CACHE$datasets %in% data$XenaDatasets)
  } else {
    xe <-
      UCSCXenaTools::XenaGenerate(subset = XenaDatasets %in% data$XenaDatasets)

    xe_query <- UCSCXenaTools::XenaQuery(xe)
    xe_query$browse <- purrr::map2(
      xe_query$datasets, xe_query$hosts,
      ~ utils::URLencode(
        paste0(
          "https://xenabrowser.net/datapages/?",
          "dataset=", .x, "&host=", .y
        )
      )
    ) %>% unlist()
  }

  return(xe_query)
}

get_data_df <- function(dataset, id) {
  if (dataset == "custom_phenotype_dataset") {
    message("Loading custom phenotype data.")
    df <- readRDS(file.path(tempdir(), "custom_phenotype_data.rds"))
  } else {
    message("Querying data of identifier ", id, " from dataset ", dataset)
    id_value <- if (dataset == "custom_feature_dataset") {
      UCSCXenaShiny:::query_custom_feature_value(id)
    } else {
      UCSCXenaShiny::query_molecule_value(dataset, id)
    }
    df <- dplyr::tibble(
      sample = names(id_value),
      X = as.numeric(id_value)
    )
    colnames(df)[2] <- id 
  }
  df
}




useShinydashboard2 <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}




# vis_gene_cross_omics = function(gene="TP53", 
#                                 tumor_samples = NULL,
#                                 n_trans = 5, n_methy = 5,seed = 42,
#                                 return_list = FALSE){
#   if(is.null(tumor_samples)){
#     tcga_gtex2 = tcga_gtex
#     tcga_clinical_fine2 = tcga_clinical_fine
#   } else {
#     tcga_gtex2 = tcga_gtex %>% 
#       dplyr::filter(type2=="normal" | sample %in% tumor_samples)
#     tcga_clinical_fine2 = tcga_clinical_fine %>% 
#       dplyr::filter(Code=="NT" | Sample %in% tumor_samples)
#   }
#   #### Omics--mRNA
#   gene_mrna = query_pancan_value(gene, "mRNA") %>% 
#     .[["expression"]] %>% as.data.frame() %>% 
#     dplyr::rename('Exp'='.') %>% 
#     tibble::rownames_to_column("sample") %>% 
#     dplyr::inner_join(tcga_gtex2)
#   if(all(is.na(gene_mrna$Exp))) stop("Please input a valid gne!")
  
#   #median value, normal samples include GTEx
#   gene_mrna_md = gene_mrna %>% 
#     dplyr::group_by(tissue, type2) %>% 
#     dplyr::summarise(Exp = median(Exp)) %>% as.data.frame() %>% 
#     dplyr::mutate(Exp = (Exp-min(Exp))/(max(Exp)-min(Exp))) %>%  # 0~1 scale
#     tidyr::pivot_wider(id_cols = tissue, names_from = type2, values_from = Exp)
  
#   gene_mrna_comp = compare_means(Exp ~ type2, data = gene_mrna,
#                                  group.by = "tissue", method = "wilcox.test") 
  
#   gene_mrna_plot = dplyr::left_join(gene_mrna_md, gene_mrna_comp[,c("tissue","p","p.adj")]) %>% 
#     as.data.frame() %>% 
#     dplyr::rename("id" = "tissue") %>% 
#     dplyr::mutate(Label = case_when(
#       p < 0.01 & tumor>normal ~ "Up-Reg.",
#       p < 0.01 & tumor<normal ~ "Down-Reg.",
#       TRUE~"Non-Sig."
#     )) %>% 
#     dplyr::select(id, normal, tumor, Label)
  
#   #### Omics--mutation
#   gene_mut = query_pancan_value(gene, "mutation") %>% 
#     as.data.frame() %>% 
#     dplyr::rename("Mut"=".") %>% 
#     tibble::rownames_to_column("sample") %>% 
#     dplyr::inner_join(tcga_clinical_fine2, by=c("sample"="Sample"))
  
#   gene_mut_plot = gene_mut %>% 
#     dplyr::group_by(Cancer) %>% 
#     dplyr::summarise(Mut = mean(Mut)) %>% 
#     as.data.frame() %>% 
#     dplyr::mutate(Ratio = NA)
  
#   for(i in seq(nrow(gene_mut_plot))){
#     # i = 1
#     gene_mut_plot$Ratio[i] = list(c(Mut=gene_mut_plot$Mut[i], Wild=1-gene_mut_plot$Mut[i]))
#   }
  
#   gene_mut_plot = gene_mut_plot %>% 
#     dplyr::mutate(Mut_str = scales::percent(Mut,accuracy = 0.1)) %>% 
#     dplyr::select(Cancer, Ratio, Mut, Mut_str)
  
#   #### Omics--CNV
#   opt_pancan = .opt_pancan
#   opt_pancan$toil_cnv$use_thresholded_data=T
#   gene_cnv = query_pancan_value(
#     gene, "cnv", opt_pancan = opt_pancan
#   ) %>% as.data.frame() %>% 
#     dplyr::select(data) %>% 
#     dplyr::rename('CNV'='data') %>% 
#     tibble::rownames_to_column("sample") %>% 
#     dplyr::inner_join(tcga_clinical_fine2, by=c("sample"="Sample"))
  
#   gene_cnv_tb = table(gene_cnv$Cancer, gene_cnv$CNV)
#   gene_cnv_plot = gene_cnv %>% 
#     dplyr::count(Cancer, name = "CNV_total") %>% 
#     dplyr::mutate(CNV_Pie=NA, CNV_Del = NA, CNV_Amp = NA)
  
#   for (i in seq(nrow(gene_cnv_plot))){
#     # i = 1
#     Cancer = gene_cnv_plot$Cancer[i]
#     gene_cnv_plot$CNV_Pie[i] = list(c(
#       ' -2' = gene_cnv_tb[Cancer,"-2"],
#       ' -1' = gene_cnv_tb[Cancer,"-1"],
#       ' 0' = gene_cnv_tb[Cancer,"0"],
#       ' 1' = gene_cnv_tb[Cancer,"1"],
#       ' 2' = gene_cnv_tb[Cancer,"2"]
#     ))
#     gene_cnv_plot$CNV_Del[i] = gene_cnv_tb[Cancer,"-2"] + gene_cnv_tb[Cancer,"-1"]
#     gene_cnv_plot$CNV_Amp[i] = gene_cnv_tb[Cancer,"2"] + gene_cnv_tb[Cancer,"1"]
#   }
  
#   gene_cnv_plot = gene_cnv_plot %>%
#     dplyr::mutate(CNV_Amp = CNV_Amp/CNV_total) %>% 
#     dplyr::mutate(CNV_Amp_str = scales::percent(CNV_Amp, accuracy = 0.1)) %>% 
#     dplyr::mutate(CNV_Del = CNV_Del/CNV_total) %>% 
#     dplyr::mutate(CNV_Del_str = scales::percent(CNV_Del, accuracy = 0.1)) %>% 
#     dplyr::select(Cancer, CNV_Pie, CNV_Amp, CNV_Amp_str, CNV_Del, CNV_Del_str)
  
#   #### Omics--Transcript
#   Trans_sub = load_data("v2_tpc_id_help")$tcga$id_trans %>% 
#     dplyr::filter(Symbol == gene) %>% 
#     dplyr::pull(Level3)
#   set.seed(seed)
#   if (class(n_trans)=="character"){
#     if(length(n_trans)>10) stop("Less than 10 CpG sites are supported!")
#     trans_sle = n_trans[n_trans %in% Trans_sub]
#   } else if (class(n_trans)=="numeric"){
#     if(n_trans>10) stop("Less than 10 transcripts are supported!")
#     trans_sle = unique(sample(Trans_sub, n_trans, replace = n_trans>length(Trans_sub)))
#   }
  
#   if(all(is.na(n_trans)) | length(trans_sle)==0){ 
#     gene_trans_n = 0 
#   } else {
#     gene_trans = lapply(seq(trans_sle), function(i){
#       # i = 1
#       gene_trans_tmp = query_pancan_value(
#         trans_sle[i], "transcript"
#       ) %>% as.data.frame() %>% 
#         dplyr::select(expression) %>% 
#         dplyr::rename("Trans"="expression") %>% 
#         tibble::rownames_to_column("sample") %>% 
#         dplyr::inner_join(tcga_gtex2) %>% 
#         dplyr::filter(type2=="tumor") %>%
#         dplyr::group_by(tissue) %>% 
#         dplyr::summarise(Trans = median(Trans))
#       colnames(gene_trans_tmp)[2] = trans_sle[i]
#       gene_trans_tmp %>% tibble::column_to_rownames("tissue") 
#     }) %>% do.call(cbind, .)
#     gene_trans_plot = gene_trans[,apply(gene_trans,2,sd)!=0,drop=FALSE] %>% 
#       as.data.frame() %>% 
#       tibble::rownames_to_column("tissue")
#     gene_trans_n = ncol(gene_trans_plot)-1
#   }
  
  
#   #### Omics--Methylation
#   Methy450_sub = load_data("v2_tpc_id_help")$tcga$id_M450 %>% 
#     dplyr::filter(Level3 == gene) %>% 
#     dplyr::pull(CpG)
#   set.seed(seed)
#   if (class(n_methy)=="character"){
#     if(length(n_methy)>10) stop("Less than 10 CpG sites are supported!")
#     cpg_sites = n_methy[n_methy %in% Methy450_sub]
#   } else if (class(n_methy)=="numeric"){
#     if(n_methy>10) stop("Less than 10 CpG sites are supported!")
#     cpg_sites = unique(sample(Methy450_sub, n_methy, replace = n_methy>length(Methy450_sub)))
#   }
#   if(all(is.na(n_methy)) | length(cpg_sites)==0){ 
#     gene_methy_n = 0 
#   } else {
#     gene_methy_cpgs = lapply(seq(cpg_sites), function(i){
#       # i = 1
#       opt_pancan = .opt_pancan
#       opt_pancan$toil_methylation$rule_out = setdiff(Methy450_sub, cpg_sites[i])
#       opt_pancan$toil_methylation$aggr = "mean"
#       gene_methy_tmp = query_pancan_value(
#         gene, "methylation", opt_pancan = opt_pancan
#       ) %>% as.data.frame() %>% 
#         dplyr::select(data) %>% 
#         dplyr::rename("Methy"='data') %>% 
#         tibble::rownames_to_column("sample") %>% 
#         # dplyr::inner_join(tcga_gtex2) %>% 
#         dplyr::inner_join(tcga_clinical_fine2, by=c("sample"="Sample")) %>% 
#         dplyr::filter(Code!="NT") %>% 
#         dplyr::group_by(Cancer) %>% 
#         dplyr::summarise(Methy = median(Methy))
#       colnames(gene_methy_tmp)[2] = cpg_sites[i]
#       gene_methy_tmp %>% tibble::column_to_rownames("Cancer") 
#     }) %>% do.call(cbind, .) %>% 
#       tibble::rownames_to_column("Cancer")
#     gene_methy_plot = gene_methy_cpgs
#     gene_methy_n = ncol(gene_methy_plot)-1
#   }
  
#   #### Ready for Plot
#   gene_cross_dat = data.frame(id = as.character(sort(unique(tcga_gtex$tissue)))) %>% 
#     dplyr::left_join(gene_mrna_plot) %>% 
#     dplyr::left_join(gene_mut_plot, by=c("id"="Cancer")) %>% 
#     dplyr::left_join(gene_cnv_plot, by=c("id"="Cancer"))
    
#   idx_1 = which(sapply(gene_cross_dat$Ratio, is.null))
#   for(i in idx_1){
#     gene_cross_dat$Ratio[[i]] = c(Mut=NA, Wild=NA)
#   }
#   idx_2 = which(sapply(gene_cross_dat$CNV_Pie, is.null))
#   for(i in idx_2){
#     gene_cross_dat$CNV_Pie[[i]] = c(` -2`=NA, ` -1`=NA, ` 0`=NA, ` 1`=NA, ` 2`=NA)
#   }  
  
#   if(gene_trans_n>0){
#     gene_cross_dat = dplyr::left_join(gene_cross_dat, gene_trans_plot, by=c("id"="tissue"))
#   }
#   if(gene_methy_n>0){
#     gene_cross_dat = dplyr::left_join(gene_cross_dat, gene_methy_plot, by=c("id"="Cancer"))
#   }
  
  
  
#   label_name = colnames(gene_cross_dat)
#   label_name[1:12] = c("TCGA","Normal Exp","Tumor Exp","Tumor vs. Normal", 
#                        "Mutation Dist", "Mutation PCT", "",
#                        "CNV Dist", "CNV Amp", "","CNV Del", "")
  
#   column_info = data.frame(
#     # columns to vis
#     id = colnames(gene_cross_dat),
#     # column name
#     name = label_name, 
#     # group name
#     group = c(NA, rep("Gene Expression", 3), rep("Gene Mutation", 3), rep("Gene CNV", 5), 
#               rep("Gene Transcript", gene_trans_n), rep("Gene Methylation", gene_methy_n)),
#     # geom type
#     geom = c("text","bar","bar","text",   "pie","bar","text",
#              "pie","bar","text","bar","text",
#              rep("funkyrect", gene_trans_n), rep("funkyrect", gene_methy_n)),
#     # palette type
#     palette = c(NA,"Tumor-Exp","Tumor-Exp",NA ,"Mut-Pie","Mut-Ratio",NA, 
#                 "CNV-Pie","CNV-Amp",NA,"CNV-Del",NA, 
#                 rep("Scale.Transcript", gene_trans_n), rep("Scale.Methylation",gene_methy_n)), 
#     # column width
#     width = c(4, 4, 4, 4,   2, 4, 4, 
#               2, 4, 4, 4, 4, rep(1.1, gene_trans_n), rep(1.1, gene_methy_n))
#   )
  
#   column_info$options = NA
#   for(i in seq(nrow(column_info))){
#     column_info$options[i] = list(list())
#   }
#   column_info$options[[2]] = list(scale=F,hjust=1)
#   column_info$options[[3]] = list(scale=F)
#   column_info$options[[6]] = list(scale=F)
#   column_info$options[[7]] = list(label="Mut_str",overlay=T)
#   column_info$options[[9]] = list(scale=F)
#   column_info$options[[10]] = list(label="CNV_Amp_str",overlay=T)
#   column_info$options[[11]] = list(scale=F)
#   column_info$options[[12]] = list(label="CNV_Del_str",overlay=T)
  
#   column_groups = data.frame(
#     group = c("Gene Expression", "Gene Mutation", "Gene CNV", "Gene Methylation", 'Gene Transcript'),
#     palette = c("palette1", "palette2", "palette3","palette4", "palette5")
#   )
  
#   palettes = list(
#     palette1 = c("#66c2a5","#66c2a5"),
#     palette2 = c("#fc8d62","#fc8d62"),
#     palette3 = c("#8da0cb","#8da0cb"),
#     palette4 = c("#e78ac3","#e78ac3"),
#     palette5 = c("#a6d854","#a6d854"),
#     `Tumor-Exp` = RColorBrewer::brewer.pal(n = 9, name = "Greys")[1:6],
#     `Mut-Ratio` = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
#     `Mut-Pie` = c(Mut="red", Wild="grey"),
#     `CNV-Pie` = c(' -2'='#5e3c99',' -1'='#b2abd2',' 0'='#f7f7f7',' 1'='#fdb863',' 2'='#e66101'),
#     `CNV-Amp` = RColorBrewer::brewer.pal(n = 9, name = "Oranges"),
#     `CNV-Del` = RColorBrewer::brewer.pal(n = 9, name = "Purples"),
#     Scale.Methylation = RColorBrewer::brewer.pal(n = 9, name = "BuPu"),
#     Scale.Transcript = RColorBrewer::brewer.pal(n = 9, name = "Greens")
#   )
  
#   position_arguments = position_arguments(expand_xmax  = 6, 
#                                           col_annot_angle = 35, 
#                                           col_width = 1.1,
#                                           col_space = 0.2,
#                                           col_annot_offset = 4)
  
#   g = funky_heatmap(gene_cross_dat, 
#                     column_info = column_info,
#                     column_groups = column_groups,
#                     palettes = palettes,
#                     position_args = position_arguments)
  
  
#   if(return_list){
#     return(list(plot = g, data = gene_cross_dat))
#   } else {
#     return(g)
#   }
# }