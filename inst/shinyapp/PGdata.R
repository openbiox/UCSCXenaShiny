### load data
## mut
OP_mut <- UCSCXenaShiny::load_data("OP_mut")
ccle_mut <- OP_mut$ccle
gdsc_mut <- OP_mut$gdsc
gCSI_mut <- OP_mut$gCSI

## cnv
OP_cnv <- UCSCXenaShiny::load_data("OP_cnv")
ccle_cnv <- OP_cnv$ccle
gdsc_cnv <- OP_cnv$gdsc
gCSI_cnv <- OP_cnv$gCSI

## mRNA,exp
OP_exp <- UCSCXenaShiny::load_data("OP_exp")
ccle_exp <- OP_exp$ccle
gdsc_exp <- OP_exp$gdsc

## meth
OP_meth <- UCSCXenaShiny::load_data("OP_meth")
ccle_meth <- OP_meth$ccle

## fusion
OP_fusion <- UCSCXenaShiny::load_data("OP_fusion")
ccle_fusion <- OP_fusion$ccle

## protein
OP_protein <- UCSCXenaShiny::load_data("OP_protein")
ccle_protein <- OP_protein$ccle

## drug,anno
OP_drug <- UCSCXenaShiny::load_data("OP_drug")
ctrp1_drug <- OP_drug$ctrp1
ctrp2_drug <- OP_drug$ctrp2
gCSI_drug <- OP_drug$gCSI
gdsc1_drug <- OP_drug$gdsc1
gdsc2_drug <- OP_drug$gdsc2
prism_drug <- OP_drug$prism

OP_anno <- UCSCXenaShiny::load_data("OP_anno")
cell_anno <- OP_anno$cell
drug_anno <- OP_anno$drug

## plot or preplot
OP_stat_plot <- UCSCXenaShiny::load_data("OP_stat_plot")
p_count_drugandcell <- OP_stat_plot$count_drugandcell
p_count_subtype <- OP_stat_plot$count_subtype
p_overlap_cell <- OP_stat_plot$overlap_cell
p_overlap_drug <- OP_stat_plot$overlap_drug

OP_drug_sens_profile <- UCSCXenaShiny::load_data("OP_drug_sens_profile")
gdsc1_ms <- OP_drug_sens_profile$ms_gdsc1
gdsc2_ms <- OP_drug_sens_profile$ms_gdsc2
prism_ms <- OP_drug_sens_profile$ms_prism
gCSI_ms <- OP_drug_sens_profile$ms_gCSI
ctrp1_ms <- OP_drug_sens_profile$ms_ctrp1
ctrp2_ms <- OP_drug_sens_profile$ms_ctrp2
gdsc1_tsne <- OP_drug_sens_profile$tsne_gdsc1
gdsc2_tsne <- OP_drug_sens_profile$tsne_gdsc2
prism_tsne <- OP_drug_sens_profile$tsne_prism
gCSI_tsne <- OP_drug_sens_profile$tsne_gCSI
ctrp1_tsne <- OP_drug_sens_profile$tsne_ctrp1
ctrp2_tsne <- OP_drug_sens_profile$tsne_ctrp2

## misc and preprocess
# source("inst/shinyapp/modules/PharmacoGenomics/Preprocess.R")
OP_misc <- UCSCXenaShiny::load_data("OP_misc")
omics_search <- OP_misc$omics_search
drugs_search <- OP_misc$drugs_search
drugs_search2 <- OP_misc$drugs_search2
profile_vec_list <- OP_misc$profile_vec_list
profile_vec_list$drug <- profile_vec_list$drug[!profile_vec_list$drug %in% c("OP_sens_plot")]

## clean OP*
rm(list = ls()[grepl("OP", ls())])

## some preprocess for Drugs-omics pairs analysis
tmp <- list()

tmp$omic_sel <- c("exp", "meth", "protein", "cnv")
tmp$tmp1 <- ls()[grepl("_drug$", ls())]
tmp$drug_vec <- gsub("_drug", "", tmp$tmp1[!grepl("^p_", tmp$tmp1)])
omics_search_list1 <- list()

for(i in tmp$omic_sel){
  i2 <- paste0("_", i)
  tmp$omic_vec <- gsub(i2, "", ls()[grepl(i2, ls())])
  tmp_list <- list()
  for(x in tmp$omic_vec){
    # x = tmp$omic_vec[1]
    for(y in tmp$drug_vec){
      # y = tmp$drug_vec[1]
      # select identical cells
      omic <- base::get(paste0(x, i2))
      drug <- base::get(paste0(y, "_drug"))
      intersected_cells <- intersect(colnames(omic), colnames(drug))
      omic <- omic[,match(intersected_cells, colnames(omic))]
      drug <- drug[,match(intersected_cells, colnames(drug))]
      tmp_list[[paste0(x, "_", y)]] <- list(
        "omic" = omic,
        "drug" = drug
      )
    }
  }
  omics_search_list1[[i]] <- tmp_list
}
names(omics_search_list1)[names(omics_search_list1) %in% "exp"] <- "mRNA"

### Discrete
tmp$tmp2 <- ls()[grepl("_mut$", ls())]
gCSI_mut$mutation <- NA
gCSI_mut$genes_muts <- NA
for(i in tmp$tmp2){
  omic <- base::get(i)
  omic_gene <- omic[,c(1,2)] %>% unique()
  omic_site <- omic[,c(4,2)] %>% unique()
  i2 <- paste0(gsub("mut", "", i), "mutation_gene")
  i3 <- paste0(gsub("mut", "", i), "mutation_site")
  assign(i2, omic_gene)
  assign(i3, omic_site)
}
rm(gCSI_mutation_site)

tmp$omic_sel2 <- c("mutation_gene", "mutation_site", "fusion")
tmp$tmp1 <- ls()[grepl("_drug$", ls())]
tmp$drug_vec <- gsub("_drug", "", tmp$tmp1[!grepl("^p_", tmp$tmp1)])
omics_search_list2 <- list()
for(i in tmp$omic_sel2){
  i2 <- paste0("_", i)
  tmp$omic_vec <- gsub(i2, "", ls()[grepl(i2, ls())])
  tmp_list <- list()
  for(x in tmp$omic_vec){
    # x = tmp$omic_vec[1]
    for(y in tmp$drug_vec){
      # y = tmp$drug_vec[1]
      # select identical cells
      omic <- base::get(paste0(x, i2))
      drug <- base::get(paste0(y, "_drug"))
      intersected_cells <- intersect(omic$cells, colnames(drug))
      omic <- omic[omic$cells %in% intersected_cells,]
      drug <- drug[,colnames(drug) %in% intersected_cells]
      tmp_list[[paste0(x, "_", y)]] <- list(
        "omic" = omic,
        "drug" = drug
      )
    }
  }
  omics_search_list2[[i]] <- tmp_list
}

## Function for ProfileDrugSens
plotMADandMedian <- function(ms, dataset){
  ms$Dataset <- dataset
  p <- ggplot(data = ms, 
              aes(text = Name, x = Mad,
                  y = Median, label = Target, label2 = Dataset)) +
    geom_point(alpha=0.4, size=3.5, 
               aes(color=Phase)) + theme_bw() + 
    scale_color_manual(values = paletteer::paletteer_d("ggsci::default_igv")) + 
    labs(x = "MAD", y = "Median") + 
    theme(
      axis.title = element_text(size = 15),
      title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 12), 
      legend.text = element_text(size = 12)
    )
  return(p)
}

plotTSNE <- function(df, dataset){
  df$Dataset <- dataset
  p <- ggplot(data = df, 
              aes(text = Name, x = TSNE1,
                  y = TSNE2, label = Target, label2 = Dataset)) +
    geom_point(alpha=0.4, size=3.5, 
               aes(color=Phase)) + theme_bw() + 
    scale_color_manual(values = paletteer::paletteer_d("ggsci::default_igv")) + 
    labs(x = "TSNE1", y = "TSNE2") + 
    theme(
      axis.title = element_text(size = 15),
      title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 12), 
      legend.text = element_text(size = 12)
    )
  return(p)
}
