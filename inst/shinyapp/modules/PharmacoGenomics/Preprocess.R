tmp <- list()

# Drugs-omics pairs analysis----
### Continuous ----
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

### Discrete ----
# Remake mutation_gene and mutation_site
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

# Make
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


