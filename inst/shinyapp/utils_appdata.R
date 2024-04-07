tcga_id = load_data("v2_tcga_id")
pcawg_id = load_data("v2_pcawg_id")
ccle_id = load_data("v2_ccle_id")

tcga_id.list = split(tcga_id$L3, tcga_id$L2)
pcawg_id.list = split(pcawg_id$L3, pcawg_id$L2)
ccle_id.list = split(ccle_id$L3, ccle_id$L2)

names(tcga_id.list)




## General Analysis
all_preload_identifiers <- c("NONE", unlist(tcga_id.list[c("Gene","Protein","miRNA")]))

tcga_id_option = list(
  "Molecular profile" = list(
    "mRNA Expression" = list(
      "all" = tcga_id.list[["Gene"]],
      "default" = "TP53"
    ),
    "Transcript Expression" = list(
      "all" = tcga_id.list[["Transcript"]],
      "default" = "ENST00000000233"
    ),
    "DNA Methylation" = list(
      "all" = tcga_id.list[["Gene"]],
      "default" = "TP53"
    ),
    "Protein Expression" = list(
      "all" = tcga_id.list[["Protein"]],
      "default" = "P53"
    ),
    "miRNA Expression" = list(
      "all" = tcga_id.list[["miRNA"]],
      "default" = "hsa-miR-769-3p"
    ),
    "Copy Number Variation" = list(
      "all" = tcga_id.list[["Gene"]],
      "default" = "TP53"
    )
  ),
  "Tumor index" = list(
    "Tumor Purity" = list(
      "all" = tcga_id.list[["Purity"]],
      "default" = "ESTIMATE"
    ),
    "Tumor Stemness" = list(
      "all" = tcga_id.list[["Stem"]],
      "default" = "RNAss"
    ),
    "Tumor Mutation Burden" = list(
      "all" = tcga_id.list[["TMB"]],
      "default" = "Non_silent_per_Mb"
    ),
    "Microsatellite Instability" = list(
      "all" = tcga_id.list[["MSI"]],
      "default" = "Total_nb_MSI_events"
    ),
    "Genome Instability" = list(
      "all" = tcga_id.list[["GI"]],
      "default" = "ploidy"
    )
  ),
  "Immune Infiltration" = list(
    "CIBERSORT" = list(
      "all" = tcga_id.list[["CIB"]],
      "default" = "Monocyte"
    ),
    "CIBERSORT-ABS" = list(
      "all" = tcga_id.list[["CIB.ABS"]],
      "default" = "Monocyte"
    ),
    "EPIC" = list(
      "all" = tcga_id.list[["EPIC"]],
      "default" = "Macrophage"
    ),
    "MCPCOUNTER" = list(
      "all" = tcga_id.list[["MCP"]],
      "default" = "Macrophage"
    ),
    "QUANTISEQ" = list(
      "all" = tcga_id.list[["Quant"]],
      "default" = "Monocyte"
    ),
    "TIMER" = list(
      "all" = tcga_id.list[["XCELL"]],
      "default" = "Monocyte"
    ),
    "XCELL" = list(
      "all" = tcga_id.list[["TIMER"]],
      "default" = "Monocyte"
    )
  ),
  "Pathway activity" = list(
    "HALLMARK" = list(
      "all" = tcga_id.list[["HM"]],
      "default" = "APOPTOSIS"
    ),
    "KEGG" = list(
      "all" = tcga_id.list[["KEGG"]],
      "default" = "CELL_CYCLE"
    ),
    "IOBR" = list(
      "all" = tcga_id.list[["IOBR"]],
      "default" = "Biotin_Metabolism"
    )
  ),
  "Phenotype data" = list(
    "Clinical Phenotype" = list(
      "all" = tcga_id.list[["Clinical"]],
      "default" = "Code"
    ),
    "Custom metadata" = list(
      "all" = NULL,
      "default" = NULL
    )
  )
)

pcawg_id_option = list(
  "Molecular profile" = list(
    "mRNA Expression" = list(
      "all" = pcawg_id.list[["Gene"]],
      "default" = "TP53"
    ),
    "Promoter Activity" = list(
      "all" = pcawg_id.list[["Promoter"]],
      "default" = "prmtr.1"
    ),
    "Gene Fusion" = list(
      "all" = pcawg_id.list[["Fusion"]],
      "default" = "SAMD11"
    ),
    "miRNA Expression" = list(
      "all" = pcawg_id.list[["miRNA"]],
      "default" = "hsa-let-7a-2-3p"
    ),
    "APOBEC Mutagenesis" = list(
      "all" = pcawg_id.list[["Muta"]],
      "default" = "A3A_or_A3B"
    )
  ),
  "Tumor index" = list(
    "Tumor Purity" = list(
      "all" = pcawg_id.list[["Purity"]],
      "default" = "purity"
    )
  ),
  "Immune Infiltration" = list(
    "CIBERSORT" = list(
      "all" = pcawg_id.list[["CIB"]],
      "default" = "Monocyte"
    ),
    "CIBERSORT-ABS" = list(
      "all" = pcawg_id.list[["CIB.ABS"]],
      "default" = "Monocyte"
    ),
    "EPIC" = list(
      "all" = pcawg_id.list[["EPIC"]],
      "default" = "Macrophage"
    ),
    "MCPCOUNTER" = list(
      "all" = pcawg_id.list[["MCP"]],
      "default" = "Macrophage"
    ),
    "QUANTISEQ" = list(
      "all" = pcawg_id.list[["Quant"]],
      "default" = "Monocyte"
    ),
    "TIMER" = list(
      "all" = pcawg_id.list[["XCELL"]],
      "default" = "Monocyte"
    ),
    "XCELL" = list(
      "all" = pcawg_id.list[["TIMER"]],
      "default" = "Monocyte"
    )
  ),
  "Pathway activity" = list(
    "HALLMARK" = list(
      "all" = pcawg_id.list[["HM"]],
      "default" = "APOPTOSIS"
    ),
    "KEGG" = list(
      "all" = pcawg_id.list[["KEGG"]],
      "default" = "CELL_CYCLE"
    ),
    "IOBR" = list(
      "all" = pcawg_id.list[["IOBR"]],
      "default" = "Biotin_Metabolism"
    )
  ),
  "Phenotype data" = list(
    "Clinical Phenotype" = list(
      "all" = pcawg_id.list[["Clinical"]],
      "default" = "Age"
    ),
    "Custom metadata" = list(
      "all" = NULL,
      "default" = NULL
    )
  )
)

ccle_id_option = list(
  "Molecular profile" = list(
    "mRNA Expression" = list(
      "all" = ccle_id.list[["Gene"]],
      "default" = "TP53"
    ),
    "Protein Expression" = list(
      "all" = ccle_id.list[["Protein"]],
      "default" = "14-3-3_beta"
    ),
    "Copy Number Variation" = list(
      "all" = ccle_id.list[["Gene"]],
      "default" = "TP53"
    ),
    "Mutation status" = list(
      "all" = ccle_id.list[["Gene"]],
      "default" = "TP53"
    )
  ),
  "Tumor index" = list(
    "Tumor Purity" = list(
      "all" = ccle_id.list[["Purity"]],
      "default" = "Purity"
    )
  ),
  "Immune Infiltration" = NULL,
  "Pathway activity" = NULL,
  "Phenotype data" = list(
    "Clinical Phenotype" = list(
      "all" = ccle_id.list[["Clinical"]],
      "default" = "Gender"
    ),
    "Custom metadata" = list(
      "all" = NULL,
      "default" = NULL
    )
  )
)


## 33 TCGAs
tcga_names = sort(unique(tcga_clinical_fine$Cancer))
## 30 PCAWGs
pcawg_names = sort(unique(pcawg_info_fine$Project))

code_types = list("NT"= "NT (normal tissue)",
                  "TP"= "TP (primary tumor)",
                  "TR"= "TR (recurrent tumor)",
                  "TB"= "TB (blood derived tumor)",
                  "TAP"="TAP (additional primary)",
                  "TM"= "TM (metastatic tumor)",
                  "TAM"="TAM (additional metastatic)")


# Global theme
themes_list <- list(
  "cowplot" = cowplot::theme_cowplot(),
  "Light" = ggplot2::theme_light(),
  "Minimal" = ggplot2::theme_minimal(),
  "Classic" = ggplot2::theme_classic(),
  "Gray" = ggplot2::theme_gray(),
  "half_open" = cowplot::theme_half_open(),
  "minimal_grid" = cowplot::theme_minimal_grid()
)
# Global color
mycolor <- c(RColorBrewer::brewer.pal(12, "Paired"))


msigdbr_types <- data.frame(
  gs_cat = c("H","C1", "C2", "C2", "C2", "C2", "C2", "C2", "C2", 
             "C3", "C3", "C3", "C3", "C4", "C4", "C5", "C5", 
             "C5", "C5", "C6", "C7", "C7", "C8"),
  gs_subcat = c("","", "CGP", "CP", "CP:BIOCARTA", "CP:KEGG", 
                "CP:PID", "CP:REACTOME", "CP:WIKIPATHWAYS", 
                "MIR:MIRDB", "MIR:MIR_Legacy", "TFT:GTRD", 
                "TFT:TFT_Legacy", "CGN", "CM", "GO:BP", 
                "GO:CC", "GO:MF", "HPO", "", "IMMUNESIGDB", 
                "VAX", "")
)
msigdbr_types = msigdbr_types %>% 
  dplyr::mutate(gs_subcat2 = ifelse(gs_subcat=="",gs_cat,gs_subcat)) %>% 
  dplyr::mutate(gs_subcat_label = paste0(gs_cat, "--",gs_subcat2))

#   gs_cat   gs_subcat  gs_subcat2 gs_subcat_label
# 1      H                       H            H--H
# 2     C1                      C1          C1--C1
# 3     C2         CGP         CGP         C2--CGP
# 4     C2          CP          CP          C2--CP



## PharmacoGenomics ----
#source(system.file("shinyapp/PGdata.R", package = "UCSCXenaShiny"), local = PGdata <- new.env(), echo = FALSE)

# appdata_path = path.expand(file.path(getOption("xena.cacheDir"), "appdata.RData"))
# message("Saving data to ", appdata_path)
# save.image(file = appdata_path)

