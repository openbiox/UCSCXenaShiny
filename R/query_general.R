query_general_id = function(){
  
  tcga_clinical_fine = load_data("tcga_clinical_fine")
  pcawg_info_fine = load_data("pcawg_info_fine")
  ccle_info_fine = load_data("ccle_info_fine")
  #### TCGA general value: index/immune/pathway/phenotype 
  ## tumor_index
  tcga_index_value = list(
    `Tumor Purity` = load_data("tcga_purity") %>% dplyr::rename("Sample"="sample"),
    `Tumor Stemness` = load_data("tcga_stemness") %>% dplyr::rename("Sample"="sample"),
    `Tumor Mutation Burden` = load_data("tcga_tmb") %>% dplyr::rename("Sample"="Tumor_Sample_ID"),
    `Microsatellite Instability` = load_data("tcga_MSI"),
    `Genome Instability` = load_data("tcga_genome_instability") %>% dplyr::rename("Sample"="sample")
  )
  # colnames(tcga_index_value$`Tumor Mutation Burden`)[3] = "sample"
  tcga_index_value$`Microsatellite Instability` = load_data("tcga_gtex") %>%
    dplyr::rename("Sample"="sample") %>%
    dplyr::mutate(Barcode = stringr::str_sub(.data$Sample, 1, 12)) %>%
    dplyr::select('Barcode', 'Sample') %>%
    dplyr::inner_join(tcga_index_value$`Microsatellite Instability`, by = "Barcode")

  ## tumor_immune
  tcga_TIL = load_data("tcga_TIL")
  colnames(tcga_TIL)[1] = "Sample"
  dim(tcga_TIL)
  # [1] 11070   120
  tcga_immune_value = list(
    `CIBERSORT` = tcga_TIL %>% dplyr::select('Sample', ends_with("CIBERSORT")) %>% dplyr::rename_with(~gsub("_CIBERSORT","",.x)),
    `CIBERSORT-ABS` = tcga_TIL %>% dplyr::select('Sample', ends_with("CIBERSORT-ABS")) %>% dplyr::rename_with(~gsub("_CIBERSORT-ABS","",.x)),
    `EPIC` = tcga_TIL %>% dplyr::select('Sample', ends_with("EPIC")) %>% dplyr::rename_with(~gsub("_EPIC","",.x)),
    `MCPCOUNTER` = tcga_TIL %>% dplyr::select('Sample', ends_with("MCPCOUNTER")) %>% dplyr::rename_with(~gsub("_MCPCOUNTER","",.x)),
    `QUANTISEQ` = tcga_TIL %>% dplyr::select('Sample', ends_with("QUANTISEQ")) %>% dplyr::rename_with(~gsub("_QUANTISEQ","",.x)),
    `TIMER` = tcga_TIL %>% dplyr::select('Sample', ends_with("TIMER")) %>% dplyr::rename_with(~gsub("_TIMER","",.x)),
    `XCELL` = tcga_TIL %>% dplyr::select('Sample', ends_with("XCELL")) %>% dplyr::rename_with(~gsub("_XCELL","",.x))
  )

  ## tumor_pathway
  tcga_PW = load_data("tcga_PW") %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Sample")
  dim(tcga_PW)
  # [1] 10534   501
  tcga_pathway_value = list(
    `HALLMARK` = tcga_PW %>% dplyr::select('Sample', contains("HALLMARK")) %>% dplyr::rename_with(~gsub("HALLMARK_","",.x)),
    `KEGG` = tcga_PW %>% dplyr::select('Sample', contains("KEGG")) %>% dplyr::rename_with(~gsub("KEGG_","",.x)),
    `IOBR` = tcga_PW %>% dplyr::select('Sample', contains("HALLMARK")) %>% dplyr::rename_with(~gsub("HALLMARK_","",.x))
  )

  ## tumor_phenotype (user upload custom metadata)
  tcga_phenotype_value = list(
    `Clinical Phenotype` = tcga_clinical_fine,
    `Custom metadata` = NULL
  )


  #### PCAWG general value: index/immune/pathway/phenotype 
  ## tumor_index
  pcawg_index_value = list(
    `Tumor Purity` = load_data("pcawg_purity") %>%
      dplyr::filter(.data$icgc_specimen_id %in% pcawg_info_fine$Sample) %>%
      dplyr::rename("Sample"="icgc_specimen_id")
  )

  ## tumor_immune
  pcawg_TIL = load_data("pcawg_TIL")
  colnames(pcawg_TIL)[1] = "Sample"
  dim(pcawg_TIL)
  # [1] 1466   120
  pcawg_immune_value = list(
    `CIBERSORT` = pcawg_TIL %>% dplyr::select('Sample', ends_with("CIBERSORT")) %>% dplyr::rename_with(~gsub("_CIBERSORT","",.x)),
    `CIBERSORT-ABS` = pcawg_TIL %>% dplyr::select('Sample', ends_with("CIBERSORT-ABS")) %>% dplyr::rename_with(~gsub("_CIBERSORT-ABS","",.x)),
    `EPIC` = pcawg_TIL %>% dplyr::select('Sample', ends_with("EPIC")) %>% dplyr::rename_with(~gsub("_EPIC","",.x)),
    `MCPCOUNTER` = pcawg_TIL %>% dplyr::select('Sample', ends_with("MCPCOUNTER")) %>% dplyr::rename_with(~gsub("_MCPCOUNTER","",.x)),
    `QUANTISEQ` = pcawg_TIL %>% dplyr::select('Sample', ends_with("QUANTISEQ")) %>% dplyr::rename_with(~gsub("_QUANTISEQ","",.x)),
    `TIMER` = pcawg_TIL %>% dplyr::select('Sample', ends_with("TIMER")) %>% dplyr::rename_with(~gsub("_TIMER","",.x)),
    `XCELL` = pcawg_TIL %>% dplyr::select('Sample', ends_with("XCELL")) %>% dplyr::rename_with(~gsub("_XCELL","",.x))
  )

  ## tumor_pathway
  pcawg_PW = load_data("pcawg_PW") %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Sample")
  dim(pcawg_PW)
  # [1] 1466   501
  pcawg_pathway_value = list(
    `HALLMARK` = pcawg_PW %>% dplyr::select('Sample', contains("HALLMARK")) %>% dplyr::rename_with(~gsub("HALLMARK_","",.x)),
    `KEGG` = pcawg_PW %>% dplyr::select('Sample', contains("KEGG")) %>% dplyr::rename_with(~gsub("KEGG_","",.x)),
    `IOBR` = pcawg_PW %>% dplyr::select('Sample', contains("HALLMARK")) %>% dplyr::rename_with(~gsub("HALLMARK_","",.x))
  )

  pcawg_phenotype_value = list(
    `Clinical Phenotype` = pcawg_info_fine,
    `Custom metadata` = NULL
  )

  #### CCLE general value: index/immune/pathway/phenotype 
  ## tumor_index
  ccle_index_value = list(
    `Tumor Purity` = load_data("ccle_absolute") %>%
      dplyr::rename("Sample"="Cell Line") %>%
      dplyr::filter(.data$Sample %in% ccle_info_fine$Sample)
  )
  ccle_phenotype_value = list(
    `Clinical Phenotype` = ccle_info_fine,
    `Custom metadata` = NULL
  )

  tcga_value_option = list("Tumor index"=tcga_index_value, "Immune Infiltration"=tcga_immune_value, 
    "Pathway activity"=tcga_pathway_value, "Phenotype data"=tcga_phenotype_value)
  pcawg_value_option = list("Tumor index"=pcawg_index_value, "Immune Infiltration"=pcawg_immune_value, 
    "Pathway activity"=pcawg_pathway_value, "Phenotype data"=pcawg_phenotype_value)
  ccle_value_option = list("Tumor index"=ccle_index_value, "Phenotype data"=ccle_phenotype_value)

  #### TCGA general id: molecule/index/immune/pathway/phenotype 
  ## molecule_profile
  pancan_identifiers <- readRDS(
    system.file(
      "extdata", "pancan_identifier_list.rds",
      package = "UCSCXenaShiny"
    )
  )
  tcga_molecule_id = list(
    `mRNA Expression` = list(all = pancan_identifiers$gene, default = "TP53"),
    `Transcript Expression` = list(all = load_data("transcript_identifier"), default = "ENST00000000233"),
    `DNA Methylation` = list(all = pancan_identifiers$gene, default = "TP53"),
    `Protein Expression` = list(all = pancan_identifiers$protein, default = "P53"),
    `miRNA Expression` = list(all = pancan_identifiers$miRNA, default = "hsa-miR-769-3p"),
    `Mutation status` = list(all = pancan_identifiers$gene, default = "TP53"),
    `Copy Number Variation` = list(all = pancan_identifiers$gene, default = "TP53")
  )

  ## tumor_index
  tcga_index_id = list(
    `Tumor Purity` = list(all = colnames(tcga_index_value$`Tumor Purity`)[3:7], default = "ESTIMATE"),
    `Tumor Stemness` = list(all = colnames(tcga_index_value$`Tumor Stemness`)[2:6], default = "RNAss"),
    `Tumor Mutation Burden` = list(all = colnames(tcga_index_value$`Tumor Mutation Burden`)[4:5], default = "Non_silent_per_Mb"),
    `Microsatellite Instability` = list(all = colnames(tcga_index_value$`Microsatellite Instability`)[3:21], default = "Total_nb_MSI_events"),
    `Genome Instability` = list(all = colnames(tcga_index_value$`Genome Instability`)[2:6], default = "ploidy")
  )

  ## tumor_immune
  tcga_immune_id = lapply(tcga_immune_value, function(x) {list(all=colnames(x)[-1])})
  tcga_immune_id$`CIBERSORT`$default = "Monocyte"
  tcga_immune_id$`CIBERSORT-ABS`$default = "Monocyte"
  tcga_immune_id$`EPIC`$default = "Macrophage"
  tcga_immune_id$`MCPCOUNTER`$default = "Monocyte"
  tcga_immune_id$`QUANTISEQ`$default = "Monocyte"
  tcga_immune_id$`TIMER`$default = "Monocyte"
  tcga_immune_id$`XCELL`$default = "Monocyte"

  ## tumor_pathway
  tcga_pathway_id = lapply(tcga_pathway_value, function(x) {list(all=colnames(x)[-1])})
  tcga_pathway_id$HALLMARK$default = "APOPTOSIS"
  tcga_pathway_id$KEGG$default = "CELL_CYCLE"
  tcga_pathway_id$IOBR$default = "Biotin_Metabolism"

  ## tumor_phenotype
  tcga_phenotype_id = list(
    `Clinical Phenotype` = list(all=colnames(tcga_phenotype_value$`Clinical Phenotype`[3:8]),default="Code"),
    `Custom metadata` = list(all=NULL, default=NULL)
  )


  ## merge
  tcga_id_option = list(
    "Molecular profile" = tcga_molecule_id,
    "Tumor index" = tcga_index_id,
    "Immune Infiltration" = tcga_immune_id,
    "Pathway activity" = tcga_pathway_id,
    "Phenotype data" = tcga_phenotype_id
  )

  #### PCAWG ID
  pcawg_id_option = tcga_id_option
  pcawg_id_referrence = load_data("pcawg_identifier")
  pcawg_id_option$`Molecular profile` = list(
       `mRNA Expression` = list(all = pcawg_id_referrence$id_gene$Level3, default = "TP53"),
       `Promoter Activity` = list(all = pcawg_id_referrence$id_pro$Level3, default = "prmtr.1"),
       `Gene Fusion` = list(all = pcawg_id_referrence$id_fusion$Level3, default = "SAMD11"),
       `miRNA Expression` = list(all = pcawg_id_referrence$id_mi$Level3, default = "hsa-let-7a-2-3p"),
       `APOBEC Mutagenesis` = list(all = pcawg_id_referrence$id_maf$Level3, default = "A3A_or_A3B")
    )
  pcawg_id_option$`Tumor index` = list(
       `Tumor Purity` = list(all = c("purity", "ploidy", "purity_conf_mad", "wgd_status", "wgd_uncertain"), 
                            default = "purity")
    )
  pcawg_id_option$`Phenotype data` = list(
       `Clinical Phenotype` = list(all = c("Age", "Gender","Type"), 
                            default = "Age"),
       `Custom metadata` = list(all=NULL, default=NULL)
  )

  #### CCLE ID
  ccle_id_option = list()
  ccle_id_referrence = load_data("ccle_identifier")
  ccle_id_option$`Molecular profile` = list(
       `mRNA Expression` = list(all = ccle_id_referrence$id_gene$Level3, default = "TP53"),
       `Protein Expression` = list(all = ccle_id_referrence$id_pro$Level3, default = "14-3-3_beta"),
       `Copy Number Variation` = list(all = ccle_id_referrence$id_cnv$Level3, default = "TP53"),
       `Mutation status` = list(all = ccle_id_referrence$id_mut$Level3, default = "TP53")
    )
  ccle_id_option$`Tumor index` = list(
       `Tumor Purity` = list(all = c("Purity", "Ploidy", "Genome Doublings", "Lineage"), 
                            default = "Purity")
    )
  ccle_id_option$`Immune Infiltration` = list(NULL)
  ccle_id_option$`Pathway activity` = list(NULL)
  ccle_id_option$`Phenotype data` = list(
       `Clinical Phenotype` = list(all = c("Gender","Histology","Type"), 
                            default = "Gender"),
       `Custom metadata` = list(all=NULL, default=NULL)
  )

  res_merge = list(
    value = list(tcga_value_option, pcawg_value_option, ccle_value_option),
    id = list(tcga_id_option, pcawg_id_option, ccle_id_option)
  )

  return(res_merge)
}







#' download data for shiny general analysis 
#'
#' @param L1 level 1  main datatype
#' @param L2 level 2  sub datatype
#' @param L3 level 3  identifier
#' @param opt_pancan      molecular datasets parameters
#' @param custom_metadata user customized metadata
#' @param database          one of c("toil","pcawg","ccle")
#' @param index_value     tumor index list
#' @param immune_value    tumor immune infiltration list
#' @param pathway_value   pathway activity list
#' @param clinical_value  clinical data.frame
#'
#' @examples
#' \dontrun{
#' general_value_id = UCSCXenaShiny:::query_general_id()
#' tcga_value_option = general_value_id[["value"]][[1]]
#' tcga_index_value = tcga_value_option[["Tumor index"]]
#' tcga_immune_value = tcga_value_option[["Immune Infiltration"]]
#' tcga_pathway_value = tcga_value_option[["Pathway activity"]]
#' tcga_phenotype_value = tcga_value_option[["Phenotype data"]]

#' clinical_phe = tcga_phenotype_value[["Clinical Phenotype"]]
#' x_data = UCSCXenaShiny:::query_general_value(
#'            "Molecular profile", "mRNA Expression", "TP53", "toil",
#'            tcga_index_value, tcga_immune_value, tcga_pathway_value, 
#'            clinical_phe)
#'                                              
#' y_data = UCSCXenaShiny:::query_general_value(
#'            "Immune Infiltration", "CIBERSORT", "Monocyte", "toil",
#'            tcga_index_value, tcga_immune_value, tcga_pathway_value, 
#'            clinical_phe)
#' }


query_general_value = function(L1, L2, L3, database = c("toil","pcawg","ccle"),
                          index_value, immune_value, pathway_value, clinical_value,
                          opt_pancan=NULL, custom_metadata=NULL){
  database = match.arg(database)

  
  if(L1 == "Molecular profile"){
    # L2 = "mRNA Expression"
    # L3 = "TP53"
    x_genomic_profile = switch(L2,
                               `mRNA Expression` = "mRNA",
                               `Transcript Expression` = "transcript",
                               `DNA Methylation` = "methylation",
                               `Protein Expression` = "protein",
                               `miRNA Expression` = "miRNA",
                               `Mutation status` = "mutation",
                               `Copy Number Variation` = "cnv",
                               # `mRNA Expression` = "mRNA",
                               `Promoter Activity` = "promoter",
                               `Gene Fusion` = "fusion",
                               # `miRNA Expression` = 'miRNA',
                               `APOBEC Mutagenesis` = "APOBEC"
    )
    if(is.null(opt_pancan)) {opt_pancan = .opt_pancan}
    x_data <- query_pancan_value(L3, 
                                 data_type = x_genomic_profile,
                                 database = database,
                                 opt_pancan = opt_pancan
    )
    if(database == "ccle" & L2 == "Mutation status"){
      x_data = x_data[,c("sampleID","genes")]
      x_data = x_data %>% 
        dplyr::select("sampleID", "genes") %>% 
        dplyr::mutate(genes = ifelse(is.na(.data$genes),0,1)) %>% 
        dplyr::arrange(.data$sampleID ,desc(.data$genes)) %>% 
        dplyr::distinct(.data$sampleID, .keep_all = T) %>% 
        dplyr::mutate(id = L3,.before = 1) %>%
        dplyr::rename("Sample"="sampleID", "value"="genes") %>% 
        dplyr::mutate(level2="Mutation status")
    } else {
      if (is.list(x_data)) x_data <- x_data[[1]]
      x_data <- data.frame('id' = L3,
                           'Sample' = names(x_data), 'value' = as.numeric(x_data),
                           'level2' = L2) 
    }

  } else {
    if(L1 == "Tumor index"){
      x_data = index_value[[L2]][,c("Sample", L3)]
    } else if (L1 == "Immune Infiltration") {
      x_data = immune_value[[L2]][,c("Sample", L3)]
    } else if (L1 == "Pathway activity") {
      x_data = pathway_value[[L2]][,c("Sample", L3)]
    } else if (L1 == "Phenotype data") {
      if(L2 =="Clinical Phenotype"){
          x_data = clinical_value[,c("Sample", L3)]
      } else {
          x_data = custom_metadata[,c("Sample", L3)]
      }
    }
    colnames(x_data) = c("Sample","value")
    x_data = x_data %>% 
      dplyr::mutate(id = L3, .before = 1) %>%
      dplyr::mutate(level2 = L2) %>%
      dplyr::filter(!is.na(.data$value))

  } 
  x_data = as.data.frame(x_data)
  # 检查重复样本
  if(is.numeric(x_data$value)){
    x_data = x_data %>%
      dplyr::group_by(.data$Sample, .data$id, .data$level2) %>%
      dplyr::summarise(value = mean(.data$value)) %>%
      as.data.frame()
  }
  x_data
  # id                  Sample value          level2
  # 1 TP53 GTEX-S4Q7-0003-SM-3NM8M 4.785 mRNA Expression
  # 2 TP53         TCGA-19-1787-01 5.887 mRNA Expression
  # 3 TP53         TCGA-S9-A7J2-01 5.517 mRNA Expression
  # 4 TP53 GTEX-QV31-1626-SM-2S1QC 4.431 mRNA Expression
  # 5 TP53         TCGA-G3-A3CH-11 2.382 mRNA Expression
  # 6 TP53         TCGA-B5-A5OE-01 5.765 mRNA Expression
}
