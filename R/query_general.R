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
    `IOBR` = tcga_PW %>% dplyr::select('Sample', contains("IOBR")) %>% dplyr::rename_with(~gsub("IOBR_","",.x))
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
    `IOBR` = pcawg_PW %>% dplyr::select('Sample', contains("IOBR")) %>% dplyr::rename_with(~gsub("IOBR_","",.x))
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
    `Tumor Purity` = list(all = sort(colnames(tcga_index_value$`Tumor Purity`)[3:7]), default = "ESTIMATE"),
    `Tumor Stemness` = list(all = sort(colnames(tcga_index_value$`Tumor Stemness`)[2:6]), default = "RNAss"),
    `Tumor Mutation Burden` = list(all = sort(colnames(tcga_index_value$`Tumor Mutation Burden`)[4:5]), default = "Non_silent_per_Mb"),
    `Microsatellite Instability` = list(all = sort(setdiff(colnames(tcga_index_value$`Microsatellite Instability`)[3:21],
                                                      c("MSI_intronic","MSI_intronic_profiled","MSI_noncoding","MSI_noncoding_profiled"))), 
                                        default = "Total_nb_MSI_events"),
    `Genome Instability` = list(all = sort(colnames(tcga_index_value$`Genome Instability`)[2:6]), default = "ploidy")
  )

  ## tumor_immune
  tcga_immune_id = lapply(tcga_immune_value, function(x) {list(all=sort(colnames(x)[-1]))})
  tcga_immune_id$`CIBERSORT`$default = "Monocyte"
  tcga_immune_id$`CIBERSORT-ABS`$default = "Monocyte"
  tcga_immune_id$`EPIC`$default = "Macrophage"
  tcga_immune_id$`MCPCOUNTER`$default = "Monocyte"
  tcga_immune_id$`QUANTISEQ`$default = "Monocyte"
  tcga_immune_id$`TIMER`$default = "Monocyte"
  tcga_immune_id$`XCELL`$default = "Monocyte"

  ## tumor_pathway
  tcga_pathway_id = lapply(tcga_pathway_value, function(x) {list(all=sort(colnames(x)[-1]))})
  tcga_pathway_id$HALLMARK$default = "APOPTOSIS"
  tcga_pathway_id$KEGG$default = "CELL_CYCLE"
  tcga_pathway_id$IOBR$default = "Biotin_Metabolism"

  ## tumor_phenotype
  tcga_phenotype_id = list(
    `Clinical Phenotype` = list(all=sort(colnames(tcga_phenotype_value$`Clinical Phenotype`[3:8])),default="Code"),
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
       `mRNA Expression` = list(all = sort(pcawg_id_referrence$id_gene$Level3), default = "TP53"),
       `Promoter Activity` = list(all = sort(pcawg_id_referrence$id_pro$Level3), default = "prmtr.1"),
       `Gene Fusion` = list(all = sort(pcawg_id_referrence$id_fusion$Level3), default = "SAMD11"),
       `miRNA Expression` = list(all = sort(pcawg_id_referrence$id_mi$Level3), default = "hsa-let-7a-2-3p"),
       `APOBEC Mutagenesis` = list(all = sort(pcawg_id_referrence$id_maf$Level3), default = "A3A_or_A3B")
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
       `mRNA Expression` = list(all = sort(ccle_id_referrence$id_gene$Level3), default = "TP53"),
       `Protein Expression` = list(all = sort(ccle_id_referrence$id_pro$Level3), default = "14-3-3_beta"),
       `Copy Number Variation` = list(all = sort(ccle_id_referrence$id_cnv$Level3), default = "TP53"),
       `Mutation status` = list(all = sort(ccle_id_referrence$id_mut$Level3), default = "TP53")
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









#' Quick molecule analysis and report generation
#'
#' @inheritParams query_pancan_value
#' @param out_dir path to save analysis result and report, default is '.'
#' @param out_report logical value wheather to generate html report
#'
#' @return a list.
#' @export
mol_quick_analysis = function(molecule, data_type, out_dir = ".", out_report = FALSE){
  
  if(dir.exists(out_dir)){
    cat("Warning: The file path already exists. Existing file may be overwritten.\n")
  } else {
    dir.create(out_dir)
  }

  print("##### Step1: Query the moleluce value... #####")
  mol_data = query_pancan_value(molecule, data_type=data_type,database="toil")
  if(is.list(mol_data)) mol_data = mol_data[[1]]


  print(paste0("=== ","Clinical phenotype"))
  mol_data_df = suppressMessages(data.frame(id = molecule,
                           value = mol_data) %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::inner_join(load_data("tcga_gtex"), by=c("Sample"="sample")))

  print(paste0("---> ","Tumor&Normal"))
  comp_TN = suppressMessages(mol_data_df %>%
    dplyr::filter(!.data$tissue %in% c("MESO","UVM")) %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::summarise(wilcox_p = stats::wilcox.test(value  ~ type2)$p.value))
  comp_TN = suppressMessages(mol_data_df %>%
    dplyr::filter(!.data$tissue %in% c("MESO","UVM")) %>%
    dplyr::group_by(.data$tissue, .data$type2) %>%
    dplyr::summarise(value=mean(.data$value)) %>%
    tidyr::pivot_wider(names_from = "type2", values_from = "value") %>%
    dplyr::inner_join(comp_TN))

  print(paste0("---> ","Age"))
  mol_data_Age = suppressMessages(mol_data_df[,1:4] %>%
    dplyr::inner_join(load_data("tcga_clinical_fine")[,c("Sample","Age")]) %>%
    dplyr::filter(!is.na(.data$Age)) %>%
    dplyr::mutate(Age = ifelse(.data$Age>=60,"Old","Young")))
  valid_types = suppressMessages(mol_data_Age %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::count(.data$Age) %>% dplyr::count(.data$tissue) %>%
    dplyr::filter(n==2) %>% dplyr::pull(.data$tissue))
  mol_data_Age = mol_data_Age[mol_data_Age$tissue %in% valid_types,]
  comp_Age = suppressMessages(mol_data_Age %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::summarise(wilcox_p = stats::wilcox.test(value  ~ Age)$p.value))
  comp_Age = suppressMessages(mol_data_Age %>%
    dplyr::group_by(.data$tissue, .data$Age) %>%
    dplyr::summarise(value=mean(.data$value)) %>%
    tidyr::pivot_wider(names_from = "Age", values_from = "value") %>%
    dplyr::inner_join(comp_Age))

  print(paste0("---> ","Gender"))
  mol_data_Gender = suppressMessages(mol_data_df[,1:4] %>%
    dplyr::inner_join(load_data("tcga_clinical_fine")[,c("Sample","Gender")]) %>%
    dplyr::filter(!is.na(.data$Gender)))
  valid_types = suppressMessages(mol_data_Gender %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::count(.data$Gender) %>% dplyr::count(.data$tissue) %>%
    dplyr::filter(n==2) %>% dplyr::pull(.data$tissue))
  mol_data_Gender = mol_data_Gender[mol_data_Gender$tissue %in% valid_types,]
  comp_Gender = suppressMessages(mol_data_Gender %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::summarise(wilcox_p = stats::wilcox.test(value  ~ Gender)$p.value))
  comp_Gender = suppressMessages(mol_data_Gender %>%
    dplyr::group_by(.data$tissue, .data$Gender) %>%
    dplyr::summarise(value=mean(.data$value)) %>%
    tidyr::pivot_wider(names_from = "Gender", values_from = "value") %>%
    dplyr::inner_join(comp_Gender))

  print(paste0("---> ","Stage"))
  mol_data_Stage = suppressMessages(mol_data_df[,1:4] %>%
    dplyr::inner_join(load_data("tcga_clinical_fine")[,c("Sample","Stage_ajcc")]) %>%
    dplyr::filter(!is.na(.data$Stage_ajcc)))
  valid_types = suppressMessages(mol_data_Stage %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::count(.data$Stage_ajcc) %>% dplyr::count(.data$tissue) %>%
    dplyr::filter(n>=2) %>% dplyr::pull(.data$tissue))
  mol_data_Stage = mol_data_Stage[mol_data_Stage$tissue %in% valid_types,]
  comp_Stage = suppressMessages(mol_data_Stage %>%
    dplyr::group_by(.data$tissue) %>%
    dplyr::summarise(aov_p = summary(stats::aov(value  ~ Stage_ajcc))[[1]][1,5]))
  comp_Stage = suppressMessages(mol_data_Stage %>%
    dplyr::group_by(.data$tissue, .data$Stage_ajcc) %>%
    dplyr::summarise(value=mean(.data$value)) %>%
    tidyr::pivot_wider(names_from = "Stage_ajcc", values_from = "value") %>%
    dplyr::inner_join(comp_Stage))

  phe_res = list(comp_TN = comp_TN %>%
                   tidyr::pivot_longer(c('normal','tumor')) %>% dplyr::rename("P.value"="wilcox_p"),
                 comp_Age = comp_Age %>%
                   tidyr::pivot_longer(c('Old','Young')) %>% dplyr::rename("P.value"="wilcox_p"),
                 comp_Gender = comp_Gender %>%
                   tidyr::pivot_longer(c("FEMALE","MALE")) %>% dplyr::rename("P.value"="wilcox_p"),
                 comp_Stage = comp_Stage %>%
                   tidyr::pivot_longer(starts_with("Stage")) %>% dplyr::rename("P.value"="aov_p")) %>%
    do.call(rbind, .) %>% as.data.frame()


  tcga_clinical = load_data("tcga_clinical")
  mol_data_df = data.frame(id = molecule,
                           value = mol_data) %>%
    tibble::rownames_to_column("Sample") %>%
    dplyr::filter(.data$Sample %in% tcga_clinical$sample) %>%
    dplyr::mutate(type = tcga_clinical$type[match(.data$Sample, tcga_clinical$sample)]) %>%
    dplyr::filter(grepl("01$", .data$Sample)) %>% # -01 primary tumor
    dplyr::arrange(.data$type, .data$Sample)

  # 依据每种癌症的中位数进行分组
  if(data_type == "mutation"){
    mol_data_df = mol_data_df %>%
      dplyr::mutate(group = ifelse(.data$value==1,"Mutate","Wild")) %>%
      as.data.frame()
  } else {
    mol_data_df = mol_data_df %>%
      dplyr::group_by(.data$type) %>%
      dplyr::mutate(group = ifelse(.data$value>median(.data$value),"Higher","Lower")) %>%
      as.data.frame()
  }

  general_value_id = query_general_id()
  tcga_value_option = general_value_id[["value"]][[1]]
  tcga_id_option = general_value_id[["id"]][[1]]

  ## 相关性分析结果
  print("##### Step2: Execute correlation analysis...  #####")
  cor_res = lapply(c("Tumor index","Immune Infiltration","Pathway activity"),function(L1_type){
    # L1_type = "Pathway activity"
    print(paste0("=== ",L1_type))
    tcga_id = tcga_id_option[[L1_type]]
    tcga_value = tcga_value_option[[L1_type]]
    L2_types = names(tcga_id)

    cor_L1 = lapply(L2_types, function(L2_type){
      # L2_type = L2_types[3]
      print(paste0("---> ",L2_type))
      L3_ids = tcga_id[[L2_type]]$all
      value_df = tcga_value[[L2_type]] %>% as.data.frame()
      L3_ids = L3_ids[sapply(L3_ids, function(id){class(value_df[,id])})!="character"]

      value_df_long = value_df[,c("Sample",L3_ids)] %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "L3", values_to = "target")

      data_merge = suppressMessages(dplyr::inner_join(mol_data_df, value_df_long) %>%
                                      dplyr::filter(!is.na(.data$target)))

      valid_L3 = data_merge %>%
        dplyr::distinct(.data$group,.data$L3) %>%
        dplyr::count(.data$L3) %>%
        dplyr::filter(n==2) %>%
        dplyr::pull(.data$L3)

      data_merge = data_merge %>%
        dplyr::filter(.data$L3 %in% valid_L3)

      cor_L2 = suppressMessages(data_merge %>%
                                  dplyr::group_by(.data$L3, .data$type) %>%
                                  dplyr::summarise(
                                    spearman_r = stats::cor.test(.data$value, .data$target, method = "spearman")$estimate,
                                    spearman_p = stats::cor.test(.data$value, .data$target, method = "spearman")$p.value
                                  ) %>%
                                  dplyr::mutate(L1 = L1_type, .before = 1) %>%
                                  dplyr::mutate(L2 = L2_type, .before = 2))
      return(cor_L2)
    }) %>% do.call(rbind, .)
    cor_L1
  }) %>% do.call(rbind, .)


  ## 生存分析结果
  print("##### Step4: Execute survival analysis...  #####")
  sur_res = lapply(c("OS","DSS","DFI","PFI"),function(event){
    # event = "OS"
    # print(event)
    print(paste0("=== ",event))
    tcga_surv_sub = load_data("tcga_surv") %>%
      dplyr::select(sample, contains(event)) %>%
      na.omit()
    colnames(tcga_surv_sub) = c("Sample","status","time")

    data_merge = suppressMessages(dplyr::inner_join(mol_data_df, tcga_surv_sub))


    sur_merge = lapply(unique(data_merge$type), function(type){
      # type = "BRCA"
      # print(type)
      sur_dat = data_merge[data_merge$type==type,]
      if(length(unique(sur_dat$group))==1) return(NA)
      surv_diff <- survival::survdiff(survival::Surv(time, status) ~ group, data = sur_dat)
      p.val = 1 - stats::pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
      hr = exp(survival::coxph(survival::Surv(time, status) ~ group, data = sur_dat)$coefficients)
      c(p.val, hr)
    }) %>% do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(type = unique(data_merge$type), .before = 1) %>%
      # tibble::rownames_to_column("type") %>%
      dplyr::rename("logrank_p"="V1") %>%
      dplyr::mutate(P.direction = ifelse(.data$groupLower<1,"Higher Risk", "Lower Risk")) %>%
      dplyr::mutate("event" = event, .before = 1) %>%
      dplyr::select(!.data$groupLower)
    sur_merge
  }) %>% do.call(rbind, .)


  utils::write.csv(phe_res, row.names = FALSE,
            file=file.path(out_dir, "molecule_clinical_result.csv"))
  utils::write.csv(cor_res, row.names = FALSE,
            file=file.path(out_dir, "molecule_correlation_result.csv"))
  utils::write.csv(sur_res, row.names = FALSE,
            file=file.path(out_dir, "molecule_survival_result.csv"))

  # phe_res = read.csv(file.path(tempdir(),"molecule_clinical_result.csv"))
  # sur_res = read.csv(file.path(tempdir(),"molecule_survival_result.csv"))
  # cor_res = read.csv(file.path(tempdir(),"molecule_correlation_result.csv"))


  
  
  if(out_report){
    params = list("sur_res" = sur_res,
                  "cor_res" = cor_res,
                  "phe_res" = phe_res,
                  "id_name" = molecule,
                  "id_type" = data_type)
    print("##### Step5: Render html report...  #####")
    file.copy(from = system.file("rmd","report_template.Rmd", package = "UCSCXenaShiny"),
              to = file.path(out_dir, "report_template.Rmd"), overwrite = TRUE)
    rmarkdown::render(file.path(out_dir, "report_template.Rmd"), "html_document",
                      file.path(out_dir, "report_result.html"),
                      params = params,
                      envir = new.env(parent = globalenv()))
    file.remove(file.path(out_dir, "report_template.Rmd"))
  }
  print("All analysis is done!")
  
  return(list(sur_res=sur_res,cor_res=cor_res,phe_res=phe_res))
}



