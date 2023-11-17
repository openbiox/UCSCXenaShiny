#' download data for shiny batch analysis 
#'
#' @param L1 level 1  main datatype
#' @param L2 level 2  sub datatype
#' @param L3 level 3  identifier
#' @param tumor_index_list Tumor index data. See shiny App.R file
#' @param tcga_TIL        Tumor immune infiltration  data. See shiny App.R file
#' @param tcga_PW         Pathway activity data. See shiny App.R file
#' @param opt_pancan      molecular datasets parameters
#' @param custom_metadata user customized metadata
#'
batch_download = function(L1, L2, L3,
                          tumor_index_list, tcga_TIL, tcga_PW, 
                          opt_pancan, custom_metadata=NULL){
  if(L1 == "Molecular_profile"){
    # L2 = "mRNA Expression"
    # L3 = "TP53"
    x_genomic_profile = switch(L2,
                               `mRNA Expression` = "mRNA",
                               `Transcript Expression` = "transcript",
                               `DNA Methylation` = "methylation",
                               `Protein Expression` = "protein",
                               `miRNA Expression` = "miRNA",
                               `Mutation status` = "mutation",
                               `Copy Number Variation` = "cnv"
    )
    x_data <- query_pancan_value(L3, 
                                 data_type = x_genomic_profile,
                                 opt_pancan = opt_pancan
    )
    if (is.list(x_data)) x_data <- x_data[[1]]
    x_data <- data.frame(id = L3,
                         sample = names(x_data), value = as.numeric(x_data),
                         level2 = L2)
  } else if (L1 == "Tumor_index"){
    # L2 = "Tumor Purity"
    # L3 = L3_candi$id_tumor_index$tcga_purity$Level3[1]
    x_tumor_index = switch(L2,
                           `Tumor Purity` = "tcga_purity",
                           `Tumor Stemness` = "tcga_stemness",
                           `Tumor Mutation Burden` = "tcga_tmb",
                           `Microsatellite Instability` = "tcga_msi",
                           `Genome Instability` = "tcga_genome_instability"
    )
    x_data = tumor_index_list[[x_tumor_index]][,c("sample", L3)]
    colnames(x_data)[2] = "value"
    x_data = x_data %>% 
      dplyr::mutate(id = L3, .before = 1) %>%
      dplyr::mutate(level2 = L2) %>%
      dplyr::filter(!is.na(.data$value))
  } else if (L1 == "Immune_Infiltration"){
    # L2 = "CIBERSORT"
    # L3 = L3_candi$id_TIL$CIBERSORT$Level3[1]
    x_data = tcga_TIL[,c("cell_type",
                         paste0(L3,"_",L2))]
    colnames(x_data) = c("sample","value")
    x_data = x_data %>% 
      dplyr::mutate(id = L3, .before = 1) %>%
      dplyr::mutate(level2 = L2) %>%
      dplyr::filter(!is.na(.data$value))
  } else if (L1 == "Pathway_activity"){
    # L2 = "HALLMARK"
    # L3 = L3_candi$id_PW$HALLMARK$Level3[1]
    x_data = tcga_PW[,paste0(L2,"_",L3),drop=FALSE]
    colnames(x_data) = "value"
    x_data = x_data %>% as.data.frame() %>%
      tibble::rownames_to_column("sample") %>%
      dplyr::mutate(id = L3, .before = 1) %>%
      dplyr::mutate(level2 = L2) %>%
      dplyr::filter(!is.na(.data$value))	
  } else if (L1 == "Custom_metadata"){
    if(is.null(custom_metadata)){
      set.seed(42)
      sp_info = query_tcga_group()$data[,"Sample"]
      scores = matrix(stats::rnorm(nrow(sp_info)*5,mean = 1, sd = 1), ncol = 5) %>% as.data.frame()
      colnames(scores) = paste0("TF",1:5)
      custom_metadata = cbind(sp_info, scores)
    }
    x_data = custom_metadata[,c("Sample", L3)]
    colnames(x_data) = c("sample","value")
    x_data = x_data %>% as.data.frame() %>%
      dplyr::mutate(id = L3, .before = 1) %>%
      dplyr::mutate(level2 = L2) %>%
      dplyr::filter(!is.na(.data$value))	
  }
  x_data
  # id                  sample value          level2
  # 1 TP53 GTEX-S4Q7-0003-SM-3NM8M 4.785 mRNA Expression
  # 2 TP53         TCGA-19-1787-01 5.887 mRNA Expression
  # 3 TP53         TCGA-S9-A7J2-01 5.517 mRNA Expression
  # 4 TP53 GTEX-QV31-1626-SM-2S1QC 4.431 mRNA Expression
  # 5 TP53         TCGA-G3-A3CH-11 2.382 mRNA Expression
  # 6 TP53         TCGA-B5-A5OE-01 5.765 mRNA Expression
}
