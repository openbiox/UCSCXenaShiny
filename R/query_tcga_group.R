#' Group TCGA samples by build-in or custom phenotype and support filtering or merging operations
#'
#' @param cancer select cancer cohort(s)
#' @param custom upload custom phenotype data
#' @param group target group names 
#' @param filter_by filter samples by one or multiple criterion
#' @param merge_by merge the target group for main categories
#' @param return_all return the all phenotype data
#'
#' @return a list object with grouping samples and statistics
#' @export
#'
#' @examples
#' \dontrun{
#' query_tcga_group(group = "Age")
#' 
#' query_tcga_group(cancer="BRCA", 
#'               group = "Stage_ajcc"
#'               )
#' 
#' query_tcga_group(cancer="BRCA", 
#'               group = "Stage_ajcc",
#'               filter_by = list(
#'                 c("Code",c("TP"),"+"),
#'                 c("Stage_ajcc",c(NA),"-"))
#'               )
#' 
#' query_tcga_group(cancer="BRCA", 
#'               group = "Stage_ajcc",
#'               merge_by = list(
#'                 "Early"=c("Stage I"), 
#'                 "Late" = c("Stage II","Stage III","Stage IV"))
#'               )
#' 
#' query_tcga_group(cancer="BRCA", 
#'               group = "Age",
#'               merge_by = list(
#'                 "Young"= 60, 
#'                 "Middle"= 80, 
#'                 "Old" = 80
#'               )
#'               )
#' }
query_tcga_group = function(cancer=NULL, 
                         custom = NULL,
                         group = "Gender",
                         filter_by = NULL,
                         merge_by = NULL,
                         return_all = FALSE
                         ){

  # step1: clean build-in data
  meta_raw <- load_data("tcga_clinical") %>% 
    dplyr::distinct()

  meta_data = meta_raw[,c(1:5,7:8,10)]
  colnames(meta_data) <- c(
    "Sample", "Patient", "Cancer", "Age", "Gender",
    "Stage_ajcc", "Stage_clinical",
    "Grade"
  )
  if(TRUE){
    # AJCC stage
    meta_data$Stage_ajcc[!grepl("Stage", meta_data$Stage_ajcc)] <- NA
    meta_data$Stage_ajcc[meta_data$Stage_ajcc %in% c("Stage 0", "Stage X")] <- NA
    meta_data$Stage_ajcc <- gsub("[ABC]", "", meta_data$Stage_ajcc)
    # Clinical stage
    meta_data$Stage_clinical[!grepl("Stage", meta_data$Stage_clinical)] <- NA
    meta_data$Stage_clinical <- gsub("[ABC12]", "", meta_data$Stage_clinical)
    meta_data$Stage_clinical[meta_data$Stage_clinical == "Stage IS"] <- "Stage I"
    # histological grade
    meta_data$Grade[!meta_data$Grade %in% paste0("G", 1:4)] <- NA

    meta_data = meta_data %>% 
      dplyr::mutate(Code = substr(.data$Sample, 14,15), .before = 4) %>% 
      dplyr::mutate(Code = case_when(
        Code == "01" ~ "TP", # Primary Solid Tumor
        Code == "02" ~ "TR", # Recurrent Solid Tumor
        Code == "03" ~ "TB", # Primary Blood Derived Cancer - Peripheral Blood
        Code == "05" ~ "TAP",# Additional - New Primary
        Code == "06" ~ "TM", # Metastatic
        Code == "07" ~ "TAM",# Additional Metastatic
        Code == "11" ~ "NT"  # Solid Tissue Normal
      ), .before = 4)
    head(meta_data)
  }


  if(!is.null(custom)){
    dup_names = intersect(colnames(meta_data)[-1:-2], colnames(custom)[-1])
    if(length(dup_names)>0){
      meta_data = meta_data %>%
        dplyr::select(!all_of(dup_names))
    }
    colnames(custom)[1] = "Sample"
    meta_data = dplyr::inner_join(meta_data, custom)
  }



  # step2: filter by cancer(s)
  if(is.null(cancer)){cancer = unique(meta_data$Cancer)}
  cancer = sort(cancer)
  meta_data_sub = meta_data %>% dplyr::filter(.data$Cancer %in% cancer)
  dim(meta_data_sub)

  
  # 对否返回全部值
  if(return_all){
    return(meta_data_sub)
  }
  
  
  if(!any(colnames(meta_data_sub)==group)){
    stop(paste0("Please input the right group names:\n",
      paste(colnames(meta_data_sub)[-1:-2], collapse = " ")))
  }

  # step3: filter by groups
  if(!is.null(filter_by)){
    for (i in seq(filter_by)){
      filter_by_L1 = trimws(filter_by[[i]][1])
      filter_by_L3 = trimws(filter_by[[i]][3])
      
      filter_by_L2 = sapply(strsplit(filter_by[[i]][2],"|",fixed = T)[[1]],
                            trimws,USE.NAMES = FALSE)
      filter_by_L2[filter_by_L2 %in% "NA"] <- NA
      
      # i = 1
      if (filter_by_L3=="+"){
        meta_data_sub = meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] %in% filter_by_L2) 
      } else if (filter_by_L3=="-"){
        meta_data_sub = meta_data_sub %>% 
          dplyr::filter(!.data[[filter_by_L1]] %in% filter_by_L2) 
      } else if (filter_by_L3==">"){
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub = meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] > filter_by_L2) 
      } else if (filter_by_L3=="<"){
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub = meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] < filter_by_L2) 
      }

    }
  }
  dim(meta_data_sub)


  # step3: merge group
  if(!is.null(merge_by)){

    if(is.character(meta_data_sub[,group,drop=T])){
      merge_by_label = data.frame(item = unlist(merge_by, use.names = FALSE),
                                  label =rep(names(merge_by),sapply(merge_by, length)))
      colnames(merge_by_label)[1] = group
      meta_data_sub[,group] = dplyr::left_join(meta_data_sub, merge_by_label) %>% dplyr::pull(.data$label)
    } else if(is.numeric(meta_data_sub[,group,drop=T])){
      
      merge_by = unlist(merge_by)
      merge_by_label = rep(tail(names(merge_by),1), nrow(meta_data_sub))
      for (i in seq(nrow(meta_data_sub))) {
        value <- as.numeric(meta_data_sub[i,group])
        if(is.na(value)) {
          merge_by_label[i] = NA
          next
        }
        for (j in head(seq(merge_by),-1)) {
          if (value <= as.numeric(merge_by[j])) {
            merge_by_label[i] <- names(merge_by)[j]  
            break
          }
        }
      }

      meta_data_sub[,group] = merge_by_label
    }
  }

  meta_data_sub2 = meta_data_sub[,c("Sample","Patient","Cancer",group)]
  meta_data_sub2 = meta_data_sub2[, !duplicated(colnames(meta_data_sub2))]
 
  sub2_group_stat = meta_data_sub2 %>%
    dplyr::select(last_col()) %>%
    dplyr::mutate(across(where(is.character), as.factor)) %>%
    summary()
  
  list(data = meta_data_sub2, stat = sub2_group_stat)
}