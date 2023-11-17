#' Group TCGA samples by build-in or custom phenotype and support filtering or merging operations
#'
#' @param cancer select cancer cohort(s)
#' @param custom upload custom phenotype data
#' @param group target group names 
#' @param filter_by filter samples by one or multiple criterion
#' @param merge_by merge the target group for main categories
#' @param return_all return the all phenotype data
#' @param filter_id directly filter samples by provided sample ids
#' @param merge_quantile  whether to merge numerical variable by percentiles 
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
#'                  group = "Stage_ajcc",
#'                  filter_by = list(
#'                    c("Age",c(0.5),"%>"))
#' )
#' 
#' query_tcga_group(cancer="BRCA", 
#'                  group = "Stage_ajcc",
#'                  filter_by = list(
#'                    c("Age",c(60),">"))
#' )
#' 
#' query_tcga_group(cancer="BRCA", 
#'               group = "Stage_ajcc",
#'               merge_by = list(
#'                 "Early"=c("Stage I"), 
#'                 "Late" = c("Stage II","Stage III","Stage IV"))
#'               )
#' 
#' query_tcga_group(cancer="BRCA", 
#'                  group = "Age",
#'                  merge_by = list(
#'                    "Young"= c(20, 60), 
#'                    "Old"= c(60, NA)
#'                 )
#' )
#' 
#' query_tcga_group(cancer="BRCA", 
#'                  group = "Age",
#'                  merge_quantile = TRUE,
#'                  merge_by = list(
#'                    "Young"= c(0, 0.5), 
#'                    "Old"= c(0.5, 1)
#'                  )
#' )
#' }
query_tcga_group = function(cancer=NULL, 
                         custom = NULL,
                         group = "Gender",
                         filter_by = NULL,
                         filter_id = NULL,
                         merge_by = NULL,
                         merge_quantile = FALSE,   
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

  

  
  
  if(!any(colnames(meta_data_sub)==group)){
    stop(paste0("Please input the right group names:\n",
      paste(colnames(meta_data_sub)[-1:-2], collapse = " ")))
  }

  # step3: filter by groups
  if(!is.null(filter_by)){
    Samples_retain = lapply(seq(filter_by), function(i){
      filter_by_L1 = trimws(filter_by[[i]][1])
      filter_by_L3 = trimws(filter_by[[i]][3])
      filter_by_L2 = sapply(strsplit(filter_by[[i]][2],"|",fixed = T)[[1]],
                            trimws,USE.NAMES = FALSE)
      filter_by_L2[filter_by_L2 %in% "NA"] <- NA
      
      # 6种过滤方式
      if (filter_by_L3=="+"){         #保留
        meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] %in% filter_by_L2) %>% dplyr::pull('Sample')
      } else if (filter_by_L3=="-"){  #剔除
        meta_data_sub %>% 
          dplyr::filter(!.data[[filter_by_L1]] %in% filter_by_L2) %>% dplyr::pull('Sample')
      } else if (filter_by_L3==">"){  #大于 绝对值
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] > filter_by_L2) %>% dplyr::pull('Sample')
      } else if (filter_by_L3=="%>"){ #大于 分位数
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub %>% 
          dplyr::group_by("Cancer") %>% 
          dplyr::filter(.data[[filter_by_L1]] > 
                          quantile(.data[[filter_by_L1]],filter_by_L2,na.rm=T)) %>% dplyr::pull('Sample')
      } else if (filter_by_L3=="<"){ #小于 绝对值
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] < filter_by_L2) %>% dplyr::pull('Sample')  
      } else if (filter_by_L3=="%<"){#小于 分位数
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub %>% 
          dplyr::group_by("Cancer") %>% 
          dplyr::filter(.data[[filter_by_L1]] < 
                          quantile(.data[[filter_by_L1]],filter_by_L2,na.rm=T)) %>% dplyr::pull('Sample')  
      }
    }) %>% unlist()
    
    # 统计频数，保留符合全部条件的样本
    Samples_freq = table(Samples_retain)
    meta_data_sub = meta_data_sub %>% 
      dplyr::filter(.data$Sample %in% names(Samples_freq)[Samples_freq==length(filter_by)])
  }

  
  # step3-2: filter by sample id
  if(!is.null(filter_id)){
    meta_data_sub = meta_data_sub %>% 
      dplyr::filter(.data$Sample %in% filter_id)
  }
  
  
  dim(meta_data_sub)


  # step3: merge group
  
  if(!is.null(merge_by)){
    # if(class(meta_data_sub[,group,drop=T])=="numeric"){
    if(inherits(meta_data_sub[, group, drop = T], "numeric")){
      # 分位数:每种肿瘤单独的分位数
      if(merge_quantile){
        for (i in seq(merge_by)){
          if(is.na(merge_by[[i]][1])){
            merge_by[[i]][1] = 0
          }
          if(is.na(merge_by [[i]][2])){
            merge_by[[i]][2] = 1
          }
        }
        meta_data_sub_split = split(meta_data_sub, meta_data_sub$Cancer)
        meta_data_sub = lapply(meta_data_sub_split, function(meta_data_one){
          # meta_data_one = meta_data_sub_split[["CESC"]]
          dat_num = meta_data_one[,group,drop=T]
          meta_data_one[,group,drop=T] = NA
          merge_by_one = merge_by
          for (i in seq(merge_by_one)){
            merge_by_one[[i]] = quantile(dat_num, merge_by_one[[i]], na.rm = T)
          }
          # 对于较大值组，左右均闭，这样可能导致大组的数目偏多（中位数）
          meta_data_one[,group,drop=T][which(findInterval(dat_num, merge_by_one[[1]])==1)] = names(merge_by_one)[1]
          meta_data_one[,group,drop=T][which(findInterval(dat_num, merge_by_one[[2]], rightmost.closed = TRUE)==1)] = names(merge_by_one)[2]
          meta_data_one
        }) %>% do.call(rbind, .)
      } else {
        dat_num = meta_data_sub[,group,drop=T]
        meta_data_sub[,group,drop=T] = NA
        for (i in seq(merge_by )){
          if(is.na(merge_by[[i]][1])){
            merge_by[[i]][1] = min(dat_num,na.rm = T)
          }
          if(is.na(merge_by [[i]][2])){
            merge_by[[i]][2] = max(dat_num,na.rm = T)
          }
        }
        if(merge_by[[1]][1] < merge_by[[2]][2] && merge_by[[1]][2] > merge_by[[2]][1]){
          stop("Please provide two independent grouping range")
        }
        meta_data_sub[,group,drop=T][which(findInterval(dat_num, merge_by[[1]])==1)] = names(merge_by)[1]
        meta_data_sub[,group,drop=T][which(findInterval(dat_num, merge_by[[2]], rightmost.closed = TRUE)==1)] = names(merge_by)[2]
      }
    # } else if (class(meta_data_sub[,group,drop=T])=="character"){
    } else if (inherits(meta_data_sub[, group, drop = T], "character")){
      dat_chr = meta_data_sub[,group,drop=T]
      meta_data_sub[,group,drop=T] = NA
      if(length(intersect(merge_by[[1]], merge_by[[2]]))){
        stop("Please provide two independent grouping range")
      }
      
      meta_data_sub[,group,drop=T][dat_chr %in% merge_by[[1]]] = names(merge_by)[1]
      meta_data_sub[,group,drop=T][dat_chr %in% merge_by[[2]]] = names(merge_by)[2]
    }  
    meta_data_sub = meta_data_sub[!is.na(meta_data_sub[,group,drop=T]),]
  }

  # 对否返回全部值
  if(return_all){
    return(meta_data_sub)
  }
  
  # if(class(meta_data_sub[,group,drop=T])=="numeric"){
  #   dat_num = meta_data_sub[,group,drop=T]
  #   
  #   # 分位数
  #   if(merge_quantile){
  #     dat_num = meta_data_sub[,group,drop=T]
  #     for (i in seq(merge_by )){
  #       if(is.na(merge_by[[i]][1])){
  #         merge_by[[i]][1] = 0
  #       }
  #       if(is.na(merge_by [[i]][2])){
  #         merge_by[[i]][2] = 1
  #       }
  #       # 存疑：分位数去标 左开右闭 最大值不包括
  #       merge_by[[i]] = quantile(dat_num, merge_by[[i]], na.rm = T)
  #     }
  #   }
  #   
  #   meta_data_sub[,group,drop=T] = NA
  #   for (i in seq(merge_by )){
  #     if(is.na(merge_by[[i]][1])){
  #       merge_by[[i]][1] = min(dat_num,na.rm = T) - 1
  #     }
  #     if(is.na(merge_by [[i]][2])){
  #       merge_by[[i]][2] = max(dat_num,na.rm = T) + 1
  #     }
  #   }
  #   if(merge_by[[1]][1] < merge_by[[2]][2] && merge_by[[1]][2] > merge_by[[2]][1]){
  #     stop("Please provide two independent grouping range")
  #   }
  #   meta_data_sub[,group,drop=T][which(findInterval(dat_num, merge_by[[1]])==1)] = names(merge_by)[1]
  #   meta_data_sub[,group,drop=T][which(findInterval(dat_num, merge_by[[2]])==1)] = names(merge_by)[2]
  # } else if(class(meta_data_sub[,group,drop=T])=="character"){
  #   dat_chr = meta_data_sub[,group,drop=T]
  #   meta_data_sub[,group,drop=T] = NA
  #   if(length(intersect(merge_by[[1]], merge_by[[2]]))){
  #     stop("Please provide two independent grouping range")
  #   }
  #   
  #   meta_data_sub[,group,drop=T][dat_chr %in% merge_by[[1]]] = names(merge_by)[1]
  #   meta_data_sub[,group,drop=T][dat_chr %in% merge_by[[2]]] = names(merge_by)[2]
  # }
  

  
  
  
  # if(!is.null(merge_by)){
  # 
  #   if(is.character(meta_data_sub[,group,drop=T])){
  #     merge_by_label = data.frame(item = unlist(merge_by, use.names = FALSE),
  #                                 label =rep(names(merge_by),sapply(merge_by, length)))
  #     colnames(merge_by_label)[1] = group
  #     meta_data_sub[,group] = dplyr::left_join(meta_data_sub, merge_by_label) %>% dplyr::pull(.data$label)
  #   } else if(is.numeric(meta_data_sub[,group,drop=T])){
  #     
  #     merge_by = unlist(merge_by)
  #     merge_by_label = rep(tail(names(merge_by),1), nrow(meta_data_sub))
  #     for (i in seq(nrow(meta_data_sub))) {
  #       value <- as.numeric(meta_data_sub[i,group])
  #       if(is.na(value)) {
  #         merge_by_label[i] = NA
  #         next
  #       }
  #       for (j in head(seq(merge_by),-1)) {
  #         if (value <= as.numeric(merge_by[j])) {
  #           merge_by_label[i] <- names(merge_by)[j]  
  #           break
  #         }
  #       }
  #     }
  # 
  #     meta_data_sub[,group] = merge_by_label
  #   }
  # }

  meta_data_sub2 = meta_data_sub[,c("Sample","Patient","Cancer",group)]
  meta_data_sub2 = meta_data_sub2[, !duplicated(colnames(meta_data_sub2))]
 
  sub2_group_stat = meta_data_sub2 %>%
    dplyr::select(last_col()) %>%
    dplyr::mutate(across(where(is.character), as.factor)) %>%
    summary()
  
  list(data = meta_data_sub2, stat = sub2_group_stat)
}
