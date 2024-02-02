#' Group TPC samples by build-in or custom phenotype and support filtering or merging operations
#'
#' @param cancer select cancer cohort(s)
#' @param custom upload custom phenotype data
#' @param group target group names 
#' @param filter_by filter samples by one or multiple criterion
#' @param merge_by merge the target group for main categories
#' @param return_all return the all phenotype data
#' @param filter_id directly filter samples by provided sample ids
#' @param merge_quantile  whether to merge numerical variable by percentiles 
#' @param database one of c("toil","pcawg","ccle")
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
query_tcga_group = function(database = c("toil","pcawg","ccle"),
                         cancer=NULL, 
                         custom = NULL,
                         group = "Gender",
                         filter_by = NULL,
                         filter_id = NULL,
                         merge_by = NULL,
                         merge_quantile = FALSE,   
                         return_all = FALSE
                         ){
  # step1: load build-in data
  database <- match.arg(database)
  # data("tcga_clinical_fine")
  # data("pcawg_info_fine")
  # data("ccle_info_fine")
  meta_data = switch(database,
                     "toil" = UCSCXenaShiny::tcga_clinical_fine,
                     "pcawg"= UCSCXenaShiny::pcawg_info_fine,
                     "ccle" = UCSCXenaShiny::ccle_info_fine)
  colnames(meta_data)[1:2] = c("Sample","Cancer") # CCLE:c("CCLE_name","Primary_Site)

  
  
  # Step2: add user-customized data
  if(!is.null(custom)){
    dup_names = intersect(colnames(meta_data)[-1:-2], colnames(custom)[-1])
    if(length(dup_names)>0){
      meta_data = meta_data %>%
        dplyr::select(!all_of(dup_names))
    } # 如果有重复列名，则去除原始metadata的重复列
    colnames(custom)[1] = "Sample"
    meta_data = dplyr::inner_join(meta_data, custom) # Note: Only consider the intersection
  }

  # step3: filter cancer(s)
  if(is.null(cancer)){cancer = unique(meta_data$Cancer)}
  meta_data_sub = meta_data %>% dplyr::filter(.data$Cancer %in% cancer)
  
  # 检查选择的分组参数是否在现有的列名中
  if(!any(colnames(meta_data_sub)==group)){
    stop(paste0("Please input the right group names:\n",
      paste(colnames(meta_data_sub)[-1:-2], collapse = " ")))
  }

  # step5: filter by specialized conditions
  if(!is.null(filter_by)){
    ## 按list内条件顺序级联筛选
    for (i in seq(filter_by)){
      # i = 1
      filter_by_L1 = trimws(filter_by[[i]][1])
      filter_by_L3 = trimws(filter_by[[i]][3])
      filter_by_L2 = sapply(strsplit(filter_by[[i]][2],"|",fixed = T)[[1]],
                            trimws,USE.NAMES = FALSE)
      filter_by_L2[filter_by_L2 %in% "NA"] <- NA

      # 6种过滤方式
      if (filter_by_L3=="+"){         #保留
        meta_data_sub <- meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] %in% filter_by_L2)
      } else if (filter_by_L3=="-"){  #剔除
        meta_data_sub <- meta_data_sub %>% 
          dplyr::filter(!.data[[filter_by_L1]] %in% filter_by_L2)
      } else if (filter_by_L3==">"){  #大于 绝对值
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub <- meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] > filter_by_L2)
      } else if (filter_by_L3=="%>"){ #大于 分位数
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub <- meta_data_sub %>% 
          dplyr::group_by("Cancer") %>% 
          dplyr::filter(.data[[filter_by_L1]] > 
                          quantile(.data[[filter_by_L1]],filter_by_L2,na.rm=T))
      } else if (filter_by_L3=="<"){ #小于 绝对值
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub <- meta_data_sub %>% 
          dplyr::filter(.data[[filter_by_L1]] < filter_by_L2)
      } else if (filter_by_L3=="%<"){#小于 分位数
        filter_by_L2 = as.numeric(filter_by_L2)
        meta_data_sub <- meta_data_sub %>% 
          dplyr::group_by("Cancer") %>% 
          dplyr::filter(.data[[filter_by_L1]] < 
                          quantile(.data[[filter_by_L1]],filter_by_L2,na.rm=T))
      }
    }
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

  meta_data_sub2 = meta_data_sub[,c("Sample","Cancer",group)]
  meta_data_sub2 = meta_data_sub2[, !duplicated(colnames(meta_data_sub2))]
 
  sub2_group_stat = meta_data_sub2 %>%
    dplyr::select(last_col()) %>%
    dplyr::mutate(across(where(is.character), as.factor)) %>%
    summary()
  
  list(data = meta_data_sub2, stat = sub2_group_stat)
}
