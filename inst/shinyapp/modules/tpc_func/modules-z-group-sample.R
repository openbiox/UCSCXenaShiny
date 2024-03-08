group_samples_UI = function(id, button_name="Filter by multi-conditions", database = "toil"){
  ns = NS(id)
  id_option = switch(database, 
      "toil"=tcga_id_option,
      "pcawg"=pcawg_id_option,
      "ccle"=ccle_id_option)

  tagList(
    # h4("1. Select one condition"),

    # shinyWidgets::actionBttn(
    #   ns("query_dist"), "Observe",
    #       style = "gradient",
    #       icon = icon("chart-simple"),
    #       color = "primary",
    #       block = TRUE,
    #       size = "sm"
    # ),
    
    fluidRow(
      column(
        6,
        selectInput(
          ns("data_L1"), "Data type:",
          choices = names(id_option),
          selected = "Phenotype data"
        )
      ),
      column(
          6,
          tabsetPanel(
            id = ns("data_L2_tab"),
            type = "hidden",
          tabPanel("Molecular profile", 
            selectInput(
              ns("genomic_profile"), "Data subtype:",
              choices = names(id_option[["Molecular profile"]]),
              selected = "mRNA Expression")
          ),
          tabPanel("Tumor index",
            selectInput(
              ns("tumor_index"), "Data subtype:",
              choices = names(id_option[["Tumor index"]]),
              selected = "Tumor Purity")
          ),
          tabPanel("Immune Infiltration",
            selectInput(
              ns("immune_infiltration"), "Data subtype:",
              choices = names(id_option[["Immune Infiltration"]]),
              selected = "CIBERSORT")
          ),
          tabPanel("Pathway activity",
            selectInput(
              ns("pathway_activity"), "Data subtype:",
              choices = names(id_option[["Pathway activity"]]),
              selected = "HALLMARK")
          ),
          tabPanel("Phenotype data",
            selectInput(
              ns("phenotype_data"), "Data subtype:",
              choices = names(id_option[["Phenotype data"]]),
              selected = "Clinical Phenotye")
          )
        )
      )
    ),
    tabsetPanel(
      id = ns("data_L3_tab"),
      type = "hidden",
      # selected = "Molecular_profile",
      tabPanel("Molecular profile",
               virtualSelectInput(
                 inputId = ns("genomic_profile_id"),
                 label = "Identifier:",
                 choices = NULL,
                 search = TRUE,
                 allowNewOption = TRUE,
                 dropboxWidth = "200%"
               )
      ),
      tabPanel("Tumor index",
               virtualSelectInput(
                 inputId = ns("tumor_index_id"),
                 label = "Identifier:",
                 choices = NULL,
                 search = TRUE,
                 allowNewOption = FALSE,
                 dropboxWidth = "200%")
      ),
      tabPanel("Immune Infiltration",
               virtualSelectInput(
                 inputId = ns("immune_infiltration_id"),
                 label = "Identifier:",
                 choices = NULL,
                 search = TRUE,
                 allowNewOption = FALSE,
                 dropboxWidth = "200%")
      ),
      tabPanel("Pathway activity",
               virtualSelectInput(
                 inputId = ns("pathway_activity_id"),
                 label = "Identifier:",
                 choices = NULL,
                 search = TRUE,
                 allowNewOption = FALSE,
                 dropboxWidth = "200%")	
      ),
      tabPanel("Phenotype data",
               virtualSelectInput(
                 inputId = ns("phenotype_data_id"),
                 label = "Identifier:",
                 choices = NULL,
                 search = TRUE,
                 allowNewOption = FALSE,
                 dropboxWidth = "200%")  
      )
    ),
    shinyWidgets::actionBttn(
      ns("query_dist"), "Observe",
          style = "gradient",
          icon = icon("chart-simple"),
          color = "primary",
          block = TRUE,
          size = "sm"
    ),
    div(verbatimTextOutput(ns("condi_dist")),
      style = "margin-top: 5px; margin-bottom: 0px;"
    ),
    
    # h4("2. Group by two range"),
    # shinyWidgets::actionBttn(
    #   ns("group_range"), "Group",
    #       style = "gradient",
    #       icon = icon("list"),
    #       color = "primary",
    #       block = TRUE,
    #       size = "sm"
    # ),

		uiOutput(ns("set_quantile.ui")),
    materialSwitch(ns("reverse_level"), "Whether reverse levels?"),
     # %>%
     #    helper(type = "markdown", size = "m", fade = TRUE, 
     #               title = "Split above phenotype into 2 groups", 
     #               content = "set_groups"),
		uiOutput(ns("set_group1.ui")),
		uiOutput(ns("set_group2.ui")),
    shinyWidgets::actionBttn(
      ns("group_range"), "Group",
          style = "gradient",
          icon = icon("list"),
          color = "primary",
          block = TRUE,
          size = "sm"
    ),

    div(verbatimTextOutput(ns("merge_out_tip")),
      style = "margin-top: 5px; margin-bottom: 0px;"
    )
    # verbatimTextOutput(ns("tmp123"))
    
  )
}


group_samples_Server = function(input, output, session, database = "toil", 
                                cancers=NULL, samples=NULL, custom_metadata=NULL, opt_pancan=NULL){
  ns <- session$ns

  id_option = switch(database, 
      "toil"=tcga_id_option,
      "pcawg"=pcawg_id_option,
      "ccle"=ccle_id_option)
  id_category = lapply(id_option, names)

  observe({
    updateTabsetPanel(inputId = "data_L2_tab", selected = input$data_L1)
    updateTabsetPanel(inputId = "data_L3_tab", selected = input$data_L1)
  }) 

  genomic_profile_choices <- reactive({
    id_option[["Molecular profile"]][[input$genomic_profile]]
  })

  tumor_index_choices <- reactive({
    id_option[["Tumor index"]][[input$tumor_index]]
  })

  immune_infiltration_choices <- reactive({
    id_option[["Immune Infiltration"]][[input$immune_infiltration]]
  })

  pathway_activity_choices <- reactive({
    id_option[["Pathway activity"]][[input$pathway_activity]]
  })

  phenotype_data_choices <- reactive({
      id_tmp = id_option[["Phenotype data"]]
    if(!is.null(custom_metadata)){
      id_tmp[["Custom metadata"]]$all = sort(colnames(custom_metadata()[-1]))
      id_tmp[["Custom metadata"]]$default = sort(colnames(custom_metadata()[-1]))[1]
    }
    id_tmp[[input$phenotype_data]]
  })

  observe({
    updateVirtualSelect(
      "genomic_profile_id",
      choices = genomic_profile_choices()$all,
      selected = genomic_profile_choices()$default
    )
    updateVirtualSelect(
      "tumor_index_id",
      choices = tumor_index_choices()$all,
      selected = tumor_index_choices()$default
    )
    updateVirtualSelect(
      "immune_infiltration_id",
      choices = immune_infiltration_choices()$all,
      selected = immune_infiltration_choices()$default
    )
    updateVirtualSelect(
      "pathway_activity_id",
      choices = pathway_activity_choices()$all,
      selected = pathway_activity_choices()$default
    )
    updateVirtualSelect(
      "phenotype_data_id",
      choices = phenotype_data_choices()$all,
      selected = phenotype_data_choices()$default
    )
  })
  
  ## query conditon & observe distribution
  condi_data = eventReactive(input$query_dist,{
    L2_x = switch(input$data_L1,
        `Molecular profile` = input$genomic_profile,
        `Tumor index` = input$tumor_index,
        `Immune Infiltration` = input$immune_infiltration,
        `Pathway activity` = input$pathway_activity,
        `Phenotype data` = input$phenotype_data
    )
    L3_x = switch(input$data_L1,
        `Molecular profile` = input$genomic_profile_id,
        `Tumor index` = input$tumor_index_id,
        `Immune Infiltration` = input$immune_infiltration_id,
        `Pathway activity` = input$pathway_activity_id,
        `Phenotype data` = input$phenotype_data_id
    )
    L1_x = names(id_category)[sapply(id_category, function(x){any(x %in% L2_x)})]
    if(is.null(opt_pancan)){
      opt_pancan = .opt_pancan
    } else {
      opt_pancan = opt_pancan()
    }
    ## 利用内部自定义下载函数获取数据
    if(database=="toil"){
      clinical_phe = tcga_phenotype_value[["Clinical Phenotype"]]
      x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
               tcga_index_value, tcga_immune_value, tcga_pathway_value, 
               clinical_phe,
               opt_pancan,custom_metadata())
    } else if(database=="pcawg"){
      clinical_phe = pcawg_phenotype_value[["Clinical Phenotype"]]
      x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
               # pcawg_index_list, pcawg_TIL, pcawg_PW, pcawg_info_fine,
               pcawg_index_value, pcawg_immune_value, pcawg_pathway_value,
               clinical_phe,
               opt_pancan,custom_metadata())
    } else if (database=="ccle"){
      clinical_phe = ccle_phenotype_value[["Clinical Phenotype"]]
      x_data = UCSCXenaShiny:::query_general_value(L1_x, L2_x, L3_x, database,
               # ccle_index_list, NULL, NULL, ccle_info_fine,
               ccle_index_value, NULL, NULL, 
               clinical_phe,
               opt_pancan,custom_metadata())
    }
    if(!is.null(samples)){
      if(!is.null(samples())){
        x_data = x_data %>%
          dplyr::filter(Sample %in% samples())
      }
    }
    # req(nrow(x_data)>0)
    if(nrow(x_data)>0){
      x_data$level1 = L1_x
      x_data$cancer = clinical_phe[,2,drop=T][match(x_data$Sample, clinical_phe$Sample)]
      x_data = x_data[,c("id","level1","level2","Sample","value","cancer")] %>%
        dplyr::arrange(cancer, Sample)
      x_data
    } else {
      NULL
    }

  })
  # output$condi_dist = renderPrint({head(condi_data())})

  output$condi_dist = renderPrint({
    if(is.null(condi_data())) return("No valid sammple for the identifier.")
    if(class(condi_data()[,"value",drop=TRUE])=="numeric"){
      summary(condi_data()[,"value",drop=TRUE])
    } else {
      table(condi_data()[,"value",drop=TRUE])
    }
  })


  output$set_quantile.ui = renderUI({
    req(!is.null(condi_data()))
    choice_chrs = condi_data()$value
    if(class(choice_chrs)!="character"){
      materialSwitch(ns("set_quantile"),"Whether group by percentile?",value = TRUE)
    }
  })


  # 创建两分组

  output$set_group1.ui = renderUI({
    req(!is.null(condi_data()))
    choice_chrs = condi_data()$value
    fluidRow(
      if(class(choice_chrs)=="character"){
        column(
          8,
          selectInput(ns("group1_range"),"Group1 Range",
            sort(unique(choice_chrs), ,na.last = T), multiple=T)
        )
      } else {
        column(8,
          fluidRow(
            column(6,
              numericInput(ns("group1_min"),"Group1 [min",value=NA)),
            column(6,
              numericInput(ns("group1_max"),"max)",
                value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T))))
          )
        )
      }
      ,
      column(
        4,
        textInput(ns("group1_name"),"Group1 Name", "Group1")
      )
    )
  })

  output$set_group2.ui = renderUI({
    req(!is.null(condi_data()))
    choice_chrs = condi_data()$value
    fluidRow(
      if(class(choice_chrs)=="character"){
        column(
          8,
          selectInput(ns("group2_range"),"Group2 Range",
            sort(unique(choice_chrs), ,na.last = T), multiple=T)
        )
      } else {
        column(8,
          fluidRow(
            column(6,
              numericInput(ns("group2_min"),"Group2 [min",
                value=ifelse(input$set_quantile,0.5,median(choice_chrs, na.rm=T)))),
            column(6,
              numericInput(ns("group2_max"),"max]",value=NA))
          )
        )
      }
      ,
      column(
        4,
        textInput(ns("group2_name"),"Group2 Name", "Group2")
      )
    )
  })




  # 提取分组条件
  merge_by = eventReactive(input$group_range, {
    merge_by.list = list(NULL, NULL)
    names(merge_by.list) = c(input$group1_name, input$group2_name)

    choice_chrs = condi_data()$value
    if(class(choice_chrs)=="character"){
      merge_by.list[[1]] = input$group1_range
      merge_by.list[[2]] = input$group2_range
    } else {
      merge_by.list[[1]] = 
        c(ifelse(is.null(input$group1_min),NA,input$group1_min),
          ifelse(is.null(input$group1_max),NA,input$group1_max))
      merge_by.list[[2]] = 
        c(ifelse(is.null(input$group2_min),NA,input$group2_min),
          ifelse(is.null(input$group2_max),NA,input$group2_max))
    }
    merge_by.list
  })

  # # 查看分组条件

    
  # 根据提供条件进行分组


  merge_out = eventReactive(input$group_range,{
    merge_tmp = query_tcga_group(database = database,
                                 cancer=cancers(), 
                                 custom=condi_data()[,c("Sample","value")],
                                 group="value", filter_id = samples(),
                                 merge_by = merge_by(),
                                 merge_quantile = input$set_quantile)[['data']]
    merge_tmp

    merge_tmp$phenotype = unique(condi_data()$id)
    colnames(merge_tmp)[3] = "group"

    if(input$reverse_level){
      group_levels = c(input$group2_name, input$group1_name)
    } else {
      group_levels = c(input$group1_name, input$group2_name)
    }
    merge_tmp[,"group"] = factor(merge_tmp[,"group",drop=TRUE], levels = group_levels)  
    #分组前数据
    merge_tmp$origin = condi_data()$value[match(merge_tmp$Sample, condi_data()$Sample)]

    merge_tmp
  })

  # 分组结果提示
  observeEvent(input$group_range,{
    output$merge_out_tip = renderPrint({
      shiny::validate(
        need(try(nrow(merge_out())>0), 
          "Please inspect whether to input valid grouping set."),
      )
      tb = table(merge_out()$group)
      cat(paste0(levels(merge_out()$group)[1],": ", tb[1], ", ",
                     levels(merge_out()$group)[2],": ", tb[2]))
    })
  })


  # output$tmp123 = renderPrint({head(merge_out())})

  # 检查是否有效分组
  # 主要针对多癌症分组情况
  merge_out_check = reactive({
    valid_cancers = merge_out() %>%
      dplyr::mutate(group = as.character(group)) %>%
      dplyr::distinct(Cancer, group) %>%
      dplyr::count(Cancer) %>%
      dplyr::filter(n==2) %>% dplyr::pull("Cancer")
    valid_group = merge_out() %>% 
      dplyr::filter(Cancer %in% valid_cancers)
    valid_group
  })

  return(merge_out_check)
}


# # A tibble: 6 × 6
#   Sample          Patient      Cancer group phenotype origin
#   <chr>           <chr>        <chr>  <fct> <chr>     <chr> 
# 1 TCGA-A7-A0CE-11 TCGA-A7-A0CE BRCA   NT    Code      NT    
# 2 TCGA-A7-A0CH-11 TCGA-A7-A0CH BRCA   NT    Code      NT    
# 3 TCGA-A7-A0D9-11 TCGA-A7-A0D9 BRCA   NT    Code      NT    
# 4 TCGA-A7-A0DB-11 TCGA-A7-A0DB BRCA   NT    Code      NT    
# 5 TCGA-A7-A0DC-11 TCGA-A7-A0DC BRCA   NT    Code      NT    
# 6 TCGA-A7-A13E-11 TCGA-A7-A13E BRCA   NT    Code      NT 
