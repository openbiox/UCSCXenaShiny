mol_quick_select_UI = function(id, database='tcga', 
        mol_types = c("mRNA","transcript","methylation","miRNA","protein","cnv","mutation")){
    ns = NS(id)
    mol_choices = list(
        tcga = c("mRNA Expression"="mRNA", "Transcript Expression"="transcript", 
                "DNA Methylation"="methylation", "miRNA Expression"="miRNA",
                "Protein Expression"="protein", "Copy Number Variation"="cnv",
                "Mutation status"="mutation"),
        pcawg = c("mRNA Expression"="mRNA", "miRNA Expression"="miRNA",
                  "Promoter Activity"="promoter", "Gene Fusion"="fusion",
                  "APOBEC mutagenesis"="APOBEC"),
        ccle = c("mRNA Expression"="mRNA", "Protein Expression"="protein", 
                "Copy Number Variation"="cnv")
    )

    tagList(
        h4("1. Select omics type"),
        selectInput(
            inputId = ns("profile"), label = NULL,
            choices  = mol_choices[[database]][mol_choices[[database]] %in% mol_types],
            selected = "mRNA"
        ),
        h4("2. Select omics molecule"),
        awesomeRadio(
            inputId = ns("mol_type"),
            label = NULL, 
            choices = c("One molecule", "Multi-molecule formula"),
            selected = "One molecule",
            inline = TRUE, checkbox = TRUE
        ),
        tabsetPanel(
            id = ns("mol_type_tab"),
            type = "hidden",
            tabPanel("One molecule",
                virtualSelectInput(
                    inputId = ns("Pancan_search_1"),
                    label = NULL, choices = NULL,
                    width = "100%", search = TRUE,
                    allowNewOption = TRUE, dropboxWidth = "200%"
                )
            ),
            tabPanel("Multi-molecule formula",
                searchInput(
                    inputId = ns("Pancan_search_2"),
                    label = NULL,
                    value = "TP53 + 1.3 * PTEN",
                    btnReset = icon("xmark"),
                    placeholder = NULL,
                    resetValue = "TP53 + 1.3 * PTEN"
                )
            )
        )
    )
}



mol_quick_select_Server = function(input, output, session, database="tcga"){
    ns <- session$ns

    observeEvent(input$mol_type, {
        updateTabsetPanel(inputId = "mol_type_tab", selected = input$mol_type)
    })

    profile_choices <- reactive({
        if(database=="tcga"){
            switch(input$profile,
                mRNA = list(all = tcga_id.list[["Gene"]], default = "TP53"),
                methylation = list(all = tcga_id.list[["Gene"]], default = "TP53"),
                protein = list(all = tcga_id.list[["Protein"]], default = "P53"),
                transcript = list(all = tcga_id.list[["Transcript"]], default = "ENST00000000233"),
                miRNA = list(all = tcga_id.list[["miRNA"]], default = "hsa-miR-769-3p"),
                cnv = list(all = tcga_id.list[["Gene"]], default = "TP53"),
                mutation = list(all = tcga_id.list[["Gene"]], default = "TP53"),
                list(all = "NONE", default = "NONE")
            )
        } else if (database=="pcawg"){
            switch(input$profile,
                mRNA = list(all = pcawg_id.list[["Gene"]], default = "TP53"),
                miRNA = list(all = pcawg_id.list[["miRNA"]], default = "hsa-let-7a-2-3p"),
                promoter = list(all = pcawg_id.list[["Promoter"]], default = "prmtr.1"),
                fusion = list(all = pcawg_id.list[["Fusion"]], default = "SAMD11"),
                APOBEC = list(all = pcawg_id.list[["Muta"]], default = "A3A_or_A3B"),
                list(all = "NONE", default = "NONE")
            )
        } else if (database=="ccle"){
            switch(input$profile,
                mRNA = list(all = ccle_id.list[["Gene"]], default = "TP53"),
                protein = list(all = ccle_id.list[["Protein"]], default = "14-3-3_beta"),
                cnv = list(all = ccle_id.list[["Gene"]], default = "TP53"),
                list(all = "NONE", default = "NONE")
            )
        }
    })

    observe({
        updateVirtualSelect(
        "Pancan_search_1",
        choices = profile_choices()$all,
        selected = profile_choices()$default
        )
    })

    Pancan_search = reactive({
        if(input$mol_type=="One molecule"){
            input$Pancan_search_1
        } else {
            input$Pancan_search_2
        }
    })

    list(
        molecule=Pancan_search,
        profile = reactive(input$profile)
    )


}