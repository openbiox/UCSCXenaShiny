

#' Fetch non-omics data of all samples from relevant databases
#'
#' @param database one of "toil" (tcga), "pcawg", "ccle"
#' @param type one of "immune", "pathway", "tumor_index"
#' @param subtype For "immune" type of toil/pcawg database, one of "CIBERSORT", "CIBERSORT-ABS", "EPIC", "MCPCOUNTER", "QUANTISEQ", "TIMER", "XCELL". 
#' For "pathway" type of toil/pcawg database, one of "HALLMARK", "KEGG", "IOBR". 
#' For "tumor_index" type of toil database, one of "Tumor_Purity","Tumor_Stemness","Tumor_Mutation_Burden","Microsatellite_Instability", "Genome_Instability".
#' For "tumor_index" type of pcawg/ccle database, set NULL.
#' If subtype is NULL, return all subtypes.
#'
#' @returns dataframe
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' tcga_immune_xcell = get_nonomics_value(database = "toil", type = "immune", subtype = "XCELL")
#' 
#' tcga_pathway_kegg = get_nonomics_value(database = "toil", type = "pathway", subtype = "KEGG")
#' 
#' # Combined with sample information
#' 
#' head(tcga_clinical_fine)
#' head(tcga_surv)
#' 
#' head(pcawg_info_fine)
#' 
#' head(ccle_info_fine)
#' 
#' }
#' 



get_nonomics_value <- function(database = c("toil", "pcawg", "ccle"),
                               type = c("immune", "pathway", "tumor_index"),
                               subtype = NULL){


    avai_databases = c("toil", "pcawg", "ccle")
    if (!database %in% avai_databases) {
        stop(paste0("Please choose a valid type (", paste(avai_databases, collapse = ", "), ")!"))
    }

    avai_types = c("immune", "pathway", "tumor_index")
    if (!type %in% avai_types) {
        stop(paste0("Please choose a valid type (", paste(avai_types, collapse = ", "), ")!"))
    }

    if (database == "pcawg"){
        if (type == "tumor_index"){
            if(!is.null(subtype)){
                stop("subtype should be NULL when database is pcawg and type is tumor_index")
            }
        }
    }
    if (database == "ccle"){
        if (type %in% c("immune", "pathway")){
            stop("type should not be immune or pathway when database is ccle")
        }
        if (type == "tumor_index"){
            if(!is.null(subtype)){
                stop("subtype should be NULL when database is pcawg and type is tumor_index")
            }
        }
    }

    if (!is.null(subtype)){
        if (type=="immune"){
            avai_subtypes = c("CIBERSORT", "CIBERSORT-ABS", "EPIC", "MCPCOUNTER", "QUANTISEQ", "TIMER", "XCELL")
            if (!subtype %in% avai_subtypes) {
                stop(paste0("Please choose a valid immune subtype (", paste(avai_subtypes, collapse = ", "), ")!"))
            }  
        } else if (type=="pathway"){
            avai_subtypes = c("HALLMARK", "KEGG", "IOBR")
            if (!subtype %in% avai_subtypes) {
                stop(paste0("Please choose a valid pathway subtype (", paste(avai_subtypes, collapse = ", "), ")!"))
            }
        } else if (type=="tumor_index"){
            avai_subtypes = c("Tumor_Purity","Tumor_Stemness","Tumor_Mutation_Burden","Microsatellite_Instability", "Genome_Instability")
            if (!subtype %in% avai_subtypes) {
                stop(paste0("Please choose a valid tumor index subtype (", paste(avai_subtypes, collapse = ", "), ")!"))
            }
        }
    }


    ## toil data
    if (database=="toil" & type=="immune"){
        datas = load_data("tcga_TIL") |> dplyr::rename("Sample"="cell_type")
        if (!is.null(subtype)){
            datas = datas |> 
                dplyr::select("Sample", dplyr::ends_with(subtype))
        }
    } 

    if (database=="toil" & type=="pathway"){
        datas = load_data("tcga_PW") |> 
            tibble::rownames_to_column("Sample") |> 
            tibble::tibble()
        if (!is.null(subtype)){
            datas = datas |> 
                dplyr::select("Sample", dplyr::starts_with(subtype))
        }
    }

    if (database=="toil" & type=="tumor_index"){
        datas_list = list(
            "Tumor_Purity" = load_data("tcga_purity")  |> 
                dplyr::rename("Sample" = "sample") |>  
                dplyr::select(!"cancer_type") |> 
                dplyr::rename_with(.cols = -1,.fn = ~ paste0("Tumor_Purity_", .x)),
            "Tumor_Stemness" = load_data("tcga_stemness")  |> 
                dplyr::rename("Sample" = "sample") |>
                dplyr::rename_with(.cols = -1,.fn = ~ paste0("Tumor_Stemness_", .x)),
            "Tumor_Mutation_Burden" = load_data("tcga_tmb")  |> 
                dplyr::rename("Sample" = "Tumor_Sample_ID") |>
                dplyr::select(!c("Cohort", "Patient_ID")) |> 
                dplyr::rename_with(.cols = -1,.fn = ~ paste0("Tumor_Mutation_Burden_", .x)),
            "Microsatellite_Instability" = load_data("tcga_genome_instability")  |> 
                dplyr::rename("Sample" = "sample") |> 
                dplyr::rename_with(.cols = -1,.fn = ~ paste0("Microsatellite_Instability_", .x))
        )

        if (!is.null(subtype)){
            datas = datas_list[subtype]
        } else {
            datas <- reduce(datas_list, dplyr::full_join, by = "Sample")
        }
    }


    ## pcawg data
    if (database=="pcawg" & type=="immune"){
        datas = load_data("pcawg_TIL") |> dplyr::rename("Sample"="cell_type")
        if (!is.null(subtype)){
            datas = datas |> 
                dplyr::select("Sample", dplyr::ends_with(subtype))
        }
    } 

    if (database=="pcawg" & type=="pathway"){
        datas = load_data("pcawg_PW") |> 
            tibble::rownames_to_column("Sample") |> 
            tibble::tibble()
        if (!is.null(subtype)){
            datas = datas |> 
                dplyr::select("Sample", dplyr::starts_with(subtype))
        }
    }

    if (database=="pcawg" & type=="tumor_index"){
        datas = load_data("pcawg_purity") %>%
            dplyr::rename("Sample" = "icgc_specimen_id")
    }

    ## ccle data
    if (database=="ccle" & type=="tumor_index"){
        datas = load_data("ccle_absolute") %>%
            dplyr::rename("Sample" = "Cell Line")
    }

    return(datas)                 
}
