# Data summary
Data_hubs_number <- length(unique(xena_table$Hub))
Cohorts_number <- length(unique(xena_table$Cohort))
Datasets_number <- length(unique(xena_table$`Dataset ID`))
Samples_number <- "~2,000,000"
Primary_sites_number <- "~37"
Data_subtypes_number <- "~45"
Xena_summary <- dplyr::group_by(xena_table, Hub) %>%
  dplyr::summarise(
    n_cohort = length(unique(.data$Cohort)),
    n_dataset = length(unique(.data$`Dataset ID`)), .groups = "drop"
  )


tcga_cancer_choices <- c(
  "SKCM", "THCA", "SARC", "PRAD", "PCPG", "PAAD", "HNSC", "ESCA",
  "COAD", "CESC", "BRCA", "TGCT", "KIRP", "KIRC", "LAML", "READ",
  "OV", "LUAD", "LIHC", "UCEC", "GBM", "LGG", "UCS", "THYM", "STAD",
  "DLBC", "LUSC", "MESO", "KICH", "UVM", "BLCA", "CHOL", "ACC"
)
