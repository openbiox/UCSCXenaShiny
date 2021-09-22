#' TCGA Survival Analysis
#'
#' - Firstly, get merged data of one molecular profile value and associated clinical data from TCGA Pan-Cancer dataset.
#' - Secondly, filter data as your wish.
#' - Finally, show K-M plot.
#'
#' @name tcga survival analysis
#' @rdname tcga_surv_analysis
#' @param item a molecular identifier, can be gene symbol (common cases), protein symbol, etc.
#' @param TCGA_cohort a TCGA cohort, e.g. "LUAD" (default), "LUSC", "ACC".
#' @param profile a molecular profile. Option can be one of "mRNA" (default), "miRNA", "methylation", "transcript", "protein", "mutation", "cnv".
#' @param TCGA_cli_data a `data.frame` containing TCGA clinical data. Default use pre-compiled TCGA clinical data in
#' this package.
#' @param time the column name for "time".
#' @param status the column name for "status".
#' @param cutoff_mode mode for grouping samples, can be "Auto" (default) or "Custom".
#' @param cutpoint cut point (in percent) for "Custom" mode, default is `c(50, 50)`.
#' @param cnv_type only used when profile is "cnv", can select from `c("Duplicated", "Normal", "Deleted")`.
#' @param data a subset of result from `tcga_surv_get()`.
#'
#' @return a `data.frame` or a plot.
#' @export
#' @examples
#' \dontrun{
#' # 1. get data
#' data <- tcga_surv_get("TP53")
#' # 2. filter data (optional)
#'
#' # 3. show K-M plot
#' tcga_surv_plot(data, time = "DSS.time", status = "DSS")
#' }
tcga_surv_get <- function(item,
                          TCGA_cohort = "LUAD",
                          profile = c("mRNA", "miRNA", "methylation", "transcript", "protein", "mutation", "cnv"),
                          TCGA_cli_data = dplyr::full_join(
                            load_data("tcga_clinical"),
                            load_data("tcga_surv"),
                            by = "sample"
                          )) {
  stopifnot(length(item) == 1)
  profile <- match.arg(profile)
  if (!requireNamespace("stringr")) {
    install.packages("stringr")
  }

  print(head(TCGA_cli_data))
  # type 有几种额外的 Xena 做过聚合的结果
  # "COADREAD" "FPPP"     "GBMLGG"   "LUNG"     "PANCAN"
  # FPPP 好像不是正常的样本，不推荐使用，已去除
  if (TCGA_cohort == "PANCAN") {
    cliMat <- TCGA_cli_data
  } else {
    .type <- switch(TCGA_cohort,
      COADREAD = c("COAD", "READ"),
      GBMLGG = c("GBM", "LGG"),
      LUNG = c("LUAD", "LUSC"),
      TCGA_cohort
    )
    cliMat <- TCGA_cli_data %>% dplyr::filter(.data$type %in% .type)
  }

  message("Querying data of molecule(s) ", item, " for survival analysis in TCGA cohort ", TCGA_cohort, ".")

  gd <- query_pancan_value(item, data_type = profile)
  if (is.list(gd)) gd <- gd[[1]]

  if (all(is.na(gd))) {
    return(NULL)
  }
  gd <- gd[nchar(names(gd)) == 15]
  merged_data <- dplyr::tibble(
    sampleID = names(gd),
    value = as.numeric(gd)
  ) %>%
    dplyr::filter(as.numeric(substr(.data$sampleID, 14, 15)) < 10) %>%
    dplyr::inner_join(cliMat, by = c("sampleID" = "sample")) %>%
    dplyr::select(c(
      "sampleID", "value", "OS", "OS.time", "DSS", "DSS.time", "DFI", "DFI.time", "PFI", "PFI.time",
      "gender", "age_at_initial_pathologic_diagnosis", "ajcc_pathologic_tumor_stage"
    )) %>%
    dplyr::rename(
      age = .data$age_at_initial_pathologic_diagnosis,
      stage = .data$ajcc_pathologic_tumor_stage
    ) %>%
    dplyr::mutate(stage = stringr::str_match(.data$stage, "Stage\\s+(.*?)[ABC]?$")[, 2]) %>%
    dplyr::mutate(
      stage = ifelse(is.na(.data$stage), "Unknown", .data$stage),
      gender = ifelse(is.na(.data$gender), "Unknown", .data$gender)
    )
  return(merged_data)
}

#' @rdname tcga_surv_analysis
#' @param palette color palette, can be "hue", "grey", "RdBu", "Blues", "npg", "aaas", etc.
#' More see `?survminer::ggsurvplot`.
#' @param ... other parameters passing to `survminer::ggsurvplot`
#' @export
tcga_surv_plot <- function(data,
                           time = "time",
                           status = "status",
                           cutoff_mode = c("Auto", "Custom"),
                           cutpoint = c(50, 50),
                           cnv_type = c("Duplicated", "Normal", "Deleted"),
                           profile = c("mRNA", "miRNA", "methylation", "transcript", "protein", "mutation", "cnv"),
                           palette = "aaas",
                           ...) {
  cutoff_mode <- match.arg(cutoff_mode)
  profile <- match.arg(profile)
  kept_cols <- c("value", time, status)
  data <- dplyr::select(data, dplyr::all_of(kept_cols))
  if (time != "time") colnames(data)[2] <- "time"
  if (status != "status") colnames(data)[3] <- "status"

  if (!requireNamespace("survminer")) {
    install.packages("survminer")
  }

  if (profile %in% c("mRNA", "miRNA", "methylation", "transcript", "protein")) {
    sur_plot(data, cutoff_mode, cutpoint, palette = palette, ...)
  } else if (profile == "mutation") {
    sur_plot_mut(data, palette = palette, ...)
  } else {
    sur_plot_cnv(data, cnv_type, palette = palette, ...)
  }
}

## Survival analysis for mRNA and protein expression
sur_plot <- function(data, cutoff_mode, cutpoint, palette = "aaas", ...) {
  if (cutoff_mode == "Auto") {
    data2 <- data %>%
      survminer::surv_cutpoint(
        time = "time", event = "status",
        variables = c("value"),
        minprop = 0.25, progressbar = TRUE
      ) %>%
      survminer::surv_categorize(labels = c("Low", "High")) %>%
      data.frame()
    data$group <- data2$value
  } else {
    if (length(cutpoint) == 1) {
      cutpoint <- c(cutpoint, cutpoint)
    }
    data <- data %>%
      dplyr::arrange(.data$value) %>%
      dplyr::mutate(per_rank = 100 / nrow(.) * (1:nrow(.)))
    data <- data %>%
      dplyr::mutate(group = dplyr::case_when(
        .data$per_rank > !!cutpoint[2] ~ "High",
        .data$per_rank <= !!cutpoint[1] ~ "Low",
        TRUE ~ NA_character_
      ))
  }
  p_survplot(data, palette = palette, ...)
}

## Survival analysis for mutation DNA
sur_plot_mut <- function(data, palette = "aaas", ...) {
  data <- data %>%
    dplyr::rename(mut = .data$value) %>%
    dplyr::mutate(group = ifelse(.data$mut == 1, "MT", "WT"))
  if (length(table(data$group)) < 2) {
    return(NULL)
  }
  p_survplot(data, palette = palette, ...)
}

## Survival analysis for CNV
sur_plot_cnv <- function(data, cnv_type = c("Duplicated", "Normal", "Deleted"), palette = "aaas", ...) {
  data <- data %>%
    dplyr::rename(mut = .data$value) %>%
    dplyr::mutate(group = ifelse(.data$mut == 0, "Normal",
      ifelse(.data$mut > 0, "Duplicated", "Deleted")
    )) %>%
    dplyr::filter(.data$group %in% cnv_type)
  if (length(table(data$group)) < 2) {
    return(NULL)
  }
  p_survplot(data, palette = palette, ...)
}

## ggsurvplot
p_survplot <- function(data, palette = "aaas", ...) {
  fit <- survival::survfit(survival::Surv(time, status) ~ group, data = data)
  p <- survminer::ggsurvplot(fit,
    data = data, pval = TRUE, pval.method = TRUE,
    palette = palette,
    size = 1.2, # change line size
    font.legend = c(14, "black"),
    font.x = c(14, "bold", "black"),
    font.y = c(14, "bold", "black"),
    font.tickslab = c(12, "bold", "black"),
    xlab = "Duration overall survival (days)",
    risk.table = TRUE,
    risk.table.col = "strata", # Risk table color by groups
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = TRUE, # plot the number of censored subjects at time t
    surv.plot.height = 0.7,
    risk.table.height = 0.15,
    ncensor.plot.height = 0.15,
    ggtheme = ggplot2::theme_classic(), # Change ggplot2 theme
    ...
  ) + ggplot2::guides(color = ggplot2::guide_legend(ncol = 3))

  attr(p, "data") <- na.omit(data[, c("time", "status", "group")])
  p
}
