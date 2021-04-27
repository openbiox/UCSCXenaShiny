<center> <h1> Usage </h1> </center>

- [Home Page](#home-page)
  - [Overview](#overview)
  - [Quick search](#quick-search)
  - [Video](#video)
- [Repository Page](#repository-page)
  - [Overview](#overview-1)
  - [Dataset filter](#dataset-filter)
  - [Dataset selection](#dataset-selection)
  - [Dataset check](#dataset-check)
  - [Dataset download](#dataset-download)
  - [Video](#video-1)
- [General Analysis Page](#general-analysis-page)
  - [Overview](#overview-2)
  - [Video](#video-2)
- [Quick PanCan Analysis Page](#quick-pancan-analysis-page)
  - [Overview](#overview-3)
  - [Notes](#notes)
  - [Videos](#videos)
- [Global Setting Page](#global-setting-page)
- [Feature: Genomic Signature](#feature-genomic-signature)
  - [Overview](#overview-4)
  - [Video](#video-3)
- [Use UCSCXenaShiny R package](#use-ucscxenashiny-r-package)

## Home Page
### Overview

Home page gives you a quick overview of this tool, which includes

- basic information and statistics about supported data hubs.
- current version of this tool and its key dependent package "UCSCXenaTools".
- where the datasets come from.

If you use this tool locally, sometimes the version number can help you check if there is a update of this tool and track any bug when you report to the developers.

### Quick search

The web GUI is mainly designed and developed for cancer researchers without programming experience or want to quickly explore the flash of inspiration. We provide a search bar on the top of this page, you can type the gene symbol (e.g. *TP53*) and a window should pop up to show the pan-cancer expression distribution of this gene.

If you want to keep this data, your either right click to save this image or please go to module "TCGA: Molecular Profile Distribution Across Cancer Types (Tumor VS Normal)" under "Quick PanCan Analysis" page. The analysis module is more powerful and provides many features to custom this figure for academic use.

NOTE: *Although we label the module with TCGA, the data come from [toil data hub](https://toil.xenahubs.net), it contains samples from TCGA, TARGET and GTEx databases*.

### Video

[Click watch operations on bilibili](https://www.bilibili.com/video/bv1R84y1F7WT)

## Repository Page

### Overview

This page provides many features related to datasets of UCSC Xena and the entry to "General Analysis". This section we will only focus on the former part, for "General Analysis", please refer to the next section.

Repository page shows multiple filters on the left panels, a dataset table on the main panel and 3 buttons on the bottom.
These widgets are combined to create a full-feature page for dataset filtering, checking and downloading.

### Dataset filter

At default, when you open this page, you will find a table showing hundreds of datasets. To quickly locate what you are looking for, you can use filters from different aspects:

1. Data hub (when you don't select any data hub, all datasets will be shown in the right data table).
2. Cohort name. A cohort contains multiple datasets for same samples. So it is the common target for a analysis.
3. Data type.
4. Data subtype.

Besides, you can use the "Filter with keyword" search bar on the top-right of the dataset table to filter the datasets with any keyword.

### Dataset selection

**The filtered dataset table are not selected, you have to use your mouse to click one or more data rows to select corresponding datasets.**
After your selection, the tool will repond you selection to query the extra information (e.g., download link, the corresponding UCSC Xena page) of the datasets from UCSC Xena server.

If you just want to download datasets, a simple way is to click the download link one by one.

After dataset selection, you can use the 3 buttons below the table to

1. Check the metadata of the datasets.
2. Request (download) datasets by multiple ways.
3. Analyze datasets (when you click this button, it will automatically jump to "General Analysis" page).

### Dataset check

After you click the "Show Metadata" button, a windown will pop up to show the detail metadata of your selected datasets.
### Dataset download

After you click the "Request Data" button, a windown will pop up to show the key information of the datasets and 3 ways to download the data:

1. "Download data directly" will download datasets from UCSC Xena data hubs and zip the datasets to provides download by your browser.
2. "Batch download" will provide a `.sh` script to you, so you can run the download process in a Unix (Linux/MacOS) environment (`wget` is required in the environment). This is recommended if you selected many big datasets.
3. "Copy R download code" will show the R code to download the datasets, so you can integerate it with your following data analysis code and enhance data analysis reproducibility.

### Video

[Click watch operations on bilibili](https://www.bilibili.com/video/bv1Mq4y1J73o)

## General Analysis Page

### Overview

The 3rd button below the dataset table in "Repository" page is "Analyze Data". When you click the button, the tool will be automatically switched to this "General Analysis" page.

(To enhance the analysis, all phenotype datasets in the same cohort will be automatically queried and loaded.)

4 analysis modules are provided for solving common analysis tasks.

1. Scatter-Correlation: analyze and visualize correlation for two variables.
2. Matrix-Correlation: analyze and visualize correlation for two or more variables.
3. Group-Comparison: analyze and visualize numeric difference of a continuous variable between multiple groups of a variable.
4. Survival-Analysis: analyze and visualize survival curves between multiple groups of a variable.

On the right panel of each analysis module, we provide a sample filter. If you want to use a sample subset of the dataset for analysis, you should click the "Click to filter!" button and follow the hint step by step.

### Video

[Click watch operations on bilibili](https://www.bilibili.com/video/bv1p64y1U7iU)

## Quick PanCan Analysis Page

### Overview

This page provides many indepedent analysis modules for TCGA and CCLE related data. They are very simple, useful and easy to get started.

For most of modules provided here, multiple types of molecules including mRNA, transcript, mutation, etc. are supported. In most cases, you can just put **gene symbol** (e.g., *TP53*). When you choose to explore the transcipt and miRNA expression, you need iuput the **Ensembl ID** (e.g., **ENST00000000233**) and **miRNA ID** (e.g., **hsa-miR-128-3p**).

### Notes

1. The DNA methylation data is gene level, it is obtained from the average of all probes mapping to a gene.
2. The copy number varation value is obtained from the GISTIC2 software, if the numeric value is used, `0` indicates the normal copy.
3. The definition and data of four major clinical outcome endpoints are from reference [***doi: 10.1016/j.cell.2018.02.052***](https://pubmed.ncbi.nlm.nih.gov/29625055/).
4. For TCGA survival analysis, when you choose **Auto** for **cutoff mode**, the function of **surv_cutpoint {survminer package}** will be called. The method is described as *Determine the optimal cutpoint for one or multiple continuous variables at once, using the maximally selected rank statistics from the 'maxstat' R package. This is an outcome-oriented methods providing a value of a cutpoint that correspond to the most significant relation with outcome (here for survival data)*.

### Video

[Click watch operations on bilibili](https://www.bilibili.com/video/bv1o64y1y7jW)

## Global Setting Page

This page is used to display widgets to control global settings of the Shiny tool.

Currently, only the mirror setting is supported.

## Feature: Genomic Signature

### Overview

Inspired by UCSC Xena browser, feature "genomic signature" is also supported in this tool. For most of analysis modules, this feature is deployed and work well.

To use this feature, you just type a formula instead of a molecule identifier.

For example, if you want to see TCGA mRNA expression distribution of a signature generated by `TP53 + 1.5 * KRAS`. Firstly, delete the default symbol in the input bar "Input a gene or formula (as signature)", then input `TP53 + 1.5 * KRAS`, click the shown "Add TP53 + 1.5 * KRAS", run analysis as usual. **When you use this feature, you must input a space in the formula, so we can understand that you are inputting a signature instead of a normal symbol like gene name**.

NOTE: the signature defined in two CCLE drug related analysis modules is different from others shown above. In the CCLE drug related analysis modules, you just select multiple genes, a signature is constructed from geometrical mean of mRNA expression of your input gene list.

### Video

[Click watch operations on bilibili](https://www.bilibili.com/video/bv1zB4y1c7eB)

## Use UCSCXenaShiny R package

For most data query and analysis, we implemented them as R functions available in our R package [`{UCSCXenaShiny}`](https://github.com/openbiox/UCSCXenaShiny).
To use the data and functions provided by us, please check the [reference list](https://openbiox.github.io/UCSCXenaShiny/reference/index.html).