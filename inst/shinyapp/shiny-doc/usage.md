<center> <h2> Home Page </h2> </center>

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

<center>
<iframe src="//player.bilibili.com/player.html?aid=458184407&bvid=BV1d5411G7U5&cid=268195199&page=1" scrolling="no" border="0" frameborder="no" framespacing="0" allowfullscreen="true" width="800" height="600"> </iframe>
</center>


<center> <h2> Repository Page </h2> </center>

### Overview

This page provides many features related to datasets of UCSC Xena and the entry to "General Analysis". This section we will only focus the former part, for "General Analysis", please refer to the next section.

#### Dataset selection

#### Dataset check

#### Dataset download

### Video

<center> <h2> General Analysis Page </h2> </center>

<center> <h2> Quick PanCan Analysis Page </h2> </center>

### Overview

#### TCGA survival analysis

This module provides survival analysis feature for TCGA cohort based on data from multiple types of molecules including mRNA, transcript, mutation, etc. Please note:

1. In most cases, you can just put **gene symbol** (e.g., *TP53*). When you choose to explore the transcipt and miRNA expression, you need iuput the **Ensembl ID** (e.g., **ENST00000000233**) and **miRNA ID** (e.g., **hsa-miR-128-3p**).

2. The definition and data of four major clinical outcome endpoints are from reference [***doi: 10.1016/j.cell.2018.02.052***](https://pubmed.ncbi.nlm.nih.gov/29625055/).

3. When you choose **Auto** for **cut off mode**, the function of **surv_cutpoint {survminer package}** will be called.

NOTE: *Determine the optimal cutpoint for one or multiple continuous variables at once, using the maximally selected rank statistics from the 'maxstat' R package. This is an outcome-oriented methods providing a value of a cutpoint that correspond to the most significant relation with outcome (here for survival data)*.

### Videos

<center> <h2> Global Setting Page </h2> </center>

