### Survival analysis

This module provides survival analysis feature for TCGA cohort based on data from multiple types of molecules including mRNA, transcript, mutation, etc. Please note:

1. In most cases, you can just put **gene symbol** (e.g., *TP53*). When you choose to explore the transcipt and miRNA expression, you need iuput the **Ensembl ID** (e.g., **ENST00000000233**) and **miRNA ID** (e.g., **hsa-miR-128-3p**).

2. The definition and data of four major clinical outcome endpoints are from reference [***doi: 10.1016/j.cell.2018.02.052***](https://pubmed.ncbi.nlm.nih.gov/29625055/).

3. When you choose **Auto** for **cut off mode**, the function of **surv_cutpoint {survminer package}** will be called.

   > Determine the optimal cutpoint for one or multiple continuous variables at once, using the maximally selected rank statistics from the 'maxstat' R package. This is an outcome-oriented methods providing a value of a cutpoint that correspond to the most significant relation with outcome (here, survival).

<img src="https://gitee.com/ShixiangWang/ImageCollection/raw/master/png/20210412231135.png" alt="sur-help" style="zoom:50%;" />


