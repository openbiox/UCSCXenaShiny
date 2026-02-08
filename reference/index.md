# Package index

## Data

Builtin datasets and functions to load data

- [`load_data()`](https://openbiox.github.io/UCSCXenaShiny/reference/load_data.md)
  : Load Dataset Provided by This Package
- [`TCGA.organ`](https://openbiox.github.io/UCSCXenaShiny/reference/TCGA.organ.md)
  : TCGA: Organ Data
- [`tcga_clinical`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_clinical.md)
  : Toil Hub: TCGA Clinical Data
- [`tcga_surv`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_surv.md)
  : Toil Hub: TCGA Survival Data
- [`tcga_genome_instability`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_genome_instability.md)
  : TCGA: Genome Instability Data
- [`tcga_gtex`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_gtex.md)
  : Toil Hub: Merged TCGA GTEx Selected Phenotype
- [`tcga_purity`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_purity.md)
  : TCGA: Purity Data
- [`tcga_subtypes`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_subtypes.md)
  : TCGA Subtype Data
- [`tcga_tmb`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_tmb.md)
  : TCGA: TMB (Tumor Mutation Burden) Data
- [`tcga_clinical_fine`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_clinical_fine.md)
  : Toil Hub: Cleaned TCGA Clinical Data for grouping
- [`ccle_absolute`](https://openbiox.github.io/UCSCXenaShiny/reference/ccle_absolute.md)
  : ABSOLUTE Result of CCLE Database
- [`ccle_info`](https://openbiox.github.io/UCSCXenaShiny/reference/ccle_info.md)
  : Phenotype Info of CCLE Database
- [`ccle_info_fine`](https://openbiox.github.io/UCSCXenaShiny/reference/ccle_info_fine.md)
  : Cleaned Phenotype Info of CCLE Database for grouping
- [`toil_info`](https://openbiox.github.io/UCSCXenaShiny/reference/toil_info.md)
  : Toil Hub: TCGA TARGET GTEX Selected Phenotype
- [`pcawg_info`](https://openbiox.github.io/UCSCXenaShiny/reference/pcawg_info.md)
  : Phenotype Info of PCAWG Database
- [`pcawg_purity`](https://openbiox.github.io/UCSCXenaShiny/reference/pcawg_purity.md)
  : Purity Data of PCAWG
- [`pcawg_info_fine`](https://openbiox.github.io/UCSCXenaShiny/reference/pcawg_info_fine.md)
  : Cleaned Phenotype Info of PCAWG Database for grouping

## Data Query

Functions to query data from UCSC Xena data hubs in single molecular
level. The functionality of functions below may overlap, we rank them by
importance.

- [`.opt_pancan`](https://openbiox.github.io/UCSCXenaShiny/reference/dot-opt_pancan.md)
  : A default setting for pan-cancer studies
- [`query_molecule_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_molecule_value.md)
  : Get Molecule or Signature Data Values from Dense (Genomic) Matrix
  Dataset of UCSC Xena Data Hubs
- [`query_pancan_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_pancan_value.md)
  : Query Single Identifier or Signature Value from Pan-cancer Database
- [`query_general_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_general_value.md)
  : Download data for shiny general analysis
- [`query_tcga_group()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_tcga_group.md)
  : Group TPC samples by build-in or custom phenotype and support
  filtering or merging operations
- [`query_toil_value_df()`](https://openbiox.github.io/UCSCXenaShiny/reference/query_toil_value_df.md)
  : Obtain ToilHub Info for Single Molecule
- [`get_ccle_cn_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_ccle_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_ccle_protein_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_ccle_mutation_status()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_transcript_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_protein_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_mutation_status()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_cn_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_methylation_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pancan_miRNA_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pcawg_gene_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pcawg_fusion_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pcawg_promoter_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pcawg_miRNA_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  [`get_pcawg_APOBEC_mutagenesis_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_pancan_value.md)
  : Fetch Identifier Value from Pan-cancer Dataset
- [`get_nonomics_value()`](https://openbiox.github.io/UCSCXenaShiny/reference/get_nonomics_value.md)
  : Fetch non-omics data of all samples from relevant databases
- [`available_hosts()`](https://openbiox.github.io/UCSCXenaShiny/reference/available_hosts.md)
  : Show Available Hosts

## Analysis and Visualiation for Pan-Cancer Study

Functions to analyze and visualize data mainly from TCGA, PCAWG and CCLE

- [`tcga_surv_get()`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_surv_analysis.md)
  [`tcga_surv_plot()`](https://openbiox.github.io/UCSCXenaShiny/reference/tcga_surv_analysis.md)
  : TCGA Survival Analysis
- [`analyze_gene_drug_response_asso()`](https://openbiox.github.io/UCSCXenaShiny/reference/analyze_gene_drug_response_asso.md)
  : Analyze Association between Gene (Signature) and Drug Response with
  CCLE Data
- [`vis_gene_drug_response_asso()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_drug_response_asso.md)
  : Visualize Gene and Drug-Target Association with CCLE Data
- [`analyze_gene_drug_response_diff()`](https://openbiox.github.io/UCSCXenaShiny/reference/analyze_gene_drug_response_diff.md)
  : Analyze Difference of Drug Response (IC50 Value (uM)) between Gene
  (Signature) High and Low Expression with CCLE Data
- [`vis_gene_drug_response_diff()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_drug_response_diff.md)
  : Visualize Gene and Drug Response Difference with CCLE Data
- [`vis_ccle_tpm()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_ccle_tpm.md)
  : Visualize CCLE Gene Expression
- [`vis_ccle_gene_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_ccle_gene_cor.md)
  : Visualize CCLE Gene Expression Correlation
- [`vis_gene_TIL_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_TIL_cor.md)
  : Heatmap for Correlation between Gene and Tumor Immune Infiltration
  (TIL)
- [`vis_gene_immune_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_immune_cor.md)
  : Heatmap for Correlation between Gene and Immune Signatures
- [`vis_gene_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_cor.md)
  : Visualize Gene-Gene Correlation in TCGA
- [`vis_gene_cor_cancer()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_cor_cancer.md)
  : Visualize Gene-Gene Correlation in a TCGA Cancer Type
- [`vis_gene_stemness_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_stemness_cor.md)
  : Visualize Correlation between Gene and Tumor Stemness
- [`vis_gene_tmb_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_tmb_cor.md)
  : Visualize Correlation between Gene and TMB (Tumor Mutation Burden)
- [`vis_gene_msi_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_msi_cor.md)
  : Visualize Correlation between Gene and MSI (Microsatellite
  instability)
- [`vis_gene_pw_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_pw_cor.md)
  : Visualize Correlation between Gene and Pathway signature Score
- [`vis_pancan_anatomy()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_pancan_anatomy.md)
  : Visualize Single Gene Expression in Anatomy Location
- [`vis_toil_TvsN()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_TvsN.md)
  : Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))
- [`vis_toil_TvsN_cancer()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_TvsN_cancer.md)
  : Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs Normal
  (TCGA & GTEx))
- [`vis_toil_Mut()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_Mut.md)
  : Visualize molecular profile difference between mutation and wild
  status of queried gene
- [`vis_toil_Mut_cancer()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_toil_Mut_cancer.md)
  : Visualize molecular profile difference between mutation and wild
  status of queried gene in Single Cancer Type
- [`vis_unicox_tree()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_unicox_tree.md)
  : Visualize Single Gene Univariable Cox Result from Toil Data Hub
- [`vis_pcawg_dist()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_pcawg_dist.md)
  : Visualize molecular profile in PCAWG
- [`vis_pcawg_gene_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_pcawg_gene_cor.md)
  : Visualize Gene-Gene Correlation in TCGA
- [`vis_pcawg_unicox_tree()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_pcawg_unicox_tree.md)
  : Visualize Single Gene Univariable Cox Result in PCAWG
- [`vis_gene_cross_omics()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_gene_cross_omics.md)
  : Visualize cross-omics of one gene among pan-cancers
- [`vis_pathway_cross_omics()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_pathway_cross_omics.md)
  : Visualize cross-omics of one pathway among pan-cancers
- [`calc_pw_mut_cnv()`](https://openbiox.github.io/UCSCXenaShiny/reference/calc_pw_mut_cnv.md)
  : Prepare Mut/CNV data of pathways given specific samples

## General Analysis and Visualiation

Functions to analyze and visualize general data, i.e., any valid data
from UCSC Xena or users

- [`vis_identifier_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_cor.md)
  : Visualize Identifier-Identifier Correlation
- [`vis_identifier_multi_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_multi_cor.md)
  : Visualize Correlation for Multiple Identifiers
- [`vis_identifier_grp_comparison()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_grp_comparison.md)
  : Visualize Comparison of an Molecule Identifier between Groups
- [`vis_identifier_grp_surv()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_grp_surv.md)
  : Visualize Identifier Group Survival Difference
- [`vis_dim_dist()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_dim_dist.md)
  : Visualize the distribution difference of samples after
  dimensionality reduction analysis
- [`vis_identifier_dim_dist()`](https://openbiox.github.io/UCSCXenaShiny/reference/vis_identifier_dim_dist.md)
  : Visualize the distribution difference of samples after Molecule
  Identifier dimensionality reduction analysis
- [`ezcor()`](https://openbiox.github.io/UCSCXenaShiny/reference/ezcor.md)
  : Run Correlation between Two Variables and Support Group by a
  Variable
- [`ezcor_batch()`](https://openbiox.github.io/UCSCXenaShiny/reference/ezcor_batch.md)
  : Run correlation between two variables in a batch mode and support
  group by a variable
- [`ezcor_partial_cor()`](https://openbiox.github.io/UCSCXenaShiny/reference/ezcor_partial_cor.md)
  : Run partial correlation

## EDA & Report

Functions for exploratory data analysis and report.

- [`mol_quick_analysis()`](https://openbiox.github.io/UCSCXenaShiny/reference/mol_quick_analysis.md)
  : Quick molecule analysis and report generation

## Shiny App

Functions related to Xena Shiny App

- [`app_run()`](https://openbiox.github.io/UCSCXenaShiny/reference/app_run.md)
  : Run UCSC Xena Shiny App
- [`app_run2()`](https://openbiox.github.io/UCSCXenaShiny/reference/app_run2.md)
  : Run UCSC Xena Shiny App with specifc content

## Others

Other docs

- [`UCSCXenaShiny`](https://openbiox.github.io/UCSCXenaShiny/reference/UCSCXenaShiny-package.md)
  [`UCSCXenaShiny-package`](https://openbiox.github.io/UCSCXenaShiny/reference/UCSCXenaShiny-package.md)
  : Xena Shiny App
- [`keep_cat_cols()`](https://openbiox.github.io/UCSCXenaShiny/reference/keep_cat_cols.md)
  : Keep Only Columns Used for Sample Selection
