# Analyze Difference of Drug Response (IC50 Value (uM)) between Gene (Signature) High and Low Expression with CCLE Data

Analyze Difference of Drug Response (IC50 Value (uM)) between Gene
(Signature) High and Low Expression with CCLE Data

## Usage

``` r
analyze_gene_drug_response_diff(
  gene_list,
  drug = "ALL",
  tissue = "ALL",
  combine = FALSE,
  cutpoint = c(50, 50)
)
```

## Arguments

- gene_list:

  a gene symbol list.

- drug:

  a drug name. Check examples.

- tissue:

  a tissue name. Check examples.

- combine:

  if `TRUE`, combine the expression of gene list as a gene signature.

- cutpoint:

  cut point (in percent) for High and Low group, default is `c(50, 50)`.

## Value

a `data.frame`.

## Examples

``` r
tissue_list <- c(
  "prostate", "central_nervous_system", "urinary_tract", "haematopoietic_and_lymphoid_tissue",
  "kidney", "thyroid", "soft_tissue", "skin", "salivary_gland",
  "ovary", "lung", "bone", "endometrium", "pancreas", "breast",
  "large_intestine", "upper_aerodigestive_tract", "autonomic_ganglia",
  "stomach", "liver", "biliary_tract", "pleura", "oesophagus"
)

drug_list <- c(
  "AEW541", "Nilotinib", "17-AAG", "PHA-665752", "Lapatinib",
  "Nutlin-3", "AZD0530", "PF2341066", "L-685458", "ZD-6474", "Panobinostat",
  "Sorafenib", "Irinotecan", "Topotecan", "LBW242", "PD-0325901",
  "PD-0332991", "Paclitaxel", "AZD6244", "PLX4720", "RAF265", "TAE684",
  "TKI258", "Erlotinib"
)

target_list <- c(
  "IGF1R", "ABL", "HSP90", "c-MET", "EGFR", "MDM2", "GS", "HDAC",
  "RTK", "TOP1", "XIAP", "MEK", "CDK4", "TUBB1", "RAF", "ALK", "FGFR"
)
if (FALSE) { # \dontrun{
analyze_gene_drug_response_diff("TP53")
analyze_gene_drug_response_diff(c("TP53", "KRAS"), drug = "AEW541")
analyze_gene_drug_response_diff(c("TP53", "KRAS"),
  tissue = "kidney",
  combine = TRUE
)

# Visualization
vis_gene_drug_response_diff("TP53")
} # }
```
