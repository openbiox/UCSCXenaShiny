# Fetch non-omics data of all samples from relevant databases

Fetch non-omics data of all samples from relevant databases

## Usage

``` r
get_nonomics_value(
  database = c("toil", "pcawg", "ccle"),
  type = c("immune", "pathway", "tumor_index"),
  subtype = NULL
)
```

## Arguments

- database:

  one of "toil" (tcga), "pcawg", "ccle"

- type:

  one of "immune", "pathway", "tumor_index"

- subtype:

  For "immune" type of toil/pcawg database, one of "CIBERSORT",
  "CIBERSORT-ABS", "EPIC", "MCPCOUNTER", "QUANTISEQ", "TIMER", "XCELL".
  For "pathway" type of toil/pcawg database, one of "HALLMARK", "KEGG",
  "IOBR". For "tumor_index" type of toil database, one of
  "Tumor_Purity","Tumor_Stemness","Tumor_Mutation_Burden","Microsatellite_Instability",
  "Genome_Instability". For "tumor_index" type of pcawg/ccle database,
  set NULL. If subtype is NULL, return all subtypes.

## Value

dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
tcga_immune_xcell = get_nonomics_value(database = "toil", type = "immune", subtype = "XCELL")

tcga_pathway_kegg = get_nonomics_value(database = "toil", type = "pathway", subtype = "KEGG")

# Combined with sample information

head(tcga_clinical_fine)
head(tcga_surv)

head(pcawg_info_fine)

head(ccle_info_fine)

} # }
```
