# Get Molecule or Signature Data Values from Dense (Genomic) Matrix Dataset of UCSC Xena Data Hubs

Get Molecule or Signature Data Values from Dense (Genomic) Matrix
Dataset of UCSC Xena Data Hubs

## Usage

``` r
query_molecule_value(dataset, molecule, host = NULL)
```

## Arguments

- dataset:

  a UCSC Xena dataset in dense matrix format (rows are features (e.g.,
  gene, cell line) and columns are samples).

- molecule:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`). **NOTE**, when a
  signature is specified, a space must exist in the input.

- host:

  a UCSC Xena host, default is `NULL`, auto-detect from the dataset.

## Value

a named vector.

## Examples

``` r
# What does dense matrix mean?
table(UCSCXenaTools::XenaData$Type)
#> 
#> clinicalMatrix  genomicMatrix genomicSegment mutationVector 
#>            616           1339            230            129 
# It is a the UCSC Xena dataset with "Type" equals to "genomicMatrix"
if (FALSE) { # \dontrun{
dataset <- "ccle/CCLE_copynumber_byGene_2013-12-03"
x <- query_molecule_value(dataset, "TP53")
head(x)

signature <- "TP53 + 2*KRAS - 1.3*PTEN" # a space must exist in the string
y <- query_molecule_value(dataset, signature)
head(y)
} # }
```
