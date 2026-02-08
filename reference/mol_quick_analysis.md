# Quick molecule analysis and report generation

Quick molecule analysis and report generation

## Usage

``` r
mol_quick_analysis(molecule, data_type, out_dir = ".", out_report = FALSE)
```

## Arguments

- molecule:

  a molecular identifier (e.g., "TP53") or a formula specifying genomic
  signature (`"TP53 + 2 * KRAS - 1.3 * PTEN"`).

- data_type:

  data type. Can be one of "mRNA", "transcript", "protein", "mutation",
  "cnv", "methylation", "miRNA".

- out_dir:

  path to save analysis result and report, default is '.'

- out_report:

  logical value wheather to generate html report

## Value

a list.
