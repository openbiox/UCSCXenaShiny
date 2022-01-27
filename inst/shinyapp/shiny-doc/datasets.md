<center> <h2> Extra Datasets </h2> </center>

Extra datasets cleaned from UCSCXena hubs or references are deposited in [Zenodo repo](https://zenodo.org/record/5548587).

You can download them directly from the Zenodo repo or using `get_data()` function provided in UCSCXenaShiny R package.

If you want to know the dataset source, you can check the "data_source" attribute.

e.g.,

```r
> str(pancan_MSI)
tibble [11,139 x 3] (S3: tbl_df/tbl/data.frame)
 $ case_id     : chr [1:11139] "10.1038_ng.2273_B085" "10.1038_ng.2273_B099" "10.1038_ng.2273_R104" "10.1038_ng.2273_T26" ...
 $ cancer_type : chr [1:11139] "CHOL_10.1038_ng.2273" "CHOL_10.1038_ng.2273" "CHOL_10.1038_ng.2273" "CHOL_10.1038_ng.2273" ...
 $ MANTIS_Score: num [1:11139] 0.285 0.257 0.275 0.292 0.281 ...
 - attr(*, "data_source")= chr "DOI:https://doi.org/10.1200/PO.17.00073"
```

