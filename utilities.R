# Utilities


# Obtain Xena Info --------------------------------------------------------

library(UCSCXenaTools)
xena_all = XenaData %>% XenaGenerate()
XenaInfo = list()
samps = list()
for (i in 1:nrow(XenaData)) {
  s = XenaData[i, ] %>% XenaGenerate() %>% samples(by = "datasets", how = "any")
  message("Processing #", i)
  samps[[i]] = s[[1]]
}
rm(s)
names(samps) = XenaData$XenaDatasets
saveRDS(samps, file = "data/XenaSamples.rds")

all_chr = purrr::map_lgl(samps, is.character)
XenaInfo$all_samples = purrr::reduce(samps[as.integer(which(all_chr))], union)
#XenaInfo$all_samples = Reduce(union, samps)
XenaInfo$n_samples = length(XenaInfo$all_samples)
XenaInfo$n_hubs = length(hosts(xena_all))
XenaInfo$n_cohorts = length(cohorts(xena_all))
XenaInfo$n_datasets = length(datasets(xena_all))
XenaInfo$all_samples = NULL
save(XenaInfo, file = "data/XenaInfo.RData")
