# Preprocessing raw data


# Obtain Xena Info --------------------------------------------------------

library(UCSCXenaTools)
library(tidyverse)

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
saveRDS(samps, file = "R/data/XenaSamples.rds")

all_chr = purrr::map_lgl(samps, is.character)
XenaInfo$all_samples = purrr::reduce(samps[as.integer(which(all_chr))], union)
#XenaInfo$all_samples = Reduce(union, samps)
XenaInfo$n_samples = length(XenaInfo$all_samples)
XenaInfo$n_hubs = length(hosts(xena_all))
XenaInfo$n_cohorts = length(cohorts(xena_all))
XenaInfo$n_datasets = length(datasets(xena_all))
XenaInfo$all_samples = NULL
save(XenaInfo, file = "data/XenaInfo.RData")


dat_datasets <- XenaData %>%
  group_by(XenaHostNames, XenaCohorts) %>%
  summarise(N = n()) %>%
  group_by(XenaHostNames) %>%
  mutate(Sample_percent = N/sum(N)) %>%
  group_by(XenaHostNames) %>%
  #  mutate(N = sort(N, decreasing = T)) %>%
  mutate(Sample_percent = N / sum(N))


# dat_samples <- XenaData %>%
#   mutate(SampleCount = as.numeric(SampleCount)) %>%
#   group_by(XenaHostNames, XenaCohorts) %>%
#   summarise(SampleCount_sum = sum(SampleCount, na.rm = T)) %>%
#   group_by(XenaHostNames) %>%
#   mutate(SampleCount_percent = SampleCount_sum/sum(SampleCount_sum)) %>%
#   group_by(XenaHostNames) %>%
#   #  mutate(SampleCount_sum = sort(SampleCount_sum, decreasing = T)) %>%
#   mutate(SampleCount_percent = SampleCount_sum / sum(SampleCount_sum))

samps <- readRDS(file = "R/data/XenaSamples.rds")

dat <- XenaData
dat$samps = samps
dat_samples <- dat %>%
  group_by(XenaHostNames, XenaCohorts) %>%
  summarise(SampleCount_sum = length(purrr:::reduce(samps, union))) %>%
  group_by(XenaHostNames) %>%
  mutate(SampleCount_percent = SampleCount_sum/sum(SampleCount_sum))
  
  
save(XenaInfo, dat_datasets, dat_samples, file = "data/XenaInfo.RData")
