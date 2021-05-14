library(tidyverse)

all(probe$chromStart == probe$chromEnd)

probe <- read_tsv("data-raw/expressedPromoterCoordinates.probeMap")
probe$chrom <- gsub("chr", "", probe$chrom)

df <- probe %>%
  mutate(loc_id = paste0(chrom, ":", chromStart, ":", gene)) %>%
  select(loc_id, id, strand) %>%
  mutate(strand = factor(strand))


df
probeMap <- df$id
names(probeMap) <- df$loc_id

probeMap[names(probeMap) == "19:12203078:ZNF788"] # 存在极少数有2个promoter id相同。

save(probeMap, file = "data-raw/pcawg_promoter_id.rda")
