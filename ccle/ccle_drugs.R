library(tidyverse)
library(gplots)
library(preprocessCore)

# remove all the data/function --------------------------------------------

rm(list = ls())

# 1. load function for the analysis ---------------------------------------

# https://github.com/cran/ppcor/blob/master/R/ppcor_v1.01.R
source("./Drug_Resistant.v1/Drug_Resistant/codes/function/my_functions.r")
source("./Drug_Resistant.v1/Drug_Resistant/codes/function/pcor.r") # modified a bit from ppcor

# 2.import genes and prepare their expression data ------------------------
# load CCLE data

# CCLE_expr <- data.table::fread("../CCLE_DepMap_18Q2_RNAseq_reads_20180502.gct") %>%
#   distinct(Description, .keep_all = T) %>%
#   column_to_rownames(var = "Description") %>%
#   select(-c("Name"))
CCLE_expr <- data.table::fread("../CCLE_DepMap_18Q2_RNAseq_RPKM_20180502.gct") %>%
  distinct(Description, .keep_all = T) %>%
  column_to_rownames(var = "Description") %>%
  select(-c("Name"))

# import selected resistant genes:
geneids <- read.table(file = "./Drug_Resistant.v1/Drug_Resistant/data/cisplatin_resistant_list.txt", sep = "\t", header = T, colClasses = "character")
geneids <- geneids$Genes
geneids <- geneids[is.element(geneids, rownames(CCLE_expr))]
head(geneids)

# quantile normalize the microarray data among all different cell lines
CCLE_mat <- preprocessCore::normalize.quantiles(as.matrix(CCLE_expr), copy = TRUE)
colnames(CCLE_mat) <- colnames(CCLE_expr)
rownames(CCLE_mat) <- rownames(CCLE_expr)

# only keep the cell lines with both expression data and IC50 values
CCLE_sampleinfo <- data.table::fread("../CCLE_sample_info_file_2012-10-18.txt")
CCLE_drug <- data.table::fread("../CCLE_NP24.2009_Drug_data_2015.02.24.csv") %>%
  left_join(CCLE_sampleinfo, by = c("CCLE Cell Line Name" = "CCLE name"))

iOrd <- intersect(colnames(CCLE_mat), CCLE_drug$`CCLE Cell Line Name`)
CCLE_mat <- CCLE_mat[, iOrd]
# cells ids in gdsc-IC50 table and gdsc_mat expresssion matrix are now in the same order.
CCLE_drug <- filter(CCLE_drug, `CCLE Cell Line Name` %in% iOrd)
# expression of selected genes
CCLE_mat.sel <- CCLE_mat[geneids, ]


# 3. Using hgsc data to compute the partial correlation of drug-gene pairs

CCLE_drug_mat <- pivot_wider(CCLE_drug[, c("CCLE Cell Line Name", "Compound", "IC50 (uM)")],
  names_from = "Compound", values_from = "IC50 (uM)", values_fill = NA
)
# row.names(CCLE_drug_mat) <- CCLE_drug_mat$`CCLE Cell Line Name`
tissues <- as.character(unique(CCLE_drug[, c("CCLE Cell Line Name", "Site Primary")])[["Site Primary"]])
drugCor <- c()
# partial correlation of gene-drug association after controlling for tissue average expression.
for (i in 1:nrow(CCLE_mat.sel)) {
  gene.exp <- CCLE_mat.sel[i, ]
  tissues.mean <- aggregate(gene.exp, by = list(tissues), mean)
  rownames(tissues.mean) <- tissues.mean[, 1]
  controls <- tissues.mean[tissues, 2] # control of partial correlation
  # c_drugs <- unique(CCLE_drug$Compound)
  for (j in 2:ncol(CCLE_drug_mat)) { # drugs are in the 6~144 columns of gdsc_IC50
    d.IC50 <- CCLE_drug_mat[, j] # IC50 values of drug j of different cells.
    # partial correlation -- variance-covariance (mat); Spearman's cor (s);Remove NAs(na.rm=T)
    dg.cor <- pcor.test(d.IC50, gene.exp, controls,
      use = "mat", method = "s", na.rm = T
    )
    drugCor <- rbind(drugCor, c(i, j, dg.cor$estimate, dg.cor$p.value, dg.cor$n))
    # Spearman correlation coefficient, significance and number of cells has IC50 and expression
    # for pair of gene i and drug j
  }
  cat(i, "\n")
}

# conver IDs in drugCor into gene names and drug names and evaluate the FDR for each pair:
drugCor <- as.data.frame(drugCor)
drugCor[, 1] <- rownames(CCLE_mat.sel)[drugCor[, 1]]
drugCor[, 2] <- colnames(CCLE_drug_mat)[drugCor[, 2]]
names(drugCor) <- c("genes", "drugs", "cor", "p.value", "num_of_cells")
# drugTarget <- read.csv(file = "./Drug_Resistant.v1/Drug_Resistant/data/drugs_targets.csv",colClasses = "character")
# rownames(drugTarget) <- drugTarget$Drugs
# drugCor$targets <- drugTarget[drugCor$drugs,"Targets"]
drugCor <- left_join(drugCor, unique(CCLE_drug[, c("Compound", "Target")]), by = c("drugs" = "Compound"))
drugCor$fdr <- p.adjust(drugCor$p.value, method = "fdr")
save(drugCor, file = "./drugCor.rda")
write.csv(drugCor, file = "./drug_gene_corr.csv")


# 4. Expression difference between high IC50 cells and low IC50 ce --------

# At least 5 cell lines in high or low, and only the average/median
# difference in different tissues were saved.

# Normalized expression of imported gene set (normalized in the same tissue)
CCLE_mat.sel.n <- CCLE_mat.sel
tissues <- unique(tissues)
for (i in tissues) {
  cells <- unique(CCLE_drug$`CCLE Cell Line Name`[CCLE_drug$`Site Primary` == i]) %>% na.omit()
  if (length(cells) == 1) {
    CCLE_mat.sel.n[, cells] <- 0
  } else {
    cells.n <- apply(CCLE_mat.sel[, cells], 1, Zscore)
    cells.n <- t(cells.n)
    CCLE_mat.sel.n[, cells] <- cells.n
  }
}

# compute the expression diff between high IC50 cells and low IC50 cells for each tissue separately:
for (i in 1:nrow(drugCor)) {
  g <- drugCor[i, 1]
  d <- drugCor[i, 2]
  d.IC50 <- CCLE_drug_mat[[d]]
  names(d.IC50) <- CCLE_drug_mat$`CCLE Cell Line Name`
  z.diff <- c()
  for (j in tissues) {
    cells <- unique(CCLE_drug$`CCLE Cell Line Name`[CCLE_drug$`Site Primary` == j])
    d.IC50.tissue <- d.IC50[cells]
    d.IC50.tissue <- del.na(d.IC50.tissue)
    if (length(d.IC50.tissue) >= 5) { # at least 5 cell lines in the tissue
      d.IC50.tissue <- sort(d.IC50.tissue)
      l <- length(d.IC50.tissue)
      l <- floor(l / 2)
      highIC50.cells <- head(names(d.IC50.tissue), l)
      lowIC50.cells <- tail(names(d.IC50.tissue), l)
      # mean(CCLE_mat.sel.n[g,highIC50.cells]) - mean(CCLE_mat.sel.n[g,lowIC50.cells])
      z.diff <- rbind(z.diff, mean(CCLE_mat.sel.n[g, highIC50.cells]) - mean(CCLE_mat.sel.n[g, lowIC50.cells]))
    }
  }
  drugCor[i, 8] <- mean(z.diff)
  drugCor[i, 9] <- median(z.diff)
  # cat(i,"\n")
}
colnames(drugCor)[8:9] <- c("mean.diff", "median.diff")
# add other infomation about genes in additional columns :
# for example, a gene attribution in variable Info, with rownames being gene symbols
# We can provide logFC value from another study (cisplatin resistant) or
# from same GDSC database by selecting gene expression from resistant high and low celllines.
GeneInfo <- read.csv(file = "./Drug_Resistant.v1/Drug_Resistant/data/gene_value.csv", colClasses = c("character", rep("numeric", 10)))
GeneInfo$logFC <- rowMeans(GeneInfo[, 2:6]) - rowMeans(GeneInfo[, 7:11])
rownames(GeneInfo) <- GeneInfo$Gene
drugCor$geneInfo <- GeneInfo[drugCor$genes, "logFC"]
save(drugCor, file = "./drugCor.rda")
write.csv(drugCor, file = "./drug_gene_corr.csv")


# 5. Burble Plot of the drug-gene association -----------------------------

range(drugCor$geneInfo)
bins <- seq(-4, 4, by = 8 / 20) # 21 color bins given the range [-5.463484  6.312507]
# divid genes into different bins according to their expression value:
my.bg <- c()
for (i in drugCor$geneInfo) {
  x <- abs(bins - i)
  iOrd <- which(x == min(x))
  my.bg <- c(my.bg, iOrd)
}
# Give each gene a color code according to their bins:
my.col <- colorpanel(length(bins), "forestgreen", "ghostwhite", "firebrick1") # really hard to pick colors! be careful
my.bg <- my.col[my.bg] # color codes for all the gene-drug associations
# iOrd <- which(-log10(drugCor$p.value) > -log10(10^-5)) # only keep the pairs with p<0.05
iOrd <- which(-log10(drugCor$p.value) > -log10(10^-3)) # only keep the pairs with p<0.05
x.pos <- drugCor$mean.diff[iOrd]
y.pos <- -log10(drugCor$p.value)[iOrd]
text.pos <- paste(drugCor$genes[iOrd], drugCor$drugs[iOrd], drugCor$targets[iOrd], sep = "|")
text.pos <- sub("_IC_50", "", text.pos)
text.pos <- sub("\\.", "-", text.pos)
symbols(drugCor$mean.diff, -log10(drugCor$p.value),
  circle = abs(drugCor$cor), inches = 0.08,
  fg = "grey", bg = my.bg,
  ylab = "P value", xlab = "Effect",
  ylim = c(0, 8.5)
)
text(x.pos, y.pos, text.pos, cex = 0.6)
lines(c(-1, 1), c(-log10(0.00014), -log10(0.00014))) # corresponding to FDR = 0.05
lines(c(0, 0), c(0, 9))
library(fields)
image.plot(col = my.col, legend.only = TRUE, zlim = c(-4, 4)) # show color legend on the right hand

# drug info
head(names(CCLE_drug_mat)[2:ncol(CCLE_drug_mat)])
# head(colnames(gdsc_IC50)[5:dim(gdsc_IC50)[2]])

# cell type info
head(unique(CCLE_drug$`Site Primary`))
# head(unique(gdsc_IC50$Tissue))


# 6. Boxplot for drug-gene pair in a specific cancer type -----------------

# gene exp and IC50 of cells of specific cancer type.
# as an example, we use breast, a specific drug and gene here,

# change to your tissue type,drug and gene correspondingly
gene <- "TM4SF1"
drug <- "AEW541"
tissue <- "lung"
cells <- as.character(CCLE_drug[CCLE_drug$`Site Primary` == tissue, "CCLE Cell Line Name"][[1]]) %>%
  unique() %>% na.omit()

# all the cells
cells <- unique(as.character(CCLE_drug$`CCLE Cell Line Name`))

exp <- CCLE_mat[, cells]
gene.exp <- exp[gene, ]
# IC50 <- CCLE_drug_mat[cells, drug]
IC50 <- CCLE_drug_mat[CCLE_drug_mat$`CCLE Cell Line Name` %in% cells, drug][[1]]
names(IC50) <- cells
gene.exp <- gene.exp[!is.na(IC50)]
IC50 <- IC50[!is.na(IC50)]
plot(gene.exp, IC50)
# box plot
gene.high <- IC50[gene.exp >= median(gene.exp)]
gene.low <- IC50[gene.exp < median(gene.exp)]
# BoxJetter(gene.high,gene.low,"high","low","IC50 between high and low gene exp","red")
# or color cells by gene expression:
bins <- cutbin(gene.exp, 20)
# gene.min <- sort(gene.exp)[2]; gene.max <- tail(sort(gene.exp),2)[1] # the 2nd biggest and smallest value to avoid single outliers.
# bins <- c(seq(gene.min,gene.max,by= (gene.max-gene.min)/20))
my.col <- colorpanel(length(bins), "steelblue", "white", "firebrick1")
high.bg <- c()
for (i in gene.exp[names(gene.high)]) {
  x <- abs(bins - i)
  iOrd <- which(x == min(x))
  high.bg <- c(high.bg, iOrd)
}
low.bg <- c()
for (i in gene.exp[names(gene.low)]) {
  x <- abs(bins - i)
  iOrd <- which(x == min(x))
  low.bg <- c(low.bg, iOrd)
}
high.bg <- my.col[high.bg]
low.bg <- my.col[low.bg]
# color codes for cells
BoxJetter(gene.high, gene.low, "high", "low", c(high.bg, low.bg))
title(
  main = paste("IC50 between high and low ", gene, " exp in ", tissue, " cancer", sep = ""),
  xlab = paste(gene, "expression"), ylab = "IC50 values"
)
library(fields)
image.plot(col = my.col, legend.only = TRUE, zlim = c(
  min(gene.exp),
  max(gene.exp)
), legend.lab = "") # show color legend on the right hand
# Geometric mean of IC50 of gene.low and gene.high：
GM.high <- prod(exp(gene.high))^(1 / length(gene.high))
GM.high <- log(GM.high)
GM.low <- prod(exp(gene.low))^(1 / length(gene.low))
GM.low <- log(GM.low)
lines(c(0.5, 1.5), c(GM.high, GM.high))
lines(c(1.5, 2.5), c(GM.low, GM.low))
