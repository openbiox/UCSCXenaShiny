############# 1. Calculate TCGA Pathway score through ssGSEA mthod ################

library(IOBR)
library(UCSCXenaShiny)
library(tidyverse)

### toil TCGA 表达信息
# https://toil.xenahubs.net/download/tcga_RSEM_gene_tpm.gz
exp_dat = data.table::fread("TcgaTargetGtex_rsem_gene_tpm")
exp_dat = as.data.frame(exp_dat)
# https://toil.xenahubs.net/download/probeMap/gencode.v23.annotation.gene.probemap
id_map = data.table::fread("gencode.v23.annotation.gene.probemap")
table(exp_dat$sample %in% id_map$id)
exp_dat2 = exp_dat %>% dplyr::mutate(gene=id_map$gene[match(exp_dat$sample,id_map$id)], .before=1)
exp_mat = as.matrix(exp_dat2[,c(-1,-2)])
rownames(exp_mat) = exp_dat2$gene
dim(exp_mat)
# [1] 60498 10535

### 500个通路基因集信息
signature_collections2 = signature_collection
names(signature_collections2) = paste0("IOBR_",names(signature_collections2))
merge.list = c(hallmark, kegg, signature_collections2)

# length(merge.list) #500
# summary(map_int(merge.list, length))

gene_anno = read.csv("Gene_basic_annotation_gencode_v23.csv")
gene_anno_pc = subset(gene_anno, gene_type=="protein_coding")
exp_mat_pc = exp_mat[rownames(exp_mat) %in% gene_anno_pc$Symbol,]
dim(exp_mat_pc)
# [1] 19657 19131

sig_score_all <- GSVA::gsva(exp_mat_pc,
                        merge.list,
                        method="ssgsea",
                        kcdf="Gaussian",
                        parallel.sz = 20)

tcga_PW_all = as.data.frame(t(sig_score_all))
dim(tcga_PW_all)
# [1] 19131   500
save(tcga_PW_all, file="tcga_PW_all.rda")

tcga_PW = tcga_PW_all[rownames(tcga_PW_all) %in% tcga_clinical$sample,]
dim(tcga_PW)
# [1] 10496   500
save(tcga_PW, file="tcga_PW.rda")




sig_meta = data.frame(sig_name=names(merge.list)) %>%
    dplyr::mutate(sig_type=str_split(sig_name,"_",simplify=T)[,1])  %>%
    dplyr::mutate(sig_name2=str_match(sig_name,'^.*?_(.*)$')[,2]) 
sig_meta$sig_origin = sapply(names(merge.list), function(sig_name){
    if(grepl('IOBR',sig_name)){
        sig_name = gsub('IOBR_','',sig_name)
        sig_link = signature_collection_citation$DOI[signature_collection_citation$Signatures==sig_name]
        sig_link = paste0("IOBR package or DOI: ",sig_link)
    } else {
        sig_link = paste0("https://www.gsea-msigdb.org/gsea/msigdb/human/geneset/",sig_name,".html")
    }
    return(sig_link)
})
sig_meta$sig_gene = map_chr(merge.list, function(x){paste(x, collapse="/")})
colnames(sig_meta) = c("ID","Type","Name","Origin","Gene")
tcga_PW_meta = sig_meta
save(tcga_PW_meta, file="tcga_PW_meta.rda")





############# 2. Calculate PCAWG Pathway score through ssGSEA mthod ################
library(UCSCXenaShiny)
library(IOBR)
library(tidyverse)

exp_dat = data.table::fread("tophat_star_fpkm_uq.v2_aliquot_gl.sp.log")
exp_dat = as.data.frame(exp_dat)
id_map = data.table::fread("gencode.v19.annotation.gene.probemap")
colnames(id_map)[1] = "id"


exp_dat2 = exp_dat %>% dplyr::mutate(gene=id_map$gene[match(exp_dat$feature,id_map$id)], .before=1)
exp_mat = as.matrix(exp_dat2[,c(-1,-2)])
rownames(exp_mat) = exp_dat2$gene
dim(exp_mat)
# [1] 57820  1521

gene_anno = read.csv("../ucsc/Gene_basic_annotation_gencode_v23.csv")
gene_anno_pc = subset(gene_anno, gene_type=="protein_coding")

exp_mat_pc = exp_mat[rownames(exp_mat) %in% gene_anno_pc$Symbol,]
dim(exp_mat_pc)
# [1] 19189  1521


signature_collections2 = signature_collection
names(signature_collections2) = paste0("IOBR_",names(signature_collections2))
merge.list = c(hallmark, kegg, signature_collections2)
length(merge.list) #500


sig_score_all <- GSVA::gsva(exp_mat_pc,
                        merge.list,
                        method="ssgsea",
                        kcdf="Gaussian",
                        parallel.sz = 20)


pcawg_PW_all = as.data.frame(t(sig_score_all))
dim(pcawg_PW_all)
# [1]  1521  500
save(pcawg_PW_all, file="pcawg_PW_all.rda")

pcawg_PW = pcawg_PW_all[rownames(pcawg_PW_all) %in% pcawg_info$icgc_specimen_id,]
dim(pcawg_PW)
# [1]  1466  500
save(pcawg_PW, file="pcawg_PW.rda")


############# 3. Calculate PCAWG immune infilltration score through immunedeconv package ################
library(UCSCXenaShiny)
library(immunedeconv)
library(tidyverse)
library(e1071)
library(parallel)

# https://xenabrowser.net/datapages/?dataset=tophat_star_fpkm_uq.v2_aliquot_gl.sp.log&host=https://pcawg.xenahubs.net
# https://pcawg-hub.s3.us-east-1.amazonaws.com/download/tophat_star_fpkm_uq.v2_aliquot_gl.sp.log
# https://pcawg-hub.s3.us-east-1.amazonaws.com/download/gencode.v19.annotation.gene.probemap
exp_dat = data.table::fread("tophat_star_fpkm_uq.v2_aliquot_gl.sp.log")
exp_dat = exp_dat %>%
    as.data.frame() %>%
    tibble::column_to_rownames("feature") %>%
    as.matrix()

##!!!
exp_dat = 2^(exp_dat) #去log2

# not change to TPM; just use the fpkm-uq data
# fpkm_to_tpm <- function(fpkm) {
#   exp(log(fpkm) - log(colSums(fpkm)) + log(1e6))
# }
# exp_dat = fpkm_to_tpm(exp_dat)

id_map = data.table::fread("gencode.v19.annotation.gene.probemap")
colnames(id_map)[1] = "id"

exp_dat = exp_dat %>% as.data.frame() %>% 
    tibble::rownames_to_column("feature") %>%
    dplyr::mutate(gene=id_map$gene[match(.data$feature,id_map$id)], .before=1)
exp_mat = as.matrix(exp_dat[,c(-1,-2)])
rownames(exp_mat) = exp_dat$gene
dim(exp_mat)
# [1] 57820  1521

gene_anno = read.csv("Gene_basic_annotation_gencode_v23.csv")
gene_anno_pc = subset(gene_anno, gene_type=="protein_coding")
exp_mat_pc = exp_mat[rownames(exp_mat) %in% gene_anno_pc$Symbol,]
dim(exp_mat_pc)
# [1] 19189  1521

## 基因去重
exp_mat_pc = exp_mat_pc[!duplicated(rownames(exp_mat_pc)),]
dim(exp_mat_pc)
# https://github.com/omnideconv/immunedeconv/issues/146
exp_mat_pc = exp_mat_pc[rownames(exp_mat_pc) %in% rownames(dataset_racle$expr_mat),]
dim(exp_mat_pc)
# [1] 17810  1521

# save(exp_mat_pc, file = "PCAWG_exp_mat_pc.rda")
# exp_mat_pc = get(load("PCAWG_exp_mat_pc.rda"))

exp_sub = exp_mat_pc
## "cibersort"
set_cibersort_binary("./CIBERSORT1.04.R")
set_cibersort_mat("./LM22.txt")
score_cibersort = deconvolute(exp_sub, method="cibersort")
score_cibersort$cell_type = paste0(score_cibersort$cell_type,"_CIBERSORT")
## "cibersort_abs"
set_cibersort_binary("./CIBERSORT1.04.R")
set_cibersort_mat("./LM22.txt")
score_cibersort_abs = deconvolute(exp_sub, method="cibersort_abs")
score_cibersort_abs$cell_type = paste0(score_cibersort_abs$cell_type,"_CIBERSORT-ABS")
## "quantiseq"
score_quantiseq = deconvolute(exp_sub, method="quantiseq")
score_quantiseq$cell_type = paste0(score_quantiseq$cell_type,"_QUANTISEQ")
## "mcp_counter" 需要访问github，可能会访问失败
score_mcp_counter = deconvolute(exp_sub, method="mcp_counter")
score_mcp_counter$cell_type = paste0(score_mcp_counter$cell_type,"_MCPCOUNTER")
## "xcell"
score_xcell = deconvolute(exp_sub, method="xcell")
score_xcell$cell_type = paste0(score_xcell$cell_type,"_XCELL")
## "epic"
score_epic = deconvolute(exp_sub, method="epic")
score_epic$cell_type = paste0(score_epic$cell_type,"_EPIC")


# 注释每个project与TCGA的关系
codes_df_anno = read.csv("codes_df_anno.csv")

table(pcawg_info$dcc_project_code)



pcawg_info_filt = pcawg_info %>%
    dplyr::filter(dcc_project_code %in% codes_df_anno$code) %>%
    dplyr::filter(icgc_specimen_id %in% colnames(exp_sub))


score_timer_code.list = lapply(seq(nrow(codes_df_anno)), function(i) {
    # i = 1
    print(i)
    code = codes_df_anno$code[i]
    indication = codes_df_anno$tcga[i]

    sp_sle = pcawg_info_filt$icgc_specimen_id[pcawg_info_filt$dcc_project_code==code]
    exp_sub2 = exp_mat_pc[,colnames(exp_mat_pc) %in% sp_sle]

    indications = rep(indication, ncol(exp_sub2))
    score_timer = deconvolute(exp_sub2, method="timer", indications = indications)
    score_timer_code = score_timer %>%
        tibble::column_to_rownames("cell_type")
    score_timer_code
})

score_timer = do.call(cbind, score_timer_code.list)
dim(score_timer)
# [1]    6 1466

score_cibersort_sub = score_cibersort %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]

score_cibersort_abs_sub = score_cibersort_abs %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]

score_quantiseq_sub = score_quantiseq %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]

score_mcp_counter_sub = score_mcp_counter %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]

score_xcell_sub = score_xcell %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]

score_epic_sub = score_epic %>%
    tibble::column_to_rownames("cell_type") %>%
    .[,colnames(score_timer)]


score_merge = do.call(rbind, list(score_timer,score_cibersort_sub,score_cibersort_abs_sub,
    score_quantiseq_sub,score_mcp_counter_sub,score_xcell_sub,score_epic_sub))

pcawg_til = as.data.frame(t(score_merge))
# [1] 1466  119
save(pcawg_til, file = "pcawg_TIL.rda")




# ############# 4. Compare PCAWG immune infilltration score whether or not TPM transformation ################
# # 以PCAWG中BRCA-US项目中的98个TCGA样本比较与tcga-TIL.rda分析结果的相关性



# pcawg_TIL = load_data("pcawg_TIL")
# tcga_TIL = load_data("tcga_TIL")

# # https://dcc.icgc.org/releases/release_28/Summary
# df = data.table::fread("../sample.all_projects.tsv.gz")
# colnames(df)
# df2 = df[,c("project_code","icgc_specimen_id","submitted_specimen_id")]
# colnames(df2) = c("project","pcawg","tcga")

# # 定位BRCA-US项目的重复样本
# df2 = df2 %>% 
#   dplyr::filter(grepl("TCGA",tcga)) %>%
#   dplyr::mutate(tcga = substr(tcga, 1, 15)) %>% 
#   dplyr::distinct() %>% 
#   dplyr::filter(pcawg %in% pcawg_TIL$cell_type) %>% 
#   dplyr::filter(tcga %in% tcga_TIL$cell_type)

# tcga_TIL_reshaped = tcga_TIL %>% 
#   reshape2::melt("cell_type") %>% 
#   dplyr::rename(tcga_val=value)

# pcawg_TIL_reshaped = pcawg_TIL %>% 
#   reshape2::melt("cell_type") %>% 
#   dplyr::rename(pcawg_val=value)


# samples_sle = df2 %>% 
#   dplyr::filter(project=="BRCA-US") %>% 
#   dplyr::inner_join(tcga_TIL_reshaped, by=c("tcga"="cell_type")) %>% 
#   dplyr::inner_join(pcawg_TIL_reshaped, by=c("pcawg"="cell_type", "variable"="variable")) %>% 
#   dplyr::mutate(variable = as.character(variable))


# # 分析TIL结果的相关性


# ## 总体的相关性
# cor(samples_sle$tcga_val, samples_sle$pcawg_val, method = "pearson")
# cor(samples_sle$tcga_val, samples_sle$pcawg_val, method = "spearman")
# summary(lm(samples_sle$tcga_val~samples_sle$pcawg_val))$r.squared

# samples_sle %>% 
#   ggplot(aes(x = tcga_val, y = pcawg_val)) + 
#   geom_point() + 
#   xlab("tcga-TIL") + ylab("pcawg-TIL") + 
#   theme(text = element_text(size = 15))


# ## 每种细胞类型的单独相关性

# r_merge = lapply(unique(samples_sle$variable), function(x){
#   tmp = subset(samples_sle, variable==x)
#   r1 = cor(tmp$tcga_val, tmp$pcawg_val, method = "pearson")
#   r2 = cor(tmp$tcga_val, tmp$pcawg_val, method = "spearman")
#   R2 = summary(lm(tmp$tcga_val~tmp$pcawg_val))$r.squared
#   return(c(r1, r2,R2))
# }) %>% do.call(rbind, .) %>% as.data.frame()
# r_merge$celltype = unique(samples_sle$variable)

# mean(r_merge$V1, na.rm = T)
# sd(r_merge$V1, na.rm = T)

# mean(r_merge$V2, na.rm = T)
# sd(r_merge$V2, na.rm = T)


# r_merge %>% 
#   ggplot(aes(x = V1)) + 
#   geom_density() + 
#   theme_bw() + 
#   xlab("R pearson") + 
#   theme(text = element_text(size = 15))

# r_merge %>% 
#   ggplot(aes(x = V2)) + 
#   geom_density() + 
#   theme_bw() + 
#   xlab("R spearman") + 
#   theme(text = element_text(size = 15))


## 按照上述方法计算经TPM转换后的相关性情况，并对比
## 结果发现fpkm-uq的结果更好一些