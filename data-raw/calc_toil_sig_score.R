library(IOBR)
library(UCSCXenaShiny)
library(tidyverse)

### toil TCGA 表达信息
# https://toil.xenahubs.net/download/tcga_RSEM_gene_tpm.gz
exp_dat = data.table::fread("tcga_RSEM_gene_tpm.gz")
exp_dat = as.data.frame(exp_dat)

# https://toil.xenahubs.net/download/probeMap/gencode.v23.annotation.gene.probemap
id_map = data.table::fread("gencode.v23.annotation.gene.probemap")
table(exp_dat$sample %in% id_map$id)
exp_dat2 = exp_dat %>% dplyr::mutate(gene=id_map$gene[match(exp_dat$sample,id_map$id)], .before=1)
exp_mat = as.matrix(exp_dat2[,c(-1,-2)])
rownames(exp_mat) = exp_dat2$gene
meta_dat = tcga_gtex %>%
    dplyr::filter(sample %in% colnames(exp_mat)) %>% 
    dplyr::filter(type2 == "tumor") %>%
    dplyr::select(sample, tissue) %>%
    dplyr::mutate(across(where(is.factor), as.character))
exp_mat = exp_mat[,meta_dat$sample]
# [1] 60498  9807


### 500个通路基因集信息
signature_collections2 = signature_collection
names(signature_collections2) = paste0("IOBR_",names(signature_collections2))
merge.list = c(signature_collections2, hallmark, kegg)



### 每个cancer跑一遍ssGSEA流程
walk(sort(unique(meta_dat$tissue)), function(x){
    print(x)
    exp_mat_sle = exp_mat[,meta_dat$sample[meta_dat$tissue==x]]
    sig_score <- GSVA::gsva(exp_mat_sle,
                            merge.list,
                            method="ssgsea",
                            kcdf="Gaussian",
                            parallel.sz = 20)
    save(sig_score, file=paste0("out/",x,".rda"))
})


### 汇总结果，保存为rda文件
toil_sig_score = lapply(sort(unique(meta_dat$tissue)), function(x){
    sig_score = get(load(paste0("out/",x,".rda")))
    sig_score
}) %>% do.call(cbind, .) %>% t()

sig_meta = data.frame(sig_name=colnames(toil_sig_score)) %>%
    dplyr::mutate(sig_type=str_split(sig_name,"_",simplify=T)[,1])  %>%
    dplyr::mutate(sig_name2=str_match(sig_name,'^.*?_(.*)$')[,2]) 

library(IOBR)
sig_meta$sig_origin = sapply(colnames(toil_sig_score), function(sig_name){
    if(grepl('IOBR',sig_name)){
        sig_name = gsub('IOBR_','',sig_name)
        sig_link = signature_collection_citation$DOI[signature_collection_citation$Signatures==sig_name]
        sig_link = paste0("IOBR package or DOI: ",sig_link)
    } else {
        sig_link = paste0("https://www.gsea-msigdb.org/gsea/msigdb/human/geneset/",sig_name,".html")
    }
    return(sig_link)
})

sig_score = list(score=toil_sig_score, meta=sig_meta)

save(sig_score, file="toil_sig_score.rda")



# ## 是否需要筛选 protein_coding genes
# dim(exp_dat)
# # [1] 60498 10536
# id_map = data.table::fread("gencode.v23.annotation.gene.probemap")
# dim(id_map)
# # [1] 60498     6

# # id_map2 = data.table::fread("gencode.v23.annotation.gtf.gz")
# # id_map2_gene = subset(id_map2, V3=="gene")
# id_map2_anno = subset(id_map2, V3=="gene", select = V9) %>% 
#     # id_map2_gene[,"V9"] %>%
#     dplyr::mutate(ensembl=gsub('["]','',gsub("gene_id ","",str_split(V9,";",simplify=T)[,1]))) %>%
#     dplyr::mutate(gene_type=gsub('["]','',gsub(" gene_type ","",str_split(V9,";",simplify=T)[,2]))) %>%
#     dplyr::mutate(gene_name=gsub('["]','',gsub(" gene_name ","",str_split(V9,";",simplify=T)[,4]))) %>%
#     dplyr::select(!V9)

# library(IOBR)
# str(unique(unlist(signature_collection)))

# id_map2_anno %>% 
#     dplyr::filter(gene_name %in% unique(unlist(signature_collection))) %>%
#     dplyr::count(gene_type)

