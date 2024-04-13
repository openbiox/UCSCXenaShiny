<center>
<h1>Custom TPC modules</h1>
</center>


<font size="5">Here, we introduced multiple custom  modules for quick TPC molecule exploration with few steps.  In general, 10 panels for common analytical scenario are available, as follows:</font>


<center>
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240413111957810.png" alt="image-20240413111957810" style="zoom:50%;" />
</center>




## 1. Box Layout

- <font size="5">The [dashboard](https://github.com/RinteRface/shinydashboardPlus)-based **box** layouts are adopted for simple and intuitive representation. The **left box** is usually for data selection and analytical parameters adjustment. The **right box** is usually for display of result plot and one **sidebar panel** can be pulled to adjust  visualization options and download results.</font>

<center>
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240413113535287.png" alt="image-20240413113535287" />
</center>

## 2. Molecular database

<font size="5">We designate one specific UCSC Xena dataset for each molecular type of TPC databases. You can check the detailed information through the links below.</font>

### 2.1 TCGA

- <font size="5">mRNA Expression —— [TcgaTargetGtex_rsem_gene_tpm](https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_gene_tpm&host=https://toil.xenahubs.net)</font>
- <font size="5">Transcript Expression —— [TcgaTargetGtex_rsem_isoform_tpm](https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_isoform_tpm&host=https://toil.xenahubs.net)</font>
- <font size="5">DNA Methylation —— [GDC-PANCAN.methylation450.tsv](https://xenabrowser.net/datapages/?dataset=GDC-PANCAN.methylation450.tsv&host=https://gdc.xenahubs.net)</font>
- <font size="5">miRNA Expression —— [pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena](https://xenabrowser.net/datapages/?dataset=pancanMiRs_EBadjOnProtocolPlatformWithoutRepsWithUnCorrectMiRs_08_04_16.xena&host=https://pancanatlas.xenahubs.net)</font>
- <font size="5">Protein Expression —— [TCGA-RPPA-pancan-clean.xena](https://xenabrowser.net/datapages/?dataset=TCGA-RPPA-pancan-clean.xena&host=https://pancanatlas.xenahubs.net)</font>
- <font size="5">Copy Number Variation —— [Gistic2_CopyNumber_Gistic2_all_data_by_genes](https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_data_by_genes&host=https://tcga.xenahubs.net)</font>
- <font size="5">Mutation status —— [mc3.v0.2.8.PUBLIC.nonsilentGene.xena](https://xenabrowser.net/datapages/?dataset=mc3.v0.2.8.PUBLIC.nonsilentGene.xena&host=https://pancanatlas.xenahubs.net)</font>


### PCAWG

- <font size="5">mRNA Expression —— [tophat_star_fpkm_uq.v2_aliquot_gl.sp.log](https://xenabrowser.net/datapages/?dataset=tophat_star_fpkm_uq.v2_aliquot_gl.sp.log&host=https://pcawg.xenahubs.net)</font>
- <font size="5">miRNA Expression —— [x3t2m1.mature.TMM.mirna.matrix.log](https://xenabrowser.net/datapages/?dataset=x3t2m1.mature.TMM.mirna.matrix.log&host=https://pcawg.xenahubs.net)</font>
- <font size="5">Promoter Activity —— [rawPromoterActivity.sp](https://xenabrowser.net/datapages/?dataset=rawPromoterActivity.sp&host=https://pcawg.xenahubs.net)</font>
- <font size="5">Gene Fusion —— [pcawg3_fusions_PKU_EBI.gene_centric.sp.xena](https://xenabrowser.net/datapages/?dataset=pcawg3_fusions_PKU_EBI.gene_centric.sp.xena&host=https://pcawg.xenahubs.net)</font>
- <font size="5">APOBEC mutagenesis —— [MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp](https://xenabrowser.net/datapages/?dataset=MAF_Aug31_2016_sorted_A3A_A3B_comparePlus.sp&host=https://pcawg.xenahubs.net)</font>



### CCLE

- <font size="5">mRNA Expression —— [ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502&host=https://ucscpublic.xenahubs.net)</font>
- <font size="5">Protein Expression —— [ccle/CCLE_RPPA_20180123](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_RPPA_20180123&host=https://ucscpublic.xenahubs.net)</font>
- <font size="5">Copy Number Variation —— [ccle/CCLE_copynumber_byGene_2013-12-03](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_copynumber_byGene_2013-12-03&host=https://ucscpublic.xenahubs.net)</font>



<br></br>

<br></br>



> <font size="5">Tips: The menu of `Personalized TPC Pipelins` supports more general and personalized TPC molecular analysis, including alternative datasets, precise data preparation and versatile modes.</font>

