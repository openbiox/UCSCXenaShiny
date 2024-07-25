<font size="4">**Visualize multi-omics profile of one pathway among TCGA cancer types** </font>

<font size="4">1. Expression Profile</font>

- <font size="4">Normal ssGSEA : The median ssGSEA  gene expression in normal tissues (Including GTEx samples).</font>
- <font size="4">Tumor ssGSEA :  The median ssGSEA  gene expression in tumor tissues.</font>
- <font size="4">T vs. N(Wilcox) : The Wilcoxon test between tumor and normal samples (P<0.001, "\*\*\*";  P<0.01, "\*\*";  P<0.05, "\*";  P>0.05, "-").</font>

> Bar plot: The 0~1 normalization on TPM expression data.



<font size="4">2. Mutation Profile</font>

- <font size="4">Mutation Dist Mean:  The distribution of mutation or wild pathway gene status in tumor tissues. If one of pathway genes is mutated, it is considered as mutation. </font>

- <font size="4">Mutation PCT Mean:  The percentage of pathway mutation status in tumor tissues.</font>

- <font size="4">Mut.Genes Mean:  The mean counts of mutated pathway genes in tumor tissues.</font>



<font size="4">3. CNV Profile</font>

- <font size="4">CNV Dist Mean: The distribution of pathway gene copy number variation status in tumor tissues.</font>
  - Amp, homozygous deletion or single copy deletion; 
  - Non, diploid normal copy; 
  - Del, Copy number amplification.


- <font size="4">Amp.Genes Mean : The copy number amplification percentage of pathway genes  in tumor tissues.</font>
- <font size="4">Del.Genes Mean : The copy number deletion percentage of pathway gene in tumor tissues.</font>



<img src="https://ucscxenashiny-1301043367.cos.ap-shanghai.myqcloud.com/Shiny-figures/helper_cross_pw.png" alt="helper_cross_pw"  width="700"/>



---

**Data source—**

- mRNA: [TcgaTargetGtex_rsem_gene_tpm](https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_gene_tpm&host=https://toil.xenahubs.net)
- Mutation: [mc3.v0.2.8.PUBLIC.nonsilentGene.xena](https://xenabrowser.net/datapages/?dataset=mc3.v0.2.8.PUBLIC.nonsilentGene.xena&host=https://pancanatlas.xenahubs.net)
- CNV: [TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes](https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes&host=https://tcga.xenahubs.net)



