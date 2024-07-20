<font size="4">**Visualize multi-omics profile of one gene among TCGA cancer types** </font>

<font size="4">1. Expression Profile</font>

- <font size="4">Normal Exp : The median gene expression in normal tissues (Including GTEx samples).</font>
- <font size="4">Tumor Exp :  The median gene expression in tumor tissues.</font>
- <font size="4">T vs. N(Wilcox): The Wilcoxon test between tumor and normal samples (P<0.001, "\*\*\*";  P<0.01, "\*\*";  P<0.05, "\*";  P>0.05, "-").</font>

> Bar plot: The 0~1 normalization on TPM expression data.



<font size="4">2. Mutation Profile</font>

- <font size="4">Mutation Dist :  The distribution of gene mutation or wild status in tumor tissues.</font>

- <font size="4">Mutation PCT :  The percentage of gene mutation status in tumor tissues.</font>



<font size="4">3. CNV Profile</font>

- <font size="4">CNV Dist : The distribution of gene copy number variation status in tumor tissues.</font>

  - -2, homozygous deletion; 
  - -1, single copy deletion; 
  - 0, diploid normal copy; 
  - 1: low-level copy number amplification;
  - 2: high-level copy number amplification


- <font size="4">CNV Amp : The percentage of gene copy number amplification (1,2) in tumor tissues.</font>
- <font size="4">CNV Del : The percentage of gene copy number deletion (-1, -2) in tumor tissues.</font>



<font size="4">4. Transcript Profile</font>

- <font size="4">Selected transcript(s) of one gene : The median transcript expression in tumor tissues.</font>



<font size="4">5. Methylation Profile</font>

- <font size="4">Selected CpG sites of one gene : The median  beta value  of CpG sites in tumor tissues.</font>



<img src="https://ucscxenashiny-1301043367.cos.ap-shanghai.myqcloud.com/Shiny-figures/helper_cross_gene.png" alt="helper_cross_gene"   width="700"/>



---

**Data source—**

- mRNA: [TcgaTargetGtex_rsem_gene_tpm](https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_gene_tpm&host=https://toil.xenahubs.net)
- Mutation: [mc3.v0.2.8.PUBLIC.nonsilentGene.xena](https://xenabrowser.net/datapages/?dataset=mc3.v0.2.8.PUBLIC.nonsilentGene.xena&host=https://pancanatlas.xenahubs.net)
- CNV: [TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes](https://xenabrowser.net/datapages/?dataset=TCGA.PANCAN.sampleMap/Gistic2_CopyNumber_Gistic2_all_thresholded.by_genes&host=https://tcga.xenahubs.net)
- Transcript: [TcgaTargetGtex_rsem_isoform_tpm](https://xenabrowser.net/datapages/?dataset=TcgaTargetGtex_rsem_isoform_tpm&host=https://toil.xenahubs.net)
- Methylation: [GDC-PANCAN.methylation450.tsv](https://xenabrowser.net/datapages/?dataset=GDC-PANCAN.methylation450.tsv&host=https://gdc.xenahubs.net)





