<font size="4">**Visualize multi-omics profile of one gene among PCAWG projects** </font>

<font size="4">1. Expression Profile</font>

- <font size="4">Normal Exp : The median gene expression in normal tissues.</font>
- <font size="4">Tumor Exp :  The median gene expression in tumor tissues.</font>
- <font size="4">T vs. N(Wilcox): The Wilcoxon test between tumor and normal samples (P<0.001, "\*\*\*";  P<0.01, "\*\*";  P<0.05, "\*";  P>0.05, "-").</font>

> Bar plot: The 0~1 normalization on log2(FPKM-UQ+1) expression data.



<font size="4">2. Mutation Profile</font>

- <font size="4">Mutation Dist :  The distribution of gene mutation or wild status in tumor tissues.</font>

- <font size="4">Mutation PCT :  The percentage of gene mutation status in tumor tissues.</font>



<font size="4">3. Fusion Profile</font>

- <font size="4">Fusion PCT : The percentage of samples with gene fusion events in tumor tissues.</font>



<font size="4">4. miRNA Profile</font>

- <font size="4">miRNA Exp : The median miRNA expression in tumor tissues.</font>



<font size="4">5. Promoter Profile</font>

- <font size="4">Selected promoter(s) of one gene : The median promoter activity in tumor tissues.</font>



---

**Data source—**

- mRNA: [tophat_star_fpkm_uq.v2_aliquot_gl.sp.log](https://xenabrowser.net/datapages/?dataset=tophat_star_fpkm_uq.v2_aliquot_gl.sp.log&host=https://pcawg.xenahubs.net)
- Mutation: [October_2016_whitelist_2583.snv_mnv_indel.maf.coding.xena](https://xenabrowser.net/datapages/?dataset=October_2016_whitelist_2583.snv_mnv_indel.maf.coding.xena&host=https://pcawg.xenahubs.net)
- Fusion: [pcawg3_fusions_PKU_EBI.gene_centric.sp.xena](https://xenabrowser.net/datapages/?dataset=pcawg3_fusions_PKU_EBI.gene_centric.sp.xena&host=https://pcawg.xenahubs.net)
- miRNA: [x3t2m1.mature.UQ.mirna.matrix.log](https://xenabrowser.net/datapages/?dataset=x3t2m1.mature.UQ.mirna.matrix.log&host=https://pcawg.xenahubs.net)
- Promoter: [relativePromoterActivity.sp](https://xenabrowser.net/datapages/?dataset=relativePromoterActivity.sp&host=https://pcawg.xenahubs.net)
