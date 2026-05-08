<font size="4">**Visualize multi-omics profile of one gene among CCLE tissues** </font>

<font size="4">1. Expression Profile</font>

- <font size="4">Median Exp : The median gene expression in cell lines grouped by primary sites.</font>

> Bar plot: The 0~1 normalization on RPKM expression data.



<font size="4">2. Mutation Profile</font>

- <font size="4">Mutation Dist :  The distribution of gene mutation or wild status.</font>

- <font size="4">Mutation PCT :  The percentage of gene mutation status.</font>



<font size="4">3. CNV Profile</font>

- <font size="4">CNV Dist : The distribution of gene copy number variation status.</font>

- <font size="4">Gain (>0.3) : The percentage of gene copy number amplification (log2 ratio > 0.3).</font>
- <font size="4">Loss (<-0.3) : The percentage of gene copy number deletion (log2 ratio < -0.3).</font>



<font size="4">4. Protein Profile</font>

- <font size="4">Selected antibody(s) of one gene : The median protein expression (RPPA) in cell lines.</font>



---

**Data source—**

- mRNA: [ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_RNAseq_RPKM_20180502&host=https://public.xenahubs.net)
- Mutation: [ccle/CCLE_DepMap_18Q2_maf_20180502](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_DepMap_18Q2_maf_20180502&host=https://public.xenahubs.net)
- CNV: [ccle/CCLE_copynumber_byGene_2013-12-03](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_copynumber_byGene_2013-12-03&host=https://public.xenahubs.net)
- Protein: [ccle/CCLE_RPPA_20180123](https://xenabrowser.net/datapages/?dataset=ccle/CCLE_RPPA_20180123&host=https://public.xenahubs.net)
