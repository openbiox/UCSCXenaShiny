<font size="4">In **S3** Step, **correlation analysis across multiple cancers** can now be performed after data preparation:</font>

- <font size="4">Firstly, you can adjust the default analysis and visualization parameters, respectively.</font>
  - <font size="4">Use **Spearman correlation** (based on the ranks of the data rather than the actual values) when input data is ordinal, not normally distributed, or you expect a monotonic but not necessarily linear relationship .</font>
  - <font size="4">Use **Pearson correlation** (based on covariance and standard deviation) when input data is continuous, normally distributed, and you expect a linear relationship.</font>
- <font size="4">Then, you will obtain the bar plot labeled with correlation coefficients after clicking the "**Run**" button.</font>
- <font size="4">Finally, you can download the 3 types of results.</font>

> 1. "**Figure**": visualization result;
> 2. "**Raw data(.csv)**": queried data for X-axis and Y-axis;
> 3. "**Analyzed data(.csv)**": detailed correlation result.

<p align="center">
<img src="https://ucscxenashiny-1301043367.cos.ap-shanghai.myqcloud.com/Shiny-figures/helper_analyze_cor_2.png" alt="helper_analyze_cor_2"   width="700" />
</p>

---

- <font size="4"> See more details in [UCSCXenaShiny v2 book](https://lishensuo.github.io/UCSCXenaShiny_Book/). </font> 

