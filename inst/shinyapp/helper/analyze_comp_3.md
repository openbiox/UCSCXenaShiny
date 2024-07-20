<font size="4">In **S3** Step, **batch comparison analysis** can now be performed after data preparation:</font>

- <font size="4">Firstly, you can adjust the default analysis parameters;</font>
  - <font size="4">Use **t.test ** method,  if input data is normally distributed (especially for small sample sizes) and meet homogeneity of variances (variances should be similar in both groups).</font>
  - <font size="4">Use **Wilcoxon test **method, if input data that does not meet the assumptions of the t-test by comparing the ranks of values between two groups.</font>
- <font size="4">Then, you will obtain the table arranged by p.value after clicking the "**Run**" button;</font>
- <font size="4">Finally, you can download the 2 types of results.</font>

> 1. "**Raw data(.csv)**": queried data for comparison between groups;
> 2. "**Analyzed data(.csv)**": detailed correlation result.


<p align="center">
<img src="https://ucscxenashiny-1301043367.cos.ap-shanghai.myqcloud.com/Shiny-figures//helper_analyze_comp_3.png" alt="helper_analyze_comp_3"    width="700"/>
</p>

---

- <font size="4"> See more details in [UCSCXenaShiny v2 book](https://lishensuo.github.io/UCSCXenaShiny_Book/). </font> 