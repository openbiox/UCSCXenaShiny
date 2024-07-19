<font size="4.5">In **S3** Step, **individual comparison analysis** can now be performed after data preparation:</font>

- <font size="4.5">Firstly, you can adjust the default analysis and visualization parameters, respectively.</font>
  - <font size="4">Use **t.test** method,  if input data is normally distributed (especially for small sample sizes) and meet homogeneity of variances (variances should be similar in both groups).</font>
  - <font size="4">Use **Wilcoxon test**method, if input data that does not meet the assumptions of the t-test by comparing the ranks of values between two groups.</font>
- <font size="4">Then, you will obtain the violin&box plot labeled with correlation result after clicking the "**Run**" button.</font>
- <font size="4">Finally, you can download the 3 types of results.</font>

> 1. "**Figure**": visualization result;
> 2. "**Raw data(.csv)**": queried data for comparison between groups;
> 3. "**Analyzed data(.csv)**": detailed correlation result.


<p align="center">
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240114205231436.png" alt="image-20240114205231436"   width="700" />
</p>

---

- <font size="4"> See more details in [UCSCXenaShiny v2 book](https://lishensuo.github.io/UCSCXenaShiny_Book/). </font> 