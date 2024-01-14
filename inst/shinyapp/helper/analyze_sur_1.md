- <font size="4">In **S3** Step, **individual survival analysis** can now be performed after data preparation;</font>
- <font size="4">Firstly, you can adjust the default analysis and visualization parameters, respectively;</font>
- <font size="4">Noteworthily, you can try whether to use **initial data** before grouping if the data type is **numerical**;</font>

> 1. For log-rank test, it will decide the optimal cutoff;
> 2. For univariate cox regression, it will directly analyze based on the numerical data. 

- <font size="4">Then, you will obtain the curve (log-rank test) or forest (univariate cox regression) plot labeled with statistical result after clicking the "**Run**" button;</font>
- <font size="4">Finally, you can download the 3 types of results.</font>

> 1. "**Figure**": visualization result;
> 2. "**Raw data(.csv)**": queried data for X-axis and Y-axis;
> 3. "**Analyzed data(.csv)**": detailed correlation result.


<p align="center">
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240114211733002.png" alt="image-20240114211733002" width="700"/>
</p>

- <font size="4"> See more details in UCSCXenaShiny v2 book site (todo list). </font> 