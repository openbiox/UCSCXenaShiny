

- <font size="4">In this step, you can filter suitable samples through two modules based on personalized analysis requirements;</font>
- <font size="4"> In "**quick filter**" module, you can quickly filter samples based on the sample's code type such as normal/tumor (TCGA,PCAWG) or tissue type such as lung, liver (CCLE); </font>


<p align="center">
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240114144035472.png" alt="image-20240114144035472"   width="450"  />
</p>

- <font size="4"> In "**exact filter**" module, you can execute detailed filter using multiple conditions for different phenotypes. </font>



> 1. Firstly, you need to add one or multiple tumor data of interest as candidate filtering conditions;
>
> 2. Then, you can get an overview of the distribution of each candidate condition;
>
> 3. Finally, you can select one or more conditions from the candidate list for sample filtering. Different methods are provided for of different data types. 
>
>    - For **character** class, set "+" or "-" to retain or discard, respectively;
>
>    - For **numeric**  class, set ">" or "<" based on absolute values (min, max) or "%>" or "%<" based on percentile values (0,1).

<p align="center">
<img src="https://raw.githubusercontent.com/lishensuo/images2/main/img01/image-20240114144419223.png" alt="image-20240114144419223"   width="700"  />
</p>


- <font size="4"> See more details in UCSCXenaShiny v2 book site (todo list). </font> 
