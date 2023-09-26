<script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
<script type="text/x-mathjax-config">
 MathJax.Hub.Config({
 tex2jax: {
 inlineMath: [['$', '$'] ],
 displayMath: [ ['$$','$$'], ["\\[","\\]"] ]
 }
 });
</script>

## Contents

* [Getting started](#sec1)

* [Overall flow](#sec2)

  * [Load files](#sec2-1)
  
  * [Check settings](#sec2-2)

  * [Perform analysis](#sec2-3)

  * [Check results](#sec2-4)
  
* [File format](#sec3)

* [Settings](#sec4)

* [Calculation principle](#sec5)

  * [STR](#sec5-1)
  
  * [Y-STR](#sec5-2)
  
  * [mtDNA](#sec5-3)

<a id="sec1"></a>

---

## Getting started

1. Ensure that R (>= 4.3.0) is installed. It is available from the <a href="http://www.R-project.org" target="_blank">R Development Core Team website</a>.

2. Begin an R session.

3. Execute the following command in R to install required packages.

```
install.packages(c("shiny", "Rcpp", "data.table", "dplyr", "DT", "magrittr", "pedtools", "ribd", "shinyjs", "shinythemes", "waiter"))
```

4. Download ”relsearch_1.0.0.zip” from the <a href="https://github.com/manabe0322/relsearch/releases" target="_blank">GitHub repository page</a>.

5. Execute the following commands in R to start GUI.

```
library(relsearch)
relsearch()
```

<a id="sec2"></a>

---

## Overall flow

<br>

<a id="sec2-1"></a>

#### Load files

Load the following files from each **"Browse..."** button.

<br>

<u>STR</u>

* [Victim database](#ff1)

* [Reference database](#ff2)

* [Allele frequencies](#ff3)

<br>

<u>Y-STR</u>

* [Victim database](#ff4)

* [Reference database](#ff5)

<br>

<u>mtDNA</u>

* [Victim database](#ff6)

* [Reference database](#ff7)

<br>

**Note**

* Example files can be downloaded from the **"Example files"** tab.

* <span style="background-color:#ffcce5">Possibility of memory over.</span>

<br>

<a id="sec2-2"></a>

#### Check settings

Check the following settings from the **"Tools"** tab.

* [Criteria](#sec4-1)

* [Information on the relationship](#sec4-2)

* [Mutation rates](#sec4-3)

* [Parameters for autosomal STR](#sec4-4)

<br>

<a id="sec2-3"></a>

#### Perform analysis

Click the **"Analysis" button in the "Load" tab** after loading required files. Then, the following analyses will be started.

<br>

<u>STR analysis</u>

The likelihood ratios of each victim-reference pair are calculated considering mutation and drop-out. Click [here](#sec5-1) for more information.

<br>

<u>Y-STR analysis</u>

The number of inconsistencies and the mutational steps in each victim-reference pair are calculated. Click [here](#sec5-2) for more information.

<br>

<u>mtDNA analysis</u>

The number of inconsistencies and the total shared lengths in each victim-reference pair are calculated. Click [here](#sec5-3) for more information.

<br>

<a id="sec2-4"></a>

#### Check results

<br>

<a id="sec3"></a>

---

## File format

<a id="ff1"></a>

<u>STR: Victim database</u>

<br>

**Example**

<style>
.fileformat{
    text-align: center;
}
</style>
<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Marker1</td>
    <td bgcolor=whitesmoke>Marker1</td>
    <td bgcolor=whitesmoke>Marker2</td>
    <td bgcolor=whitesmoke>Marker2</td>
    <td bgcolor=whitesmoke>Marker3</td>
    <td bgcolor=whitesmoke>Marker3</td>
  </tr>
  <tr>
    <td>Victim1</td>
    <td>15</td>
    <td>16</td>
    <td>14</td>
    <td>17</td>
    <td>10</td>
    <td>10</td>
  </tr>
  <tr>
    <td>Victim2</td>
    <td>15</td>
    <td>17</td>
    <td>16</td>
    <td></td>
    <td>10</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Victim3</td>
    <td>16</td>
    <td>17</td>
    <td></td>
    <td></td>
    <td>12</td>
    <td></td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName" and columns for each marker (two columns in each).

* The marker with two empty cells (e.g., Marker 2 of sample 'Victim3' in the above table) is ignored when calculating the likelihood ratio.

* The marker with one empty cell (e.g., Marker 2 of sample 'Victim2' in the above table) can be regarded as both homozygote (i.e., no drop-out) and heterozygote with drop-out of one allele when calculating the likelihood ratio.

<br>

<a id="ff2"></a>

<u>STR: Reference database</u>

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Relationship</td>
    <td bgcolor=whitesmoke>Marker1</td>
    <td bgcolor=whitesmoke>Marker1</td>
    <td bgcolor=whitesmoke>Marker2</td>
    <td bgcolor=whitesmoke>Marker2</td>
    <td bgcolor=whitesmoke>Marker3</td>
    <td bgcolor=whitesmoke>Marker3</td>
  </tr>
  <tr>
    <td>Reference1</td>
    <td>parent-child</td>
    <td>15</td>
    <td>15</td>
    <td>14</td>
    <td>16</td>
    <td>10</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference2</td>
    <td>sibling</td>
    <td>15</td>
    <td>16</td>
    <td>14</td>
    <td></td>
    <td>10</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference3</td>
    <td>sibling</td>
    <td>16</td>
    <td>17</td>
    <td></td>
    <td></td>
    <td>12</td>
    <td>13</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the columns "SampleName", "Relationship", and columns for each marker (two columns in each).

* <span style="background-color:#ffcce5">Relationships must be the same as those of other database.</span>

* The marker with two empty cells (e.g., Marker 2 of sample 'Reference3' in the above table) is ignored when calculating the likelihood ratio.

* The marker with one empty cell (e.g., Marker 2 of sample 'Reference2' in the above table) can be regarded as both homozygote (i.e., no drop-out) and heterozygote with drop-out of one allele when calculating the likelihood ratio.

<br>

<a id="ff3"></a>

<u>STR: Allele frequencies</u>

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>Allele</td>
    <td bgcolor=whitesmoke>D3S1358</td>
    <td bgcolor=whitesmoke>vWA</td>
    <td bgcolor=whitesmoke>D16S539</td>
    <td bgcolor=whitesmoke>CSF1PO</td>
    <td bgcolor=whitesmoke>TPOX</td>
    <td bgcolor=whitesmoke>D8S1179</td>
  </tr>
  <tr>
    <td>10</td>
    <td></td>
    <td></td>
    <td>0.200266</td>
    <td>0.215471</td>
    <td>0.033566</td>
    <td>0.128775</td>
  </tr>
  <tr>
    <td>10.1</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>10.2</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>10.3</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>11</td>
    <td></td>
    <td></td>
    <td>0.186981</td>
    <td>0.206175</td>
    <td>0.356597</td>
    <td>0.106206</td>
  </tr>
  <tr>
    <td>11.1</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>11.2</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>11.3</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>12</td>
    <td>0.002324</td>
    <td></td>
    <td>0.172036</td>
    <td>0.420983</td>
    <td>0.038219</td>
    <td>0.123133</td>
  </tr>
  <tr>
    <td>12.2</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>13</td>
    <td>0.001328</td>
    <td>0.000664</td>
    <td>0.069744</td>
    <td>0.069389</td>
    <td>0.001329</td>
    <td>0.225357</td>
  </tr>
  <tr>
    <td>13.2</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>14</td>
    <td>0.02656</td>
    <td>0.194887</td>
    <td>0.008967</td>
    <td>0.017596</td>
    <td>0.000997</td>
    <td>0.207766</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the columns "Allele" and columns for each marker (one column in each).


<br>

<a id="ff4"></a>

<u>Y-STR: Victim database</u>

<br>

**Example**

<br>

<a id="ff5"></a>

<u>Y-STR: Reference database</u>

<br>

**Example**

<br>

<a id="ff6"></a>

<u>mtDNA: Victim database</u>

<br>

**Example**

<br>

<a id="ff7"></a>

<u>mtDNA: Reference database</u>

<br>

**Example**

<br>

<a id="sec4"></a>

---

## Settings

<br>

<a id="sec4-1"></a>

#### Criteria

<br>

<a id="sec4-2"></a>

#### Information on the relationship

<br>

<a id="sec4-3"></a>

#### Mutation rates

<br>

<a id="sec4-4"></a>

#### Parameters for autosomal STR

<br>

<a id="sec5"></a>

---

## Calculation principle

<br>

<a id="sec5-1"></a>

#### STR

$$ LR = \frac{Pr(E|H1)}{Pr(E|H2)} $$

<br>

<a id="sec5-2"></a>

#### Y-STR

<br>

<a id="sec5-3"></a>

#### mtDNA
