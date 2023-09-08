## Contents

* [Getting started](#sec1)

* [Overall flow](#sec2)

  * [Load files](#sec2-1)
  
  * [Check settings](#sec2-2)

  * [Perform analysis](#sec2-3)

  * [Check results](#sec2-4)
  
* [File format](#sec3)

* [Calculation principle](#sec4)

  * [STR](#sec4-1)
  
  * [Y-STR](#sec4-2)
  
  * [mtDNA](#sec4-3)

<a id="sec1"></a>

---

## Getting started

1. Ensure that R (>= 4.3.0) is installed. It is available from the <a href="http://www.R-project.org" target="_blank">R Development Core Team website</a>.

2. Begin an R session.

3. Execute the following command in R to install required packages.

```
install.packages(c("shiny", "Rcpp", "data.table", "dplyr", "DT", "magrittr", "pedtools", "ribd", "shinyjs", "shinythemes", "waiter"))
```

4. Go to https://github.com/manabe0322/relsearch/releases.

5. Download ”relsearch_1.0.0.zip”.

6. Execute the following commands in R to start GUI.

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

* Victim database

* Reference database

* Allele frequencies

<br>

<u>Y-STR</u>

* Victim database

* Reference database

<br>

<u>mtDNA</u>

* Victim database

* Reference database

<br>

**Note**

* See [file format](#sec3)
* Possibility of memory over

<br>

<a id="sec2-2"></a>

#### Check settings

Check the following settings from the **"Tools"** tab.

* Criteria

* Information on the relationship

* Mutation rates

* Parameters for autosomal STR

<br>

<a id="sec2-3"></a>

#### Perform analysis

Click the **"Analysis"** button after loading required files. Then, the following analyses will be started.

<br>

<u>STR analysis</u>

The likelihood ratios of each victim-reference pair are calculated considering mutation and drop-out. Click [here](#sec4-1) for more information.

<br>

<u>Y-STR analysis</u>

The number of inconsistencies and the mutational steps in each victim-reference pair are calculated. Click [here](#sec4-2) for more information.

<br>

<u>mtDNA analysis</u>

The number of inconsistencies and the total shared lengths in each victim-reference pair are calculated. Click [here](#sec4-3) for more information.

<br>

<a id="sec2-4"></a>

#### Check results

<br>

<a id="sec3"></a>

---

## File format

<u>STR: Victim database</u>

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
    <td>11</td>
  </tr>
  <tr>
    <td>Victim2</td>
    <td>15</td>
    <td>17</td>
    <td>14</td>
    <td>19</td>
    <td>10</td>
    <td>11</td>
  </tr>
</table>

<br>

<a id="ff2"></a>

<u>STR: Reference database</u>

<br>

**Note**

* Relationships must be the same as those of other database

<br>

<a id="ff3"></a>

<u>STR: Allele frequencies</u>

<br>

<a id="ff4"></a>

<u>Y-STR: Victim database</u>

<br>

<a id="ff5"></a>

<u>Y-STR: Reference database</u>

<br>

<a id="ff6"></a>

<u>mtDNA: Victim database</u>

<br>

<a id="ff7"></a>

<u>mtDNA: Reference database</u>

<br>

<a id="sec4"></a>

---

## Calculation principle

<br>

<a id="sec4-1"></a>

#### STR

$$ LR = \frac{Pr(E|H1)}{Pr(E|H2)} $$

<br>

<a id="sec4-2"></a>

#### Y-STR

<br>

<a id="sec4-3"></a>

#### mtDNA
