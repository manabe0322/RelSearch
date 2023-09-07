## Contents

* [Getting started](#sec1)

* [Overall flow](#sec2)

  * [Load files](#sec2-1)

  * [Perform analysis](#sec2-2)

  * [Check results](#sec2-3)
  
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

#### Perform analysis

<a id="sec2-3"></a>

#### Check results

<a id="sec3"></a>

---

## File format

<u>STR: Victim database</u>

<table border="1" width="80%">
<tr><td align="center" bgcolor=whitesmoke>SampleName<td align="center" bgcolor=whitesmoke>Marker1<td align="center" bgcolor=whitesmoke>Marker1<td align="center" bgcolor=whitesmoke>Marker2<td align="center" bgcolor=whitesmoke>Marker2<td align="center" bgcolor=whitesmoke>Marker3<td align="center" bgcolor=whitesmoke>Marker3<td align="center" bgcolor=whitesmoke>...
<tr><td align="center">Victim1<td align="center">15<td align="center">16<td align="center">14<td align="center">17<td align="center">10<td align="center">11<td align="center">...
<tr><td align="center">Victim2<td align="center">15<td align="center">17<td align="center">14<td align="center">19<td align="center">10<td align="center">11<td align="center">...
<tr><td align="center">...<td align="center">...<td align="center">...<td align="center">...<td align="center">...<td align="center">...<td align="center">...<td align="center">...
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

<br>

<a id="sec4-2"></a>

#### Y-STR

<br>

<a id="sec4-3"></a>

#### mtDNA
