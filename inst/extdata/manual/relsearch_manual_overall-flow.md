## Contents

* [Load files](#sec2-1)
  
* [Check settings](#sec2-2)

* [Perform analysis](#sec2-3)

* [Check results](#sec2-4)
  
* [Save project](#sec2-5)

---

<a id="sec2-1"></a>

<br>

### Load files

Load the following files from each **"Browse..."** button.

<br>

<u>STR</u>

* [Victim database](#sec3-1)

* [Reference database](#sec3-2)

* [Allele frequencies](#sec3-3)

<br>

<u>Y-STR</u>

* [Victim database](#sec3-4)

* [Reference database](#sec3-5)

<br>

<u>mtDNA</u>

* [Victim database](#sec3-6)

* [Reference database](#sec3-7)

<br>

**Note**

* Example files can be downloaded from the **"Example files"** tab.

* Loaded data can be checked from the **"Database"** tab.

* Large data size may cause memory overflow.

<a id="sec2-2"></a>

<br>

### Check settings

Check the following settings from the **"Tools"** tab.

* [Criteria](#sec4-1)

* [Relationships](#sec4-2)

* [Mutation rates](#sec4-3)

* [Minimum allele frequency](#sec4-4)

<a id="sec2-3"></a>

<br>

### Perform analysis

Click the **"Analysis"** button in the "Load" tab. Then, the following analyses will be started.

<br>

<u>STR analysis</u>

* The likelihood ratios of each victim-reference pair are calculated considering mutation and drop-out.

* Click [here](#sec5-1) for more information.

<br>

<u>Y-STR analysis</u>

* The number of mismatches and the mutational steps in each victim-reference pair are calculated.

* Click [here](#sec5-2) for more information.

<br>

<u>mtDNA analysis</u>

* The number of mismatches and the total shared lengths in each victim-reference pair are calculated.

* Click [here](#sec5-3) for more information.

<a id="sec2-4"></a>

<br>

### Check results

<br>

<img src="fig_summary.jpg" width="80%">

<br>

<br>

<u>1. Summary data.</u>

* <span style="background-color:#e0ffe0">Green-shaded row</span>  : The relationship of the victim-reference pair is identified.

* <span style="background-color:#ffffe0">Yellow-shaded row</span> : The victim-reference pair is one of the candidate relationship and there are other candidate pairs including at least victim or reference.

<u>2. Change displayed data.</u>

* <span style="background-color:#e0ffff">Default display</span> : Show data that satisfies at least one of the criteria for STR, Y-STR, and mtDNA.

* <span style="background-color:#e0ffe0">Identified pair</span> : Select green-shaded rows from the default displayed data.

* <span style="background-color:#ffffe0">Multiple candidates</span> : Select yellow-shaded rows from the default displayed data.

* <span style="background-color:#e0e0ff">Paternal lineages</span> : Select data that supports the paternal lineage from the default displayed data.

* <span style="background-color:#ffe0e0">Maternal lineages</span> : Select data that supports the maternal lineage from the default displayed data.

* <span style="background-color:#eeeeee">Minimum LR displayed</span> : Select data with the LR greater than the set value from the default displayed data.

<u>3. Show the selected data in detail.</u>

* Select a row in the summary table and press the "Selected data in detail".

<u>4. Show the analysis conditions.</u>

<a id="sec2-5"></a>

<br>

### Save project

1. Go to **Project > Save project**.

2. Enter the project name.

3. Press the **"Save as"** button.

<br>

**Note**

* The saved project can be loaded from **Project > Load project**.

<br>

<div align="right"><a href="#">To Top</a></div>

<br>
