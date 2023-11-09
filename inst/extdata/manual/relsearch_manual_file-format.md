## Contents

* [STR: Victim database](#sec3-1)

* [STR: Reference database](#sec3-2)

* [STR: Allele frequencies](#sec3-3)

* [Y-STR: Victim database](#sec3-4)

* [Y-STR: Reference database](#sec3-5)

* [mtDNA: Victim database](#sec3-6)

* [mtDNA: Reference database](#sec3-7)

---

<a id="sec3-1"></a>

<br>

### STR: Victim database

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
    <td bgcolor=whitesmoke>D3S1358</td>
    <td bgcolor=whitesmoke>D3S1358</td>
    <td bgcolor=whitesmoke>vWA</td>
    <td bgcolor=whitesmoke>vWA</td>
    <td bgcolor=whitesmoke>D16S539</td>
    <td bgcolor=whitesmoke>D16S539</td>
    <td bgcolor=whitesmoke>CSF1PO</td>
    <td bgcolor=whitesmoke>CSF1PO</td>
    <td bgcolor=whitesmoke>TPOX</td>
    <td bgcolor=whitesmoke>TPOX</td>
  </tr>
  <tr>
    <td>Victim1</td>
    <td>17</td>
    <td>17</td>
    <td>17</td>
    <td></td>
    <td>11</td>
    <td></td>
    <td>12</td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>Victim2</td>
    <td>17</td>
    <td></td>
    <td>18</td>
    <td>18</td>
    <td></td>
    <td></td>
    <td>10</td>
    <td>11</td>
    <td>8</td>
    <td></td>
  </tr>
  <tr>
    <td>Victim3</td>
    <td>15</td>
    <td>17</td>
    <td>17</td>
    <td>18</td>
    <td>10</td>
    <td>10</td>
    <td>10</td>
    <td>12</td>
    <td>8</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Victim4</td>
    <td>15</td>
    <td>18</td>
    <td>15</td>
    <td>18</td>
    <td>9</td>
    <td>12</td>
    <td>11</td>
    <td>12</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Victim5</td>
    <td>15</td>
    <td>15</td>
    <td>18</td>
    <td></td>
    <td>9</td>
    <td>9</td>
    <td>10</td>
    <td></td>
    <td>9</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Victim6</td>
    <td>16</td>
    <td>16</td>
    <td>17</td>
    <td></td>
    <td></td>
    <td></td>
    <td>10</td>
    <td>13</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Victim7</td>
    <td>14</td>
    <td>15</td>
    <td>17</td>
    <td>18</td>
    <td>12</td>
    <td></td>
    <td></td>
    <td></td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Victim8</td>
    <td>16</td>
    <td>18</td>
    <td>17</td>
    <td>18</td>
    <td>11</td>
    <td></td>
    <td>9</td>
    <td>12</td>
    <td>8</td>
    <td></td>
  </tr>
  <tr>
    <td>Victim9</td>
    <td>15</td>
    <td>15</td>
    <td>16</td>
    <td>18</td>
    <td>11</td>
    <td>11</td>
    <td>12</td>
    <td>12</td>
    <td>8</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Victim10</td>
    <td>15</td>
    <td>16</td>
    <td>18</td>
    <td>19</td>
    <td>9</td>
    <td>10</td>
    <td>12</td>
    <td>12</td>
    <td>8</td>
    <td>11</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName" and columns for each marker (two columns in each).

* The marker with two empty cells (e.g., TPOX of sample "Victim1" in the above table) is ignored when calculating the likelihood ratio.

* The marker with one empty cell (e.g., vWA of sample "Victim1" in the above table) can be regarded as both homozygote (i.e., no drop-out) and heterozygote with drop-out of one allele when calculating the likelihood ratio.

<a id="sec3-2"></a>

<br>

### STR: Reference database

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Relationship</td>
    <td bgcolor=whitesmoke>D3S1358</td>
    <td bgcolor=whitesmoke>D3S1358</td>
    <td bgcolor=whitesmoke>vWA</td>
    <td bgcolor=whitesmoke>vWA</td>
    <td bgcolor=whitesmoke>D16S539</td>
    <td bgcolor=whitesmoke>D16S539</td>
    <td bgcolor=whitesmoke>CSF1PO</td>
    <td bgcolor=whitesmoke>CSF1PO</td>
    <td bgcolor=whitesmoke>TPOX</td>
    <td bgcolor=whitesmoke>TPOX</td>
  </tr>
  <tr>
    <td>Reference1</td>
    <td>parent-child</td>
    <td>17</td>
    <td>17</td>
    <td>14</td>
    <td>17</td>
    <td>10</td>
    <td>10</td>
    <td>12</td>
    <td>12</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Reference2</td>
    <td>parent-child</td>
    <td>17</td>
    <td>18</td>
    <td>17</td>
    <td>18</td>
    <td>12</td>
    <td>13</td>
    <td>10</td>
    <td>11</td>
    <td>11</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference3</td>
    <td>parent-child</td>
    <td>15</td>
    <td>16</td>
    <td>17</td>
    <td>18</td>
    <td>9</td>
    <td>10</td>
    <td>11</td>
    <td>12</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Reference4</td>
    <td>parent-child</td>
    <td>15</td>
    <td>16</td>
    <td>15</td>
    <td>18</td>
    <td>9</td>
    <td>9</td>
    <td>10</td>
    <td>12</td>
    <td>9</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference5</td>
    <td>sibling</td>
    <td>15</td>
    <td>16</td>
    <td>15</td>
    <td>18</td>
    <td>9</td>
    <td>9</td>
    <td>10</td>
    <td>12</td>
    <td>9</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference6</td>
    <td>sibling</td>
    <td>16</td>
    <td>17</td>
    <td>17</td>
    <td>17</td>
    <td>10</td>
    <td>12</td>
    <td>13</td>
    <td>13</td>
    <td>8</td>
    <td>11</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>sibling</td>
    <td>16</td>
    <td>17</td>
    <td>14</td>
    <td>18</td>
    <td>9</td>
    <td>9</td>
    <td>12</td>
    <td>15</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>parent-child</td>
    <td>16</td>
    <td>17</td>
    <td>14</td>
    <td>18</td>
    <td>9</td>
    <td>9</td>
    <td>12</td>
    <td>15</td>
    <td>8</td>
    <td>8</td>
  </tr>
  <tr>
    <td>Reference8</td>
    <td>sibling</td>
    <td>15</td>
    <td>16</td>
    <td>17</td>
    <td>18</td>
    <td>9</td>
    <td>12</td>
    <td>11</td>
    <td>12</td>
    <td>8</td>
    <td>9</td>
  </tr>
  <tr>
    <td>Reference9</td>
    <td>uncle-nephew</td>
    <td>15</td>
    <td>15</td>
    <td>14</td>
    <td>16</td>
    <td>10</td>
    <td>11</td>
    <td>11</td>
    <td>12</td>
    <td>8</td>
    <td>9</td>
  </tr>
  <tr>
    <td>Reference10</td>
    <td>cousin</td>
    <td>15</td>
    <td>16</td>
    <td>17</td>
    <td></td>
    <td>9</td>
    <td>12</td>
    <td></td>
    <td></td>
    <td>8</td>
    <td>12</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the columns "SampleName", "Relationship", and columns for each marker (two columns in each).

* The relationship of a missing family member should be designated in the column "Relationship". The name of the relationship should be defined in Settings > Relationships.

* If a reference has multiple missing family members, add rows for each relationship of the members (e.g., sibling and parent-child of the sample "Reference7" in the above table).

* The marker with two empty cells (e.g., CSF1PO of sample "Reference10" in the above table) is ignored when calculating the likelihood ratio.

* The marker with one empty cell (e.g., vWA of sample "Reference10" in the above table) can be regarded as both homozygote (i.e., no drop-out) and heterozygote with drop-out of one allele when calculating the likelihood ratio.

<a id="sec3-3"></a>

<br>

### STR: Allele frequencies

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

<a id="sec3-4"></a>

<br>

### Y-STR: Victim database

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>DYS456</td>
    <td bgcolor=whitesmoke>DYS390</td>
    <td bgcolor=whitesmoke>DYS438</td>
    <td bgcolor=whitesmoke>DYS392</td>
    <td bgcolor=whitesmoke>DYS518</td>
    <td bgcolor=whitesmoke>DYS570</td>
    <td bgcolor=whitesmoke>DYS437</td>
    <td bgcolor=whitesmoke>DYS385</td>
  </tr>
  <tr>
    <td>Victim1</td>
    <td>15</td>
    <td>24</td>
    <td>10</td>
    <td>11</td>
    <td>37</td>
    <td>17</td>
    <td>14</td>
    <td>13,17</td>
  </tr>
  <tr>
    <td>Victim2</td>
    <td>15</td>
    <td>22</td>
    <td>13</td>
    <td>13</td>
    <td>38</td>
    <td>19</td>
    <td>14</td>
    <td>10,20</td>
  </tr>
  <tr>
    <td>Victim3</td>
    <td>15</td>
    <td>25</td>
    <td>10</td>
    <td>11</td>
    <td>37</td>
    <td>17</td>
    <td>14</td>
    <td>14</td>
  </tr>
  <tr>
    <td>Victim4</td>
    <td>15</td>
    <td>26</td>
    <td>11</td>
    <td>11</td>
    <td>37</td>
    <td>16</td>
    <td>14</td>
    <td>13</td>
  </tr>
  <tr>
    <td>Victim5</td>
    <td>15</td>
    <td>23</td>
    <td>11</td>
    <td></td>
    <td></td>
    <td>18</td>
    <td></td>
    <td>13,17</td>
  </tr>
  <tr>
    <td>Victim6</td>
    <td></td>
    <td></td>
    <td></td>
    <td>11</td>
    <td>40</td>
    <td></td>
    <td>14</td>
    <td>14</td>
  </tr>
  <tr>
    <td>Victim7</td>
    <td>15</td>
    <td>23</td>
    <td>10</td>
    <td>11</td>
    <td>38</td>
    <td>16</td>
    <td>14</td>
    <td>11,19</td>
  </tr>
  <tr>
    <td>Victim8</td>
    <td>16</td>
    <td>24</td>
    <td>10</td>
    <td>11</td>
    <td>38</td>
    <td>19</td>
    <td>14</td>
    <td>13,15</td>
  </tr>
  <tr>
    <td>Victim9</td>
    <td>14</td>
    <td>24</td>
    <td>11</td>
    <td>14</td>
    <td>37</td>
    <td>18</td>
    <td>14</td>
    <td>13,18</td>
  </tr>
  <tr>
    <td>Victim10</td>
    <td>16</td>
    <td>24</td>
    <td>11</td>
    <td>14</td>
    <td></td>
    <td>16</td>
    <td>15</td>
    <td>13,17</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName" and columns for each marker (one column in each).

* In the marker with more than one allele, each allele must be separated by a comma without any spaces (e.g., DYS385).

* The marker with an empty cell (e.g., DYS456 of sample ‘Victim6’) is ignored for analysis.

<a id="sec3-5"></a>

<br>

### Y-STR: Reference database

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Relationship</td>
    <td bgcolor=whitesmoke>DYS456</td>
    <td bgcolor=whitesmoke>DYS390</td>
    <td bgcolor=whitesmoke>DYS438</td>
    <td bgcolor=whitesmoke>DYS392</td>
    <td bgcolor=whitesmoke>DYS518</td>
    <td bgcolor=whitesmoke>DYS570</td>
    <td bgcolor=whitesmoke>DYS437</td>
    <td bgcolor=whitesmoke>DYS385</td>
  </tr>
  <tr>
    <td>Reference1</td>
    <td>parent-child</td>
    <td>15</td>
    <td>24</td>
    <td>10</td>
    <td>11</td>
    <td>37</td>
    <td>17</td>
    <td>14</td>
    <td>13,17</td>
  </tr>
  <tr>
    <td>Reference2</td>
    <td>parent-child</td>
    <td>15</td>
    <td>22</td>
    <td>13</td>
    <td>13</td>
    <td>38</td>
    <td>19</td>
    <td>14</td>
    <td>10,20</td>
  </tr>
  <tr>
    <td>Reference3</td>
    <td>parent-child</td>
    <td>15</td>
    <td>25</td>
    <td>10</td>
    <td>11</td>
    <td>37</td>
    <td>17</td>
    <td>14</td>
    <td>14,17</td>
  </tr>
  <tr>
    <td>Reference4</td>
    <td>parent-child</td>
    <td>15</td>
    <td>26</td>
    <td>11</td>
    <td>11</td>
    <td>37</td>
    <td>16</td>
    <td>14</td>
    <td>13,16</td>
  </tr>
  <tr>
    <td>Reference5</td>
    <td>sibling</td>
    <td>15</td>
    <td>23</td>
    <td>11</td>
    <td>12</td>
    <td>42</td>
    <td>18</td>
    <td>14</td>
    <td>13,17</td>
  </tr>
  <tr>
    <td>Reference6</td>
    <td>sibling</td>
    <td>16</td>
    <td>26</td>
    <td>10</td>
    <td>11</td>
    <td>40</td>
    <td>20</td>
    <td>14</td>
    <td>14</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>sibling</td>
    <td>15</td>
    <td>23</td>
    <td>10</td>
    <td>11</td>
    <td>38</td>
    <td>16</td>
    <td>14</td>
    <td>11,19</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>parent-child</td>
    <td>15</td>
    <td>23</td>
    <td>10</td>
    <td>11</td>
    <td>38</td>
    <td>16</td>
    <td>14</td>
    <td>11,19</td>
  </tr>
  <tr>
    <td>Reference8</td>
    <td>sibling</td>
    <td>16</td>
    <td>24</td>
    <td>10</td>
    <td>11</td>
    <td>38</td>
    <td>19</td>
    <td>14</td>
    <td>13,15</td>
  </tr>
  <tr>
    <td>Reference9</td>
    <td>uncle-nephew</td>
    <td>14</td>
    <td>24</td>
    <td>11</td>
    <td>14</td>
    <td>37</td>
    <td>18</td>
    <td>14</td>
    <td>13,18</td>
  </tr>
  <tr>
    <td>Reference10</td>
    <td>cousin</td>
    <td>16</td>
    <td>24</td>
    <td>11</td>
    <td>14</td>
    <td></td>
    <td>16</td>
    <td>15</td>
    <td>13,17</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName", "Relationship", and columns for each marker (one column in each).

* The relationship of a missing family member should be designated in the column "Relationship". The name of the relationship should be defined in Settings > Relationships.

* If a reference has multiple missing family members, add rows for each relationship of the members (e.g., sibling and parent-child of the sample "Reference7" in the above table).

* In the marker with more than one allele, each allele must be separated by a comma without any spaces (e.g., DYS385).

* The marker with an empty cell (e.g., DYS518 of sample ‘Reference10’) is ignored for analysis.

<a id="sec3-6"></a>

<br>

### mtDNA: Victim database

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Range</td>
    <td bgcolor=whitesmoke>Haplotype</td>
  </tr>
  <tr>
    <td>Victim1</td>
    <td>73-340 16024-16365</td>
    <td>16183C 16189C 16217C 16311C 73G 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Victim2</td>
    <td>73-340 16024-16365</td>
    <td>16093C 16114A 16223T 16362C 73G 191.1A 194T 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Victim3</td>
    <td>73-167 240-340 16024-16365</td>
    <td>16095T 16189C 16223T 16265C 16274A 16362C 73G 143A 152C 263G 315.1C</td>
  </tr>
  <tr>
    <td>Victim4</td>
    <td>73-265 16024-16284</td>
    <td>16223T 73G 152C 263G</td>
  </tr>
  <tr>
    <td>Victim5</td>
    <td>16117-16365</td>
    <td>16140C 16182C 16183C 16189C 16234T 16243C 16291T</td>
  </tr>
  <tr>
    <td>Victim6</td>
    <td>73-167 16117-16209</td>
    <td>16172C 73G 150T</td>
  </tr>
  <tr>
    <td>Victim7</td>
    <td>73-340 16024-16365</td>
    <td>16172C 16223T 16257A 16261T 73G 150T 263G 309.1C 309.2C 315.1C</td>
  </tr>
  <tr>
    <td>Victim8</td>
    <td>73-340 16024-16365</td>
    <td>16129A 16183C 16189C 16223T 16297C 16298C 16311C 73G 150T 199C 263G 309.1C 309.2C 315.1C</td>
  </tr>
  <tr>
    <td>Victim9</td>
    <td>73-340 16024-16132 16179-16365</td>
    <td>16183C 16189C 16209C 16223T 16324C 73G 207A 263G 284G 309.1C 309.2C 315.1C</td>
  </tr>
  <tr>
    <td>Victim10</td>
    <td>73-265 16024-16209 16266-16365</td>
    <td>16362C 73G 263G</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName", "Range", and "Haplotype".

<a id="sec3-7"></a>

<br>

### mtDNA: Reference database

<br>

**Example**

<table class="fileformat" border="1" width="80%">
  <tr>
    <td bgcolor=whitesmoke>SampleName</td>
    <td bgcolor=whitesmoke>Relationship</td>
    <td bgcolor=whitesmoke>Range</td>
    <td bgcolor=whitesmoke>Haplotype</td>
  </tr>
  <tr>
    <td>Reference1</td>
    <td>parent-child</td>
    <td>16024-16365 73-340</td>
    <td>16183C 16189C 16217C 16311C 73G 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference2</td>
    <td>parent-child</td>
    <td>16024-16365 73-340</td>
    <td>16093C 16114A 16223T 16362C 73G 191.1A 194T 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference3</td>
    <td>parent-child</td>
    <td>16024-16365 73-340</td>
    <td>16095T 16189C 16223T 16265C 16274A 16362C 73G 143A 152C 263G 315.1C</td>
  </tr>
  <tr>
    <td>Reference4</td>
    <td>parent-child</td>
    <td>16024-16365 73-340</td>
    <td>16223T 16319A 16362C 73G 152C 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference5</td>
    <td>sibling</td>
    <td>16024-16365 73-340</td>
    <td>16111T 16140C 16182C 16183C 16189C 16234T 16243C 16291T 73G 131C 195C 204C 263G 309.1C 309.2C 315.1C</td>
  </tr>
  <tr>
    <td>Reference6</td>
    <td>sibling</td>
    <td>16024-16365 73-340</td>
    <td>16172C 16223T 16250T 16257A 16261T 73G 150T 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>sibling</td>
    <td>16024-16365 73-340</td>
    <td>16172C 16189C 16223T 16355T 16362C 73G 150T 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference7</td>
    <td>parent-child</td>
    <td>16024-16365 73-340</td>
    <td>16172C 16189C 16223T 16355T 16362C 73G 150T 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference8</td>
    <td>sibling</td>
    <td>16024-16365 73-340</td>
    <td>16223T 16362C 73G 263G 315.1C</td>
  </tr>
  <tr>
    <td>Reference9</td>
    <td>uncle-nephew</td>
    <td>16024-16365 73-340</td>
    <td>16183G 16223T 16274A 16290T 16319A 16362C 73G 195C 263G 309.1C 315.1C</td>
  </tr>
  <tr>
    <td>Reference10</td>
    <td>cousin</td>
    <td>16024-16365 73-340</td>
    <td>16111T 16140C 16154C 16183C 16189C 16217C 16261T 16274A 73G 263G 315.1C</td>
  </tr>
</table>

<br>

**Note**

* File type: .csv

* This file requires the column "SampleName", "Relationship", "Range", and "Haplotype".

* The relationship of a missing family member should be designated in the column "Relationship". The name of the relationship should be defined in Settings > Relationships.

* If a reference has multiple missing family members, add rows for each relationship of the members (e.g., sibling and parent-child of the sample "Reference7" in the above table).

<br>

<div align="right"><a href="#">To Top</a></div>

<br>
