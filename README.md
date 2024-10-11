# RelSearch

KinBN is an R-shiny package (GNU General Public License v3.0) for disaster victim identification. RelSearch can deal with profiles of the autosomal STR, Y-STR, and mtDNA in parallel. For the autosomal STR, the LR values of all victim-reference pairs are calculated considering the allelic drop-out and the mutational events for assumed parent-child relationship. For the Y-STR, RelSearch investigates the number of mismatched loci and the total mismatched steps between victim and reference profiles. For the mtDNA, RelSearch investigates the number of mismatched nucleotides between the victim and the reference sequences. To support paternal or maternal lineages, some mismatches can be allowed by assuming allelic drop-out and mutations.

## Initial setup

1. Ensure that R (>= 4.4.1) is installed. It is available from the [R Development Core Team website](http://www.R-project.org).

2. Begin an R session.

3. Execute the following command in R to install required packages.

``` r
install.packages('https://github.com/manabe0322/RelSearch/releases/download/ v1.0.0/RelSearch_1.0.0.zip', repos = NULL, type = 'win.binary')
install.packages(c("shiny", "data.table", "dplyr", "DT", "magrittr", "pedtools", "Rcpp", "ribd", "shinyFeedback", "shinyjs", "shinythemes", "waiter"))
```

4. Execute the following commands in R to launch the software interface.

``` r
library(RelSearch)
RelSearch()
```
