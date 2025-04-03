# RelSearch

## Initial setup

1. Ensure that R (>= 4.4.0) is installed. It is available from the [R Development Core Team website](http://www.R-project.org).

2. Begin an R session.

3. Execute the following command in R to install required packages.

``` r
install.packages('https://github.com/manabe0322/RelSearch/releases/download/v0.21.0/RelSearch_0.21.0.zip', repos = NULL, type = 'win.binary')
install.packages(c("shiny", "data.table", "dplyr", "DT", "magrittr", "pedtools", "Rcpp", "ribd", "shinyFeedback", "shinyjs", "shinythemes", "waiter"))
```

4. Execute the following commands in R to launch the software interface.

``` r
library(RelSearch)
RelSearch()
```
