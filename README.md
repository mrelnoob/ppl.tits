
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {ppl.tits} package

<!-- badges: start -->
<!-- badges: end -->

The goal of {ppl.tits} package is to encapsulate all the `R` code used
to prepare and analyse the data for the *Tits reproduction study* of the
PubPrivLands project. The data generated by the functions of this
package are progressively made accessible as the package is developed.
To access data or functions, you need first to install this package.  
  

## How to install this package?

You can install the development version of {ppl.tits} with the
`{devtools}` package:

``` r
install.packages("devtools") # To install the {devtools} package.
devtools::install_github("mrelnoob/ppl.tits") # To install the {ppl.tits} package.
```

## Where can I find the data?

If you want to access the **raw** datasets used in this package, you
have to download them from the *mydata/* folder on the [{ppl.tits}
project’s page](https://github.com/mrelnoob/ppl.tits) of my Github
account. You can also contact me directly through:
<francois-marie.martin@univ-fcomte.fr>

To load the latest version of the **cleaned dataset** directly in your
`R` session, you need to call the `tdata_upD_final()` function as in:

``` r
tits_data <- ppl.tits::tdata_upD_final()$final_dataset
```

For access to other datasets (e.g. raw data, working datasets), please
refer to the functions documentation pages:

``` r
help(package=ppl.tits, try.all.packages = TRUE) # To go to the package's index page allowing
# access to all functions' documentation pages (help pages). Alternatively, you can go to a 
# function's help page directly by using the command `?myfunction` as in:
?tdata_upD_final
```

## Where can I find *progress reports*?

You can find **HTML reports** detailing most data processing and
analyses performed in this study in the *output/texts/* folder of the
{ppl.tits} project. To access them, you need to download a copy of the
project’s folder by clicking the green **Code** button (and then
**Download ZIP**) on the [{ppl.tits} project’s
page](https://github.com/mrelnoob/ppl.tits).

## Where can I find the Random Forest object from the *local habitat quality* model?

This `randomForest` object is generated and exported by the
`local_quality_model()` function and can be accessed as follows:

``` r
ppl.tits::local_quality_model()$rf4pm
```

------------------------------------------------------------------------

TO BE CONTINUED…(ongoing development)
