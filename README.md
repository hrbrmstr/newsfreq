newsfreq is a package that provides access to the newsfreq.com API, which enables searching media sources for keywords in news articles.

The following functions are implemented:

-   `news_search` - search for articles

The following data sets are included:

### News

-   Version `0.1` released

### Installation

``` r
devtools::install_github("hrbrmstr/newsfreq")
```

### Usage

``` r
library(newsfreq)
```

    ## Loading required package: magrittr

``` r
# current verison
packageVersion("newsfreq")
```

    ## [1] '0.1'

### Test Results

``` r
library(newsfreq)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following objects are masked from 'package:magrittr':
    ## 
    ##     equals, is_less_than, not

``` r
date()
```

    ## [1] "Sat Jan 31 10:50:04 2015"

``` r
test_dir("tests/")
```

    ## basic functionality :
