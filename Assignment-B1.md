Assignment-B1-STAT545B
================

When working on the mini data analysis, since the dataset I used
includes lots of data, I had to narrow down the range that I looked
into. When deciding which subset to look into, the summary information
of different groups is a key factor. Thus, I think it would be helpful
to have a summary function that can be used repeatedly.

``` r
# import libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(roxygen2))
suppressPackageStartupMessages(library(testthat))
```

``` r
group_summary <- function(data, ...) {
    summary_info <- data %>%
        drop_na() %>%
        group_by(...) %>%
        summarise(
            n = n(),
            across(where(is.numeric), ~mean(.x), .names="mean_{.col}"),
            across(where(is.factor), ~n_distinct(.x), .names = "num_of_unique_{.col}")
        )
    
    return(summary_info)
}
```

``` r
summary_info = group_summary(penguins, species)

print(summary_info, n = Inf, width = Inf)
```

    ## # A tibble: 3 x 10
    ##   species       n mean_bill_len mean_bill_dep mean_flipper_len mean_body_mass
    ##   <fct>     <int>         <dbl>         <dbl>            <dbl>          <dbl>
    ## 1 Adelie      146          38.8          18.3             190.          3706.
    ## 2 Chinstrap    68          48.8          18.4             196.          3733.
    ## 3 Gentoo      119          47.6          15.0             217.          5092.
    ##   mean_year mean_n num_of_unique_island num_of_unique_sex
    ##       <dbl>  <dbl>                <int>             <int>
    ## 1     2008.    146                    3                 2
    ## 2     2008.     68                    1                 2
    ## 3     2008.    119                    1                 2
