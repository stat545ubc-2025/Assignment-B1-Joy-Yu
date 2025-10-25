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
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(roxygen2))
suppressPackageStartupMessages(library(testthat))
```

Here’s the function:

``` r
#' Summarise the dataset by group
#' 
#' For each group, calculate the number of rows, numeric columns' mean value
#' and categorical columns' number of unique values
#' 
#' @param data dataset to summarise
#' @param ... ellipses, column(s) to group by
#' 
#' @return the summary result
group_summary <- function(data, ...) {

    if (!is.data.frame(data)){
        stop("Input must be a data frame or tibble.")
    }

    vars <- ensyms(...)                 # get the arguments as symbols
    for (var in vars) {
        # check if the arguments are columns in data
        if (!(as_string(var) %in% names(data))) {
            stop(paste("Column", as_string(var), "does NOT exist in the dataset"))
        }
    }

    summary_info <- data %>%
        drop_na() %>%                   # drop rows with missing data
        group_by(...) %>%               # group by the input column(s)
        summarise(
            n = n(),                    # calculate the row number
            # calculate the mean value for numeric columns
            across(where(is.numeric), ~mean(.x), .names="mean_{.col}"),
            # calculate the number of unique values for categorical columns
            across(where(is.factor), ~n_distinct(.x), .names = "num_of_unique_{.col}")
        )
    
    return(summary_info)
}
```

Below shows the examples of how to use the function with the dataset
`penguins`.

``` r
summary_info = group_summary(penguins, species, island)

# set n and width to Inf to show the full result
print(summary_info, n = Inf, width = Inf)
```

    ## # A tibble: 5 x 10
    ## # Groups:   species [3]
    ##   species   island        n mean_bill_len mean_bill_dep mean_flipper_len
    ##   <fct>     <fct>     <int>         <dbl>         <dbl>            <dbl>
    ## 1 Adelie    Biscoe       44          39.0          18.4             189.
    ## 2 Adelie    Dream        55          38.5          18.2             190.
    ## 3 Adelie    Torgersen    47          39.0          18.5             192.
    ## 4 Chinstrap Dream        68          48.8          18.4             196.
    ## 5 Gentoo    Biscoe      119          47.6          15.0             217.
    ##   mean_body_mass mean_year mean_n num_of_unique_sex
    ##            <dbl>     <dbl>  <dbl>             <int>
    ## 1          3710.     2008.     44                 2
    ## 2          3701.     2008.     55                 2
    ## 3          3709.     2008.     47                 2
    ## 4          3733.     2008.     68                 2
    ## 5          5092.     2008.    119                 2

The following example shows if the input column doesn’t exist in the
dataset, the function will return an error with a corresponding error
message.

``` r
summary_info = group_summary(penguins, spe, island)
```

    ## Error in group_summary(penguins, spe, island): Column spe does NOT exist in the dataset

The third example shows that if the first input argument is not a data
frame, the function will return an error with an error message.

``` r
penguin = c(1, 2, 3)

summary_info = group_summary(penguin, spe)
```

    ## Error in group_summary(penguin, spe): Input must be a data frame or tibble.
