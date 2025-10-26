Assignment-B1-STAT545B
================

## Function

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

    # check if the input data is a data frame or tibble
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
            # calculate the mean value for numeric columns
            across(where(is.numeric), ~mean(.x), .names="mean_{.col}"),
            # calculate the number of unique values for categorical columns
            across(where(is.factor), ~n_distinct(.x), .names = "num_of_unique_{.col}"),
            n = n(),                    # calculate the row number
        )
    
    return(summary_info)
}
```

## Examples

Below shows the examples of how to use the function with the dataset
`penguins`. The first parameter is the data frame or table, and the
others are the columns in the data that you would like to group by with.

``` r
summary_info = group_summary(penguins, species, island)

# set n and width to Inf to show the full result
print(summary_info, n = Inf, width = Inf)
```

    ## # A tibble: 5 x 9
    ## # Groups:   species [3]
    ##   species   island    mean_bill_len mean_bill_dep mean_flipper_len
    ##   <fct>     <fct>             <dbl>         <dbl>            <dbl>
    ## 1 Adelie    Biscoe             39.0          18.4             189.
    ## 2 Adelie    Dream              38.5          18.2             190.
    ## 3 Adelie    Torgersen          39.0          18.5             192.
    ## 4 Chinstrap Dream              48.8          18.4             196.
    ## 5 Gentoo    Biscoe             47.6          15.0             217.
    ##   mean_body_mass mean_year num_of_unique_sex     n
    ##            <dbl>     <dbl>             <int> <int>
    ## 1          3710.     2008.                 2    44
    ## 2          3701.     2008.                 2    55
    ## 3          3709.     2008.                 2    47
    ## 4          3733.     2008.                 2    68
    ## 5          5092.     2008.                 2   119

The following example shows that if the first input argument is not a
data frame, the function will return an error with an error message.

``` r
penguin = c(1, 2, 3)

summary_info = group_summary(penguin, spe)
```

    ## Error in group_summary(penguin, spe): Input must be a data frame or tibble.

The third example shows if the input column doesn’t exist in the
dataset, the function will return an error with a corresponding error
message.

``` r
summary_info = group_summary(penguins, spe, island)
```

    ## Error in group_summary(penguins, spe, island): Column spe does NOT exist in the dataset

The last example shows that if there is no other columns arguments
specified, the function will apply the summarise to the whole table.

``` r
print(group_summary(penguins), n=Inf, width=Inf)
```

    ## # A tibble: 1 x 9
    ##   mean_bill_len mean_bill_dep mean_flipper_len mean_body_mass mean_year
    ##           <dbl>         <dbl>            <dbl>          <dbl>     <dbl>
    ## 1          44.0          17.2             201.          4207.     2008.
    ##   num_of_unique_species num_of_unique_island num_of_unique_sex     n
    ##                   <int>                <int>             <int> <int>
    ## 1                     3                    3                 2   333

# Test the Function

Use `testthat` package to test the `group_summary` function.

``` r
test_that("group_summary function works well for different kinds of input", {
    # example 1:
    penguins_removed_na <- penguins %>% drop_na()
    answer <- tibble(
        mean_bill_len = mean(penguins_removed_na$bill_len),
        mean_bill_dep = mean(penguins_removed_na$bill_dep),
        mean_flipper_len = mean(penguins_removed_na$flipper_len),
        mean_body_mass = mean(penguins_removed_na$body_mass),
        mean_year = mean(penguins_removed_na$year),
        num_of_unique_species = length(unique(penguins_removed_na$species)),
        num_of_unique_island = length(unique(penguins_removed_na$island)),
        num_of_unique_sex = length(unique(penguins_removed_na$sex)),
        n = nrow(penguins_removed_na)
    )
    print(answer, n=Inf, width=Inf)
    expect_equal(group_summary(penguins), answer)

    # example 2: test that the function runs into an error when the input is not
    # a data frame or tibble
    vec = c(1, 2, 3)
    expect_error(group_summary(vec, species))

    # example 3: test that the function runs into an error when the input column
    # does not exist in the dataset
    expect_error(group_summary(penguins, specie))
})
```

    ## # A tibble: 1 x 9
    ##   mean_bill_len mean_bill_dep mean_flipper_len mean_body_mass mean_year
    ##           <dbl>         <dbl>            <dbl>          <dbl>     <dbl>
    ## 1          44.0          17.2             201.          4207.     2008.
    ##   num_of_unique_species num_of_unique_island num_of_unique_sex     n
    ##                   <int>                <int>             <int> <int>
    ## 1                     3                    3                 2   333
    ## Test passed
