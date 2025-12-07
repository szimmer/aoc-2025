# AOC 2025 Day 5


``` r
library(readr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringr)
library(purrr)
```

``` r
dat_in <- read_lines(here::here("Inputs", glue::glue("{if_else(params$test, 'test-', '')}input-day{params$day}.txt")))

ing_ids <- dat_in |> str_subset("\\d+-\\d+") |> str_split("-") |> map(as.numeric) 
avail_ings <- dat_in |> str_subset("^\\d+$") |> as.numeric()
```

# Part 1

``` r
check_ingredient <- function(x){
  any(map_lgl(ing_ids, ~between(x, .x[1], .x[2])))
}
fresh <- avail_ings |> map_lgl(check_ingredient)
sum(fresh)
```

    [1] 756
