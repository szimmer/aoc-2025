# AOC 2025 Day 6


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

dat_remsp_in <- dat_in |> str_squish() |> str_trim()

dat_op <- dat_remsp_in[length(dat_remsp_in)] |> str_split_1("\\s")

dat_nums <- dat_remsp_in[-length(dat_remsp_in)] |> str_split("\\s", simplify = TRUE) |> apply(2, as.numeric)
```

# Part 1

``` r
domath <- function(nums, op){
  if (op=="+"){
    return(sum(nums))
  } else{
    return(prod(nums))
  }
}

1:length(dat_op) |>
  map_dbl(~domath(dat_nums[, .x], dat_op[.x])) |>
  sum() |>
  print(digits=20)
```

    [1] 6635273135233
