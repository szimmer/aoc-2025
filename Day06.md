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

# Part 2

``` r
starts <- which(str_split_1(dat_in[length(dat_in)], "") %in% c("+", "*"))
ends <- c(starts[-1]-1, str_length(dat_in[1]))
widths <- ends-starts+1
names(widths) <- str_c("X", 1:length(widths))

dat_align <- 
  cbind(dat_in[-length(dat_in)]) |>
  as_tibble() |>
  separate_wider_position(cols=V1, widths=widths) |>
  mutate(
    across(all_of(names(widths)[-length(widths)]), ~str_sub(.x, 1, str_length(.x)-1))
    )
```

    Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    `.name_repair` is omitted as of tibble 2.0.0.
    ℹ Using compatibility `.name_repair`.

``` r
make_new_nums <- function(x){
  max_len <- max(str_length(x))
  1:max_len |>
    map_dbl(~str_sub(x, .x, .x) |> str_c(collapse="") |> as.numeric() )
}

new_nums <- dat_align |> map(make_new_nums)

1:length(dat_op) |>
  map_dbl(~domath(new_nums[[.x]], dat_op[.x])) |>
  sum() |>
  print(digits=20)
```

    [1] 12542543681221
