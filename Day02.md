# AOC 2025 Day 2


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
dat_in <- read_csv(here::here("Inputs", glue::glue("{if_else(params$test, 'test-', '')}input-day{params$day}.txt")), col_names = FALSE)
```

    Rows: 1 Columns: 31
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (31): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, ...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dat <- dat_in |>
  pivot_longer(cols=everything()) |>
  select(-name) |>
  separate_wider_delim(value, names=c("X1", "X2"), delim="-") |>
  mutate(
    across(starts_with("X"), as.numeric),
    rng=map2(X1, X2, seq)) |>
  unnest(rng) |>
  mutate(
    rng_char=as.character(rng),
  ) |>
  select(Value=rng_char, Val_num=rng)
```

# Part 1

``` r
dat |>
  mutate(
    RepeatVal=str_detect(Value, "^(\\d+)\\1$")
  ) |>
  filter(RepeatVal) |>
  pull(Val_num) |>
  sum()    
```

    [1] 15873079081

# Part 2

``` r
dat |>
  mutate(
    RepeatVal=str_detect(Value, "^(\\d+)(\\1)+$")
  ) |>
  filter(RepeatVal) |>
  pull(Val_num) |>
  sum()    
```

    [1] 22617871034
