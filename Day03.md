# AOC 2025 Day 3


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
dat_in <- read_csv(here::here("Inputs", glue::glue("{if_else(params$test, 'test-', '')}input-day{params$day}.txt")), col_names="inp", col_types=cols(inp=col_character()))

dat <- dat_in |>
  mutate(
    ID=row_number(),
    inpvec=str_split(inp, "") |> map(as.integer)
  )
```

# Part 1

``` r
find_max_pair <- function(vec){
  n <- length(vec)
  maxstart <- which.max(vec[-n])
  maxsecond <- which.max(vec[seq(maxstart+1, n)]) + maxstart
  vec[maxstart]*10 + vec[maxsecond]
}

dat1 <- 
  dat |>
  mutate(
    maxjolt = map_dbl(inpvec, find_max_pair)
  )

sum(dat1$maxjolt)
```

    [1] 17359
