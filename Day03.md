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

# Part 2

``` r
find_max_many <- function(vec, n){
  maxlocs <- rep(NA, n)
  maxprior <- 0
  lv <- length(vec)
  for (i in seq(1, n)){
    subvec <- vec[seq(maxprior+1, lv-(n-i))]
    maxlocs[i] <- maxprior <- which.max(subvec) + maxprior
  }
  sum(10^(seq(n-1, 0))*vec[maxlocs])
}

dat2 <- 
  dat |>
  mutate(
    maxjolt = map_dbl(inpvec, ~find_max_many(.x, 12))
  )

sum(dat2$maxjolt) |> print(digits=20)
```

    [1] 172787336861064
