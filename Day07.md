# AOC 2025 Day 7


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

dat_mat <- dat_in |> str_split("", simplify=TRUE)
```

# Part 1

``` r
cur_beam_loc <- which(dat_mat[1,]=="S")
nsplits <- 0
for (i in 2:nrow(dat_mat)){
  enc_next_row <- dat_mat[i, cur_beam_loc]
  remain_loc <- cur_beam_loc[enc_next_row=="."]
  split_loc <- cur_beam_loc[enc_next_row=="^"]
  cur_beam_loc <- c(remain_loc, split_loc-1, split_loc+1) |> unique() |> sort()
  nsplits <- nsplits + length(split_loc)
}
nsplits
```

    [1] 1658
