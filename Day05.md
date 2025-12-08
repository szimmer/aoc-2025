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

# Part 2

``` r
starts <- map_dbl(ing_ids, first)
ends <- map_dbl(ing_ids, last)
reord <- order(starts)
starts <- starts[reord]
ends <- ends[reord]

ranges <- NULL
currange <- c(starts[1], ends[1])
for (i in seq(2, length(starts))){

  if (any(between(currange, starts[i], ends[i]))){
    # overlapping ranges
    currange[2] <- max(currange[2], ends[i])
    if (i==length(starts)){
      ranges <- rbind(ranges, currange )
    }
  } else{
    # non-overlapping ranges
    ranges <- rbind(ranges, currange )
    currange <- c(starts[i], ends[i])
  }
}

sum(ranges[,2]-ranges[,1]+1) |> print(digits=20)
```

    [1] 366243422188711
