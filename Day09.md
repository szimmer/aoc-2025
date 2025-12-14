# AOC 2025 Day 9


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
library(future)
library(furrr)
```

``` r
dat_in <- read_csv(here::here("Inputs", glue::glue("{if_else(params$test, 'test-', '')}input-day{params$day}.txt")), col_names = c("X", "Y"), col_types="ii")

dat_mat <- dat_in |>
  as.matrix()
```

# Part 1

``` r
combos <- combn(nrow(dat_in), 2)
calc_area <- function(i){
  coords <- dat_mat[combos[,i],]
  abs(prod(coords[1,]-coords[2,]+1))
}

areas <- map_dbl(1:ncol(combos), calc_area)

max(areas)
```

    [1] 4729332959

# Part 2

``` r
# Identify red and green borders
# Red is 1, Green is 2

make_edge_border <- function(i){
  curcoord <- dat_mat[i,]
  if (i==1){
    priorcoord <- dat_mat[nrow(dat_mat), ]
  } else{
    priorcoord <- dat_mat[i-1, ]
  }
  
  redtiles <- c(curcoord, 1) # red
  
  # edge between corners as green
  if (priorcoord[1]==curcoord[1]){
    fillgreen_w_corner <- seq(priorcoord[2], curcoord[2])
    fillgreen <- fillgreen_w_corner[-c(1, length(fillgreen_w_corner))]
    greentiles <- matrix(NA, ncol=3, nrow=length(fillgreen))
    greentiles[,1] <- priorcoord[1]
    greentiles[,2] <- fillgreen
    greentiles[,3] <- 2
  } else{
    fillgreen_w_corner <- seq(priorcoord[1], curcoord[1])
    fillgreen <- fillgreen_w_corner[-c(1, length(fillgreen_w_corner))]
    greentiles <- matrix(NA, ncol=3, nrow=length(fillgreen))
    greentiles[,2] <- priorcoord[2]
    greentiles[,1] <- fillgreen
    greentiles[,3] <- 2
  }
  
  out <- rbind(redtiles, greentiles)
  row.names(out) <- NULL
  return(out)
  
}

list_of_tiles <- map(seq_len(nrow(dat_mat)), make_edge_border)
tiles_locations <-  do.call(rbind, list_of_tiles)

rows <- sort(unique(tiles_locations[, 1])) |>
  set_names()

if (params$test){
  layout <- matrix(NA, nrow=max(dat_mat[,1]), ncol=max(dat_mat[,2]))
  for (i in seq_len(nrow(tiles_locations))){
    if (tiles_locations[i, 3]==1){
      layout[tiles_locations[i,1], tiles_locations[i, 2]] <- "R"
    } else{
      layout[tiles_locations[i,1], tiles_locations[i, 2]] <- "G"
    }
    
  }
  print(layout)
} else{
  rm(list_of_tiles)
}
```

``` r
# Fill in the green

fill_green <- function(rowi){
  # if (rowi%%1000==0){
  #   cat("Row: ", rowi, "\n")
  # }
  colsi <- tiles_locations[tiles_locations[,1]==rowi, 2]
  range(colsi)
  
}

plan(multisession, workers = 4)

list_of_tiles_fill <-  future_map(rows, fill_green, .progress = TRUE)
tiles_locations_filler <-  do.call(rbind, list_of_tiles_fill)
tile_loc_summary <- cbind(row.names(tiles_locations_filler) |> as.numeric(), tiles_locations_filler) 
colnames(tile_loc_summary) <- c("row", "colstart", "colend")
tile_loc_df <- tile_loc_summary |> as_tibble()

if (params$test){
  layout2 <- matrix(NA, nrow=max(dat_mat[,1]), ncol=max(dat_mat[,2]))
  for (i in seq_len(nrow(tile_loc_summary))){
    layout2[tile_loc_summary[i,1], tile_loc_summary[i, 2]:tile_loc_summary[i, 3]] <- "X"
  }
  print(layout)
  print(layout2)
} else{
  rm(list_of_tiles_fill, list_of_tiles_fill, list_of_tiles_fill, rows)
}
```

    Warning in rm(list_of_tiles_fill, list_of_tiles_fill, list_of_tiles_fill, :
    object 'list_of_tiles_fill' not found
    Warning in rm(list_of_tiles_fill, list_of_tiles_fill, list_of_tiles_fill, :
    object 'list_of_tiles_fill' not found

``` r
# Calculate areas

calc_area_colored <- function(i){
  
  coords <- dat_mat[combos[,i],]
  rect <-
    tibble(
      row=seq(coords[1,1], coords[2,1]),
      colstart_rect=min(c(coords[1, 2], coords[2, 2])),
      colend_rect=max(c(coords[1, 2], coords[2, 2]))
    ) |>
    left_join(tile_loc_df, by="row") |>
    replace_na(list(colstart=0, colend=0)) |>
    mutate(
      contained=colstart_rect >=colstart & colend_rect <= colend
    ) 
  
  
  if (all(pull(rect, contained))){
    return(calc_area(i))
  } else{
    return(NA)
  }
}

check_order <- rev(order(areas))

for (i in check_order){
  check_max <- calc_area_colored(i)
  if (!is.na(check_max)){
    max_colored_area <- check_max
    break
  }
}

which(check_order==i)
```

    [1] 266

``` r
length(check_order)
```

    [1] 122760

``` r
max_colored_area
```

    [1] 4615465514
