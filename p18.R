library(tidyverse)
library(data.table)
library(bit64)

data <-
  read_delim("inputs/18.txt", delim = " ", col_names = c("dir", "steps", "rgb"), col_types = "cdc") %>% 
  mutate(
    rgb = str_extract(rgb, "[^()#]+"),
    dir2 = str_sub(rgb, -1, -1),
    dir2 = case_when(
      dir2 == "0" ~ "R",
      dir2 == "1" ~ "D",
      dir2 == "2" ~ "L",
      dir2 == "3" ~ "U",
      TRUE ~ NA_character_
    ),
    steps2 = str_sub(rgb, 1, 5) %>% paste0("0x", .) %>% strtoi()
  )
  # separate_longer_delim(
  #   cols = data, 
  #   delim = ","
  # )


range <- find_scale(data$dir, data$steps)
grid <- create_path(range, data$dir, data$steps)
for (i in seq_len(nrow(grid)) )
  grid[i, ] <- mark_regions(grid[i, ])
## 47139



range <- find_scale(data$dir, data$steps)
corners <- create_corners(range, data$dir, data$steps)

count_holes(corners)


range2 <- find_scale(data$dir2, data$steps2)
corners2 <- create_corners(range2, data$dir2, data$steps2)

count_holes(corners2)


## correct
173152345887206
create_corners <- function(range, dir, steps) {
  
  x <- 1 - range[4]
  y <- range[5] + range[2]
  
  
  xs <- integer(length(dir))
  ys <- integer(length(dir))
  
  
  for (i in seq_along(dir)) {
    
    if (dir[i] == "R") {
      xs[i] <- x <- x + steps[i]   
      ys[i] <- y 
    } else if (dir[i] == "L") {
      xs[i] <- x <- x - steps[i]   
      ys[i] <- y 
    } else if (dir[i] == "U") {
      xs[i] <- x   
      ys[i] <- y <- y - steps[i] 
    } else {
      xs[i] <- x   
      ys[i] <- y <- y + steps[i] 
    }
  }
  
  data.table(
    x = xs,
    y = ys
  )
  
}

count_holes <- function(moves_dtb) {

  # prepare_moves_dtb  
  
  ## all x,y need to be +ve and work with the arryas
  
  
  get_x_pairs <- function(y_) {
    moves_dtb[y == y_, x]
  }
  count_change_row <- function(state, x_pairs) {
    # browser()
    state2 <-
      tibble(
        state = c(state, x_pairs)
      ) %>% 
      mutate(
        polarity = rep(c(1,-1), n()/2)
      ) %>% 
      arrange(state) %>% 
      mutate(
        c = cumsum(polarity) %>% pmin(1),
      ) %>% 
      filter(
        !(c == 1 & lag(c, default = 0) == 1)
      ) %>% 
      pull(state) %>% 
      update_state(integer())
      
    out2 <- count_state(state2)
    
    # raw_state1[] <- FALSE
    # raw_state2[] <- FALSE
    # 
    # for (i in seq_len(length(state)/2))
    #   raw_state1[state[2*i-1]:state[2*i]] <- TRUE
    # for (i in seq_len(length(x_pairs)/2))
    #   raw_state2[x_pairs[2*i-1]:x_pairs[2*i]] <- TRUE
    # 
    # out <- sum(raw_state1 | raw_state2) %>% as.integer64()
    # 
    # print(out)
    # print(out2)
    # 
    # out
  }
  count_state <- function(state) {
    
    count <- 0
    for (i in seq_len(length(state)/2)) 
      count <- count + state[2*i] - state[2*i-1] + 1
    
    count %>% as.integer64()
    
  }
  update_state <- function(state, x_pairs) {
    state <- c(state, x_pairs) %>% sort()
    state[!vctrs::vec_duplicate_detect(state)]
  }  
  
  moves_dtb <- moves_dtb %>% as.data.table %>% setkey(y, x)
  # raw_state1 <- logical(max(moves_dtb$x))
  # raw_state2 <- logical(max(moves_dtb$x))
  
  state <- integer()
  count <- as.integer64(0)
  ys <- moves_dtb[, y] %>% unique() %>% sort()
  prev_y <- min(ys)
  
  for (i in seq_along(ys)) {
    # browser()
    x_pairs <- get_x_pairs(ys[i])
    count <- count + count_change_row(state, x_pairs)
    count <- count + as.integer64(ys[i] - prev_y - 1) * count_state(state)
    state <- update_state(state, x_pairs)
    prev_y <- ys[i]
    
    if (i %% 10 == 0)
      message(count)
    
  }
  
  count                                   
   
 }   
   
  
  
  



## 47139
which(grid != "-1") %>% length()


create_path <- function(range, dir, steps) {
  
 
  x <- 1 - range[4]
  y <- range[5] + range[2]
  grid <- rep(".", range[5]*range[6]) %>% matrix(ncol = range[6])
  
  prev_sym <- "" 
  first_sym <- dir[1]
  temp_sym <- "*"
  
  for (i in seq_along(dir)) {

    if (dir[i] == "R") {
      if (prev_sym == "U")
        temp_sym <- "F"
      if (prev_sym == "D")
        temp_sym <- "L"
      grid[y, x] <- temp_sym
      dx <- (x+1):(x+steps[i])
      grid[y, dx] <- "-"
      x <- x + steps[i]
      prev_sym <- "R"
    } else if (dir[i] == "L") {
      if (prev_sym == "U")
        temp_sym <- "7"
      if (prev_sym == "D")
        temp_sym <- "J"
      grid[y, x] <- temp_sym
      dx <- (x-1):(x-steps[i])
      grid[y, dx] <- "-"
      x <- x - steps[i]
      prev_sym <- "L"
    } else if (dir[i] == "U") {
      if (prev_sym == "L")
        temp_sym <- "L"
      if (prev_sym == "R")
        temp_sym <- "J"
      grid[y, x] <- temp_sym
      dy <- (y-1):(y-steps[i])   
      grid[dy, x] <- "|"
      y <- y - steps[i]
      prev_sym <- "U"
    } else if (dir[i] == "D") {
      if (prev_sym == "L")
        temp_sym <- "F"
      if (prev_sym == "R")
        temp_sym <- "7"
      grid[y, x] <- temp_sym
      dy <- (y+1):(y+steps[i])   
      grid[dy, x] <- "|"
      y <- y + steps[i]
      prev_sym <- "D"
    }
    
  }
  
  if (first_sym == "R") {
    if (prev_sym == "U")
      temp_sym <- "F"
    if (prev_sym == "D")
      temp_sym <- "L"
    grid[y, x] <- temp_sym
  } else if (first_sym == "L") {
    if (prev_sym == "L")
      temp_sym <- "L"
    if (prev_sym == "R")
      temp_sym <- "J"
    grid[y, x] <- temp_sym
  } else if (first_sym == "U") {
    if (prev_sym == "L")
      temp_sym <- "L"
    if (prev_sym == "R")
      temp_sym <- "J"
    grid[y, x] <- temp_sym
  } else if (first_sym == "D") {
    if (prev_sym == "L")
      temp_sym <- "F"
    if (prev_sym == "R")
      temp_sym <- "7"
    grid[y, x] <- temp_sym
  }
  
  grid
  
}

mark_regions <- function(syms) {
  
  state <- 0L
  marker <- -1L
  region <- integer(length(syms))
  
  for (i in seq_along(syms)) {
    
    pipe <- TRUE
    if (syms[i] == "|")
      state <- 2L
    else if (syms[i] == "-")
      ## do nothing
      TRUE
    else if (syms[i] == "F")
      state <- state + 1L
    else if (syms[i] == "J")
      state <- state + 1L
    else if (syms[i] == "7")
      state <- state - 1L
    else if (syms[i] == "L")
      state <- state - 1L
    else
      pipe <- FALSE
    
    
    if (state %in% c(-2L, 2L)) {
      marker <- -1L * marker
      state <- 0L
    }
    
    if (!pipe)
      region[i] <- marker
    
  }
  
  region
  
}




find_scale <- function(dir, steps) {
  
  s <- c(0,0)
  # smax <- s
  # smin <- s
  
  hmax <- 0
  hmin <- 0
  vmax <- 0
  vmin <- 0
  
  
  for (i in seq_along(dir)) {
    
    if (dir[i] == "R") 
      s[2] <- s[2] + steps[i]   
    else if (dir[i] == "L") 
      s[2] <- s[2] - steps[i]   
    else if (dir[i] == "U") 
      s[1] <- s[1] + steps[i]   
    else if (dir[i] == "D") 
      s[1] <- s[1] - steps[i]   
    
    hmax <- max(s[2], hmax)
    hmin <- min(s[2], hmin)
    vmax <- max(s[1], vmax)
    vmin <- min(s[1], vmin)
    
  }
  
  c(vmax, vmin, hmax, hmin, vmax - vmin + 1, hmax - hmin + 1)
  
}




