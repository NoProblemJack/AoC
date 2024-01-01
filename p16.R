
library(tidyverse)
library(zeallot)


grid_raw <- read_csv("inputs/16.txt", col_names = "grid_data")
WIDTH <- grid_raw[1,, drop = TRUE] %>% str_length()

grid <-
  grid_raw %>% 
  # mutate(
  #   y = seq_len(n()), 
  #   .before = grid_data
  # ) %>% 
  separate_wider_position(
    cols = grid_data,
    widths = setNames(rep(1, WIDTH), seq_len(WIDTH))
  ) %>% 
  as.matrix()

en <- grid
en[][] <- 0L
en <- matrix(as.integer(en), ncol = ncol(en))

tracks <- grid
tracks[][] <- 0L
tracks <- matrix(as.integer(tracks), ncol = ncol(tracks))

c(t, e) %<-% walk_grid(1,1, c(0, 1), grid, tracks, en)
# c(t, e) %<-% walk_grid(9,9, c(0, -1), grid, tracks, en)
# c(t, e) %<-% walk_grid(7,2, c(1, 0), grid, tracks, en)

## 1658 - wrong
## 7562
sum(e)

startings <-
  bind_rows(
    tibble(
      i = seq_len(WIDTH),
      j = 1L,
      d = rep(list(c(0L,1L)), WIDTH)
    ),
    tibble(
      i = seq_len(WIDTH),
      j = 110L,
      d = rep(list(c(0L,-1L)), WIDTH)
    ),
    tibble(
      i = 1L,
      j = seq_len(WIDTH),
      d = rep(list(c(1L,0L)), WIDTH)
    ),
    tibble(
      i = WIDTH,
      j = seq_len(WIDTH),
      d = rep(list(c(-1L,0L)), WIDTH)
    )
  ) %>% 
  mutate(id = seq_len(n()))

## 7793
startings %>% 
  group_by(id) %>% 
  mutate(
    ans = walk_grid(i,j, d[[1]], grid, tracks, en, en_only = TRUE) %>% sum
  ) %>% 
  ungroup() %>% 
  summarise(max(ans))


walk_grid <- function(i, j, d, grid, tracks, en, en_only = FALSE) {
  
#  i r/l
 # j u/d
  
  ## n/e/s/w  1/2/3/4
  
  c(imax, jmax) %<-% dim(grid)
  
  while (TRUE) {
  
    ## OOB
    if (i < 1L || i > imax || j < 1L || j > jmax)
      break
    
    marker <- 
      if (d[1] == 1L) 1L
      else if (d[1] == -1L) 2L
      else if (d[2] == 1L) 4L
      else 8L

    ## already visited?
    if ((tracks[i,j] %/% marker) %% 2L == 1L)
      break
    
    en[i,j] <- 1L
    tracks[i,j] <- marker
    sym <- grid[i,j]
    
    if (sym == "|") {
      if (d[1] == 0L) {
        ## moving horizontally
        ## recursively move up
        c(tracks, en) %<-% walk_grid(i + 1L, j, c(1L, 0L), grid, tracks, en)
        ## locally move down
        d <- c(-1L, 0L)
        i <- i - 1L
      } else
        ## moving vertically
        i <- i + d[1]
      next
    }
      
    if (sym == "-") {
      if (d[2] == 0L) {
        ## moving horizontally
        ## recursively move right
        c(tracks, en) %<-% walk_grid(i, j + 1L, c(0L, 1L), grid, tracks, en)
        ## locally move left
        d <- c(0L, -1L)
        j <- j - 1L
      } else
        ## moving vertically
        j <- j + d[2]
      next
    }
      
    if (sym == "\\") {
      if (d[1] == 1L) {
        ## moving down - turn right
        d <- c(0L, 1L)
        j <- j + 1L
      } else if (d[1] == -1L) {
        ## moving up - turn left
        d <- c(0L, -1L)
        j <- j - 1L
      } else if (d[2] == 1L) {
        ## moving right - turn down
        d <- c(1L, 0L)
        i <- i + 1L
      } else {
        ## moving left - turn up
        d <- c(-1L, 0L)
        i <- i - 1L
      }
      next
    }
    
    if (sym == "/") {
      if (d[1] == 1L) {
        ## moving down - turn left
        d <- c(0L, -1L)
        j <- j - 1L
      } else if (d[1] == -1L) {
        ## moving up - turn right
        d <- c(0L, 1L)
        j <- j + 1L
      } else if (d[2] == 1L) {
        ## moving right - turn up
        d <- c(-1L, 0L)
        i <- i - 1L
      } else {
        ## moving left - turn down
        d <- c(1L, 0L)
        i <- i + 1L
      }
      next
    }
  
    if (sym == ".") {
      i <- i + d[1]
      j <- j + d[2]
    }
    
  }
  
  if (en_only)
    en
  else
    list(
      tracks = tracks,
      energy = en
    )
  
}
