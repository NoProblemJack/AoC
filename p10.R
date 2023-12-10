library(tidyverse)
library(data.table)

grid <- read_csv("inputs/10.txt", col_names = "grid_data")
WIDTH <- grid[1,, drop = TRUE] %>% str_length()

make_adj_template <- function() {
  
  tribble(
    ~sym, ~x1_d, ~y1_d, ~x2_d, ~y2_d,
    "|",  0,  1,   0, -1,
    "-",  1,  0,  -1,  0,
    "L",  0, -1,   1,  0,
    "F",  1,  0,   0,  1,
    "7",  0,  1,  -1,  0,
    "J",  0, -1,  -1,  0,
  )

}


grid_long <-
  grid %>% 
  mutate(
    y = seq_len(n()), 
    .before = grid_data
  ) %>% 
  separate_wider_position(
    cols = grid_data,
    widths = setNames(rep(1, WIDTH), seq_len(WIDTH))
  ) %>% S
  pivot_longer(
    cols = -y,
    names_to = "x",
    values_to = "sym",
    names_transform = as.integer
  ) %>% 
  left_join(
    make_adj_template(),
    by = "sym",
    relationship = "many-to-one"
  ) %>% 
  mutate(
    x1 = x + x1_d,
    y1 = y + y1_d,
    x2 = x + x2_d,
    y2 = y + y2_d,
  ) %>% 
  as.data.table()

S_pos <- grid_long[sym == "S"]


S_neighbours <- 
  grid_long[
    (x1 == S_pos$x & y1 == S_pos$y) |
      (x2 == S_pos$x & y2 == S_pos$y)
  ]

grid_long[
  sym == "S",
  `:=`(
    x1 = S_neighbours[1, x],
    y1 = S_neighbours[1, y],
    x2 = S_neighbours[2, x],
    y2 = S_neighbours[2, y]
  )
]

# grid_long[
#   grid_long,
#   on = .(x = x1, y = y1),
#   `:=`(
#     x1 = if_else(sym == "S", i.x, x.x1),
#     y1 = if_else(sym == "S", i.y, x.y1)
#   )
# ][
#   grid_long,
#   on = .(x = x2, y = y2),
#   `:=`(
#     x2 = if_else(sym == "S", i.x, x.x2),
#     y2 = if_else(sym == "S", i.y, x.y2)
#   )
# ]

grid_long[, id := seq_len(.N)]
grid_long[
  grid_long,
  on = .(x1 = x, y1 = y),
  id1 := i.id
]
grid_long[
  grid_long,
  on = .(x2 = x, y2 = y),
  id2 := i.id
]

walk_the_pipe(grid_long)

walk_the_pipe <- function(grid_long) {
  
  get_unwalked_neighbours <- function(ids_, grid_long) {
    id1_ <- grid_long[ids_, id1]
    id1_ <- grid_long[id1_][is.na(dist), id]
    id2_ <- grid_long[ids_, id2]
    id2_ <- grid_long[id2_][is.na(dist), id]
    c(id1_, id2_) %>% unique()  
  }  
  
  grid_long[, dist := NA_integer_]
  unwalked_places <- grid_long[sym == "S", id]
  dist_ <- 0L
 
  while (TRUE) {
    # print(unwalked_places)
    # Sys.sleep(1)
    grid_long[unwalked_places, dist := dist_]
    unwalked_places <- get_unwalked_neighbours(unwalked_places, grid_long)
    
    if (!length(unwalked_places))
      break
    
    dist_ <- dist_ + 1L
  }
 
  dist_ 

}

