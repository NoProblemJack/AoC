library(tidyverse)

cards <- 
  ## read data in as a table with 2 columns, 'card' and 'nums'
  read_delim("inputs/4.txt", delim = ":", col_names = c("card", "nums")) %>% 
  ## redefine 'card' field to its number
  mutate(
    card = str_extract(card, "\\d+") %>% as.integer()
  ) %>% 
  ## split num column into "winning_nums", "my_nums"
  separate_wider_delim(
    cols = nums,
    delim = "|",
    names = c("winning_nums", "my_nums")
  ) %>% 
  mutate(
    ## transform winning_nums columns into a list column with each element an
    ## array of integers representing the winning numbers for that card
    winning_nums = 
      winning_nums %>% 
      str_trim() %>% 
      str_split(pattern = " +") %>% 
      map(as.integer),
    ## ditto for my_nums
    my_nums = 
      my_nums %>% 
      str_trim() %>% 
      str_split(pattern = " +") %>% 
      map(as.integer),
    ## for each row work out intersection of each array, and return its length
    ## as an integer
    winners = map2_int(
      winning_nums,
      my_nums,
      ~{intersect(.x, .y) %>% length()}
    ),
    ## append "points" column
    points = if_else(winners > 0, 2^(winners-1), 0)
  )


#### part 1 ####.

## sum "points" column
cards %>% 
  summarise(sum(points))


#### part 2 ####.

p4_copy_cards <- function(copies, winners) {
  
  for (i in seq_along(copies)) {
    if (winners[i] > 0) {
      indices <- (i+1):(i+winners[i])
      copies[indices] <- copies[indices] + copies[i]
    }
  }
  
  copies
  
}

cards %>% 
  mutate(
    ## append "copies" field - defined as p4_copy_cards called with an array of
    ## 1's and the 'winners' column
    copies = p4_copy_cards(rep(1, n()), winners)
  ) %>% 
  ## sum "copies" field
  summarise(sum(copies))
