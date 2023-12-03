library(data.table)
library(tidyverse)


p1_func <- function(x) {
  
  paste0(
    str_extract(x, "^[^\\d]*(\\d)", group = 1),
    str_extract(x, "(\\d)[^\\d]*$", group = 1)
  ) %>% as.integer()
  
  
}

p1_replace_func <- function(x) {
  
  x <- str_replace_all(x, "one", "o1e")
  x <- str_replace_all(x, "two", "t2o")
  x <- str_replace_all(x, "three", "t3e")
  x <- str_replace_all(x, "four", "f4r")
  x <- str_replace_all(x, "five", "f5e")
  x <- str_replace_all(x, "six", "s6x")
  x <- str_replace_all(x, "seven", "s7n")
  x <- str_replace_all(x, "eight", "e8t")
  x <- str_replace_all(x, "nine", "n9e")
  
  x
  
}

read_csv("inputs/1.csv") %>% 
  mutate(
    answer1 = inputs %>% p1_func(),
    inputs2 = inputs %>% p1_replace_func(),
    answer2 = inputs2 %>% p1_func()
  ) %>%
  summarise_if(is.numeric, sum)



