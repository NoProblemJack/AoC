library(tidyverse)
library(bit64)


calc_record_distances_count <- function(time, currect_record) {
  
  tibble(
    hold = as.integer64(1):time
    # hold = seq_len(time)
  ) %>%
  mutate(
    dist = hold * (!!time - hold)
  ) %>% 
  filter(dist > !!currect_record) %>% 
  nrow()
  
}
 

data <-
  read_delim("inputs/6.txt", delim = ":", col_names = c("measure", "race_data")) %>% 
  mutate(
    measure = tolower(measure),
    race_data = str_trim(race_data)
  ) %>% 
  pivot_wider(
    names_from = measure,
    values_from = race_data
  ) %>% 
  mutate_all(
    str_split, pattern = " +"
  ) %>% 
  unchop(c(time, distance)) %>% 
  mutate(
    race_id = seq_len(n()),
    time = as.integer(time),
    distance = as.integer(distance),
  )


data %>% 
  mutate(
    options = map2_int(
      time, distance,
      calc_record_distances_count
    )
  ) %>% 
   summarise(prod(options))



data2 <-
  read_delim("inputs/6.txt", delim = ":", col_names = c("measure", "race_data")) %>% 
  mutate(
    measure = tolower(measure),
    race_data = str_remove_all(race_data, " ")
  ) %>% 
  pivot_wider(
    names_from = measure,
    values_from = race_data
  ) %>% 
  mutate_all(
    str_split, pattern = " +"
  ) %>% 
  unchop(c(time, distance)) %>% 
  mutate(
    race_id = seq_len(n()),
    time = as.integer64(time),
    distance = as.integer64(distance),
  )

data2 %>% 
  mutate(
    options = map2_int(
      time, distance,
      calc_record_distances_count
    )
  ) %>% 
  summarise(prod(options))





