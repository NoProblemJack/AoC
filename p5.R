# library(lobstr)
# library(data.table)
library(tidyverse)
library(bit64)

conform_range <- function(range_tbl, include_dest) {
  
  range_tbl <- 
    range_tbl %>% 
    mutate(
      source_end = source_start + range_length
    ) %>% 
    select(
      source_start, source_end, everything()
    )
  
  if (include_dest)
    range_tbl <- 
      range_tbl %>% 
      mutate(
        dest_end = dest_start + range_length
      ) %>% 
      select(
        source_start, source_end, dest_start, dest_end, everything()
      )
  
  
  range_tbl %>% arrange(source_start)
  
}
complete_ranges <- function(range_tbl, max_id) {
  
  ## creates complementary ranges so that there are no gaps between ranges
  
  range_tbl <-
    conform_range(
      range_tbl, TRUE
    )
  
  bind_rows(
    range_tbl,
    range_tbl %>% 
      arrange(
        source_start
      ) %>% 
      transmute(
        temp = source_start,
        source_start = lag(source_end, default = as.integer64(0)),
        source_end = temp,
        dest_start = source_start, 
        dest_end = source_end 
      ) %>% 
      select(-temp),
    tibble(
      source_start = min(max(range_tbl$source_end), max_id),
      source_end = max_id,
      dest_start = source_start, 
      dest_end = source_end 
    )
  ) %>% 
  filter(
    source_start < source_end
  ) %>% 
  arrange(source_start)
  
}

map_source_to_dest <- function(source_tbl, map_tbl) {
  
  source_tbl %>% 
    left_join(
      map_tbl,
      by = join_by(id >= source_start, id < source_end),
      relationship = "many-to-one"
    ) %>% 
    mutate(
      dest_id = id - source_start + dest_start,
      dest_id = data.table::fcoalesce(dest_id, id)
    ) %>% 
    transmute(
      id = dest_id
    )
  
}
map_source_range_to_dest <- function(src_tbl, map_tbl) {
  
  ## conform map_tbl
    
  src_tbl %>% 
    select(
      source_start, source_end
    ) %>% 
    inner_join(
      map_tbl,
      by = join_by(source_end > source_start, source_start < source_end),
      relationship = "many-to-many"
    ) %>% 
    mutate(
      delta = dest_start - source_start.y,
      source_start = pmax(source_start.x, source_start.y),
      source_end = pmin(source_end.x, source_end.y),
      dest_start = source_start + delta,
      dest_end = source_end + delta
    ) %>% 
    select(
      source_start = dest_start,
      source_end = dest_end
    ) %>% print()
  
}



data_raw <-
  # read_csv("inputs/5a.txt", col_names = c("data")) %>%
  read_csv("inputs/5.txt", col_names = c("data")) %>%
  filter(data != "") %>% 
  mutate(
    is_seeds = str_detect(data, "seeds:"),
    is_header = str_detect(data, ":") & !is_seeds,
    data_group_id = consecutive_id(is_header)
  )

seeds <- 
  data_raw %>% 
  filter(is_seeds) %>% 
  mutate(
    id = str_remove(data, "^[^\\d]+") %>% str_split(pattern = " +")
  ) %>% 
  unchop(id) %>% 
  transmute(id = as.integer64(id))#' %>%
  # pull(id) %>% 
  # as.integer64()

seeds_max_id <- seeds$id %>% max()

headers <-
  data_raw %>% 
  filter(is_header) %>% 
  mutate(header = str_extract(data, "^[A-Za-z-]+"))



body <-
  data_raw %>% 
  filter(!is_header & !is_seeds) %>% 
  separate_wider_delim(
    cols = data,
    delim = " ", 
    names = c("dest_start", "source_start", "range_length")
  ) %>% 
  mutate(
    across(c(dest_start, source_start, range_length), as.integer64)
  ) %>% 
  nest(
    mapping_data = c(dest_start, source_start, range_length),
    .by = data_group_id
  )

maps <- 
  body$mapping_data %>% 
  setNames(headers$header) %>% 
  map(~complete_ranges(.x, seeds_max_id))

seeds2 <-
  tibble(
    source_start = seeds$id[seq_len(nrow(seeds)) %% 2 == 1],
    range_length = seeds$id[seq_len(nrow(seeds)) %% 2 == 0]
  ) %>% 
  conform_range(include_dest = FALSE)

seeds %>% 
  map_source_to_dest(maps[["seed-to-soil"]]) %>% 
  map_source_to_dest(maps[["soil-to-fertilizer"]]) %>% 
  map_source_to_dest(maps[["fertilizer-to-water"]]) %>% 
  map_source_to_dest(maps[["water-to-light"]]) %>% 
  map_source_to_dest(maps[["light-to-temperature"]]) %>% 
  map_source_to_dest(maps[["temperature-to-humidity"]]) %>% 
  map_source_to_dest(maps[["humidity-to-location"]]) %>% 
  summarise(min(id))

seeds2_max_id <- seeds2$source_end %>% max()

maps2 <- 
  body$mapping_data %>% 
  setNames(headers$header) %>% 
  ## bug!!! seeds2_max_id is not always the corect max to pass - oh well
  map(~complete_ranges(.x, seeds2_max_id))


seeds2 %>% 
  map_source_range_to_dest(maps2[["seed-to-soil"]]) %>% 
  map_source_range_to_dest(maps2[["soil-to-fertilizer"]]) %>% 
  map_source_range_to_dest(maps2[["fertilizer-to-water"]]) %>% 
  map_source_range_to_dest(maps2[["water-to-light"]]) %>% 
  map_source_range_to_dest(maps2[["light-to-temperature"]]) %>% 
  map_source_range_to_dest(maps2[["temperature-to-humidity"]]) %>% 
  map_source_range_to_dest(maps2[["humidity-to-location"]]) %>% 
  summarise(min(source_start))



  
