library(tidyverse)


eval_hand <- function(hand) {
 
  if (eval_hand__is_5ofak(hand))
    return(7)
  if (eval_hand__is_4ofak(hand))
    return(6)
  if (eval_hand__is_full_house(hand))
    return(5)
  if (eval_hand__is_3ofak(hand))
    return(4)
  if (eval_hand__is_2pair(hand))
    return(3)
  if (eval_hand__is_1pair(hand))
    return(2)
  if (eval_hand__is_high_card(hand))
    return(1)

  stop("hand valuation fail: ", hand)  
}
eval_hand__is_5ofak <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 1
  
}
eval_hand__is_4ofak <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 2 && ((hand[1] == hand[2]) != (hand[4] == hand[5]))
  
}
eval_hand__is_full_house <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 2 && ((hand[2] == hand[3]) != (hand[3] == hand[4]))
  
}
eval_hand__is_3ofak <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 3 && 
    ((hand[1] == hand[2] && hand[2] == hand[3]) ||
      (hand[2] == hand[3] && hand[3] == hand[4]) ||
      (hand[3] == hand[4] && hand[4] == hand[5]))
       
}
eval_hand__is_2pair <- function(hand) {
  
  if (eval_hand__is_3ofak(hand))
    return(FALSE)
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 3
       
}
eval_hand__is_1pair <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 4
       
}
eval_hand__is_high_card <- function(hand) {
  
  hand <- hand %>% str_split_1(pattern = "") %>% sort()

  data.table::uniqueN(hand) == 5
       
}

get_best_card <- function(hand) {
  
  if (!str_length(hand))
    return("A")
  
  cards <- hand %>% str_split_1(pattern = "")
  
  counts <- str_count(hand, cards)
  
  cards[counts == max(counts)] %>% unique() %>% paste0(collapse = "") %>% get_highest_card()
  
}
get_highest_card <- function(hand) {

    if (str_detect(hand, "A"))
      return("A")
    if (str_detect(hand, "K"))
      return("K")
    if (str_detect(hand, "Q"))
      return("Q")
    if (str_detect(hand, "J"))
      return("J")
    if (str_detect(hand, "T"))
      return("T")
  
    hand <- hand %>% str_split_1(pattern = "") %>% sort(decreasing = TRUE)
    
    hand[1]
  
}



data <-
  read_delim("inputs/7.txt", delim = " ", col_names = c("hand", "bid")) %>% 
  separate_wider_position(
    cols = hand,
    widths = setNames(rep(1, 5), paste0("card_", seq_len(5))),
    cols_remove = FALSE
  )


#### part1 ####.

data_p1 <- 
  data %>% 
  mutate(
    hand_type = 
      map_int(
        hand,
        eval_hand
      )
  ) %>% 
  mutate(across(
    starts_with("card_"), 
    ~case_when(
      .x == "A" ~ 14L,
      .x == "K" ~ 13L,
      .x == "Q" ~ 12L,
      .x == "J" ~ 11L,
      .x == "T" ~ 10L,
      TRUE ~ suppressWarnings(as.integer(.x))
    ))
  ) %>% 
  arrange(
    hand_type, card_1, card_2, card_3, card_4, card_5
  ) %>% 
  mutate(
    rank = seq_len(n())
  )


data_p1 %>% 
  summarise(sum(rank*bid))



#### part2 ####.

data_p2 <- 
  data %>% 
  mutate(
    best_card = map_chr(str_remove_all(hand, "J"), get_best_card),
    hand2 = str_replace_all(hand, "J", best_card),
    hand_type = 
      map_int(
        hand2,
        eval_hand
      )
  ) %>% 
  mutate(across(
    starts_with("card_"), 
    ~case_when(
      .x == "A" ~ 14L,
      .x == "K" ~ 13L,
      .x == "Q" ~ 12L,
      .x == "J" ~ -1L,
      .x == "T" ~ 10L,
      TRUE ~ suppressWarnings(as.integer(.x))
    ))
  ) %>% 
  arrange(
    hand_type, card_1, card_2, card_3, card_4, card_5
  ) %>% 
  mutate(
    rank = seq_len(n())
  )

# write_csv(data_p2, "outputs/p7_2.csv")


## 249467142 wrong
## 248266145 wrong
data_p2 %>% 
  summarise(sum(rank*bid))

