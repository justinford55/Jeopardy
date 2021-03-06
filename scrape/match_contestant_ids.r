library(tidyverse)


# this function matches contestant ids with their nicknames for the game
# names is the list of all responses (will contain names of all contestants plus "Triple Stumper")
# full_names is the tibble

# now that I changed the scrape_contestants function, this might be irrelevant
# I won't delete it yet though

get_id_from_full_name <- function(names, full_names) {
  
  # names is the names that are used to denote a contestant response
  # full_names is the full names of the contestants
  
  unique_names <- unique(names)
  
  full_names <- full_names %>%
    separate(player_name, c("first", "rest"), sep = " ", extra = "merge")
  
  unique_names <- tibble(nickname = unique_names[! unique_names == "Triple Stumper"])
  
  names_and_ids <- unique_names %>%
    left_join(full_names, by = c("nickname" = "first")) %>%
    select(-rest)
 
  return(names_and_ids)
   
}


# this function should take in a dataframe containing full_name and
get_nickname_from_full <- function(df) {
  
}

cont_37 <- get_contestants(37)
