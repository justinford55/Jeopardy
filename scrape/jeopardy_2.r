library(tidyverse)
library(rvest)
source("get_season_ep_ids.R")
source("scrape_clues.r")

# this function scrapes data on 
get_clue_buzzes <- function(season, wrong_buzz = TRUE) {
  
  s <- get_season_ep_ids(season)
  
  read_game_clues <- function(ep_id) {
    
    # read html
    game <- httr::GET(paste0("https://j-archive.com/showgame.php?game_id=", ep_id)) |>
      httr::content(as = "text")
    responses <- httr::GET(paste0("https://j-archive.com/showgameresponses.php?game_id=", ep_id)) |>
      httr::content(as = "text")
    
    game_page <- read_html(game)
    responses_page <- read_html(responses)
    
    clues <- game_page %>%
      html_nodes(".round .clue_text") %>%
      html_attr("id")
    
    # get clue nodes
    nodes <- responses_page %>%
      html_nodes(".round .clue_text") %>%
      html_elements("table")
    
    n <- nodes %>%
      html_elements("table")
    
    # this can get each name
    r_n <- nodes %>%
      html_text2()
    
    # this is the names of contestants who buzzed in
    col_1 <- unlist(str_split(r_n, "\t"))
    
    # indicates whether a buzz is right or wrong
    col_2 <- nodes %>%
      html_elements("td") %>%
      html_attr("class")
    
    len_clues <- rep(NA, length(col_2))
    i <- 1
    
    
    
    # this basically counts the number of times a clue is buzzed in on
    for (c in seq_along(clues)) {
      
      # gets a string of names of contestants who responded on this clue
      resp_str <- responses_page %>%
        html_nodes(".round .clue_text") %>%
        pluck(c) %>%
        html_elements("table") %>%
        html_text2()
      
      # separates names into a vector
      resp_names <- unlist(str_split(resp_str, "\t"))
      
      # inserts into the vector of clues
      len_clues[i:(i + length(resp_names) - 1)] <- rep(clues[c], length(resp_names))
      
      # this keeps track of and iterates where i am in the vector
      i <- i + length(resp_names)
      
    }
    
    contestant_buzzes <- tibble(game_id = ep_id, clue = len_clues, buzz_name  = col_1, r_or_w = col_2)
    return(contestant_buzzes)
    
  }
  
  pages <- map(s, read_game_clues)
  clue_buzzes <- bind_rows(pages)
  
  season_info <- get_game_info(season)
  
  # clue_buzzes has a row for every time a contestant buzzes in plus triple stumper flags
  # this just separates the clue id to get row/category info
  clue_buzzes <- clue_buzzes %>%
    separate(clue, into = c("junk", "round", "category_id", "row"), sep = "_") %>%
    select(-junk) %>%
    mutate(category_id = as.integer(category_id),
           row = as.integer(row))
  
  # joins the buzz-in data with the clue data
  df <- season_info %>%
    right_join(clue_buzzes, by = c("game_id" = "game_id", "round" = "round", 
                                   "category_id" = "category_id", "row" = "row"))
  
  # if wrong buzzes shouldn't be included
  # i.e. each revealed clue has exactly one row
  if (!wrong_buzz) {
    # this filters out only correct responses, triple stumper flags, and daily doubles.
    # it should have only one row for each clue
    df <- df %>%
      filter(r_or_w == "right" | buzz_name == "Triple Stumper" | dd == TRUE)
    
    # on rare occasions, a contestant will be given credit for a previously incorrect response.
    # this creates a possibility that multiple contestants can give correct answers on the same clue.
    
    # this grabs only rows with values in certain features, but doesn't include things like buzz_name
    # it basically removes rows where multiple contestants got the same
    df <- df %>%
      distinct(season, game_id, round, category_name, clue, corr_resp, category_id, row, clue_value, dd, clue_order,
               dd_wager, r_or_w)
  }
  
  return(df)
}

df <- map(33:37, get_clue_buzzes)
df_full <- bind_rows(df)

saveRDS(df_full, "data.rds")
# this is very cool

# alright this works
#df %>%
#  group_by(game_id, round, category_id, row) %>%
#  summarize(n = n()) %>%
#  filter(n > 1)



