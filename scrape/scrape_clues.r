library(rvest)
library(tidyverse)
source("get_season_ep_ids.R")

# this basically gets clue information for all games of a season, including:
# - the game_id of the game
# - the text of the clue
# - the correct response
# - the board position and round of the clue
# - daily double flag and wager
# - order in which the clue was revealed

# it only contains data on clues that were revealed

get_game_info <- function(season) {
  
  # retrieves episode ids for the season
  ep_ids <- get_season_ep_ids(season)
  
  # initializing vectors and lists to be populated by the following loop
  clues <- c()
  clue_ids <- c()
  corr_resps <- c()
  clue_info <- c()
  daily_doubles <- c()
  clue_order <- c()
  game_ids <- c()
  categories <- list()
  
  # for each episode
  for (ep_id in seq(ep_ids)) {
    
    # gets urls of the pages for that game (one for the clues and one for the answers)
    game <- httr::GET(paste0("https://j-archive.com/showgame.php?game_id=", ep_ids[ep_id])) |>
      httr::content(as = "text")
    responses <- httr::GET(paste0("https://j-archive.com/showgameresponses.php?game_id=", ep_ids[ep_id])) |>
      httr::content(as = "text")
    
    game_page <- read_html(game)
    responses_page <- read_html(responses)
    
    # gets the text for all clues for the episode
    ep_clues <- game_page %>%
      html_nodes(".clue_text") %>%
      html_text()
    
    # gets clue ids which encode clue position/category information
    ep_clue_ids <- game_page %>%
      html_nodes(".clue_text") %>%
      html_attr("id")
    
    # pulls clue value information with the dd information
    ep_clue_info <- game_page %>%
      html_nodes(".clue_header td:nth-child(2)") %>%
      html_text()
    
    # the order in which the clues are revealed
    ep_clue_order <- game_page %>%
      html_nodes(".clue_order_number a") %>%
      html_text()
    
    # finds the clues that were daily doubles
    ep_daily_doubles <- grepl("^DD", ep_clue_info)
    
    # this just adds a row at the end of these lists so they fit into the tibble
    # they need an extra row since final jeopardy rows won't have this information
    ep_clue_order[length(ep_clue_order) + 1] <- NA
    ep_clue_info[length(ep_clue_info) + 1] <- NA
    ep_daily_doubles[length(ep_daily_doubles) + 1] <- NA
    
    # names of the categories
    ep_categories <- game_page %>%
      html_nodes(".category_name") %>%
      html_text()
    
    # if there is a tiebreaker, you need to add another row to the above columns
    if (length(ep_categories) == 14) {
      ep_clue_order[length(ep_clue_order) + 1] <- NA
      ep_clue_info[length(ep_clue_info) + 1] <- NA
      ep_daily_doubles[length(ep_daily_doubles) + 1] <- NA
    }
    
    # correct responses
    ep_corr_resps <- responses_page %>%
      html_nodes(".correct_response") %>%
      html_text()

    # list of all categories for the episode
    clue_categories <- list(ep_categories)
    
    # appends this episode data to the existing data
    game_ids <- append(game_ids, rep(ep_ids[ep_id], length(ep_clues)))
    clues <- append(clues, ep_clues)
    clue_ids <- append(clue_ids, ep_clue_ids)
    corr_resps <- append(corr_resps, ep_corr_resps)
    clue_info <- append(clue_info, ep_clue_info)
    daily_doubles <- append(daily_doubles, ep_daily_doubles)
    clue_order <- append(clue_order, ep_clue_order)
    categories <- append(categories, clue_categories)
    }
  
  game_clues_ids <- tibble(season = season, game_id = game_ids, clue = clues, clue_id = clue_ids, corr_resp = corr_resps,
                           clue_value = clue_info, dd = daily_doubles, clue_order = clue_order)
  
  # each game id is the name of an index of the categories list,
  # where all the names of the categories for that game are stored
  names(categories) <- unique(game_ids)
  
  # this function matches the clues with the correct category name
  get_category_name <- function(game_id, category_id, round) {
    if (round == "J") {
      return(categories[[game_id]][category_id])
    } else if (round == "DJ") {
      return(categories[[game_id]][category_id + 6])
    } else if (round == "FJ") {
      return(categories[[game_id]][13])
    } else {
      return(categories[[game_id]][14])
    }
  }
  
  # formatting
  game_clues <- game_clues_ids %>%
    separate(clue_id, into = c("junk", "round", "category_id", "row"), sep = "_") %>%
    select(-junk) %>%
    mutate(category_id = as.numeric(category_id),
           row = as.numeric(row)) %>%
    mutate(category_name = unlist(pmap(list(game_id, category_id, round), get_category_name))) %>%
    mutate(dd_wager = ifelse(dd == TRUE, gsub("DD: \\$", "", clue_value), NA),
           dd_wager = ifelse(dd == TRUE, gsub(",", "", dd_wager), NA),
           dd_wager = as.numeric(dd_wager)) %>%
    mutate(round = ordered(round, levels = c("J", "DJ", "FJ", "TB"))) %>%
    select(season, game_id, round, category_name, clue, corr_resp, everything()) %>%
    arrange(-season, game_id, round, category_id, row)
  
  return(game_clues)
  
}