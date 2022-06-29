library(rvest)
library(tidyverse)
source("scrape/get_season_ep_ids.R")

# url for the page for the desired game

# should go through each season and build basically a lookup table containing:
# - game id
# - the nickname of the first selector

get_first_selector <- function(season) {
  
  season_url <- httr::GET(paste0("https://j-archive.com/showseason.php?season=", season)) |>
    httr::content(as = "text")
  
  # gets the html for that page
  season_page <- read_html(season_url)

  fs <- season_page |>
    html_nodes("td:nth-child(2)") %>%
    html_text()
  
  first_selector_j <- str_trim(sub("vs\\..*", "", fs))
  
  game_ids <- get_season_ep_ids(season = season, all_games = TRUE)
  
  selector_j <- tibble(game_ids, round = "J", first_selector = first_selector_j)
  
  return(selector_j)
}

# this looks to work, only it gets full name instead of nickname
j <- get_first_selector(37)

get_first_selector_dj <- function(season_num) {
  
  season_num <- 37
  
  game_ids <- get_season_ep_ids(season = season_num, all_games = TRUE)
  
  selector_vec <- rep(NA, length(game_ids))
  
  for (ep in seq_along(game_ids)) {
    
    # reads ep page html
    ep_url <- httr::GET(paste0("https://j-archive.com/showgame.php?game_id=", game_ids[ep])) |>
      httr::content(as = "text")
    ep_page <- read_html(ep_url)
    
    # gets the "halftime" scores
    # this determines who selects first in double jeopardy
    scores <- ep_page %>%
      html_nodes("table:nth-child(6) td") %>%
      html_text()
    
    # formats the score strings into integers
    #s <- parse_number(scores[4:6])
    s <- gsub("[^0-9.-]", "", scores[4:6])
    
    # index of the contestant with the lowest score
    # luckily this still works when scores are tied, as the contestant
    # closest to the lectern picks first in this case
    selector <- scores[which.min(s)]
    
    selector_vec[ep] <- selector
    
  }
  
  selector_dj <- tibble(game_ids, round = "DJ", first_selector = selector_vec)
  
  return(selector_dj)
}

# this correctly gets the first selector for each double jeopardy round
dj <- get_first_selector_dj(37)

full <- rbind(j, dj)

# I still need to make a function that can convert full names to nicknames
