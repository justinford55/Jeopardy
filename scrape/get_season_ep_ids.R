library(tidyverse)
library(rvest)

# this gets the game_ids for all games in the given season
# at the moment this function automatically filters for only "regular" games

get_season_ep_ids <- function(season, all_games = FALSE) {
  
  season <- 37
  
  season_url <- httr::GET(paste0("https://j-archive.com/showseason.php?season=", season)) |>
    httr::content(as = "text")
  
  # gets the html for that page
  season_page <- read_html(season_url)
  
  # gets the links for all episodes of the season
  season_eps <- season_page |>
    html_nodes("td:nth-child(1) a") |>
    html_attr("href")
  
  # retrieves episode ids from these links
  ep_ids <- gsub("^.*=", "", season_eps)
  
  des <- season_page %>%
    html_nodes(".left_padded") %>%
    html_text()
  
  games <- tibble(season = rep(season, length(ep_ids)), game_id = ep_ids, des = des)
  
  games <- games %>%
    mutate(game_type = case_when(
      grepl("Professors", des) ~ "PROF",
      grepl("Teen", des) ~ "TEEN",
      grepl("College Championship", des) ~ "COLL",
      grepl("Teachers", des) ~ "TEACH",
      grepl("Tournament of Champions", des) ~ "ToC",
      grepl("Celebrity", des) ~ "CELEB",
      grepl("Kids Week", des) ~ "KIDS",
      grepl("All-Star", des) ~ "ALL-STAR",
      TRUE ~ "REG"
    )) %>%
    mutate(tournament_round = case_when(
      grepl("quarterfinal game", des) ~ "QF",
      grepl("semifinal game", des) ~ "SF",
      grepl("final game", des) ~ "F",
      TRUE ~ NA_character_
    )) %>%
    select(-des)
  
  if (all_games == FALSE) {
    
    ep_ids <- games %>%
      filter(game_type == "REG")
    
    return(ep_ids$game_id)
    
  } else {
    
    return(games$game_id)
  
  }
}
