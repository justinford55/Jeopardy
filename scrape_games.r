library(rvest)
library(tidyverse)

# this function gets game information

get_games <- function(season, game_type = NA) {
  
  season_url <- paste0("https://j-archive.com/showseason.php?season=", season)
  
  # gets the html for that page
  season_page <- read_html(season_url)
  
  links <- season_page %>%
    html_nodes("td:nth-child(1) a")
  
  des <- season_page %>%
    html_nodes(".left_padded") %>%
    html_text()
  
  tournaments <- c("Professors Tournament", "Teen Tournament", "College Championship", "Teachers Tournament",
                   "Tournament of Champions", "Celebrity", "All-Star Games")
  
  # I want to get game_type or special tournament info included
  # If any of the strings in the tournaments vector are found in 
  
  # gets the links for all episodes of the season
  season_eps <- links %>%
    html_attr("href")
  
  ep_ids <- gsub("^.*=", "", season_eps)
  
  air_dates <- links %>%
    html_text()
  
  air_dates <- as.Date(gsub(".*aired.", "", air_dates))
  
  tape_dates <- links %>%
    html_attr("title")
  
  tape_dates <- as.Date(gsub(".*Taped.", "", tape_dates))
  
  games <- tibble(season = rep(season, length(ep_ids)), game_id = ep_ids, air_date = air_dates, tape_date = tape_dates, des = des)

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
  
  games
  
  # let's include contestant_id
  
  #final_jeopardy_round table:nth-child(4) td
  
  game_id <- 7346
  game_url <- paste0("https://j-archive.com/showgameresponses.php?game_id=", game_id)
  game_page <- read_html(game_url)
  
  final_scores <- game_page %>%
    html_nodes("#final_jeopardy_round table:nth-child(4) td") %>%
    html_text()
  
  finals <- tibble(game_id = rep(game_id, 3), name = final_scores[1:3], 
                   total = final_scores[4:6], result = final_scores[7:9])

  finals <- finals %>%
    mutate(place = case_when(
      grepl("champion", result) ~ 1,
      grepl("2nd", result) ~ 2,
      grepl("3rd", result) ~ 3
    ))
  
  games %>%
    mutate(returning_champ = (filter(finals, ))$name[1])
  
  finals %>%
    filter(game_id == "7345")
  
}
