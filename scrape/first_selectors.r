library(rvest)
library(tidyverse)
source("scrape/get_season_ep_ids.R")

# These functions get the selectors of the clues at the beginning of rounds.

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

# WARNING:: this uses cont_37 which is from the scrape_contestants function
fs_j <- get_first_selector(37) %>%
  left_join(cont_37, by = c("game_ids" = "game_id", "first_selector" = "player_name")) %>%
  arrange(game_ids) %>%
  rename(c("player_name" = "first_selector"))

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
fs_dj <- get_first_selector_dj(37) %>%
  mutate(first_selector = gsub("!", "", first_selector)) %>%
  left_join(cont_37, by = c("game_ids" = "game_id", "first_selector" = "player_nickname")) %>%
  arrange(game_ids) %>%
  rename(c("player_nickname" = "first_selector"))

first_selectors <- rbind(fs_j, fs_dj)

first_selectors$clue_order <- 1

# I still need to make a function that can convert full names to nicknames

first_selectors %>%
  left_join(cont_37, by = c("game_ids" = "game_id", "first_selector" = "player_name")) %>%
  arrange(game_ids)

w_37 <- w %>%
  left_join(cont_full, by = c("game_id" = "game_id", "buzz_name" = "player_nickname")) %>%
  select(-player_name) %>%
  rename(c("buzz_id" = "player_id")) %>%
  left_join(first_selectors, c("game_id" = "game_ids", "clue_order" = "clue_order", "round" = "round")) %>%
  select(-player_name) %>%
  mutate(selector = ifelse(is.na(selector), player_nickname, selector)) %>%
  filter(season == 37) %>%
  group_by(game_id, round) %>%
  fill(selector, .direction = "down") %>%
  ungroup()

w_37

colSums(is.na(w_37))

  
