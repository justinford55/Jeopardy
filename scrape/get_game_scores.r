library(tidyverse)
library(rvest)
source("get_season_ep_ids.R")

# GETS THE FINAL SCORES FOR EACH GAME
# INCLUDES:
#   - game_id
#   - name, contestant name
#   - total, the final amount of money for the contestant at the end of the game
#   - place, the final place of the contestant for that game

# doesn't work yet

get_game_scores <- function(season) {
  
  # episodes for the season
  ep_ids <- get_season_ep_ids(season)
  
  game_scores <- rep(NA, 3*length(ep_ids))
  
  for (ep_id in ep_ids) {
    
    # get the game page
    game <- httr::GET(paste0("https://j-archive.com/showgame.php?game_id=", ep_id)) |>
      httr::content(as = "text")
    game_page <- read_html(game)
    
    # scrape the final scores data
    final_scores <- game_page %>%
      html_nodes("#final_jeopardy_round table:nth-child(4) td") %>%
      html_text()
    
    # formatting
    finals <- tibble(game_id = rep(ep_id, 3), name = final_scores[1:3], 
                     total = final_scores[4:6], result = final_scores[7:9])
  }
  
  finals <- finals %>%
    mutate(place = case_when(
      grepl("champion", result) ~ 1,
      grepl("2nd", result) ~ 2,
      grepl("3rd", result) ~ 3
    )) %>%
    mutate(total = gsub("[^0-9.-]", "", total))
  
}


