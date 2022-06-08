library(rvest)
library(tidyverse)

# returns a datafrane consisting of:
# - player_name: the player's full name
# - player_id
# - game_id
# - tape_date
# - air_date

get_contestants <- function(season) {
  
  # url for the page for the desired season
  season <- httr::GET(paste0("https://j-archive.com/showseason.php?season=", season)) |>
    httr::content(as = "text")
  
  # gets the html for that page
  season_page <- read_html(season)
  
  # gets the links for all episodes of the season
  season_eps <- season_page %>%
    html_nodes("td a") %>%
    html_attr("href")
  
  season_dates_full <- season_page %>%
    html_nodes("td a") %>%
    html_text()
  
  season_dates <- gsub("^.*aired", "", season_dates_full)
  
  # declaring vectors for contestant names/ids, and game ids
  contestants <- c()
  contestant_ids <- c()
  game_ids <- c()
  tape_dates <- c()
  air_dates <- c()
  
  # for each episode of the season
  for (ep in 1:length(season_eps)) {
    
    # gets url of the page for that game
    url <- season_eps[ep]
    game <- read_html(url)
    
    # gets the name of all contestants for the game
    ep_contestants <- game %>%
      html_nodes(".contestants") %>%
      html_children() %>%
      html_text()
    
    # gets the tape date of the game
    tape_date <- game %>%
      html_nodes("h6") %>%
      html_text()
    
    tape_date <- gsub("^.*?: ","", tape_date)
    
    # gets contestant ids for the game
    # these come from the hyperlink to each player's individual page on j-archive
    # I use this hyperlink to retrieve the contestant id
    ep_contestant_ids <- game %>%
      html_nodes(".contestants") %>% # this gets the nodes for the three contestants
      html_children() %>% # this gets the tags for the hyperlink 
      html_attr("href")
    
    num_contestants <- length(ep_contestants)
    
    # appends the data from this game to the full season data
    contestants <- append(contestants, ep_contestants)
    contestant_ids <- append(contestant_ids, ep_contestant_ids)
    game_ids <- append(game_ids, rep(url, num_contestants))
    tape_dates <- append(tape_dates, rep(tape_date, num_contestants))
    air_dates <- append(air_dates, rep(season_dates[ep], num_contestants))
  }
  
  # puts data into a tibble
  # also removes all the irrelevant information from the hyperlinks so I am left with just
  # the game_id and player_id
  contestants_full <- 
    tibble(player_name = contestants, player_id = contestant_ids, 
           game_id = game_ids, tape_date = tape_dates, air_date = air_dates) %>%
      separate(player_id, into = c("rest", "player_id"), sep = "=") %>%
      separate(game_id, into = c("rest2", "game_id"), sep = "=") %>%
      select(-rest, -rest2) 
  
  return(contestants_full)
}

  
  





