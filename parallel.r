# doesn't do much right now


library(doParallel)
library(rvest)
library(tidyverse)
source("get_season_ep_ids.r")

get_game_info_par <- function(season) {
  
  # retrieves episode ids for the season
  ep_ids <- get_season_ep_ids(season)
  
  # for each episode
  foreach (ep_id = seq_along(ep_ids),
           .packages = "rvest",
           .combine = "c") %dopar% {
             
             # gets urls of the pages for that game (one for the clues and one for the answers)
             game_url <- paste0("https://j-archive.com/showgame.php?game_id=", ep_ids[ep_id])
             responses_url <- paste0("https://j-archive.com/showgameresponses.php?game_id=", ep_ids[ep_id])
             
             game_page <- read_html(game_url)
             responses_page <- read_html(responses_url)
             
             return(game_page)
           }
}


t1 <- Sys.time()
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
result <- get_game_info_par(38)
stopCluster(cl)
t2 <- Sys.time()
tibble(result)

t_parallel <- t2 - t1
t_parallel

result
