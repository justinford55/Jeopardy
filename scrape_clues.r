library(rvest)
library(tidyverse)
source("get_season_ep_ids.R")


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

full_games <- get_game_info(38)


s35 <- get_game_info(35)

# Things I want to include:
#    - whether or not the question was answered correctly
#    - the selecting contestant
#    - the correct contestant
# I might also want to include:
#    - any contestants that answered incorrectly

# this is like a cool table counting daily double positions
full_games %>%
  filter(round == "J") %>%
  group_by(category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  pivot_wider(names_from = category_id, values_from = dd)

# let's try and do a visualization here

full_games %>%
  filter(round %in% c("J", "DJ")) %>%
  mutate(row = ordered(row, levels = c(5, 4, 3, 2, 1)),
         category_id = as.factor(category_id)) %>%
  group_by(round, category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = category_id, y = row, fill = dd), color = "black") +
  facet_wrap(~round, labeller = labeller(round = c("J" = "Jeopardy", "DJ" = "Double Jeopardy"))) +
  geom_text(aes(category_id, row, label = round(dd, digits = 3)), color="black", size=rel(4.5)) +
  theme(
    plot.background = element_rect(fill = "#00003a", color = NA),
    panel.background = element_rect(fill = "#00003a", color = NA),
    strip.background = element_rect(fill = "#00003a"),
    strip.text = element_text(color = "gold", size = 12),
    text = element_text(color = "gold", family = "sans"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#00003a", colour = NA),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_blank()
  ) +
  labs(
    y = "Clue Value",
    title = "Frequency of Daily Double Locations",
    subtitle = "Seasons 36-38"
  ) +
  xlab(NULL) +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(
    name = "Clue Value",
    labels = c("$1,000", "$800", "$600", "$400", "$200")
  ) +
  scale_fill_gradient("Daily Double\nProbability", low = "white", high = "#060CE9") +
  coord_fixed()

full_games %>%
  filter(round == "DJ") %>%
  mutate(row = ordered(row, levels = c(5, 4, 3, 2, 1)),
         category_id = as.factor(category_id)) %>%
  group_by(category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = category_id, y = row, fill = dd), color = "black") +
  geom_text(aes(category_id, row, label = round(dd, digits = 2)), color="black", size=rel(4)) +
  theme(
    plot.background = element_rect(fill = "#00003a", color = NA),
    panel.background = element_rect(fill = "#00003a", color = NA),
    text = element_text(color = "gold", family = "sans"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#00003a", colour = NA),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    panel.grid.major = element_blank()
  ) +
  labs(
    y = "Clue Value",
    title = "Frequency of Daily Double Locations, Jeopardy Round",
    subtitle = "Season 38"
  ) +
  xlab(NULL) +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(
    name = "Clue Value",
    labels = c("$1,000", "$800", "$600", "$400", "$200")
  ) +
  scale_fill_gradient("Daily Double\nProbability", low = "white", high = "#060CE9")


full_games %>%
  filter(round == "DJ") %>%
  group_by(category_id, row) %>%
  summarize(dd = sum(dd)) %>%
  ungroup() %>%
  pivot_wider(names_from = category_id, values_from = dd)




