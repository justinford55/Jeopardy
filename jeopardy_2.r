library(tidyverse)
library(rvest)
source("get_season_ep_ids.R")


# trying to get rid of some for loops
s <- get_season_ep_ids(37)

# this 
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

t1 <- Sys.time()
pages <- map(s, read_game_clues)
t2 <- Sys.time()
t2-t1

clue_buzzes <- bind_rows(pages)

s_37 <- get_game_info(37)

clue_buzzes <- clue_buzzes %>%
  separate(clue, into = c("junk", "round", "category_id", "row"), sep = "_") %>%
  select(-junk) %>%
  mutate(category_id = as.integer(category_id),
         row = as.integer(row))

df <- s_37 %>%
  right_join(clue_buzzes, by = c("game_id" = "game_id", "round" = "round", "category_id" = "category_id", "row" = "row")) %>%
  filter(r_or_w == "right" | buzz_name == "Triple Stumper" | dd == TRUE)

# alright 

# could do a barplot with this
df %>%
  group_by(round, row) %>%
  filter(dd == FALSE) %>%
  summarize(n = n(),
            percent_correct = sum(r_or_w == "right")/ n()) %>%
  mutate(single_cont_prob = 1 - (1 - percent_correct)^(1/3))
# single_cont_prob estimates the probability that the average contestant would get the correct answer
# if only they were playing.

df %>%
  group_by(round, row) %>%
  filter(dd == FALSE) %>%
  summarize(n = n(),
            percent_correct = sum(r_or_w == "right")/n()) %>%
  mutate(row = ordered(row, 5:1)) %>%
  ggplot() +
  aes(row, percent_correct) +
  geom_col(aes(fill = round), position = "dodge", color = "white") +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = "#00003a", color = NA),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "#00003a", color = NA),
    strip.background = element_rect(fill = "#00003a"),
    strip.text = element_text(color = "#E5A561", size = 12),
    legend.background = element_rect(fill = "#00003a"),
    text = element_text(color = "white", family = "sans"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    ax
  ) +
  labs(
    y = "",
    title = "Percentage of Correct Responses by Row",
    subtitle = "Seasons 36-38"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual("Round", values = c("#E5A561", "#0000AF"))




df %>%
  group_by(round, row) %>%
  filter(dd == TRUE) %>%
  summarize(n = n(), 
            percent_correct = sum(r_or_w == "right")/ n())
# Daily doubles are answered more correctly than the probabilities in the previous df would suggest.
# Good contestants are more likely to reveal daily double clues, so daily double answer probability is higher.

