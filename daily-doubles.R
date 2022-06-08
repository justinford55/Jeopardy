library(tidyverse)
library(rvest)

# this includes one row for every clue that was revealed in the Jeopardy and Double Jeopardy Rounds
full_games <- readRDS("data.rds")

full_games

# Things I want to include:
#    - the selecting contestant



full_games %>%
  filter(round == "J") %>%
  group_by(category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  pivot_wider(names_from = category_id, values_from = dd)


# visualization
full_games %>%
  filter(round %in% c("J", "DJ")) %>%
  mutate(row = ordered(row, levels = c(5, 4, 3, 2, 1)),
         category_id = as.factor(category_id),
         round = ordered(round, levels = c("J", "DJ"))) %>%
  group_by(round, category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = category_id, y = row, fill = dd), color = "black") +
  facet_wrap(~round, labeller = labeller(round = c("J" = "Jeopardy", "DJ" = "Double Jeopardy"))) +
  geom_text(aes(category_id, row, label = round(dd, digits = 2)), color="black", size=rel(4.5)) +
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
    subtitle = "Seasons 33-37"
  ) +
  xlab(NULL) +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(
    name = "Row"
  ) +
  scale_fill_gradient("Daily Double\nProbability", low = "white", high = "#060CE9") +
  coord_fixed()

# let's just get the data we need
df_full <- full_games %>%
  filter(round %in% c("J", "DJ")) %>%
  select(season, game_id, round, dd, category_id, row, clue_order) %>%
  mutate(clue_pos = category_id + 6*(row-1),
         clue_order = as.integer(clue_order))

# this is a multiclass classification problem with 30 classes, representing the 30
# let's just start with the jeopardy round

j <- df %>%
  filter(round == "J") %>%
  arrange(season, game_id, clue_order)

# so basically each row should have a feature which say whether or not the clue at
# each position is the daily double if revealed


######################################### THis is the machine learning stuff, I'll set this to the side

test <- data.frame(matrix(ncol = 30, nrow = dim(j)[1]))
colnames(test) <- paste0("clue_", 1:30)

j <- cbind(j, test)

test_fun <- function(last_clue) {
  assign(paste0("clue_", last_clue), 0)
}

j %>%
  group_by(game_id) %>% # make sure to group by round if j and dj are included
  mutate(last_clue = lag(clue_pos)) %>%
  select(season, game_id, round, dd, category_id, row, clue_order, clue_pos, last_clue, everything())
  # basically want to look at the last clue (c), and then mark a 0 for clue_c


######################################## This is looking only at daily doubles

dd <- df %>%
  filter(dd==TRUE)

dd %>%
  group_by(game_id, round) %>%
  mutate(l = lag(category_id)) %>%
  ungroup() %>%
  summarize(sum(l == category_id, na.rm = TRUE)/n())
# daily doubles in double jeopardy have never appeared in the same column in the same game


dd %>%
  filter(round == "DJ") %>%
  group_by(game_id, round) %>%
  mutate(prev_dd = lag(row),
         next_dd = lead(row)) %>%
  ungroup() %>%
  summarize(sum((prev_dd == row) | (next_dd == row), na.rm = TRUE)/n())
# daily doubles in double jeopardy appear in the same row in about 24% of games in my sample

# In Double Jeopardy, how likely is the daily double to be in the same row as the uncovered DD?

df %>%
  arrange(game_id, round, clue_order)



# Questions:
# Show why daily doubles are valuable
#1. Where do Daily Doubles tend to be located on the Jeopardy Board?
#2. Is there a pattern in these locations?
#3. Can you predict these locations?
#4. Do contestants do a good job of picking clues in these locations?


