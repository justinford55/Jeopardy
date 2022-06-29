library(tidyverse)
library(rvest)
library(extrafont)
loadfonts(device = "win")

# this includes one row for every buzz in the Jeopardy/DJ rounds
# It includes multiple rows for some clues
buzzes <- readRDS("data.rds")

buzzes <- buzzes %>%
  mutate(clue_order = as.integer(clue_order))

# this had only one row per clue, and each clue is denoted as being answered correctly or not
clues <- buzzes %>%
  filter(r_or_w == "right" | buzz_name == "Triple Stumper" | dd == TRUE) %>%
  distinct(season, game_id, round, category_name, clue, corr_resp, category_id, row, clue_value, dd, clue_order,
           dd_wager, r_or_w)

 


# Things I want to include:
#    - the selecting contestant (eventually)

# this is the percentage of times that each clue location has been revealed to be a DD
# IE the denominator is the number of times the clue has been revealed in total
clues %>%
  filter(round == "J") %>%
  group_by(category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  pivot_wider(names_from = category_id, values_from = dd)


# visualization
clues %>%
  filter(round %in% c("J", "DJ")) %>%
  mutate(row = ordered(row, levels = c(5, 4, 3, 2, 1)),
         category_id = as.factor(category_id),
         round = ordered(round, levels = c("J", "DJ"))) %>%
  group_by(round, category_id, row) %>%
  summarize(dd = sum(dd)/n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = category_id, y = row, fill = dd), color = "black") +
  facet_wrap(~round, labeller = labeller(round = c("J" = "Jeopardy!", "DJ" = "Double Jeopardy!"))) +
  geom_text(aes(category_id, row, label = round(dd, digits = 2)), color="black", size=rel(4.5)) +
  theme(
    plot.background = element_rect(fill = "#00003a", color = NA),
    plot.margin = unit(c(2,1,2,1), "cm"),
    panel.background = element_rect(fill = "#00003a", color = "white"),
    strip.background = element_rect(fill = "#00003a"),
    strip.text = element_text(color = "#E5A561", size = 12, family = "Gyparody Hv"),
    text = element_text(color = "#E5A561", family = "Trebuchet MS"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(color = "#E5A561"),
    legend.background = element_rect(fill = "#00003a", color = NA),
    axis.ticks.y = element_blank(),
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
    name = "",
    breaks = c()
  ) +
  scale_fill_gradient("Daily Double\nProbability", low = "white", high = "#060CE9") +
  coord_fixed()


# This tries to create a clue_position
clues %>%
  filter(round %in% c("J", "DJ")) %>%
  select(season, game_id, round, dd, category_id, row, clue_order) %>%
  mutate(clue_pos = category_id + 6*(row-1),
         clue_order = as.integer(clue_order))

# this is a multiclass classification problem with 30 classes, representing the 30
# let's just start with the jeopardy round

j <- clues %>%
  filter(round == "J") %>%
  arrange(season, game_id, clue_order)

# so basically each row should have a feature which says whether or not the clue at
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

# daily doubles only
dd <- clues %>%
  filter(dd==TRUE)

# daily doubles in double jeopardy have never appeared in the same column in the same game
dd %>%
  group_by(game_id, round) %>%
  mutate(l = lag(category_id)) %>%
  ungroup() %>%
  summarize(sum(l == category_id, na.rm = TRUE)/n())

#  this looks at how often double jeopardy dd appear in the same row in the same game
dd %>%
  filter(round == "DJ") %>%
  group_by(game_id, round) %>%
  mutate(prev_dd = lag(row),
         next_dd = lead(row)) %>%
  ungroup() %>%
  summarize(sum((prev_dd == row) | (next_dd == row), na.rm = TRUE)/n())
# daily doubles in double jeopardy appear in the same row in about 26% of games in my sample from S33-37

# How often do daily doubles appear in each row
dd %>%
  filter(round == "DJ") %>%
  group_by(row) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n))

# In Double Jeopardy, how likely is the daily double to be in the same row as the uncovered DD?
# Answer: the row of DDs are independent from each other.

# Is there some kind of pattern?
dd %>%
  filter(round == "J") %>%
  select(game_id, round, category_id, row)

# scratch
clues %>%
  filter(round == "DJ") %>%
  mutate(row = ordered(row, levels = c(5, 4, 3, 2, 1)),
         category_id = as.factor(category_id),
         round = ordered(round, levels = c("J", "DJ"))) %>%
  group_by(round, category_id, row) %>%
  summarize(num_dd = sum(dd), dd = sum(dd)/n(), count = n())


# Questions:
# Show why daily doubles are valuable
# How much money do contestants how reveal the daily double win on that clue?
dd %>%
  mutate(clue_value = as.integer(gsub("[^0-9.-]", "", clue_value))) %>%
  mutate(plus_minus = ifelse(r_or_w == "right", 1, -1)) %>%
  mutate(clue_result = clue_value * plus_minus) %>%
  select(-dd_wager) %>%
  group_by(round, row) %>%
  summarize(mean_result = mean(clue_result),
            n = n())

# this has a row for every buzz
# looking at how much a contestant gains when buzzing in on a clue in a particular row/category
buzzes %>%
  filter(!dd & buzz_name != "Triple Stumper") %>%
  mutate(clue_value = as.integer(gsub("[^0-9.-]", "", clue_value))) %>%
  mutate(plus_minus = ifelse(r_or_w == "right", 1, -1)) %>%
  mutate(clue_result = clue_value * plus_minus) %>%
  select(-dd_wager) %>%
  group_by(round, row) %>%
  summarize(mean_result = mean(clue_result),
            n = n())



buzzes %>%
  filter(!dd)

#1. Where do Daily Doubles tend to be located on the Jeopardy Board?
#2. Is there a pattern in these locations?
#3. How valuable are daily doubles?
#4. Do contestants do a good job of picking clues in these locations?

# could do a barplot with this
# ALERT: THIS SHOULD ONLY WORK WITH DATA THAT HAS ONLY ONE ROW PER CLUE
clues %>%
  group_by(round, row) %>%
  filter(dd == FALSE) %>%
  summarize(n = n(),
            percent_correct = sum(r_or_w == "right")/ n()) %>%
  mutate(single_cont_prob = 1 - (1 - percent_correct)^(1/3))
# single_cont_prob estimates the probability that the average contestant would get the correct answer
# if only they were playing.

clues %>%
  group_by(round, row, dd) %>%
  summarize(n = n(),
            percent_correct = sum(r_or_w == "right")/n()) %>%
  mutate(row = ordered(row, 5:1)) %>%
  ggplot() +
  aes(row, percent_correct) +
  geom_col(aes(fill = round), position = "dodge", color = "white") +
  facet_wrap(~dd, labeller = labeller(dd = c("FALSE" = "Regular Clue", "TRUE" = "Daily Double"))) +
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
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Proportion of Correct Responses",
    x = "Row on Board",
    title = "Percentage of Correct Responses by Row",
    subtitle = "Seasons 33-37"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual("Round", values = c("#0000AF", "#E5A561"),
                    breaks=c("J", "DJ"))

# percentage of DDs answered correctly, by round and row
clues %>%
  group_by(round, row) %>%
  filter(dd == TRUE) %>%
  summarize(n = n(), 
            percent_correct = sum(r_or_w == "right")/ n())
# Daily doubles are answered more correctly than the probabilities in the previous df would suggest.
# Good contestants are more likely to reveal daily double clues, so daily double answer probability is higher.


# How valuable are daily doubles?
# this has triple stumpers where nobody rang in.

clues %>%
  mutate(clue_value = as.integer(gsub("[^0-9.-]", "", clue_value))) %>%
  mutate(plus_minus = ifelse(r_or_w == "right", 1, -1)) %>%
  mutate(clue_result = clue_value * plus_minus) %>%
  select(-dd_wager)

# selecting contestant
w <- buzzes %>%
  filter(r_or_w == "right" | buzz_name == "Triple Stumper" | dd == TRUE) %>%
  arrange(desc(season), game_id, desc(round), clue_order) %>%
  group_by(game_id, round) %>%
  mutate(prev_buzzer = lag(buzz_name),
         prev_r_w = lag(r_or_w)) %>%
  select(-category_name, -clue, -corr_resp) %>%
  # selector is the contestant who got the previous question correct
  mutate(selector = ifelse(prev_r_w == "right", prev_buzzer, NA)) %>%
  fill(selector, .direction = "down") %>%
  select(-prev_buzzer, -prev_r_w)

no_select <- w %>%
  filter(is.na(selector))

# need to write a function to determine the selector of clue 1 for each round

get_first_clue_selector <- function() {
  # first determine if the round is J or DJ
  if (round == "J") {
    # the selector for the first clue in J round should be the returning champ
    # I believe that this is always "contestant 1"
    # I want this to be the first name of the contestant (the name that is used in the clue data)
    
    #pseudo code
    # scrape contestants for the game
    # probably best not to do the scraping here for time purposes
    # actually its probably just best to have a function that scrapes all the first selectors
    
  }
  
  else {
    # this selector should be the lowest scoring player in the jeopardy round.
    # if there is a tie, I'm not sure how it is determined, but it is noted on j-archive who selected first
    
  }

}

table(no_select$clue_order)

test <- w %>%
  filter(game_id == "5899")


