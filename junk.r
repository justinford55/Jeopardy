# Next I would love to be able to include who gave correct answers, incorrect answers and such

# So, a new clue is starting if:
# - The previous answer was right OR (done)
# - The previous clue was a "triple stumper" (done)
# - The previous answer was a daily double and was wrong (done)

# I want to include:
#    - at least the name of contestant who got the right answer
#         - ideally the names of all contestants who buzzed in


# I'm having trouble getting matching the right/wrong information with the clues

# get responses for this game
responses_page <- httr::GET("https://j-archive.com/showgameresponses.php?game_id=7342") |>
  httr::content(as = "text") |>
  read_html()


clues_and_buzzes <- responses_page %>%
  html_nodes("#jeopardy_round .wrong , #jeopardy_round .right, #jeopardy_round a") %>%
  html_text()

right_or_wrong <- responses_page %>%
  html_nodes("#jeopardy_round .wrong , #jeopardy_round .right, #jeopardy_round a") %>%
  html_attr("class")

clues_and_buzzes
right_or_wrong

# list for buzzes
clues_and_buzzes_clean <- c()

# this function should paste together contestant names for clues where multiple people buzz in
paste_names <- function(remaining_clues_and_buzzes) {
  
  # start with the first 
  to_append <- paste(remaining_clues_and_buzzes[1], remaining_clues_and_buzzes[2], sep = ", ")
  i <- 3
  
  # while in range of the list indices
  while (i < length(remaining_clues_and_buzzes)) {
    
    # if the next element of the list is another name to append to the string
    if (is.na(as.numeric(remaining_clues_and_buzzes[i]))) {
      to_append <- paste(to_append, remaining_clues_and_buzzes[i], sep = ", ")
    } else {
      return(to_append)
    }
    i <- i + 1
  }
  return(to_append)
}

i <- 1
# basically each clue should be a row in a dataframe
while (i <= length(clues_and_buzzes)) {
  
  # if the value at the index is a clue number, just append it to the new list
  if (!is.na(as.numeric(clues_and_buzzes[i]))) {
    print(i)
    clues_and_buzzes_clean <- append(clues_and_buzzes_clean, as.numeric(clues_and_buzzes[i]))
  }
  
  # if the value at the index is the name of the contestant buzzing in...
  else {
    
    # if the contestant is the only one to buzz in for this clue, just append their name to the new list.
    if (!is.na(as.numeric(lead(clues_and_buzzes)[i]))) {
      clues_and_buzzes_clean <- append(clues_and_buzzes_clean, clues_and_buzzes[i])
    }
    
    # otherwise, paste together all the names into one string, and append that to the list
    else {
      to_append <- paste_names(clues_and_buzzes[i:length(clues_and_buzzes)])
      clues_and_buzzes_clean <- append(clues_and_buzzes_clean, to_append)
      i <- i + length(to_append)
    }
  }
  i <- i + 1
  print(i)
}

clues_and_buzzes_clean

game <- full_games %>%
  filter(game_id == 7342, round == "J") %>%
  select(clue_order, dd)

game$clue_order[which(game$dd == TRUE)]


# a list of which contestants buzzed in clue order (not the order they appeared on the show)
# does not include final jeopardy
contestant_buzz <- responses_page %>%
  html_nodes(".round .clue_text td") %>%
  html_text()

# a list which corresponds to contestant_buzz, that shows whether the response was right or wrong
right_or_wrong <- responses_page %>%
  html_nodes(".round .clue_text td") %>%
  html_attr("class")

responses <- tibble(contestant_buzz, right_or_wrong)
responses$clue_num <- 1:dim(responses)[1]

responses

# So, a new clue is starting if:
# - The previous answer was right OR (done)
# - The previous clue was a "triple stumper" (done)
# - The previous answer was a daily double and was wrong (done)

# I want to include:
#    - at least the name of contestant who got the right answer
#         - ideally the names of all contestants who buzzed in
#

# 

game <- full_games %>%
  filter(game_id == 7342, round == "J") %>%
  select(clue_order, dd)

dd_pos <- as.numeric(game$clue_order[which(game$dd == TRUE)])