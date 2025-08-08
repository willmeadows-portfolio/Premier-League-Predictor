library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(readxl)


#loading in dataset for model
prem_data <- read_excel("test_dataset.xlsx")

#creating dataset to run model off
season22 <- apply(prem_data, 1, function(row){
  data.frame(team = c(row['home_team'], row['away_team']),
             opponent = c(row['away_team'], row['home_team']),
             goals = c(row['home_goals'], row["away_goals"]),
             home = c(1, 0))
})
season22 <- do.call(rbind, season22)

#changing goals variable to number variable
season22$goals <- as.numeric(as.character(season22$goals))

#creating model
model <- glm(goals ~ home + team + opponent,
             family = poisson(link=log), data = season22)




output <- summary(model)
names(output)
output$coefficients


setwd("C:\\Users\\willz\\OneDrive\\University\\MTH3035 Group Project (Year 3)")
pred_season <- read_excel("new_epl-2022-UTC.xlsx") %>%
#selecting relevant variables
  select(-c("Location", "Match Number")) %>%
  #putting variables in snake case
  rename(round_number = "Round Number",
         date = Date,
         home_team = "Home Team",
         away_team = "Away Team",
         result = "Result") %>%
  #changing variable to character variable
  mutate(round_number = as.character(round_number))

#splitting the result column into home and away goals
pred_season_extra <- pred_season %>%
  mutate(home_goals = str_extract(result, "^\\d*\\s"),
         away_goals = str_extract(result, "\\s\\d*$")) %>%
  mutate(home_goals = str_remove(home_goals, "\\s"),
         away_goals = str_remove(away_goals, "\\s")) %>%
  mutate(game = row_number()) %>%
  #removing unnecessary column
  select(-result, - round_number) %>%
  select(game, everything()) %>%
  #duplicating every game
  slice(rep(1:n(), each = 2)) %>%
  #creating home variable
  mutate(home_adv_first = row_number()) %>%
  mutate(home_adv = case_when(home_adv_first%%2== 0 ~ "0",
                              TRUE ~ "1")) %>%
  select(-home_adv_first) %>%
  #creating model variables
  mutate(team = case_when(home_adv == "1" ~ home_team,
                          home_adv == "0" ~ away_team),
         opponent = case_when(home_adv == "1" ~ away_team,
                              home_adv == "0" ~ home_team)) %>%
  select(game, date, team, opponent, home_adv, home_goals, away_goals) %>%
  rename(home = home_adv) %>%
  mutate(home = as.numeric(home))

View(pred_season_extra)

#getting rid of Nottingham Forest for the model
predicting <- pred_season_extra %>%
  filter(team != "Nottingham Forest",
         opponent != "Nottingham Forest")

#predicting results
predictions <- predicting %>%
  mutate(model_mean = predict(model, newdata = predicting ,type = "response"),
         simulated_score = rpois(684, model_mean))

#Evaluating the results
flagging_scores <- predictions %>%
  mutate(correct_score = case_when(home == "1" & home_goals == simulated_score ~ "Y",
                                   home == "0" & away_goals == simulated_score ~ "Y",
                                   TRUE ~ "N"))

counts <- flagging_scores %>%
  filter(correct_score == "N")

nrow(counts)


write.csv(flagging_scores, "Flagging Scores.csv")

# Tests

t_test_data <- read.csv("t tests.csv")
View(t_test_data)
head(t_test_data)

#Home
result_home <- t.test(t_test_data$home_goals, t_test_data$simulated_home_goals, paired = TRUE)
print(result_home)

#Away
result_away <- t.test(t_test_data$away_goals, t_test_data$simulated_away_goals, paired = TRUE)
print(result_away)

#Total
result_total <- t.test(t_test_data$Total_Goals, t_test_data$Total_Sim_goals, paired = TRUE)
print(result_total)


View(flagging_scores)
View(t_test_data)
