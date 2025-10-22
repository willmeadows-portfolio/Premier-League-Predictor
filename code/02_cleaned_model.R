# ============================================================
# Premier League Goal Prediction Model
# ============================================================
# This script builds a Poisson regression model to predict football match goals.
# It uses sample Premier League data for demonstration.
# ------------------------------------------------------------
# Author: William Meadows
# Repository: willmeadows-portfolio/Premier-League-Predictor
# ============================================================

# ---- Load libraries ----
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(here)

# ---- 1. Load training data ----
# The dataset should include: home_team, away_team, home_goals, away_goals
prem_data <- read_excel("data/sample_test_dataset.xlsx")

# ---- 2. Prepare data for Poisson regression ----
# Each match is split into two rows (home and away) so we can model team performance
season_data <- prem_data %>%
  rowwise() %>%
  do(data.frame(
    team = c(.$home_team, .$away_team),
    opponent = c(.$away_team, .$home_team),
    goals = c(.$home_goals, .$away_goals),
    home = c(1, 0)
  )) %>%
  ungroup()

# Ensure goals are numeric
season_data$goals <- as.numeric(season_data$goals)

# ---- 3. Fit Poisson regression model ----
# Predict number of goals based on team, opponent, and home advantage
model <- glm(goals ~ home + team + opponent,
             family = poisson(link = "log"),
             data = season_data)

summary(model)

# ---- 4. Load data for prediction ----
# The dataset should include columns: Round Number, Date, Home Team, Away Team, Result
pred_season <- read_excel("data/sample_epl_data.xlsx") %>%
  select(-c(Location, `Match Number`)) %>%
  rename(
    round_number = `Round Number`,
    date = Date,
    home_team = `Home Team`,
    away_team = `Away Team`,
    result = Result
  ) %>%
  mutate(round_number = as.character(round_number))

# ---- 5. Process results to separate home and away goals ----
pred_season_extra <- pred_season %>%
  mutate(
    home_goals = str_extract(result, "^\\d+"),
    away_goals = str_extract(result, "\\d+$"),
    home_goals = as.numeric(home_goals),
    away_goals = as.numeric(away_goals),
    game = row_number()
  ) %>%
  select(game, date, home_team, away_team, home_goals, away_goals)

# ---- 6. Create prediction dataset ----
# Duplicate each match for home/away to allow prediction
pred_input <- pred_season_extra %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(
    home = rep(c(1, 0), nrow(pred_season_extra)),
    team = if_else(home == 1, home_team, away_team),
    opponent = if_else(home == 1, away_team, home_team)
  ) %>%
  select(game, date, team, opponent, home)

# ---- 7. Generate expected goals ----
pred_input$predicted_goals <- predict(model, newdata = pred_input, type = "response")

# ---- 8. Summarise results ----
pred_summary <- pred_input %>%
  group_by(game) %>%
  summarise(
    home_team = first(if_else(home == 1, team, opponent)),
    away_team = first(if_else(home == 0, team, opponent)),
    home_xG = predicted_goals[home == 1],
    away_xG = predicted_goals[home == 0]
  )

# ---- 9. Display results ----
print(pred_summary)

# ---- 10. Optional: Plot expected goals ----
ggplot(pred_summary, aes(x = home_team, y = home_xG)) +
  geom_col(fill = "steelblue") +
  labs(title = "Predicted Home Expected Goals", x = "Team", y = "Expected Goals") +
  theme_minimal()

# ============================================================
# End of script
# ============================================================
