#changing goals variable to number variable
season22$goals <- as.numeric(as.character(season22$goals))
#creating model
model <- glm(goals ~ home + team + opponent,
             family = poisson(link=log), data = season22)
summary(model)
pred_season <- read_excel("epl-2022-UTC.xlsx")
pred_season <- read_excel("epl-2022-UTC.xlsx")
View(pred_season)
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
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
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
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
View(pred_season)
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
  #selecting relevant variables
  select(-c("Location", "Match Number"))
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
  #selecting relevant variables
  subset(select(-c("Location", "Match Number")))
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
  #selecting relevant variables
  subset(select(-("Location", "Match Number")))
#Loading dataset for predictions
pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
  #selecting relevant variables
  subset(select(-c("Location", "Match Number")))
pred_season <- read_excel("epl-2022-UTC.xlsx")
View(prem_data)
View(pred_season)
#selecting relevant variables
subset(select(-("Location"))
       View(pred_season)
       #selecting relevant variables
       subset(select(-("Location"))
              pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                #selecting relevant variables
                subset(select(-("Location"))
                       pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                         #selecting relevant variables
                         subset(select(-("Location"))
                                pred_season <- read_excel("epl-2022-UTC.xlsx")
                                pred_season <- read_excel("epl-2022-UTC.xlsx")
                                #Loading dataset for predictions
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(-("Location"))
                                #Loading dataset for predictions
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(-"Location")
                                #Loading dataset for predictions
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(-Location)
                                drop <- c("Location","Match Number")
                                drop <- c("Location","Match Number")
                                pred_season <- pred_season[,!(names(pred_season) %in% drop)]
                                View(pred_season)
                                #putting variables in snake case
                                pred_season %>%  rename(round_number = "Round Number",
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
                                library(dplyr)
                                library(ggplot2)
                                library(readr)
                                library(stringr)
                                library(readxl)
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
                                pred_season_extra <- pred_season %>%
                                  mutate(home_goals = str_extract(result, "^\\d*\\s"),
                                         away_goals = str_extract(result, "\\s\\d*$")) %>%
                                  mutate(home_goals = str_remove(home_goals, "\\s"),
                                         away_goals = str_remove(away_goals, "\\s")) %>%
                                  mutate(game = row_number())
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
                                pred_season <- read_excel("epl-2022-UTC.xlsx")
                                #Loading dataset for predictions
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(-Location)
                                drop <- c("Location","Match Number")
                                pred_season <- pred_season[,!(names(pred_season) %in% drop)]
                                #putting variables in snake case
                                pred_season %>%  rename(round_number = "Round Number",
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
                                  mutate(game = row_number())
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
                                summary(model)
                                library(dplyr)
                                pred_season <- read_excel("epl-2022-UTC.xlsx")
                                View(pred_season)
                                library(dplyr)
                                library(ggplot2)
                                library(readr)
                                library(stringr)
                                library(readxl)
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
                                pred_season <- read_excel("epl-2022-UTC.xlsx")
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
                                View(pred_season)
                                glimpse(pred_season)
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
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
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(pred_season,-c("Location", "Match Number")) %>%
                                  #putting variables in snake case
                                  rename(round_number = "Round Number",
                                         date = Date,
                                         home_team = "Home Team",
                                         away_team = "Away Team",
                                         result = "Result") %>%
                                  #changing variable to character variable
                                  mutate(round_number = as.character(round_number))
                                ?select()
                                pred_season <- read_excel("epl-2022-UTC.xlsx") %>%
                                  #selecting relevant variables
                                  select(.data,-c("Location", "Match Number")) %>%
                                  #putting variables in snake case
                                  rename(round_number = "Round Number",
                                         date = Date,
                                         home_team = "Home Team",
                                         away_team = "Away Team",
                                         result = "Result") %>%
                                  #changing variable to character variable
                                  mutate(round_number = as.character(round_number))
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
                                setwd("C:\\Users\\willz\\OneDrive\\Documents")
                                epl_past = read_csv("Prem results from 2019-2022 (no covid season).csv")
                                View(prem_data)
                                gc()
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
                                summary(model)
                                setwd("C:\\Users\\willz\\OneDrive\\University\\MTH3035 Group Project (Year 3)")
                                pred_season <- read_csv("new_epl-2022-UTC.csv")
                                View(pred_season)
                                pred_season %>%
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
                                pred_season <- read_csv("new_epl-2022-UTC.csv")
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
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
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
                                View(pred_season)
                                ?select()
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
                                  #selecting relevant variables
                                  pred_season <-select(pred_season,-c("Location", "Match Number"))
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
                                  #selecting relevant variables
                                  pred_season <-select(pred_season,-"Location")
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
                                  #selecting relevant variables
                                  pred_season <-select(pred_season,-c("Location"))
                                source("~/.active-rstudio-document", echo=TRUE)
                                detach("package:MASS", unload = TRUE)
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
                                  #selecting relevant variables
                                  pred_season <-select(pred_season,-c("Location"))
                                pred_season <- read_csv("new_epl-2022-UTC.csv") %>%
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
                                pred_season_extra
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
                                library(dplyr)
                                