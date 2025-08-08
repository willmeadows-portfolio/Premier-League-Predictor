# ⚽ Premier League Match Outcome Predictor

Welcome! This project is a statistical model designed to predict the number of goals scored in English Premier League matches using historical match data. It was developed as part of my final year at the University of Exeter, where I studied Mathematics with Economics.

The goal was to explore how well a Poisson regression model could forecast football match outcomes, using past performance and home advantage as predictors.

---

## 🔍 What this project does

- Cleans and restructures raw Premier League match data
- Uses Poisson regression to model expected goals per team per game
- Generates simulated scores and compares them to actual match results
- Evaluates the accuracy of predictions using paired t-tests

---

## 🧠 Key Concepts & Techniques

- **Poisson regression** for modelling count data (goals)
- **Feature engineering**: converting match-level data into team-level structure
- **Simulation** using `rpois()` to generate likely match scores
- **Evaluation** using paired t-tests on predicted vs actual goals

---

## 🛠 Tools Used

- **Language**: R
- **Libraries**: `dplyr`, `ggplot2`, `readxl`, `stringr`
- **Output**: CSV files for match predictions and evaluation metrics

---

## 📈 Evaluation

The model was assessed using:
- **Correct score flagging**: to track prediction accuracy for individual matches
- **Paired t-tests**: comparing actual vs predicted home goals, away goals, and total goals

---

## 🙋 What I learned

This was a valuable experience in applying statistical modelling to a real-world problem. I gained a better understanding of:

- Structuring data for predictive modelling
- The strengths and limitations of Poisson assumptions in sports analytics
- Communicating results clearly and testing model accuracy

---

## 🚧 Future Improvements

- Incorporate more features (e.g. recent form, player stats, possession)
- Build confidence intervals and probabilities around expected outcomes
- Use a Bayesian approach or machine learning models for comparison

---

## 📫 Contact

If you’d like to chat about the project or see more of my work, feel free to reach out:

📧 [william.james.meadows@gmail.com](mailto:william.james.meadows@gmail.com)  
  


