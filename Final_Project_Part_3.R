#  Load required libraries
library(tidyverse)
library(dplyr)
library(pROC)
library(ggplot2)
# ---- חלק ג: ניתוח הנתונים ----

# 1. ביצוע ניתוחים סטטיסטיים - רגרסיה לינארית מרובה ורגרסיה לוגיסטית
# רגרסיה לוגיסטית - ניבוי ניצחון קבוצת הבית
logistic_model <- glm(home_win ~ home_game + match_type, 
                      data = results_expanded, 
                      family = binomial)

summary(logistic_model)

# רגרסיה לינארית - ניבוי הפרש השערים
linear_model <- lm(goal_diff ~ home_game + match_type, 
                   data = results_expanded)

summary(linear_model)

# 2.הסבר התוצאות 

# תוצאות הרגרסיה הלוגיסטית:
# החיתוך (Intercept) הוא -0.1079, כלומר במשחקי חוץ וידידות הסיכוי לניצחון נמוך יחסית. 
# משחק תחרותי (match_type) משפיע באופן חיובי עם מקדם של 0.1093, מה שמעיד כי במשחקים תחרותיים הסיכוי לניצחון של קבוצת הבית גבוה יותר (p < 0.001).  
# עם זאת, המשחק עצמו בבית (home_game) קיבל מקדם זניח של 3.589e-14 והוא אינו מובהק סטטיסטית (p = 1), כלומר אין עדות לכך שלמשחק בית יש השפעה ישירה על סיכויי הזכייה.

# תוצאות הרגרסיה הלינארית:
# החיתוך (Intercept) הוא 0.506, כלומר במשחקי חוץ וידידות, קבוצת הבית מובילה בממוצע ב-0.506 שערים.  
# משחק תחרותי (match_type) נמצא כבעל השפעה חיובית עם מקדם של 0.1126 (p < 0.001), מה שמעיד כי במשחקים תחרותיים קבוצת הבית מנצחת בהפרש ממוצע גבוה יותר.  
# מנגד, משחק בבית (home_game) קיבל מקדם זניח של -3.015e-16 והוא אינו מובהק (p = 1), כלומר אין עדות לכך שהמשחק בבית משפיע ישירות על הפרש השערים.


# 3. גרפים לאפקטים
# חיזוי הסתברויות לניצחון
predicted_probs <- data.frame(
  match_type = c(0, 1),
  predicted_win = predict(logistic_model, 
                          newdata = data.frame(home_game = 1, match_type = c(0, 1)), 
                          type = "response")
)

# גרף ההסתברויות לניצחון
ggplot(predicted_probs, aes(x = factor(match_type), y = predicted_win)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Probability of Home Team Winning by Match Type",
       x = "Match Type (0 = Friendly, 1 = Competitive)",
       y = "Predicted Probability of Winning") +
  theme_minimal()

# חיזוי הפרש שערים
predicted_goal_diff <- data.frame(
  match_type = c(0, 1),
  predicted_diff = predict(linear_model, 
                           newdata = data.frame(home_game = 1, match_type = c(0, 1)))
)

# גרף הפרש השערים
ggplot(predicted_goal_diff, aes(x = factor(match_type), y = predicted_diff)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(title = "Predicted Goal Difference by Match Type",
       x = "Match Type (0 = Friendly, 1 = Competitive)",
       y = "Predicted Goal Difference") +
  theme_minimal()


# יצירת נתונים לחיזוי לפי שני המשתנים
predicted_probs <- expand.grid(home_game = c(0, 1), match_type = c(0, 1))
predicted_probs$predicted_win <- predict(logistic_model, 
                                         newdata = predicted_probs, 
                                         type = "response")

# גרף השפעת משחק בית וסוג המשחק על סיכוי לניצחון
ggplot(predicted_probs, aes(x = factor(match_type), y = predicted_win, fill = factor(home_game))) +
  geom_col(position = "dodge") +
  labs(title = "Probability of Winning by Match Type & Home/Away Game",
       x = "Match Type (0 = Friendly, 1 = Competitive)",
       y = "Predicted Probability of Winning",
       fill = "Home Game (1=Yes, 0=No)") +
  theme_minimal()

# יצירת נתונים לחיזוי הפרש שערים לפי שני המשתנים
predicted_goal_diff <- expand.grid(home_game = c(0, 1), match_type = c(0, 1))
predicted_goal_diff$predicted_diff <- predict(linear_model, 
                                              newdata = predicted_goal_diff)

# גרף השפעת משחק בית וסוג המשחק על הפרש השערים
ggplot(predicted_goal_diff, aes(x = factor(match_type), y = predicted_diff, fill = factor(home_game))) +
  geom_col(position = "dodge") +
  labs(title = "Predicted Goal Difference by Match Type & Home/Away Game",
       x = "Match Type (0 = Friendly, 1 = Competitive)",
       y = "Predicted Goal Difference",
       fill = "Home Game (1=Yes, 0=No)") +
  theme_minimal()


# 4. הצגת גרף ROC עם AUC
# חישוב הערכים החזויים של המודל
roc_curve <- roc(results_expanded$home_win, 
                 predict(logistic_model, type = "response"))

# שרטוט עקומת ROC
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression")

# הוספת ערך AUC לגרף
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)