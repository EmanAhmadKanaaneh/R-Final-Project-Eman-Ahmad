# Load required libraries
library(tidyverse)
library(dplyr)
library(ROCR)  

# ---- חלק ב: עיבוד מקדים של הנתונים ----

# 1. הגדרת משתנים מנבאים ומנובאים
# רגרסיה לוגיסטית
# -----------------
# משתנה תלוי (DV):
# - home_win (1 אם קבוצת הבית ניצחה, 0 אחרת)
#
# משתנים בלתי תלויים (IVs):
# - home_game (1 אם המשחק בבית, 0 אם בחוץ)
# - match_type (1 אם המשחק תחרותי, 0 אם משחק ידידות)
#
# רגרסיה לינארית
# -----------------
# משתנה תלוי (DV):
# - goal_diff (הפרש שערים: home_score - away_score)
#
# משתנים בלתי תלויים (IVs):
# - home_game (1 אם המשחק בבית, 0 אם בחוץ)
# - match_type (1 אם המשחק תחרותי, 0 אם משחק ידידות)


# 2. עיבוד נתונים בעזרת dplyr


# עיבוד בעזרת dplyr על מנת להכין משתני עזר לבדיקה:
# 1) יצירת עמודת home_win (0/1) - האם קבוצת הבית ניצחה
# 2) יצירת עמודת goal_diff (הפרש שערים לטובת הבית)
# 3) יצירת עמודת match_type (0=Friendly, 1=Competitive)
# 4) הוספת year (אם רוצים לבחון השפעות לאורך השנים)

results_prepared <- results %>%
  mutate(
    # המרת תאריך לסוג Date
    date = as.Date(date),
    
    # הוצאת השנה מהתאריך)
    year = as.integer(format(date, "%Y")),
    
    # משתנה בינארי המייצג ניצחון של קבוצת הבית
    home_win = if_else(home_score > away_score, 1, 0),
    
    # הפרש שערים לטובת קבוצת הבית
    goal_diff = home_score - away_score,
    
    # הגדרת סוג המשחק: 0=Friendly, 1=כל דבר אחר (תחרותי)
    match_type = if_else(tournament == "Friendly", 0, 1)
  ) %>%
  select(
    date, year, home_team, away_team,
    home_score, away_score,
    home_win, goal_diff, match_type, tournament
  ) %>%
  arrange(desc(date))  

# הצצה לנתונים שעיבדנו
head(results_prepared)


# 3. יצירת פונקציה מותאמת אישית

calculate_home_advantage <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      total_matches    = n(),
      home_win_rate    = mean(home_win,  na.rm = TRUE),
      avg_goal_diff    = mean(goal_diff, na.rm = TRUE),
      .groups = "drop"
    )
}
results_expanded <- results_prepared %>%
  pivot_longer(cols = c(home_team, away_team), 
               names_to = "location", 
               values_to = "team") %>%
  mutate(
    home_game = if_else(location == "home_team", 1, 0)
  ) %>%
  select(date, year, team, home_game, match_type, home_score, away_score, goal_diff, home_win, tournament)

# הצצה לנתונים המעובדים
head(results_expanded)


# הפעלת הפונקציה על הדאטה (שכבר עיבדנו בשלב הקודם)
# לדוגמה, נבצע קיבוץ לפי סוג משחק (match_type) כדי לראות
# את המדדים במשחקים ידידותיים (0) לעומת תחרותיים (1)

home_adv_summary <- calculate_home_advantage(results_prepared, "match_type")
home_adv_summary

# 4. שימוש בפונקציה מחבילה שלא למדנו במהלך הקורס 
library(skimr)
skim(results_prepared)


