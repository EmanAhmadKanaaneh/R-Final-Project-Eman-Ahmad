# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggdist)
# ---- חלק א: הגדרת שאלת המחקר ----

# 1. בחירת דאטאסט ותיאורו
# הנתונים כוללים תוצאות של משחקי כדורגל בינלאומיים מ-1872 ועד 2024, כולל תוצאות משחקים רגילים, בעיטות עונשין, ומידע על כובשי שערים.
# הדאטאסט מכיל מידע על:
# - תוצאות משחקים (נבחרות, תוצאה ביתית וחיצונית, תאריך ומיקום המשחק).
# - תוצאות דו-קרב פנדלים (קבוצות משתתפות ומנצחת הפנדלים).
# - מידע על כובשי שערים בכל משחק.
# בחרתי בדאטאסט זה מכיוון שאני אוהבת כדורגל, ואני חושבת שיהיה מעניין לבדוק מה נותן יתרון לקבוצות.

# 2. הצגת נתונים ראשונית
results       <- read.csv("C:/Users/ahmad/Desktop/Master's/courses/R/International football results from 1872 to 2024/results.csv")
goalscorers   <- read.csv("C:/Users/ahmad/Desktop/Master's/courses/R/International football results from 1872 to 2024/goalscorers.csv")
shootouts     <- read.csv("C:/Users/ahmad/Desktop/Master's/courses/R/International football results from 1872 to 2024/shootouts.csv")
former_names  <- read.csv("C:/Users/ahmad/Desktop/Master's/courses/R/International football results from 1872 to 2024/former_names.csv")


# משתנה חדש 
results <- results %>%
  mutate(total_score = home_score + away_score)

# הצגת סטטיסטיקות כלליות
summary(results)

summary(shootouts)

# התפלגות שערים במשחקי בית וחוץ
ggplot(results, aes(x = home_score)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "התפלגות שערי בית במשחקים בינלאומיים", x = "שערי בית", y = "מספר משחקים")

ggplot(results, aes(x = away_score)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
  labs(title = "התפלגות שערי חוץ במשחקים בינלאומיים", x = "שערי חוץ", y = "מספר משחקים")

ggplot(results, aes(x = home_score)) +
  stat_halfeye(aes(fill = home_score), alpha = 0.7) +
  labs(title = "התפלגות שערי הבית במשחקים בינלאומיים", x = "שערי בית", y = "צפיפות")

ggplot(results, aes(x = away_score)) +
  stat_halfeye(aes(fill = away_score), alpha = 0.7) +
  labs(title = "התפלגות שערי החוץ במשחקים בינלאומיים", x = "שערי חוץ", y = "צפיפות")


# התפלגות לכל השערים 
ggplot(results, aes(x = total_score)) +
  geom_histogram(binwidth = 1, fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Total Goals (Home + Away)", 
       x = "Total Goals in a Match", 
       y = "Number of Matches")



#  השוואת משחקים עם הרבה שערים מול משחקים עם מעט שערים
results <- results %>%
  mutate(score_category = if_else(total_score >= 4, "High-Scoring", "Low-Scoring"))

ggplot(results, aes(x = score_category, y = home_score)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "השוואת משחקים עם הרבה שערים מול מעט שערים", x = "קטגוריית משחק", y = "שערי בית")

#  מגמות שערים לאורך השנים
results$date <- as.Date(results$date)
results$year <- as.numeric(format(results$date, "%Y"))

goal_trend <- results %>%
  group_by(year) %>%
  summarise(
    avg_home_score = mean(home_score, na.rm = TRUE),
    avg_away_score = mean(away_score, na.rm = TRUE),
    avg_total_score = mean(total_score, na.rm = TRUE)
  )

ggplot(goal_trend, aes(x = year)) +
  geom_line(aes(y = avg_home_score, color = "Avg Home Score")) +
  geom_line(aes(y = avg_away_score, color = "Avg Away Score")) +
  geom_line(aes(y = avg_total_score, color = "Avg Total Score")) +
  labs(title = "Goals Trend Over the Years",
       x = "Year", 
       y = "Average Goals") +
  scale_color_manual(name = "Type", 
                     values = c("Avg Home Score" = "blue", 
                                "Avg Away Score" = "red",
                                "Avg Total Score" = "purple"))
# ניצחון בפנדלים 
shootouts <- shootouts %>%
  mutate(
    is_home_winner = if_else(winner == home_team, 1, 0),
    is_away_winner = if_else(winner == away_team, 1, 0)
  )

df2 <- data.frame(
  team = c("Home", "Away"),
  wins = c(
    sum(shootouts$is_home_winner, na.rm = TRUE),
    sum(shootouts$is_away_winner, na.rm = TRUE)
  )
)

ggplot(df2, aes(x = team, y = wins)) +
  geom_col()

shootouts_clean <- shootouts %>%
  filter(!is.na(first_shooter)) %>% 
  mutate(is_first_shooter_win = if_else(winner == first_shooter, 1, 0))

df2 <- data.frame(
  shooter = c("First Shooter", "Second Shooter"),
  wins = c(
    sum(shootouts_clean$is_first_shooter_win, na.rm = TRUE),
    sum(shootouts_clean$is_first_shooter_win == 0, na.rm = TRUE)
  )
)

ggplot(df2, aes(x = shooter, y = wins)) +
  geom_col()

#ניצחון בבית / בחוץ 

home_wins <- sum(results$home_score > results$away_score, na.rm = TRUE)
away_wins <- sum(results$away_score > results$home_score, na.rm = TRUE)

df2 <- data.frame(
  winner = c("Home Wins", "Away Wins"),
  count  = c(home_wins, away_wins)
)


ggplot(df2, aes(x = winner, y = count)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Number of Home vs. Away Wins",
    x = "Winner",
    y = "Count of Matches"
  )

# 3. ניסוח שאלת המחקר
# “Do home teams have a higher probability of winning and a larger margin of victory in international football matches, and does this effect differ between friendly matches and competitive matches?”