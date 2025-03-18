# Home Advantage in International Football: A Data Analysis with R  

## Project Overview  
This project analyzes home advantage in international football matches (1872–2024) to examine whether home teams have a higher probability of winning and whether this effect differs between friendly and competitive matches.  

## Research Question  
**Do home teams have a higher probability of winning and a larger margin of victory in international football matches, and does this effect differ between friendly and competitive matches?**  

## Files  
```
├── Final_Project_Part_1.R  # Data Exploration & Visualization  
├── Final_Project_Part_2.R  # Data Preprocessing  
├── Final_Project_Part_3.R  # Statistical Analysis & Visualization  
├── README.md               # Project Documentation  
└── data/                   # Dataset  
```

## Methods  
- **Exploratory Data Analysis:** Summary statistics, goal distributions, and trends over time.  
- **Data Preprocessing:** Feature creation (`home_win`, `goal_diff`, `match_type`), transformations using `dplyr`.  
- **Statistical Analysis:**  
  - Logistic regression for home win probability.  
  - Linear regression for goal difference.  
  - Visualization of effects using `ggplot2`.  
  - ROC curve analysis for model evaluation.  

## Key Findings  
- Competitive matches increase the probability of home wins and goal differences.  
- Direct home advantage was not statistically significant.  

## How to Run  
1. Install required R packages:  
   ```r
   install.packages(c("tidyverse", "dplyr", "ggplot2", "ggdist", "pROC", "ROCR", "skimr"))
   ```
2. Run scripts in order:  
   - `Final_Project_Part_1.R`  
   - `Final_Project_Part_2.R`  
   - `Final_Project_Part_3.R`  

