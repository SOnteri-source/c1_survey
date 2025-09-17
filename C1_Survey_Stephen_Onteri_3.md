#Class_1_Survey
C1_Survey 2025
================
Stephen Onteri
2025-09-03

``` r
# Loading tidyverse
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.4.3

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'tidyr' was built under R version 4.4.3

    ## Warning: package 'readr' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

    ## Warning: package 'dplyr' was built under R version 4.4.3

    ## Warning: package 'stringr' was built under R version 4.4.3

    ## Warning: package 'forcats' was built under R version 4.4.3

    ## Warning: package 'lubridate' was built under R version 4.4.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

install.packages(“rmarkdown”)

# Q1. 1. Import class one survey data from our Github site (<https://github.com/kijohnson/Advanced-Data-Analysis>) (The dataset is called ‘Class 1 Survey Fall 2025 2.csv’), calling the R dataframe that you create C1survey.

``` r
# Importing the data and naming it C1_Survey
C1_Survey <- read.csv("https://raw.githubusercontent.com/kijohnson/Advanced-Data-Analysis/main/Class%201%20Survey%20Fall%202025%202.csv")
```

# Q2. 2. Determine: a. the number of observations (i.e. the number of people who filled out the survey) and b. the number of variables in the dataframe.

``` r
# Number of participants who filled the survey
nrow(C1_Survey)
```

    ## [1] 58

``` r
# Number of variables in C1_Survey
ncol(C1_Survey)
```

    ## [1] 27

# Q3. Rename the column variables to something shorter and that is descriptive of what the variable is about (for example like_dogs for the ‘Do you like dogs?’ question variable) and b. Display that they are renamed. Hints: You can use either the rename_with function or the names function.

``` r
# Rename columns in C1_Survey
C1_Survey <- C1_Survey %>%
  rename_with(~ c(
    "id", "like_cats", "like_dogs", "have_desert", "slogan", "fav_day",
    "larkORowl", "fav_food", "fav_drink", "fav_season", "fav_month",
    "hobby", "program", "specialization", "stat_software", "R_exp",
    "coding_comfort", "coding_length", "top_three", "public_health_interest",
    "fav_num", "bday", "bmonth", "country", "state", "city",
    "highest_educ_level"
  ), .cols = 1:27)
```

``` r
# Load dplyr
library(dplyr)

# Rename columns in C1_Survey
C1_Survey <- rename_with(
  C1_Survey,
  ~ c(
    "id", "like_cats", "like_dogs", "have_desert", "slogan", "fav_day",
    "larkORowl", "fav_food", "fav_drink", "fav_season", "fav_month",
    "hobby", "program", "specialization", "stat_software", "R_exp",
    "coding_comfort", "coding_length", "top_three", "public_health_interest",
    "fav_num", "bday", "bmonth", "country", "state", "city",
    "highest_educ_level"
  ),
  .cols = 1:27
)
```

``` r
# Check first few rows
head(C1_Survey)
```

    ##   id  like_cats  like_dogs                  have_desert
    ## 1  1        Yes        Yes                 My phone ofc
    ## 2  2         No        Yes                        Bible
    ## 3  3         No        Yes                        Bible
    ## 4  4 Ambivalent        Yes  atmospheric water generator
    ## 5  5         No Ambivalent                        Water
    ## 6  6         No        Yes                Phone Network
    ##                                         slogan  fav_day
    ## 1 "Living with purpose, leaving with meaning."   Friday
    ## 2             Every moment is a new beginning! Saturday
    ## 3                         Work hard, Play Hard Thursday
    ## 4                                                Friday
    ## 5    Thanks to God for his indescribable gifts Saturday
    ## 6                             Faith over Fear  Saturday
    ##                                      larkORowl
    ## 1                Lark ( I am a morning person)
    ## 2 Hummingbird (I am neither a lark nor an owl)
    ## 3                Lark ( I am a morning person)
    ## 4 Hummingbird (I am neither a lark nor an owl)
    ## 5                    Owl (I am a night person)
    ## 6                    Owl (I am a night person)
    ##                                                                fav_food
    ## 1 Injera with anything :-) (  it's an Ethiopian soft Traditional bread)
    ## 2                                                                 Stake
    ## 3                             Ethiopian Cusine-Doro Wot is my favourite
    ## 4                                       Steak Sandwich with chimichurri
    ## 5                                                          Chopped meat
    ## 6           Ethiopian food (Enjera+Doro Wot). M mom makes it the best. 
    ##                                  fav_drink fav_season fav_month
    ## 1                                   Fanta      Spring      June
    ## 2                                   Coffee       Fall September
    ## 3                                   Coffee       Fall September
    ## 4 Tequila lol Im jk I would say Ginger Ale     Spring       May
    ## 5                                    Water       Fall       May
    ## 6                       Vanilla Milkshake        Fall    August
    ##                hobby  program             specialization stat_software R_exp
    ## 1             chess  MPH only Epidemiology/Biostatistics          SPSS     2
    ## 2  Watching football MPH only Epidemiology/Biostatistics          SPSS     2
    ## 3             Soccer MPH only Epidemiology/Biostatistics          SPSS     2
    ## 4 Language Learning  MPH only Epidemiology/Biostatistics          SPSS     2
    ## 5             Soccer MPH only Epidemiology/Biostatistics          SPSS     1
    ## 6           Reading  MPH only Epidemiology/Biostatistics          SPSS     2
    ##   coding_comfort coding_length
    ## 1              2    0-3 months
    ## 2              2    0-3 months
    ## 3              2    0-3 months
    ## 4              2    0-3 months
    ## 5              2    0-3 months
    ## 6              2    4-6 months
    ##                                                                                                                                                                                                          top_three
    ## 1                                                                                          1. Code better\n2. Application in a real-world scenario\n3. How to interpret my analysis better for different audience 
    ## 2                                                                                                                             Use R comfortably, start coding confidently, and apply advanced methods to real data
    ## 3                                                                                                                                                                      Coding,data analysis in a very great detail
    ## 4 All the necessary skill set to be able to handle and interact with biostatisticians as part of a research team, such as improving my R skills and and learn a bit about how to handel complex regression models.
    ## 5                                                                                                                                                                                   Coding, R Studio, and Analysis
    ## 6                                       Being familiar with different databases, choosing the right statistical tool for data analysis, getting more familiar with coding in R and be proficient in data analysis 
    ##                                        public_health_interest fav_num     bday
    ## 1                             Health Insurance and Nutrition.      28       10
    ## 2                                                    Oncology       1       13
    ## 3                                                      cancer       7       18
    ## 4                                                      Cancer       3        5
    ## 5 Non communicable disease and quality of healthcare delivery      17       13
    ## 6                                                     Cancer        7 June 2nd
    ##       bmonth   country        state        city highest_educ_level
    ## 1          6  Ethiopia Addis Ketema Addis Ababa  Bachelor's degree
    ## 2          3       USA           MO   St. Louis  Bachelor's degree
    ## 3          6  Ethiopia  Addis Ababa Addis Ababa  Bachelor's degree
    ## 4 Septemeber  Ethiopia  Addis Ababa Addis Ababa    Doctoral degree
    ## 5          5  Ethiopia  Addis Ababa Addis Ababa    Master's degree
    ## 6      June  Ethiopia   Addis Ababa Addis Ababa  Bachelor's degree

# Q4. Write code to determine and display the number of factor, integer, numerical, and character variables there are in the C1survey dataset.

``` r
# Count number of variables by class
table(sapply(C1_Survey, class))
```

    ## 
    ## character   integer 
    ##        23         4

# Q5. 5. a. Using code, check the bday and bmonth variables for any unusual or missing values (hint: use table function). If you find any, b. describe what you will do with the unusual values in a comment before or immediately following the code that does it, and c. after you have cleaned up any unusual values, find the median bday and month.

``` r
# Check bday variable
table(C1_Survey$bday, useNA = "ifany")  # includes NA values
```

    ## 
    ##                               1              10              12              13 
    ##               2               2               2               4               3 
    ##              14          15-Nov              16              17              18 
    ##               1               1               2               2               3 
    ##              19               2              20              21              22 
    ##               1               1               1               4               2 
    ##              23              24          24-Jun              25              26 
    ##               1               2               1               2               1 
    ##              27              28              29               3              30 
    ##               4               2               2               1               1 
    ##              31               5               6               7               8 
    ##               2               1               2               1               1 
    ##               9 August 2nd 1997        June 2nd 
    ##               1               1               1

``` r
# Check bmonth variable
table(C1_Survey$bmonth, useNA = "ifany")  # includes NA values
```

    ## 
    ##                     1         10         11          2          3          4 
    ##          1          3          5          4          5          2          5 
    ##          5          6          7          8          9     August   December 
    ##          8          5          4          4          3          1          1 
    ##    January       june      June         May   November    October Septemeber 
    ##          1          1          1          1          1          1          1

# Plan for unusual or missing values:

# For bday: any values \<1 or \>31 or NA will be set to NA or corrected if we have proper info

# For bmonth: any values \<1 or \>12 or NA will be set to NA or corrected if known

``` r
# Convert bday and bmonth to numeric if they are not already
C1_Survey$bday <- as.numeric(C1_Survey$bday)
```

    ## Warning: NAs introduced by coercion

``` r
C1_Survey$bmonth <- as.numeric(C1_Survey$bmonth)
```

    ## Warning: NAs introduced by coercion

``` r
# Check the structure of these variables to confirm
str(C1_Survey[, c("bday", "bmonth")])
```

    ## 'data.frame':    58 obs. of  2 variables:
    ##  $ bday  : num  10 13 18 5 13 NA 1 6 17 20 ...
    ##  $ bmonth: num  6 3 6 NA 5 NA 5 4 6 11 ...

``` r
# Ensure bday and bmonth are numeric (handles factors imported from CSV)
C1_Survey$bday <- as.numeric(as.character(C1_Survey$bday))
C1_Survey$bmonth <- as.numeric(as.character(C1_Survey$bmonth))
```

``` r
# Clean unusual values
C1_Survey$bday[C1_Survey$bday < 1 | C1_Survey$bday > 31] <- NA
C1_Survey$bmonth[C1_Survey$bmonth < 1 | C1_Survey$bmonth > 12] <- NA
```

``` r
# Check for any remaining unusual or missing values
table(C1_Survey$bday, useNA = "ifany")
```

    ## 
    ##    1    2    3    5    6    7    8    9   10   12   13   14   16   17   18   19 
    ##    2    1    1    1    2    1    1    1    2    4    3    1    2    2    3    1 
    ##   20   21   22   23   24   25   26   27   28   29   30   31 <NA> 
    ##    1    4    2    1    2    2    1    4    2    2    1    2    6

``` r
table(C1_Survey$bmonth, useNA = "ifany")
```

    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11 <NA> 
    ##    3    5    2    5    8    5    4    4    3    5    4   10

``` r
# Calculate median bday and bmonth, ignoring NAs
median_bday <- median(C1_Survey$bday, na.rm = TRUE)
median_bmonth <- median(C1_Survey$bmonth, na.rm = TRUE)
```

# 6. Create a new variable called bseason that gives the season according to Northern Meteorological season in which respondents were born (winter = 12/1 to 2/29, spring = 3/1 to 5/31, summer = 6/1 to 8/31, fall = 9/1 to 11/30). b. Using the table function, print a table of Seasons in the columns and bmonths in the rows that allows you to check that the coding is correct c. Sum the columns using the addmargins function to answer the question of how many classmates were born in each season? Include your answer in the code.

``` r
library(dplyr)
```

``` r
C1_Survey <- C1_Survey %>%
  mutate(bseason = case_when(
    (bmonth == 12 & bday >= 1) | bmonth == 1 | bmonth == 2 ~ "Winter",
    bmonth >= 3 & bmonth <= 5 ~ "Spring",
    bmonth >= 6 & bmonth <= 8 ~ "Summer",
    bmonth >= 9 & bmonth <= 11 ~ "Fall",
    TRUE ~ NA_character_
  ))
```

# 

``` r
# Cross-tabulation of bmonth (rows) and bseason (columns)
table(C1_Survey$bmonth, C1_Survey$bseason, useNA = "ifany")
```

    ##       
    ##        Fall Spring Summer Winter <NA>
    ##   1       0      0      0      3    0
    ##   2       0      0      0      5    0
    ##   3       0      2      0      0    0
    ##   4       0      5      0      0    0
    ##   5       0      8      0      0    0
    ##   6       0      0      5      0    0
    ##   7       0      0      4      0    0
    ##   8       0      0      4      0    0
    ##   9       3      0      0      0    0
    ##   10      5      0      0      0    0
    ##   11      4      0      0      0    0
    ##   <NA>    0      0      0      0   10

``` r
# Create a cross-tabulation of bmonth (rows) vs bseason (columns)
season_table <- table(C1_Survey$bmonth, C1_Survey$bseason, useNA = "ifany")

# Sum the columns to get number of classmates born in each season
season_summary <- addmargins(season_table, margin = 2)  # margin = 2 sums columns

# Print the table with column sums
season_summary
```

    ##       
    ##        Fall Spring Summer Winter <NA> Sum
    ##   1       0      0      0      3    0   3
    ##   2       0      0      0      5    0   5
    ##   3       0      2      0      0    0   2
    ##   4       0      5      0      0    0   5
    ##   5       0      8      0      0    0   8
    ##   6       0      0      5      0    0   5
    ##   7       0      0      4      0    0   4
    ##   8       0      0      4      0    0   4
    ##   9       3      0      0      0    0   3
    ##   10      5      0      0      0    0   5
    ##   11      4      0      0      0    0   4
    ##   <NA>    0      0      0      0   10  10

``` r
# Answer: Number of classmates born in each season
# Winter: season_summary["Sum", "Winter"]
# Spring: season_summary["Sum", "Spring"]
# Summer: season_summary["Sum", "Summer"]
# Fall:   season_summary["Sum", "Fall"]
```

# Pick your favorite variable or variables to analyze, come up with a question you want to answer with that variable or variables, generate the code, and provide an answer to your question. Describe what you did using comments (i.e. #’s).

# Research Question: Which hobbies are most common among classmates?

``` r
# Check the distribution of the hobby variable, including missing values
table(C1_Survey$hobby, useNA = "ifany")  # counts per hobby
```

    ## 
    ## Any activity associated with football "The real football not the NFL")  (Playing in real life, playing in video games or watching the game) 
    ##                                                                                                                                           1 
    ##                                                                                                   anyting that involves making/fixing stuff 
    ##                                                                                                                                           1 
    ##                                                                                                                                    Canoeing 
    ##                                                                                                                                           1 
    ##                                                                                                                                      chess  
    ##                                                                                                                                           1 
    ##                                                                                                                                     Cooking 
    ##                                                                                                                                           2 
    ##                                                                                                                                    Cooking  
    ##                                                                                                                                           1 
    ##                                                                                                                                     Crochet 
    ##                                                                                                                                           1 
    ##                                                                                                                                  Crocheting 
    ##                                                                                                                                           1 
    ##                                                                                                                                     Dancing 
    ##                                                                                                                                           1 
    ##                                                                                            Gaming! Both Outdoor and Indoor(Board Games,PS5) 
    ##                                                                                                                                           1 
    ##                                                                                                                                   Gardening 
    ##                                                                                                                                           1 
    ##                                                                                                                                Gym workout  
    ##                                                                                                                                           1 
    ##                                                                                                                    Hanging out with friends 
    ##                                                                                                                                           1 
    ##                                                                                                                                      Hiking 
    ##                                                                                                                                           1 
    ##                                                                                                                                     hiking! 
    ##                                                                                                                                           1 
    ##                                                                                                                          Language Learning  
    ##                                                                                                                                           1 
    ##                                                                                                                                 Photography 
    ##                                                                                                                                           1 
    ##                                                                                                                                  Pickleball 
    ##                                                                                                                                           1 
    ##                                                                                                              Playing and Listening to Music 
    ##                                                                                                                                           1 
    ##                                                                                                                 Playing and watching sports 
    ##                                                                                                                                           1 
    ##                                                                                                                         Playing Bass Guitar 
    ##                                                                                                                                           1 
    ##                                                                                                                              Playing guitar 
    ##                                                                                                                                           2 
    ##                                                                                                                         Playing Lawn Tennis 
    ##                                                                                                                                           1 
    ##                                                                                                                        playing table tennis 
    ##                                                                                                                                           1 
    ##                                                                                                                              playing tennis 
    ##                                                                                                                                           1 
    ##                                                                                                                         Playing Video Games 
    ##                                                                                                                                           1 
    ##                                                                                                                                     reading 
    ##                                                                                                                                           1 
    ##                                                                                                                                     Reading 
    ##                                                                                                                                           6 
    ##                                                                                                                                    Reading  
    ##                                                                                                                                           1 
    ##                                                                                                                                     Running 
    ##                                                                                                                                           1 
    ##                                                                                                                                    Running  
    ##                                                                                                                                           1 
    ##                                                                                                                                      skiing 
    ##                                                                                                                                           1 
    ##                                                                                                                                      Soccer 
    ##                                                                                                                                           4 
    ##                                                                                                                  spend time with loved ones 
    ##                                                                                                                                           1 
    ##                                                                                                                                      sports 
    ##                                                                                                                                           1 
    ##                                                                                                                                 Star gazing 
    ##                                                                                                                                           1 
    ##                                                                                                                               Table tennis  
    ##                                                                                                                                           1 
    ##                                                                                                                                  Travelling 
    ##                                                                                                                                           1 
    ##                                                                                                                                Video games  
    ##                                                                                                                                           1 
    ##                                                                                                                           Watching football 
    ##                                                                                                                                           1 
    ##                                                                                                                              watching movie 
    ##                                                                                                                                           1 
    ##                                                                                                                             Watching movies 
    ##                                                                                                                                           1 
    ##                                                                                                                             Watching Movies 
    ##                                                                                                                                           1 
    ##                                                                                                                             watching soccer 
    ##                                                                                                                                           1 
    ##                                                                                                                             Watching soccer 
    ##                                                                                                                                           2 
    ##                                                                                                                             Watching Sports 
    ##                                                                                                                                           1 
    ##                                                                                                                                 working-out 
    ##                                                                                                                                           1

``` r
# Calculate percentages for each hobby
prop.table(table(C1_Survey$hobby)) * 100
```

    ## 
    ## Any activity associated with football "The real football not the NFL")  (Playing in real life, playing in video games or watching the game) 
    ##                                                                                                                                    1.724138 
    ##                                                                                                   anyting that involves making/fixing stuff 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                    Canoeing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      chess  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Cooking 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                                    Cooking  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Crochet 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Crocheting 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Dancing 
    ##                                                                                                                                    1.724138 
    ##                                                                                            Gaming! Both Outdoor and Indoor(Board Games,PS5) 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                   Gardening 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                Gym workout  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                    Hanging out with friends 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      Hiking 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     hiking! 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                          Language Learning  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 Photography 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Pickleball 
    ##                                                                                                                                    1.724138 
    ##                                                                                                              Playing and Listening to Music 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                 Playing and watching sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                         Playing Bass Guitar 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              Playing guitar 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                         Playing Lawn Tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                        playing table tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              playing tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                         Playing Video Games 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     reading 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Reading 
    ##                                                                                                                                   10.344828 
    ##                                                                                                                                    Reading  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Running 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                    Running  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      skiing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      Soccer 
    ##                                                                                                                                    6.896552 
    ##                                                                                                                  spend time with loved ones 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 Star gazing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                               Table tennis  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Travelling 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                Video games  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                           Watching football 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              watching movie 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching movies 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching Movies 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             watching soccer 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching soccer 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                             Watching Sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 working-out 
    ##                                                                                                                                    1.724138

``` r
# Calculate percentages for each hobby
prop.table(table(C1_Survey$hobby)) * 100
```

    ## 
    ## Any activity associated with football "The real football not the NFL")  (Playing in real life, playing in video games or watching the game) 
    ##                                                                                                                                    1.724138 
    ##                                                                                                   anyting that involves making/fixing stuff 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                    Canoeing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      chess  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Cooking 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                                    Cooking  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Crochet 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Crocheting 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Dancing 
    ##                                                                                                                                    1.724138 
    ##                                                                                            Gaming! Both Outdoor and Indoor(Board Games,PS5) 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                   Gardening 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                Gym workout  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                    Hanging out with friends 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      Hiking 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     hiking! 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                          Language Learning  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 Photography 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Pickleball 
    ##                                                                                                                                    1.724138 
    ##                                                                                                              Playing and Listening to Music 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                 Playing and watching sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                         Playing Bass Guitar 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              Playing guitar 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                         Playing Lawn Tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                        playing table tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              playing tennis 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                         Playing Video Games 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     reading 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Reading 
    ##                                                                                                                                   10.344828 
    ##                                                                                                                                    Reading  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                     Running 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                    Running  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      skiing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      Soccer 
    ##                                                                                                                                    6.896552 
    ##                                                                                                                  spend time with loved ones 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                      sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 Star gazing 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                               Table tennis  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                  Travelling 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                Video games  
    ##                                                                                                                                    1.724138 
    ##                                                                                                                           Watching football 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                              watching movie 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching movies 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching Movies 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             watching soccer 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                             Watching soccer 
    ##                                                                                                                                    3.448276 
    ##                                                                                                                             Watching Sports 
    ##                                                                                                                                    1.724138 
    ##                                                                                                                                 working-out 
    ##                                                                                                                                    1.724138

# Selected variable of interest: hobby

# Research question: Which hobbies are most common among classmates?

# Check the column names to make sure variable hobby exists

# Generate a frequency table of hobbies including missing values

# Calculate percentages for each hobby

# converts counts to proportions; multiply by 100 for %
