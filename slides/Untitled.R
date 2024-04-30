#### **Assignment 1 (Dichotomous Outcomes and Logistic Regression)**

Objectives:
  
  - Examine dichotomous outcome data
- Use logistic regression to estimate a relationship where the dependent variable is dichotomous
- Describe, interpret, and draw inferences from the model results

Due: April 26

**[Assignment details](./assignments/assignment01_Sp23.html)**
  
  #### **Assignment 2 (Count Outcomes)**
  
  Objectives:
  
  - Examine count outcome data
- Use the appropriate regression model to estimate a relationship where the dependent variable is a count
- Describe, interpret, and draw inferences from the model results

Due: May 12

**[Assignment details](./assignments/assignment02_Sp23.html)**
  
  #### **Final Project (Nested Data Analysis and Addressing Missing Data)**
  
  Objectives:
  
  - Use mixed-effects models to appropriately analyze data with a nested structure 
- Examine and address missing outcome data using multiple imputation

Due: June 13

**[Instructions](./assignments/final_Sp23.html)**
  
  
  
  
  
  * Datasets: [seda](./data/seda_oregon.csv) | [popular](./data/popular.RData) | [respire](./data/respire.RData)

library(tidyverse)


NHANES_data1112 <- NHANES::NHANES %>% 
  select(ID, SurveyYr, Gender, Age, AgeDecade, Race3, Education, Poverty, Diabetes, DaysMentHlthBad, Depressed, PhysActive, SleepTrouble, SleepHrsNight) %>% 
  mutate(Race_Eth = as_factor(case_when(Race3 == "Mexican" | Race3 == "Hispanic" ~ "Hispanic/Latino",
                                        TRUE ~ Race3)),
         PhysActive = as_factor(case_when(PhysActive == "No" ~ "Inactive",
                                          PhysActive == "Yes" ~ "Active")),
         PhysActive = factor(PhysActive, levels = c("Active", "Inactive")),
         DaysMHBad_most = as_factor(case_when(DaysMentHlthBad < 10 ~ "No",
                                              DaysMentHlthBad >= 10 ~ "Yes")),
         DaysMHBad_most = factor(DaysMHBad_most, levels = c("No", "Yes"))) %>% 
  rename(Depress_Freq = Depressed, Sex = Gender, DaysMHBad_count = DaysMentHlthBad, Sleep_Trouble = SleepTrouble) %>% 
  select(ID, SurveyYr, Age, Sex, Race_Eth, Diabetes, Depress_Freq, PhysActive, DaysMHBad_count, DaysMHBad_most, Sleep_Trouble, SleepHrsNight) %>%   
  filter(SurveyYr == "2011_12", Age > 12) %>% 
  filter(if_all(everything(), ~!is.na(.))) %>% 
  select(-SurveyYr)

ah_happiness <- rio::import(here::here("data", "ah_happiness.csv")) %>% 
  drop_na() %>% 
  select(q1:q8) %>% 
  mutate(q1 = recode (q1, "0"=3, "1"=2, "2"=1, "3"=0),
         q2 = recode (q2, "0"=3, "1"=2, "2"=1, "3"=0),
         q3 = recode (q3, "0"=3, "1"=2, "2"=1, "3"=0),
         q4 = recode (q4, "0"=3, "1"=2, "2"=1, "3"=0),
         q7 = recode (q7, "0"=3, "1"=2, "2"=1, "3"=0),
         q8 = recode (q8, "0"=3, "1"=2, "2"=1, "3"=0))


save(ah_happiness, file = "ah_happiness.RData")
save(NHANES_data1112, file = "NHANES_data1112.RData")


NHANES_data0910 <- NHANES::NHANES %>% 
  select(ID, SurveyYr, Age, Gender, Education, Depressed, AlcoholYear) %>% 
  filter(SurveyYr == "2009_10", !is.na(Age), !is.na(Gender), !is.na(Depressed)) %>% 
  mutate(Education_level = factor(Education, levels = c("High School", "8th Grade", "9 - 11th Grade", "Some College", "College Grad")),
         Depression_level = case_when(Depressed == "None" ~ "No",
                                      Depressed == "Several" | Depressed == "Most" ~ "Yes"),
         Depression_level = as.character(Depression_level)) %>% 
  rename(Sex = Gender) %>% 
  select(ID, Age, Sex, Education_level, Depression_level, AlcoholYear)

NHANES_data0910_t <- NHANES_data0910 %>% 
  filter(Age >= 20) %>% 
  mutate(Depression_level = factor(Depression_level),
         Education_level = factor(Education_level, levels = c("8th Grade", "9 - 11th Grade", "High School", "Some College", "College Grad")))

NHANES_data0910_t %>% 
  summarize(Age_med = median(Age, na.rm = TRUE),
            Age_IQR_l = quantile(Age, probs = 0.25, na.rm = TRUE),
            Age_IQR_u = quantile(Age, probs = 0.75, na.rm = TRUE),
            AlcoholYear_m = mean(NHANES_data0910_t$AlcoholYear, na.rm = TRUE),
            AlcoholYear_SD = sd(AlcoholYear, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  print.data.frame() 

NHANES_data0910_t %>% 
  group_by(Sex) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n / sum(n))

mutate(freq = n / sum(n))
  



missMethods::count_NA(NHANES_data0910_t)

library(mice)

md.pattern(NHANES_data0910_t, rotate.names = TRUE)


save(NHANES_data0910, file = "NHANES_data0910.RData")


NHANES_data0910_t_imp <- mice(NHANES_data0910_t, method = "cart", m = 20, maxit = 10)
densityplot(NHANES_data0910_t_imp)


NHANES_data0910_t %>% 
  ggplot(aes(AlcoholYear)) +
  geom_histogram(binwidth = 20)





