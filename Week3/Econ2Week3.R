#### Load packages and init ####################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vtreat, caret, rpart, ohenery, stargazer, xtable, rio)
set.seed(42)

#### Data #################################################################
##### Load #####
q2_origin <- import("Week3/assignment3.dta")
q2 <- drop_na(q2_origin) %>% select(-id)

#### Q2 ####
##### 2i #####
# How many in group and how many would lie
q2 %>% group_by(treatment) %>% count
q2 %>% group_by(treatment, correct) %>% count

# Fraction of students assumed to have lied
round(((21/51)-(1/6)) / (5/6),2) # Candy
round(((19/55)-(1/6)) / (5/6),2) # Fruit
round(((37/93)-(1/6)) / (5/6),2) # Nothing

# Balanced datasets
q2 %>% group_by(treatment) %>% summarise_if(is.numeric, mean) %>%
  xtable(., type='latex')

##### 2ii #####
lm1 <- lm(correct ~ treatment, data=q2)
#summary(lm1) %>% xtable(., type='latex')
#summary(lm1) 
lm2 <- lm(correct ~ ., data=q2)
#summary(lm1) %>% xtable(., type='latex')
#summary(lm1) 
stargazer(lm1,lm2,title = "Model to treatments VS Model includes all variables", 
          dep.var.labels = "correct",
          column.labels = c("Model to treatments", "Model includes all variables"))

##### 2iii #####
