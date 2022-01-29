# install packages and import dataset
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr,
               stargazer,
               ivreg,
               tidyverse,
               sandwich,
               dplyr,
               rio)
df <- import("Week4/assignment4.dta")
set.seed(2022)
# Question 3: Flu shots for young children
# i) and ii)
df %>% group_by(treatgroup) %>% summarise(variance_of_flu = var(flu), mean_of_flushot = mean(flushot)) # you can use two summary functions in one group-by

# iii
df$group <- ifelse(df$treatgroup ==0 & df$flushot == 0, "Control group",
                ifelse(df$treatgroup ==1 & df$flushot == 1, "Treated treatment group",
                       "Untreated treatment group"))

df %>% group_by(group) %>%summarise(genderchild= mean(genderchild),
                                                                            nationality=mean(nationality),
                                                                            agemother=mean(agemother),
                                                                            educmother=mean(educmother),
                                                                            married=mean(married),
                                                                            housincome=mean(housincome))

# iv
df_treatment <- df[df$treatgroup == 1,]
OLS <- lm(flu~flushot, df_treatment)
OLS_include <- lm(flu~flushot+genderchild+nationality+agemother+educmother+married
                  +housincome, df_treatment)
stargazer(OLS,OLS_include,title = "OLS models with/without individual characteristics",
          dep.var.labels = "Flu",object.names = TRUE)

# v
ols_iv <- ivreg(flu~flushot | treatgroup, data=df)
ols_iv_char <- ivreg(flu~ flushot   +genderchild+nationality+agemother+educmother+married+housincome | 
                          treatgroup+genderchild+nationality+agemother+educmother+married+housincome, data=df)

stargazer(ols_iv,ols_iv_char,title = "IV-OLS models with/without individual characteristics",
          dep.var.labels = "Flu",object.names = TRUE)

# vi
ols_first <- lm(flushot~treatgroup, df_treatment)

# Yes, afraid of a weak instrument problem

# vii
