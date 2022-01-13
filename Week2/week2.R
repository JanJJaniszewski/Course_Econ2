library(pacman)
p_load(haven, SciViews, plm)

#### 2 ####
q2 <- read_dta("Week2/assignment2.dta")

##### i #####
pooled_with <-
  plm(
    ln(earnings) ~ school + age + I(agesq ^ 2) + ethblack + urban + regne + regnc + regw + regs + asvabc,
    data = q2, model = "pooling"
  )
pooled_without <-
  plm(
    ln(earnings) ~ school + age + I(agesq ^ 2) + ethblack + urban + regne + regnc + regw + regs,
    data = q2, model = "pooling"
  )

pooled_with$coefficients['school']
pooled_without$coefficients['school']

##### ii #####
pooled_ethnic <-
  plm(
    ln(earnings) ~ school + age + I(agesq ^ 2) + ethblack + urban + regne + regnc + regw + regs + I(school *
                                                                                                      ethblack),
    data = q2, model = "pooling"
  )
summary(pooled_ethnic)

##### iii #####
rem_ethnic <-
  plm(
    ln(earnings) ~ school + age + I(agesq ^ 2) + ethblack + urban + regne + regnc + regw + regs + I(school *
                                                                                                      ethblack),
    data = q2, model = "random", effect = "time"
  )
