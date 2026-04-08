#   title: "Week 10 DAM Starter Code"
# author: "Juhong"
# date: "2025-10-15"
# output:
#   pdf_document: default


library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stargazer)
library(lme4)
library(psych)


sedadf <- read_dta('SEDA19.dta')

### Coding pathway ###
# code adapted from LS week 2

sedadf %>%
  summarize(mean_all = mean(meanavg, na.rm = TRUE))


dat <- sedadf %>%
  filter(stateabb == "MA" | stateabb == "TX") 

dat$MA <- ifelse(dat$stateabb == "MA", "1", "0") %>% factor()

table(dat$stateabb)
table(dat$MA)

dat$SES <- dat$sesavgall


reg1 <- lm(meanavg ~ SES, data = dat)  
summary(reg1) 

reg2 <- lm(meanavg ~ SES + MA, data = dat)
summary(reg2) 

reg3 <- lm(meanavg ~ MA*SES, data = dat)
summary(reg3) 

library(sandwich)
library(lmtest)
se1 <- sqrt(diag(vcovHC(reg1, type = "HC1")))
se2 <- sqrt(diag(vcovHC(reg2, type = "HC1")))
se3 <- sqrt(diag(vcovHC(reg3, type = "HC1")))

invisible(stargazer(
  reg1, reg2, reg3,
  type = "text",
  se = list(se1, se2, se3),
  title = "Achievement vs. SES in MA and TX",
  dep.var.labels.include = FALSE,
  dep.var.caption = "",
  intercept.bottom = TRUE,
  covariate.labels = c("SES", "MA × SES", "MA", "Constant"),
  omit.stat = c("f", "ser", "rsq","adj.rsq","n"),
  digits = 3,
  add.lines = list(c("N", nobs(reg1), nobs(reg2), nobs(reg3))),
  notes = c(
    "The outcome in all columns is average achievement in grade levels.",
    "The overall average grade level is 5.6. SES is standardized and centered, so",
    "the average SES is 0. All regressions are restricted to districts in MA and",
    "TX only. Robust standard errors in parentheses. * p<0.05; ** p<0.01; *** p<0.001"
  ),
  notes.label = "",
  notes.append = FALSE
))