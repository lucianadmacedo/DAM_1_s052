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

dat <- sedadf %>%
  filter(stateabb == "MA" | stateabb == "TX") 

dat$MA <- ifelse(dat$stateabb == "MA", "1", "0") %>% factor()

table(dat$stateabb)
table(dat$MA)

dat$SES <- dat$sesavgall


reg1 <- lm(meanavg ~ SES, data = dat)  
summary(reg1) # this is in line with the reference table

reg2 <- lm(meanavg ~ SES + MA, data = dat)
summary(reg2) # this is in line with the reference table

reg3 <- lm(meanavg ~ MA*SES, data = dat)
summary(reg3) # this is in line with the reference table

stargazer(
  reg1, reg2, reg3,
  type  = "text",
  title = "Achievement vs. SES in MA and TX",
  
  dep.var.caption = "",
  covariate.labels = c("SES", "MA", "MA x SES", "Constant"),
  
  star.cutoffs = c(0.05, 0.01, 0.001),
  star.char    = c("*", "**", "***"),
  
  omit.stat=c("f", "ser", "rsq","adj.rsq","n"),
  digits = 3,
  
  add.lines=list(c("\\textit{N}", nobs(reg1), nobs(reg2), nobs(reg3))),
  
  notes = c("The outcome in all columns is average achievement in grade levels.",
  "The overall average grade level is 5.6. SES is standardized and centered, so", 
  "the average SES is 0. All regressions are restricted to districts in MA and",
  "TX only. Robust standard errors in parentheses. * p<0.05; ** p<0.01; *** p<0.001"
  ),
  notes.label = " ", #from chatgpt
  notes.align = "l", #from chatgpt
  notes.append = FALSE
)


