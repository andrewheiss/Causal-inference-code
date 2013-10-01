setwd("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 2/")
source("../Causal inference code/PS2/Fancy R graphics.R")
library(foreign)
library(car)
library(MASS)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)
library(stargazer)

# For robust standard errors
# vcovHC(model, type="HC1") is what Stata does
library(sandwich)
library(lmtest)

katrina <- read.dta("PS2.dta")
health.labels <- c("Excellent", "Very good", "Good", "Fair", "Poor")
katrina$health.cat <- factor(katrina$health, labels=health.labels, ordered=TRUE)
katrina$everevac.bin <- factor(katrina$everevac, labels=c("Not evacuated", "Evacuated"))
katrina$year <- factor(katrina$year, ordered=TRUE)
# Make sure ordering is correct for the interactions to work right. If not, the model matrices will be linear transformations of each other and will not match Stata (or be correctly interpreted; though significance will be correct)
# See here for more details: http://stats.stackexchange.com/questions/19271/different-ways-to-write-interaction-terms-in-lm 
katrina$storm <- factor(ifelse(katrina$year==2005, "Before", "After"), levels=c("Before", "After"))

# Compliance
katrina$noncomplier[katrina$denier == 1 | katrina$backhome == 1] <- "Non-complier"
katrina$noncomplier[katrina$denier == 0 & katrina$backhome == 0] <- "Complier"
katrina$noncomplier <- factor(katrina$noncomplier, ordered=TRUE)
# Something's wonky with the ordering of the factors, and I don't want to take the time to figure it out
# So we just use numbers instead of factors :)
katrina$noncomplier.num[katrina$denier == 1 | katrina$backhome == 1] <- 1
katrina$noncomplier.num[katrina$denier == 0 & katrina$backhome == 0] <- 0
