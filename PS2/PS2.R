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


#-------------
# Question 1
#-------------
katrina.2006.evac <- subset(katrina, year==2006 & everevac==1)

# Weeks worked
t.test(katrina.2006.evac$wkswork)

# Earnings
t.test(katrina.2006.evac$earnings)

# Unemployment
t.test(katrina.2006.evac$unempinc)

# Health
(health.table <- xtabs(~ health.cat, data=katrina.2006.evac))
chisq.test(health.table)
prop.table(health.table)

# Plot stuff
# Regular variables
plot.data <- melt(katrina.2006.evac[,c("wkswork","earnings","unempinc")])
levels(plot.data$variable) <- c("Weeks worked", "Earnings", "Unemployment income")
q1.p1 <- ggplot(aes(x=value, y = ..scaled.., fill=variable), data=plot.data)
q1.p1 <- q1.p1 + geom_density() + facet_grid(. ~ variable, scales="free") +
  labs(x=NULL, y="Scaled density\n", title="Employment outcomes\n") + 
  theme_bw() + scale_fill_brewer(palette="Accent") + theme(legend.position="none")
q1.p1

# Health
plot.data <- data.frame(prop.table(health.table))
q1.p2 <- ggplot(plot.data, aes(health.cat, Freq))
q1.p2 <- q1.p2 + geom_bar(stat="identity", fill="#386CB0") + 
  labs(x=NULL, y="Frequency\n", title="Health status\n") + 
  theme_bw() + scale_y_continuous(labels=percent)
q1.p2

q1 <- arrangeGrob(q1.p1, q1.p2, ncol=1)
q1
ggsave(q1, file="q1.pdf", width=6, height=4, scale=2)


#-------------
# Question 2
#-------------
katrina.evac <- subset(katrina, everevac==1)

# Weeks worked last year
# Stata assumes equal variance by default
bartlett.test(wkswork ~ year, data=katrina.evac)
t.test(wkswork ~ year, data=katrina.evac, var.equal=TRUE)

plot.data <- summarySE(katrina.evac, measurevar="wkswork", groupvars=c("year"))
q2.p1 <- ggplot(plot.data, aes(x=year, y=wkswork, group=1, colour=year))
q2.p1 <- q2.p1 + geom_errorbar(aes(ymin=wkswork-ci, ymax=wkswork+ci), width=.1, size=1) + 
  geom_point(size=3) + geom_line(colour="darkgrey") +
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked\n") + 
  theme_bw() + scale_colour_brewer(palette="Set1") + theme(legend.position="none")
q2.p1


# Earnings
bartlett.test(earnings ~ year, data=katrina.evac)
t.test(earnings ~ year, data=katrina.evac, var.equal=FALSE)

plot.data <- summarySE(katrina.evac, measurevar="earnings", groupvars=c("year"))
q2.p2 <- ggplot(plot.data, aes(x=year, y=earnings, group=1, colour=year))
q2.p2 <- q2.p2 + geom_errorbar(aes(ymin=earnings-ci, ymax=earnings+ci), width=.1, size=1) + 
  geom_point(size=3) + geom_line(colour="darkgrey") +
  labs(x=NULL, y="Income\n", title="Income\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set1") + theme(legend.position="none")
q2.p2


# Unemployment
bartlett.test(unempinc ~ year, data=katrina.evac)
t.test(unempinc ~ year, data=katrina.evac, var.equal=FALSE)

plot.data <- summarySE(katrina.evac, measurevar="unempinc", groupvars=c("year"))
q2.p3 <- ggplot(plot.data, aes(x=year, y=unempinc, group=1, colour=year))
q2.p3 <- q2.p3 + geom_errorbar(aes(ymin=unempinc-ci, ymax=unempinc+ci), width=.1, size=1) + 
  geom_point(size=3) + geom_line(colour="darkgrey") +
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set1") + theme(legend.position="none")
q2.p3


# Health as categorical
health.table <- xtabs(~ health.cat + year, data=katrina.evac)
chisq.test(health.table)
prop.table(health.table, 2)

plot.data <- ddply(katrina.evac, .(year), summarise,
                   prop=prop.table(table(health.cat)), 
                   health.cat=factor(names(table(health.cat)), levels=health.labels, ordered=TRUE))
q2.p4 <- ggplot(plot.data, aes(health.cat, prop, fill=year))
q2.p4 <- q2.p4 + geom_bar(stat="identity", position='dodge') +
  labs(x=NULL, y="Proportion\n", title="Health status", fill=NULL) + 
  theme_bw() + scale_y_continuous(labels=percent) + scale_fill_brewer(palette="Set1") + 
  theme(legend.position="top")
q2.p4


# Arrange everything nicely
q2.raw <- arrangeGrob(q2.p1, q2.p2, q2.p3, q2.p4, ncol=2, main="Evacuees: 2005 vs. 2006\nAverage for all individuals")
q2.raw <- arrangeGrob(q2.p1, q2.p2, q2.p3, q2.p4, ncol=2)
ggsave(q2.raw, file="q2_raw.pdf", width=6, height=4, scale=2)

# Control for stuff
model.work <- lm(wkswork ~ year + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model.work)
work.se <- sqrt(diag(vcovHC(model.work, type="HC1")))
# coeftest(model.work, vcovHC(model.work, type="HC1"))  # This works too

model.income <- lm(earnings ~ year + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model.income)
income.se <- sqrt(diag(vcovHC(model.income, type="HC1")))

model.unemp <- lm(unempinc ~ year + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model.unemp)
unemp.se <- sqrt(diag(vcovHC(model.unemp, type="HC1")))

model.health <- polr(health.cat ~ year + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model.health)

cov.labels <- c("Year (2006)", "Age", "Race (Black)", "Sex (Female)", "High school", "Some college", "Bachelor's degree", "Post-graduate work")
col.labels <- c("Weeks worked", "Earnings", "Unemployment income", "Health")
stargazer(model.work, model.income, model.unemp, model.health, type="latex", out="q2table.tex", 
          covariate.labels=cov.labels, dep.var.labels=col.labels, 
          se=list(work.se, income.se, unemp.se, NULL), notes="OLS models use robust standard errors")
system("latex2rtf q2table.tex")  # Totally cheating here :) ... install latex2rtf first

# You have to use lrm + robcov in the rms package to get robust ologit standard errors
# library(rms)
# model.health.robust <- lrm(health.cat ~ as.numeric(year) + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac, x=TRUE, y=TRUE)
# robcov(model.health.robust)


# Simulate results to actually understand what's going on
# Scenario
# X <- cbind(my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), 1, 1, 1, 0)  # College grads
X <- cbind(my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), 1, 0, 0, 0)  # High graduates only
# X <- cbind(my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), 0, 0, 0, 0)  # High school dropouts

# Build range for varied parameter
evacyear <- 0:1


# Weeks worked simulation
draw <- mvrnorm(1000, coef(model.work), vcov(model.work))
evac.2005 <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac.2006 <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(evac.2005, evac.2006), year=rep(c("2005", "2006"), each=length(evac.2005)))

q2.p5 <- ggplot(plot.data, aes(x=year, y=estimate, fill=year))
q2.p5 <- q2.p5 + geom_violin(colour="white") + 
  stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked\n") + 
  theme_bw() + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q2.p5


# Earnings simulation
draw <- mvrnorm(1000, coef(model.income), vcov(model.income))
evac.2005 <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac.2006 <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(evac.2005, evac.2006), year=rep(c("2005", "2006"), each=length(evac.2005)))

q2.p6 <- ggplot(plot.data, aes(x=year, y=estimate, fill=year))
q2.p6 <- q2.p6 + geom_violin(colour="white") + 
  stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Income\n", title="Income\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q2.p6


# Unemployment simulation
draw <- mvrnorm(1000, coef(model.unemp), vcov(model.unemp))
evac.2005 <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac.2006 <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(evac.2005, evac.2006), year=rep(c("2005", "2006"), each=length(evac.2005)))

q2.p7 <- ggplot(plot.data, aes(x=year, y=estimate, fill=year))
q2.p7 <- q2.p7 + geom_violin(colour="white") + 
  stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q2.p7


# Health simulation
# Create prediction matrix based on X
X.mat <- as.matrix(cbind(evacyear, rbind(X, X)))
rownames(X.mat) <- NULL
colnames(X.mat) <- NULL

# Plot simulated predicted probabilities
q2.p8 <- ologit.spaghetti(model.health, X.mat, y.range=evacyear, y.limit=c(0, 0.50), runs=300,
                          y.plot.label="Probability of reporting\n",
                          legend.title="Health status: ",
                          cat.labels=health.labels,
                          x.labels=c("2005", "2006"),
                          x.plot.label=NULL,
                          plot.title="Probability of health status")
q2.p8


# Arrange everything nicely
q2.control <- arrangeGrob(q2.p5, q2.p6, q2.p7, q2.p8, ncol=2)
ggsave(q2.control, file="q2_control.pdf", width=6, height=4, scale=2)


#-------------
# Question 3
#-------------
# Evacuation vs. nonevacuation in 2006
katrina.2006 <- subset(katrina, year==2006)

# Weeks worked last year
bartlett.test(wkswork ~ everevac.bin, data=katrina.2006)
t.test(wkswork ~ everevac.bin, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="wkswork", groupvars=c("everevac.bin"))
q3.p1 <- ggplot(plot.data, aes(x=everevac.bin, y=wkswork, group=1, colour=everevac.bin))
q3.p1 <- q3.p1 + geom_errorbar(aes(ymin=wkswork-ci, ymax=wkswork+ci), width=.1, size=1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked\n") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q3.p1


# Earnings
bartlett.test(earnings ~ everevac, data=katrina.2006)
t.test(earnings ~ everevac, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="earnings", groupvars=c("everevac.bin"))
q3.p2 <- ggplot(plot.data, aes(x=everevac.bin, y=earnings, group=1, colour=everevac.bin))
q3.p2 <- q3.p2 + geom_errorbar(aes(ymin=earnings-ci, ymax=earnings+ci), width=.1, size=1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Income\n", title="Income\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q3.p2


# Unemployment
bartlett.test(unempinc ~ everevac, data=katrina.2006)
t.test(unempinc ~ everevac, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="unempinc", groupvars=c("everevac.bin"))
q3.p3 <- ggplot(plot.data, aes(x=everevac.bin, y=unempinc, group=1, colour=everevac.bin))
q3.p3 <- q3.p3 + geom_errorbar(aes(ymin=unempinc-ci, ymax=unempinc+ci), width=.1, size=1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q3.p3


# Health as categorical
health.table <- xtabs(~ health.cat + everevac.bin, data=katrina.2006)
(health.chisq <- chisq.test(health.table))  # summary(health.table) also works
prop.table(health.table, 2)
round(health.chisq$residuals^2, 3)  # Components of chi^2

plot.data <- ddply(katrina.2006, .(everevac.bin), summarise,
                   prop=prop.table(table(health.cat)), 
                   health.cat=factor(names(table(health.cat)), levels=health.labels, ordered=TRUE))
q3.p4 <- ggplot(plot.data, aes(health.cat, prop, fill=everevac.bin))
q3.p4 <- q3.p4 + geom_bar(stat="identity", position='dodge') +
  labs(x=NULL, y="Proportion\n", title="Health status", fill=NULL) + 
  theme_bw() + scale_y_continuous(labels=percent) + scale_fill_brewer(palette="Set2") + 
  theme(legend.position="top")
q3.p4

# Arrange everything nicely
q3.raw <- arrangeGrob(q3.p1, q3.p2, q3.p3, q3.p4, ncol=2)
q3.raw
ggsave(q3.raw, file="q3_raw.pdf", width=6, height=4, scale=2)


# Control for stuff
model.work <- lm(wkswork ~ everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
summary(model.work)
work.se <- sqrt(diag(vcovHC(model.work, type="HC1")))

model.income <- lm(earnings ~ everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
summary(model.income)
income.se <- sqrt(diag(vcovHC(model.income, type="HC1")))

model.unemp <- lm(unempinc ~ everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
summary(model.unemp)
unemp.se <- sqrt(diag(vcovHC(model.unemp, type="HC1")))

model.health <- polr(health.cat ~ everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
summary(model.health)

cov.labels <- c("Evacuated", "Age", "Race (Black)", "Sex (Female)", "High school", "Some college", "Bachelor's degree", "Post-graduate work")
col.labels <- c("Weeks worked", "Earnings", "Unemployment income", "Health")
stargazer(model.work, model.income, model.unemp, model.health, type="latex", out="q3table.tex", 
          covariate.labels=cov.labels, dep.var.labels=col.labels, 
          se=list(work.se, income.se, unemp.se, NULL), notes="OLS models use robust standard errors")
system("latex2rtf q3table.tex")


# Simulate results to actually understand what's going on
# Scenario
# X <- cbind(my.mode(katrina.2006$age), my.mode(katrina.2006$black), my.mode(katrina.2006$sex), 1, 1, 1, 0)  # College grads
X <- cbind(my.mode(katrina.2006$age), my.mode(katrina.2006$black), my.mode(katrina.2006$sex), 1, 0, 0, 0)  # High graduates only
# X <- cbind(my.mode(katrina.2006$age), my.mode(katrina.2006$black), my.mode(katrina.2006$sex), 0, 0, 0, 0)  # High school dropouts

# Weeks worked simulation
draw <- mvrnorm(1000, coef(model.work), vcov(model.work))
noevac <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(noevac, evac), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")

q3.p5 <- ggplot(plot.data, aes(x=type, y=estimate, fill=type))
q3.p5 <- q3.p5 + geom_violin(colour="white") + 
#   stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked\n") + 
  theme_bw() + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q3.p5


# Earnings simulation
draw <- mvrnorm(1000, coef(model.income), vcov(model.income))
noevac <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(noevac, evac), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")

q3.p6 <- ggplot(plot.data, aes(x=type, y=estimate, fill=type))
q3.p6 <- q3.p6 + geom_violin(colour="white") + 
#   stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Income\n", title="Income\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q3.p6


# Unemployment simulation
draw <- mvrnorm(1000, coef(model.unemp), vcov(model.unemp))
noevac <- draw %*% c(1, 0, X)  # First 1 is for the intercept
evac <- draw %*% c(1, 1, X)
plot.data <- data.frame(estimate=c(noevac, evac), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")

q3.p7 <- ggplot(plot.data, aes(x=type, y=estimate, fill=type))
q3.p7 <- q3.p7 + geom_violin(colour="white") + 
#   stat_summary(aes(group=1), fun.y=mean, geom="line", color="darkgrey", size=1) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", color="darkgrey", size=4) +
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_fill_brewer(palette="Set1") + theme(legend.position="none")
q3.p7


# Health simulation
# Create prediction matrix based on X
everevac.test <- 0:1
X.mat <- as.matrix(cbind(everevac.test, rbind(X, X)))
rownames(X.mat) <- NULL
colnames(X.mat) <- NULL

# Plot simulated predicted probabilities
q3.p8 <- ologit.points(model.health, X.mat, y.range=everevac.test, y.limit=c(0, 0.50), runs=500,
                       y.plot.label="Probability of reporting\n",
                       legend.title="Health status: ",
                       cat.labels=health.labels,
                       x.labels=c("Not evacuated", "Evacuated"),
                       x.plot.label=NULL,
                       plot.title="Probability of health status")
q3.p8


# Arrange everything nicely
q3.control <- arrangeGrob(q3.p5, q3.p6, q3.p7, q3.p8, ncol=2)
q3.control
ggsave(q3.control, file="q3_control.pdf", width=6, height=4, scale=2)


#-------------
# Question 4
#-------------
# Two groups: evacuees, non-evacuees (everevac)
# Two times: 2005, 2006 

# Models without controls
did.work <- lm(wkswork ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.work)

did.income <- lm(earnings ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.income)

did.unemp <- lm(unempinc ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.unemp)

did.health <- polr(health.cat ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.health)


# Models with controls
did.work.full <- update(did.work, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
summary(did.work.full)

did.income.full <- update(did.income, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
summary(did.income.full)

did.unemp.full <- update(did.unemp, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
summary(did.unemp.full)

did.health.full <- update(did.health, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
summary(did.health.full)


#-------------
# Question 5
#-------------
# Part A
# Noncompliers (i.e. people who are not currently evacuated) vs. compliers (i.e. people who are still evacuated) in 2006
complier.ids <- subset(katrina, evacnow==1)$persid
noncomplier.ids <- setdiff(katrina$persid, complier.ids)

noncompliers <- subset(katrina, persid %in% noncomplier.ids) 

did.work.noncompliers <- lm(wkswork ~ storm + everevac.bin + storm:everevac.bin + age + black + sex + hsgrad + someco + ba + postgrad, data=noncompliers)
summary(did.work.noncompliers)

# Etc.


# Part B
asdf <- lm(evacnow ~ everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
asdf <- lm(everevac ~ evacnow + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006)
summary(asdf)

asdf.logit <- glm(factor(evacnow) ~ factor(everevac) + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.2006, family=binomial(link="logit"))
summary(asdf.logit)
exp(asdf.logit$coefficients[2])
# Huh?