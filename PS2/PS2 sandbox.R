library(foreign)
library(car)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)

katrina <- read.dta("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 2/PS2.dta")
health.labels <- c("Excellent", "Very good", "Good", "Fair", "Poor")
katrina$health.cat <- factor(katrina$health, labels=health.labels, ordered=TRUE)
katrina$everevac.bin <- factor(katrina$everevac, labels=c("Not evacuated", "Evacuated"))
katrina$year <- factor(katrina$year, ordered=TRUE)

# Question 1
# Evacuation vs. nonevacuation in 2006
katrina.2006 <- subset(katrina, year==2006)

# Weeks worked last year
# Stata assumes equal variance by default
bartlett.test(wkswork ~ everevac.bin, data=katrina.2006)
t.test(wkswork ~ everevac.bin, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="wkswork", groupvars=c("everevac.bin"))
q1.p1 <- ggplot(plot.data, aes(x=everevac.bin, y=wkswork, group=1, colour=everevac.bin))
q1.p1 <- q1.p1 + geom_errorbar(aes(ymin=wkswork-ci, ymax=wkswork+ci), width=.1, size=1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Weeks worked\n", title="Average number of weeks worked in the previous year\n") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q1.p1


# Earnings
bartlett.test(earnings ~ everevac, data=katrina.2006)
t.test(earnings ~ everevac, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="earnings", groupvars=c("everevac.bin"))
q1.p2 <- ggplot(plot.data, aes(x=everevac.bin, y=earnings, group=1, colour=everevac.bin))
q1.p2 <- q1.p2 + geom_errorbar(aes(ymin=earnings-ci, ymax=earnings+ci), width=.1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Income\n", title="Average income earned in the previous year\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q1.p2


# Unemployment
bartlett.test(unempinc ~ everevac, data=katrina.2006)
t.test(unempinc ~ everevac, data=katrina.2006, var.equal=FALSE)

plot.data <- summarySE(katrina.2006, measurevar="unempinc", groupvars=c("everevac.bin"))
q1.p3 <- ggplot(plot.data, aes(x=everevac.bin, y=unempinc, group=1, colour=everevac.bin))
q1.p3 <- q1.p3 + geom_errorbar(aes(ymin=unempinc-ci, ymax=unempinc+ci), width=.1, size=1) + 
  geom_point(size=3) + 
  labs(x=NULL, y="Unemployment compensation\n", title="Average unemployment compensation in the previous year\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set2") + theme(legend.position="none")
q1.p3


# Health as categorical
health.table <- xtabs(~ health.cat + everevac.bin, data=katrina.2006)
chisq.test(health.table)  # summary(health.table) also works
prop.table(health.table, 2)

plot.data <- ddply(katrina.2006, .(everevac.bin), summarise,
                   prop=prop.table(table(health.cat)), 
                   health.cat=factor(names(table(health.cat)), levels=health.labels, ordered=TRUE))
q1.p4 <- ggplot(plot.data, aes(health.cat, prop, fill=everevac.bin))
q1.p4 <- q1.p4 + geom_bar(stat="identity", position='dodge') +
  labs(x=NULL, y="Proportion\n", title="Self-reported health outcomes in March 2006\n", fill=NULL) + 
  theme_bw() + scale_y_continuous(labels=percent) + scale_fill_brewer(palette="Set2") + 
  theme(legend.position="bottom")
q1.p4

# Arrange everything nicely
grid.arrange(q1.p1, q1.p2, q1.p3, q1.p4, ncol=2)


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
  labs(x=NULL, y="Weeks worked\n", title="Average number of weeks worked\n") + 
  theme_bw() + scale_colour_brewer(palette="Set1") + theme(legend.position="none")
q2.p1


# Earnings
bartlett.test(earnings ~ year, data=katrina.evac)
t.test(earnings ~ year, data=katrina.evac, var.equal=FALSE)

plot.data <- summarySE(katrina.evac, measurevar="earnings", groupvars=c("year"))
q2.p2 <- ggplot(plot.data, aes(x=year, y=earnings, group=1, colour=year))
q2.p2 <- q2.p2 + geom_errorbar(aes(ymin=earnings-ci, ymax=earnings+ci), width=.1, size=1) + 
  geom_point(size=3) + geom_line(colour="darkgrey") +
  labs(x=NULL, y="Income\n", title="Average income\n") + 
  theme_bw() + scale_y_continuous(labels=dollar) + scale_colour_brewer(palette="Set1") + theme(legend.position="none")
q2.p2


# Unemployment
bartlett.test(unempinc ~ year, data=katrina.evac)
t.test(unempinc ~ year, data=katrina.evac, var.equal=FALSE)

plot.data <- summarySE(katrina.evac, measurevar="unempinc", groupvars=c("year"))
q2.p3 <- ggplot(plot.data, aes(x=year, y=unempinc, group=1, colour=year))
q2.p3 <- q2.p3 + geom_errorbar(aes(ymin=unempinc-ci, ymax=unempinc+ci), width=.1, size=1) + 
  geom_point(size=3) + geom_line(colour="darkgrey") +
  labs(x=NULL, y="Unemployment compensation\n", title="Average unemployment compensation\n") + 
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
  labs(x=NULL, y="Proportion\n", title="Self-reported health outcomes\n", fill=NULL) + 
  theme_bw() + scale_y_continuous(labels=percent) + scale_fill_brewer(palette="Set1") + 
  theme(legend.position="bottom")
q2.p4


# Arrange everything nicely
grid.arrange(q2.p1, q2.p2, q2.p3, q2.p4, ncol=2)


# Control for stuff
# Ordered logit
model <- polr(health.cat ~ evacpost + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model)

my.mode <- function(x) {
  which.max(table(x))
}

# Build range for varied parameter
sample.range <- 0:1

# Build new data for prediction
X <- cbind(sample.range, my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), my.mode(katrina.evac$hsgrad), my.mode(katrina.evac$someco), my.mode(katrina.evac$ba), my.mode(katrina.evac$postgrad))  # All modal values, which includes 1s for all education levels
X <- cbind(sample.range, my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), my.mode(katrina.evac$hsgrad), my.mode(katrina.evac$someco), 1, 0)  # College grads
X <- cbind(sample.range, my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), 0, 0, 0, 0)  # High school dropouts

# Plot simulated probabilities
test <- ologit.spaghetti(model, X, y.range=sample.range, y.limit=c(0,1), runs=250,
                          y.plot.label="Probability of health status\n",
                          legend.title="Health status: ",
                          cat.labels=c("Very good", "Good", "Okay", "Bad", "Very bad"),
                          x.labels=c("2005", "2006"),
                          x.plot.label="\nYear")
test