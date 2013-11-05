source("http://www.haptonstahl.org/R/Decruft/Decruft.R")
library(ggplot2)
library(plyr)
library(foreign)
setwd("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 4/")
educ <- read.dta("Assignment4.dta")

#-------------
# Question 1
#-------------
# Replicate Figures I and II
# Reshape data for plotting
plot.data <- ddply(educ, ~ YOB + QOB, summarise,
                   years.completed=mean(EDUC, na.rm=T))
# Convert quarter numbers to fractions of years
plot.data$yob <- 1900 + plot.data$YOB + 
  sapply(plot.data$QOB, function(x) switch(x, 0.0, 0.25, 0.5, 0.75))

plot.data$QOB <- factor(plot.data$QOB, labels=c("1st", "2nd", "3rd", "4th"), ordered=TRUE)
plot.data$decade <- factor(ifelse(plot.data$yob < 1940, "1930-39", "1940-49"))

fig1 <- ggplot(plot.data, aes(x=yob, y=years.completed))
fig1 <- fig1 + geom_line(colour="grey", size=1) + 
  geom_point(aes(colour=QOB, shape=QOB), size=4) + 
  scale_colour_brewer(name="Quarter of birth", palette="Set1") + 
  scale_shape_manual(name="Quarter of birth", values=c(20, 20, 20, 17)) +
  scale_x_continuous(breaks=seq(1930, 1950, 2)) + theme_bw() +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_rect(fill=NA, colour=NA)) + 
  labs(title=NULL, x=NULL, y="Years of completed education\n") + facet_wrap(~ decade, scales="free_x", nrow=2)
fig1
ggsave(fig1, file="fig1.png", width=6, height=4, scale=2)
ggsave(fig1, file="fig1.png", width=6.5, units="in")



plot.data <- ddply(educ, ~ YOB + QOB, summarise,
                   income=mean(LWKLYWGE, na.rm=T))
# Convert quarter numbers to fractions of years
plot.data$yob <- 1900 + plot.data$YOB + 
  sapply(plot.data$QOB, function(x) switch(x, 0.0, 0.25, 0.5, 0.75))

plot.data$QOB <- factor(plot.data$QOB, labels=c("1st", "2nd", "3rd", "4th"), ordered=TRUE)
plot.data$decade <- factor(ifelse(plot.data$yob < 1940, "1930-39", "1940-49"))

fig2 <- ggplot(plot.data, aes(x=yob, y=income))
fig2 <- fig2 + geom_line(colour="grey", size=1) + 
  geom_point(aes(colour=QOB, shape=QOB), size=4) + 
  scale_colour_brewer(name="Quarter of birth", palette="Set1") + 
  scale_shape_manual(name="Quarter of birth", values=c(20, 20, 20, 17)) +
  scale_x_continuous(breaks=seq(1930, 1950, 2)) + theme_bw() +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_rect(fill=NA, colour=NA)) + 
  labs(title=NULL, x=NULL, y="Logged weekly income\n") + facet_wrap(~ decade, scales="free_x", nrow=2)
fig2
# ggsave(fig2, file="fig2.png", width=6, height=4, scale=2)
ggsave(fig2, file="fig2.png", width=6.5, units="in")


#----------------------------------------------------------
#                    IV stuff with R
#----------------------------------------------------------
library(AER)
library(stargazer)

educ$YOB.f <- as.factor(educ$YOB)
educ$QOB.f <- as.factor(educ$QOB)
educ.30s <- subset(educ, YOB < 40)

# Table 5, column 1
model1 <- lm(LWKLYWGE ~ EDUC + YOB.f, data=educ.30s)
summary(model1)

# Table 5, column 2
# Manually
first.stage <- lm(EDUC ~ YOB.f + YOB.f*QOB.f, data=educ.30s)
summary(first.stage)
educ.30s$educ.predicted <- fitted(first.stage)

second.stage <- lm(LWKLYWGE ~ educ.predicted + YOB.f, data=educ.30s)
summary(second.stage)
# It works! But standard errors are wrong and need to be corrected somehow

# Automatically (see last page of http://www.schmidheiny.name/teaching/iv2up.pdf)
model2 <- ivreg(LWKLYWGE ~ EDUC + YOB.f | YOB.f + YOB.f*QOB.f, data=educ.30s)
summary(model2)

# But there's no apparent way to do liml regression like in Stata...


# Output the models
stargazer(model1, model2, type="text", omit="YOB", 
          star.cutoffs=c(0.05, .01, .001), no.space=TRUE, 
          covariate.labels=c("Years of education"), 
          dep.var.caption=c("Logged weekly wage"),
          column.labels=c("OLS"), dep.var.labels="")
