# Get this started...
source("http://www.haptonstahl.org/R/Decruft/Decruft.R")
setwd("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 3/")
library(foreign)
library(rdd)
library(ggplot2)
library(scales)
library(gridExtra)
source("../Causal inference code/PS3/rd_ggplot_data.R")

# Load data 
schools <- read.dta("PS3.dta")


#-------------
# Question 1
#-------------
schools$growth <- factor(ifelse(schools$avgrow05 <= 0, "Negative", "Positive"), ordered=TRUE)
schools$expected <- factor(schools$expgrow, labels=c("Did not meet", "Met"), ordered=TRUE)

(compliers <- xtabs(~ growth + expected, data=schools))
(compliers.chisq <- chisq.test(compliers))
residuals(compliers.chisq)^2  # Components of chi^2
prop.table(compliers)


#-------------
# Question 2
#-------------
# For some reason the bandwidth rdd estimates is totally different from what Stata's rd finds. 
# Manually inputting Stata's bandwidth works, but it's lame
# model.rd <- RDestimate(avgrow06 ~ avgrow05 + expgrow, data=schools.small)  # Gives the wrong bandwidth
model.rd <- RDestimate(avgrow06 ~ avgrow05 + expgrow, data=schools, bw=.0609604377684203)  # bw from Stata
IKbandwidth(schools$avgrow05, schools$avgrow06, cutpoint=0, verbose=TRUE)

summary(model.rd)


#-------------
# Question 3
#-------------
# Part a
plot.data <- rd.ggplot.data(model.rd, bw.level=1, variable=2)

p.outcome <- ggplot()
p.outcome <- p.outcome + geom_point(data=plot.data$raw.data, aes(x=X, y=Y), alpha=0.4) + geom_vline(xintercept=0, size=2, colour="grey") +
  geom_ribbon(aes(x=lval, ymin=llwr, ymax=lupr), data=plot.data$left, alpha=0.4, fill="red") + 
  geom_ribbon(aes(x=rval, ymin=rlwr, ymax=rupr), data=plot.data$right, alpha=0.4, fill="darkgreen") + 
  geom_line(aes(x=lval, y=lest), data=plot.data$left, size=2, colour="red") +
  geom_line(aes(x=rval, y=rest), data=plot.data$right, size=2, colour="darkgreen") +
  coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-.25, 1.25)) + 
  labs(x="\nAverage Growth (2005)", y="Treatment Status\n", title="Probability of receiving teacher bonus after 2005\n") + 
  scale_x_continuous(labels=percent) + theme_bw()

# Part b
plot.data.100 <- rd.ggplot.data(model.rd, bw.level=1)
plot.data.50 <- rd.ggplot.data(model.rd, bw.level=2)
plot.data.200 <- rd.ggplot.data(model.rd, bw.level=3)

p.100 <- ggplot()
p.100 <- p.100 + geom_point(data=plot.data.100$raw.data, aes(x=X, y=Y), alpha=0.2) + 
  geom_vline(xintercept=0, size=2, colour="grey") +
  geom_ribbon(aes(x=lval, ymin=llwr, ymax=lupr), data=plot.data.100$left, alpha=0.4, fill="red") + 
  geom_ribbon(aes(x=rval, ymin=rlwr, ymax=rupr), data=plot.data.100$right, alpha=0.4, fill="darkgreen") + 
  geom_line(aes(x=lval, y=lest), data=plot.data.100$left, size=2, colour="red") +
  geom_line(aes(x=rval, y=rest), data=plot.data.100$right, size=2, colour="darkgreen") +
  coord_cartesian(xlim=c(-0.25, 0.25), ylim=c(-.25, .25)) + 
  labs(x="\nAverage Growth (2005)", y="Average Growth (2006)\n", title="2006 growth explained by 2005 growth\n(Bandwidth: 0.061; 100%)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

p.50 <- ggplot()
p.50 <- p.50 + geom_point(data=plot.data.50$raw.data, aes(x=X, y=Y), alpha=0.2) + 
  geom_vline(xintercept=0, size=2, colour="grey") +
  geom_ribbon(aes(x=lval, ymin=llwr, ymax=lupr), data=plot.data.50$left, alpha=0.4, fill="red") + 
  geom_ribbon(aes(x=rval, ymin=rlwr, ymax=rupr), data=plot.data.50$right, alpha=0.4, fill="darkgreen") + 
  geom_line(aes(x=lval, y=lest), data=plot.data.50$left, size=2, colour="red") +
  geom_line(aes(x=rval, y=rest), data=plot.data.50$right, size=2, colour="darkgreen") +
  coord_cartesian(xlim=c(-0.25, 0.25), ylim=c(-.25, .25)) + 
  labs(x="\nAverage Growth (2005)", y="Average Growth (2006)\n", title="2006 growth explained by 2005 growth\n(Bandwidth: 0.031; 50%)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

p.200 <- ggplot()
p.200 <- p.200 + geom_point(data=plot.data.200$raw.data, aes(x=X, y=Y), alpha=0.2) + 
  geom_vline(xintercept=0, size=2, colour="grey") +
  geom_ribbon(aes(x=lval, ymin=llwr, ymax=lupr), data=plot.data.200$left, alpha=0.4, fill="red") + 
  geom_ribbon(aes(x=rval, ymin=rlwr, ymax=rupr), data=plot.data.200$right, alpha=0.4, fill="darkgreen") + 
  geom_line(aes(x=lval, y=lest), data=plot.data.200$left, size=2, colour="red") +
  geom_line(aes(x=rval, y=rest), data=plot.data.200$right, size=2, colour="darkgreen") +
  coord_cartesian(xlim=c(-0.25, 0.25), ylim=c(-.25, .25)) + 
  labs(x="\nAverage Growth (2005)", y="Average Growth (2006)\n", title="2006 growth explained by 2005 growth\n(Bandwidth: 0.122; 200%)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

q3.rd <- arrangeGrob(p.outcome, p.100, p.50, p.200, ncol=2)
ggsave(q3.rd, file="q3_rd.pdf", width=6, height=4, scale=2)

# Part c
p.enroll <- ggplot(data=schools, aes(x=avgrow05, y=enroll))
p.enroll <- p.enroll + geom_point(alpha=0.4) + geom_vline(xintercept=0, size=2, colour="grey") + 
  coord_cartesian(xlim=c(-0.5, 0.5)) + 
  labs(x="\nAverage Growth (2005)", y="Enrollment\n", title="Enrollment (2005-2006)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=comma) + theme_bw()

p.black <- ggplot(data=schools, aes(x=avgrow05, y=pblack))
p.black <- p.black + geom_point(alpha=0.4) + geom_vline(xintercept=0, size=2, colour="grey") + 
  coord_cartesian(xlim=c(-0.5, 0.5)) + 
  labs(x="\nAverage Growth (2005)", y="Percent Black\n", title="Percent Black (2005-2006)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

p.hisp <- ggplot(data=schools, aes(x=avgrow05, y=phisp))
p.hisp <- p.hisp + geom_point(alpha=0.4) + geom_vline(xintercept=0, size=2, colour="grey") + 
  coord_cartesian(xlim=c(-0.5, 0.5)) + 
  labs(x="\nAverage Growth (2005)", y="Percent Hispanic\n", title="Percent Hispanic (2005-2006)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

p.frl <- ggplot(data=schools, aes(x=avgrow05, y=pfrl))
p.frl <- p.frl + geom_point(alpha=0.4) + geom_vline(xintercept=0, size=2, colour="grey") + 
  coord_cartesian(xlim=c(-0.5, 0.5)) + 
  labs(x="\nAverage Growth (2005)", y="Percent free and feduced lunch\n", title="Percent receiving free and reduced lunch (2005-2006)\n") + 
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) + theme_bw()

q3.covariates <- arrangeGrob(p.enroll, p.black, p.hisp, p.frl, ncol=2)
ggsave(q3.covariates, file="q3_covariates.pdf", width=6, height=4, scale=2)


#-------------
# Question 4
#-------------
model.cubic <- lm(avgrow06 ~ avgrow05 + I(avgrow05^2) + I(avgrow05^3) + pblack + phisp + pfrl + enroll + elemschl + midschl + highschl + expgrow, data=schools, na.action="na.exclude")
summary(model.cubic)


#-------------
# Question 5
#-------------
# Only look at stuff around the cutoff
# schools.orig <- schools
# schools <- subset(schools.orig, avgrow05 <= 0.2 & avgrow06 <= 0.2 & avgrow05 >= -0.2 & avgrow06 >= -0.2)

# Part a
model.cubic.simple <- lm(avgrow06 ~ avgrow05 + I(avgrow05^2) + I(avgrow05^3) + expgrow, data=schools, na.action="na.exclude")
summary(model.cubic.simple)

# Part b
model.squared <- lm(avgrow06 ~ avgrow05 + I(avgrow05^2) + pblack + phisp + pfrl + enroll + elemschl + midschl + highschl + expgrow, data=schools, na.action="na.exclude")
summary(model.squared)

# Part c
model.quartic <- lm(avgrow06 ~ avgrow05 + I(avgrow05^2) + I(avgrow05^3) + I(avgrow05^4) + pblack + phisp + pfrl + enroll + elemschl + midschl + highschl + expgrow, data=schools, na.action="na.exclude")
summary(model.quartic)

# Part d
model.elem <- lm(avgrow06 ~ avgrow05 + I(avgrow05^2) + I(avgrow05^3) + I(avgrow05^4) + pblack + phisp + pfrl + enroll + expgrow, data=schools[], subset=elemschl==1, na.action="na.exclude")
summary(model.elem)

  
# Predictions
# Cubic simple
cubic.simple.no <- data.frame(avgrow05=seq(-.5, 0, .01), expgrow=0)
cubic.simple.no$avgrow06 <- predict(model.cubic.simple, cubic.simple.no)

cubic.simple.yes <- data.frame(avgrow05=seq(0, .5, .01), expgrow=1)
cubic.simple.yes$avgrow06 <- predict(model.cubic.simple, cubic.simple.yes)

# Quadratic
squared.no <- data.frame(avgrow05=seq(-.5, 0, .01), pblack=mean(schools$pblack, na.rm=T), 
                         phisp=mean(schools$phisp, na.rm=T), pfrl=mean(schools$pfrl, na.rm=T), 
                         enroll=mean(schools$enroll, na.rm=T), elemschl=0, midschl=0, highschl=1, expgrow=0)
squared.no$avgrow06 <- predict(model.squared, squared.no)

squared.yes <- data.frame(avgrow05=seq(0, .5, .01), pblack=mean(schools$pblack, na.rm=T), 
                          phisp=mean(schools$phisp, na.rm=T), pfrl=mean(schools$pfrl, na.rm=T), 
                          enroll=mean(schools$enroll, na.rm=T), elemschl=0, midschl=0, highschl=1, expgrow=1)
squared.yes$avgrow06 <- predict(model.squared, squared.yes)

# Quartic
quartic.no <- squared.no
quartic.no$avgrow06 <- predict(model.quartic, quartic.no)

quartic.yes <- squared.yes
quartic.yes$avgrow06 <- predict(model.quartic, quartic.yes)


# Plot everything
p <- ggplot(aes(y=avgrow06, x=avgrow05), data=schools)
p + geom_point(alpha=0.2) + geom_vline(size=2, colour="grey") +
  geom_line(data=cubic.simple.no, size=2, aes(colour="Simple cubic")) +
  geom_line(data=cubic.simple.yes, size=2, aes(colour="Simple cubic")) + 
  geom_line(data=squared.no, size=2, aes(colour="Squared")) +
  geom_line(data=squared.yes, size=2, aes(colour="Squared")) + 
  geom_line(data=quartic.no, size=2, aes(colour="Quartic")) +
  geom_line(data=quartic.yes, size=2, aes(colour="Quartic")) + 
  coord_cartesian(xlim=c(-0.25, 0.25), ylim=c(-.25, .25)) +
  labs(x="\nAverage Growth (2005)", y="Average Growth (2006)\n", title="Parametric discontinuity models") + 
  scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent) +
  theme_bw() + theme(legend.position="top", legend.key=element_blank()) +
  scale_colour_manual(name="Models", values=c("Simple cubic"="#1B9E77", "Squared"="#D95F02", "Quartic"="#7570B3"))