# Get this started...
setwd("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 3/")
library(foreign)
library(rdd)
library(ggplot2)
library(scales)

# Load data and create variables
schools <- read.dta("PS3.dta")
schools$growth <- factor(ifelse(schools$avgrow05 <= 0, "Negative", "Positive"), ordered=TRUE)
schools$expected <- factor(schools$expgrow, labels=c("Did not meet", "Met"), ordered=TRUE)

#-------------
# Question 1
#-------------
(compliers <- xtabs(~ growth + expected, data=schools))
(compliers.chisq <- chisq.test(compliers))
residuals(compliers.chisq)^2  # Components of chi^2
prop.table(compliers)


#-------------------
# Question 2 and 3
#-------------------
# No easy way to recreate the fancy loess thing `rd` does in Stata
p <- ggplot(data=schools, aes(x=avgrow05, y=avgrow06))
p + geom_point(alpha=0.2) + geom_vline() + coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-0.5, 0.5)) + 
  geom_smooth(data=subset(schools, avgrow05 > 0), se=F, size=2, colour="darkgreen") + 
  geom_smooth(data=subset(schools, avgrow05 <= 0), se=F, size=2, colour="red") 

p <- ggplot(data=schools, aes(x=avgrow05, y=expgrow))
p + geom_point(alpha=0.2) + geom_vline() + coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-0.25, 1.25)) + 
  geom_smooth(data=subset(schools, avgrow05 > 0), se=F, size=2, colour="darkgreen", span=.1219208755368406, method="loess") + 
  geom_smooth(data=subset(schools, avgrow05 <= 0), se=F, size=2, colour="red") 

# For some reason the bandwidth rdd estimates is totally different from what Stata's rd finds. 
# Manually inputting Stata's bandwidth works, but it's lame
model.rd <- RDestimate(avgrow06 ~ avgrow05 + expgrow, data=schools, bw=.0609604377684203)
summary(model.rd)


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
  labs(x="Average Growth (2005)", y="Average Growth (2006)", title="Parametric discontinuity models") + 
  scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent) +
  theme_bw() + theme(legend.position="top", legend.key=element_blank()) +
  scale_colour_manual(name="Models", values=c("Simple cubic"="#1B9E77", "Squared"="#D95F02", "Quartic"="#7570B3"))