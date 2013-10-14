setwd("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 3/")
library(foreign)
library(rdd)
library(ggplot2)

schools <- read.dta("PS3.dta")
schools$growth <- factor(ifelse(schools$avgrow05 <= 0, "Negative", "Positive"), ordered=TRUE)
schools$expected <- factor(schools$expgrow, labels=c("Did not meet", "Met"), ordered=TRUE)

(asdf <- xtabs(~ growth + expected, data=schools))
chisq.test(asdf)
round(chisq.test(asdf)$residuals^2, 3)  # Components of chi^2
prop.table(asdf)

library(ggplot2)
p <- ggplot(data=schools, aes(x=avgrow05, y=avgrow06))
p + geom_point() + geom_vline() + #coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-1, 1)) + 
  geom_smooth(data=subset(schools, avgrow05 > 0), se=F, colour="darkgreen") + 
  geom_smooth(data=subset(schools, avgrow05 <= 0), se=F, colour="red") 

p <- ggplot(data=schools, aes(x=avgrow05, y=expgrow))
p + geom_point() + geom_vline() + #coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-1, 1)) + 
  geom_smooth(data=subset(schools, avgrow05 > 0), se=F, colour="darkgreen") + 
  geom_smooth(data=subset(schools, avgrow05 <= 0), se=F, colour="red") 


asdf <- RDestimate(avgrow06 ~ avgrow05 + expgrow, data=schools, bw=.0609604377684203)
summary(asdf)

