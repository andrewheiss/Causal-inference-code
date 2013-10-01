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
          covariate.labels=cov.labels, dep.var.labels=col.labels, star.cutoffs=c(0.05, .01, .001),
          se=list(work.se, income.se, unemp.se, NULL), notes="OLS models use robust standard errors")
system("latex2rtf q2table.tex")  # Totally cheating here :) ... install latex2rtf first

# You have to use lrm + robcov in the rms package to get robust ologit standard errors
# library(rms)
# model.health.robust <- lrm(health.cat ~ as.numeric(year) + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac, x=TRUE, y=TRUE)
# robcov(model.health.robust)


# Simulate results to actually understand what's going on
# Scenario
# X <- cbind(my.mode(katrina.evac$age), my.mode(katrina.evac$black), my.mode(katrina.evac$sex), 1, 1, 1, 0)  # College grads
X <- cbind(my.mode(katrina.evac$age), my.mode(katrina.evac$black), 1, 1, 0, 0, 0)  # High school graduates only
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