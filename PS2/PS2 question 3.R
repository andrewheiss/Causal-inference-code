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
          covariate.labels=cov.labels, dep.var.labels=col.labels, star.cutoffs=c(0.05, .01, .001),
          se=list(work.se, income.se, unemp.se, NULL), notes="OLS models use robust standard errors")
system("latex2rtf q3table.tex")


# Simulate results to actually understand what's going on
# Scenario
# X <- cbind(my.mode(katrina.2006$age), my.mode(katrina.2006$black), my.mode(katrina.2006$sex), 1, 1, 1, 0)  # College grads
X <- cbind(my.mode(katrina.2006$age), my.mode(katrina.2006$black), 1, 1, 0, 0, 0)  # High school graduates only
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
