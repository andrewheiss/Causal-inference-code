#-------------
# Question 5
#-------------
# Part A
# Noncompliers (i.e. people who are not currently evacuated) vs. compliers (i.e. people who are still evacuated) in 2006
# Split storm:everevac.bin (aka evacpost) into compliers and noncompliers to see if there was an effect on the noncompliers

noncomp.work <- lm(wkswork ~ noncomplier.num + evacnow + storm + everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina)
summary(noncomp.work)
noncomp.work.se <- sqrt(diag(vcovHC(noncomp.work, type="HC1")))

noncomp.income <- lm(earnings ~ noncomplier.num + evacnow + storm + everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina)
summary(noncomp.income)
noncomp.income.se <- sqrt(diag(vcovHC(noncomp.income, type="HC1")))

noncomp.unemp <- lm(unempinc ~ noncomplier.num + evacnow + storm + everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina)
summary(noncomp.unemp)
noncomp.unemp.se <- sqrt(diag(vcovHC(noncomp.unemp, type="HC1")))

noncomp.health <- polr(health.cat ~ noncomplier.num + evacnow + storm + everevac + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina)
summary(noncomp.health)

cov.labels <- c("Noncomplier*2006", "Complier*2006", "2006", "Ever evacuated", "Age", "Race (Black)", "Sex (Female)", "High school", "Some college", "Bachelor's degree", "Post-graduate work", "Treatment (2006 * evacuated)")
col.labels <- c("Weeks worked", "Earnings", "Unemployment income", "Health")
stargazer(noncomp.work, noncomp.income, noncomp.unemp, noncomp.health, type="latex", out="q5table.tex", 
          covariate.labels=cov.labels, dep.var.labels=col.labels, star.cutoffs=c(0.05, .01, .001),
          se=list(noncomp.work.se, noncomp.income.se, noncomp.unemp.se, NULL),
          notes="OLS models use robust standard errors")
system("latex2rtf q5table.tex")


# Part B
# Probability of being a long term refugee
total.evacs <- length(katrina.2006$everevac[katrina.2006$everevac==1])
longterm.evacs <- xtabs(~ evacnow + everevac, data=katrina.2006)[2,2]
p.longterm <- longterm.evacs/total.evacs

# Or with regression
long.term <- lm(evacnow ~ everevac, data=katrina.2006)
summary(long.term)

# Get ITT effects
itt.work <- did.work.full$coefficients[11]
itt.income <- did.income.full$coefficients[11] 
itt.unemp <- did.unemp.full$coefficients[11]
itt.health <- did.health.full$coefficients[10]

# TOT effects
tot.work <- itt.work/p.longterm
tot.income <- itt.income/p.longterm
tot.unemp <- itt.unemp/p.longterm
tot.health <- itt.health/p.longterm

tot.output <- data.frame(Outcome=c("(Weeks worked)", "(Income)", "Unemployment compensation", "(Health status)"), ITT=c(itt.work, itt.income, itt.unemp, itt.health), TOT=c(tot.work, tot.income, tot.unemp, tot.health))
stargazer(tot.output, type="text", summary=FALSE)