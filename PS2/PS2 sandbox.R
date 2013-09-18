library(foreign)
library(car)
katrina <- read.dta("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 2/PS2.dta")
katrina$health.cat <- ordered(katrina$health)
katrina$everevac.bin <- as.factor(katrina$everevac)

# Question 1
# Evacuation vs. nonevacuation in 2006
katrina.2006 <- subset(katrina, year==2006)

# Weeks worked last year
# Stata assumes equal variance by default
bartlett.test(wkswork ~ everevac, data=katrina.2006)
t.test(wkswork ~ everevac, data=katrina.2006, var.equal=FALSE)

# Earnings
bartlett.test(earnings ~ everevac, data=katrina.2006)
t.test(earnings ~ everevac, data=katrina.2006, var.equal=FALSE)

# Unemployment
bartlett.test(unempinc ~ everevac, data=katrina.2006)
t.test(unempinc ~ everevac, data=katrina.2006, var.equal=FALSE)

# Health as categorical
(health.table <- xtabs(~ health.cat + everevac.bin, data=katrina.2006))
summary(health.table)  # Both of these work
chisq.test(health.table)
prop.table(health.table, 2)
plot(health.table)


# Question 2
katrina.evac <- subset(katrina, everevac==1)

# Weeks worked last year
# Stata assumes equal variance by default
bartlett.test(wkswork ~ year, data=katrina.evac)
t.test(wkswork ~ year, data=katrina.evac, var.equal=TRUE)

# Earnings
bartlett.test(earnings ~ year, data=katrina.evac)
t.test(earnings ~ year, data=katrina.evac, var.equal=FALSE)

# Unemployment
bartlett.test(unempinc ~ year, data=katrina.evac)
t.test(unempinc ~ year, data=katrina.evac, var.equal=FALSE)

# Health as categorical
(health.table <- xtabs(~ health.cat + year, data=katrina.evac))
summary(health.table)  # Both of these work
chisq.test(health.table)
prop.table(health.table, 2)
plot(health.table)


# Ordered logit
model <- polr(health.cat ~ evacpost + age + black + sex + hsgrad + someco + ba + postgrad, data=katrina.evac)
summary(model)