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