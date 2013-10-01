#-------------
# Question 4
#-------------
# Two groups: evacuees, non-evacuees (everevac)
# Two times: 2005, 2006 

# Models without controls
# This could totally be simplified with a function, but I'm too lazy and tired to funcionalize it.
# So yay for horrible repetition :)
did.work <- lm(wkswork ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.work)
did.work.se <- sqrt(diag(vcovHC(did.work, type="HC1")))

plot.data <- summarySE(katrina, measurevar="wkswork", groupvars=c("everevac.bin", "storm"))

# Get hypotheetical and actual end points
hypo.end <- plot.data[2,"wkswork"] + did.work$coefficients[3]
evac.end <- plot.data[4,"wkswork"]
nonevac.end <- plot.data[2,"wkswork"]
evac.begin <- plot.data[3,"wkswork"]

# Build hypothetical data frame
hypothetical <- data.frame(everevac.bin="Hypothetical",
                           storm=c("Before", "After"), 
                           wkswork=c(plot.data[3,"wkswork"], hypo.end))

# Plot!
q4.p1 <- ggplot(plot.data, aes(x=storm, y=wkswork, group=everevac.bin, colour=everevac.bin))
q4.p1 <- q4.p1 + geom_ribbon(aes(ymin=wkswork-ci, ymax=wkswork+ci), alpha=0.2, fill="gold", colour=NA) + 
  geom_line(data=hypothetical, colour="#FC8D62", size=1, linetype=3) + 
  geom_point(size=3) + geom_line(size=1) + 
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + 
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  coord_cartesian(xlim=c(0.75, 2.25))
q4.p1

# Add annotations
p <- q4.p1 + annotate("segment", x=1, xend=2, y=evac.begin, yend=evac.begin, colour="darkgrey", size=1, linetype=3) + 
  annotate("text", x=2.08, y=mean(c(hypo.end, evac.end)), label="Treatment\neffect", hjust=0, size=4) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  annotate("text", x=2.13, y=mean(c(hypo.end, nonevac.end)), label="Year\neffect", hjust=0, size=4) + 
  annotate("errorbar", x=2.1, xend=2.1, ymin=hypo.end+0.1, ymax=nonevac.end, colour="darkorchid4", width=0.03) + 
  annotate("text", x=2.13, y=mean(c(hypo.end, evac.begin)), label="Evacuation\neffect", hjust=0, size=4) + 
  annotate("errorbar", x=2.1, xend=2.1, ymin=hypo.end, ymax=evac.begin, colour="darkorchid4", width=0.03)
#   annotate("segment", x=2.05, xend=2.05, y=hypo.end, yend=evac.begin, colour="blue")
p


# Income
did.income <- lm(earnings ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.income)
did.income.se <- sqrt(diag(vcovHC(did.income, type="HC1")))

plot.data <- summarySE(katrina, measurevar="earnings", groupvars=c("everevac.bin", "storm"))

# Get hypotheetical and actual end points
hypo.end <- plot.data[2,"earnings"] + did.income$coefficients[3]
evac.end <- plot.data[4,"earnings"]
nonevac.end <- plot.data[2,"earnings"]
evac.begin <- plot.data[3,"earnings"]

# Build hypothetical data frame
hypothetical <- data.frame(everevac.bin="Hypothetical",
                           storm=c("Before", "After"), 
                           earnings=c(plot.data[3,"earnings"], hypo.end))

# Plot!
q4.p2 <- ggplot(plot.data, aes(x=storm, y=earnings, group=everevac.bin, colour=everevac.bin))
q4.p2 <- q4.p2 + geom_ribbon(aes(ymin=earnings-ci, ymax=earnings+ci), alpha=0.2, fill="gold", colour=NA) + 
  geom_line(data=hypothetical, colour="#FC8D62", size=1, linetype=3) + 
  geom_point(size=3) + geom_line(size=1) + 
  labs(x=NULL, y="Income\n", title="Income") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_y_continuous(labels=dollar) +
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  coord_cartesian(xlim=c(0.75, 2.25))
q4.p2


# Unemployment
did.unemp <- lm(unempinc ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.unemp)
did.unemp.se <- sqrt(diag(vcovHC(did.unemp, type="HC1")))

plot.data <- summarySE(katrina, measurevar="unempinc", groupvars=c("everevac.bin", "storm"))

# Get hypotheetical and actual end points
hypo.end <- plot.data[2,"unempinc"] + did.unemp$coefficients[3]
evac.end <- plot.data[4,"unempinc"]
nonevac.end <- plot.data[2,"unempinc"]
evac.begin <- plot.data[3,"unempinc"]

# Build hypothetical data frame
hypothetical <- data.frame(everevac.bin="Hypothetical",
                           storm=c("Before", "After"), 
                           unempinc=c(plot.data[3,"unempinc"], hypo.end))

# Plot!
q4.p3 <- ggplot(plot.data, aes(x=storm, y=unempinc, group=everevac.bin, colour=everevac.bin))
q4.p3 <- q4.p3 + geom_ribbon(aes(ymin=unempinc-ci, ymax=unempinc+ci), alpha=0.2, fill="gold", colour=NA) + 
  geom_line(data=hypothetical, colour="#FC8D62", size=1, linetype=3) + 
  geom_point(size=3) + geom_line(size=1) + 
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_y_continuous(labels=dollar) +
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  coord_cartesian(xlim=c(0.75, 2.25))
q4.p3


# Health
did.health <- polr(health.cat ~ storm + everevac.bin + storm:everevac.bin, data=katrina)
summary(did.health)

health.table <- xtabs(~ health.cat + storm + everevac.bin, data=katrina)
plot.data <- as.data.frame(ftable(round(prop.table(health.table, c(3,2)), 2)))

# Grouped by evacuation
q4.p4 <- ggplot(plot.data, aes(x=storm, y=Freq, group=health.cat, colour=health.cat))
q4.p4 <- q4.p4 + facet_wrap(~ everevac.bin, ncol=2) + 
  geom_point(size=3) + geom_line(size=1) + 
  labs(x=NULL, y="Percent reporting\n", title="Health outcomes") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_y_continuous(labels=percent) +
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  coord_cartesian(xlim=c(0.75, 2.25))
q4.p4

# Grouped by outcome
q4.p4a <- ggplot(plot.data, aes(x=storm, y=Freq, group=everevac.bin, colour=everevac.bin))
q4.p4a <- q4.p4a + facet_wrap(~ health.cat) + theme(legend.position = c(1, 0), legend.justification = c(1, 0)) + 
  geom_point(size=3) + geom_line(size=1) + 
  labs(x=NULL, y="Percent reporting\n", title="Health outcomes\n") + 
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_y_continuous(labels=percent) +
  theme(legend.position=c(1, 0), legend.justification=c(1, 0), legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  coord_cartesian(xlim=c(0.75, 2.25))
q4.p4a

q4.raw <- arrangeGrob(q4.p1, q4.p2, q4.p3, q4.p4a, ncol=2)
q4.raw
ggsave(q4.raw, file="q4_raw.pdf", width=6, height=4, scale=2)


# Models with controls
did.work.full <- update(did.work, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
did.work.full.se <- sqrt(diag(vcovHC(did.work.full, type="HC1")))
summary(did.work.full)

did.income.full <- update(did.income, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
did.income.full.se <- sqrt(diag(vcovHC(did.income.full, type="HC1")))
summary(did.income.full)

did.unemp.full <- update(did.unemp, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
did.unemp.full.se <- sqrt(diag(vcovHC(did.unemp.full, type="HC1")))
summary(did.unemp.full)

did.health.full <- update(did.health, . ~ . + age + black + sex + hsgrad + someco + ba + postgrad)
summary(did.health.full)


cov.labels <- c("2006", "Evacuated", "Age", "Race (Black)", "Sex (Female)", "High school", "Some college", "Bachelor's degree", "Post-graduate work", "Treatment (2006 * evacuated)")
col.labels <- c("Weeks worked", "Earnings", "Unemployment income", "Health")
stargazer(did.work, did.work.full, did.income, did.income.full, did.unemp, did.unemp.full, did.health, did.health.full, 
          type="latex", out="q4table.tex", 
          covariate.labels=cov.labels, dep.var.labels=col.labels, star.cutoffs=c(0.05, .01, .001),
          se=list(did.work.se, did.work.full.se, did.income.se, did.income.full.se, did.unemp.se, did.unemp.full.se, NULL, NULL),
          notes="OLS models use robust standard errors")
system("latex2rtf q4table.tex")


# Simulation!
X <- cbind(my.mode(katrina$age), my.mode(katrina$black), 2, 1, 0, 0, 0)  # High school graduates only

# Weeks worked
draw <- mvrnorm(1000, coef(did.work.full), vcov(did.work.full))
noevac.2005 <- draw %*% c(1, 0, 0, X, 0)  # Intercept, after, evacuated, X, after:evacuated
noevac.2006 <- draw %*% c(1, 1, 0, X, 0)  
evac.2005 <- draw %*% c(1, 0, 1, X, 0)
evac.2006 <- draw %*% c(1, 1, 1, X, 1)
plot.data <- data.frame(estimate=c(noevac.2005, noevac.2006, evac.2005, evac.2006), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac.2005)*2), post=rep(c("Before", "After"), each=length(noevac.2005)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")
plot.data$post <- relevel(plot.data$post, "Before")

plot.means <- c(mean(noevac.2005), mean(noevac.2006), mean(evac.2005), mean(evac.2006))
plot.data1 <- data.frame(estimate=plot.means, type=rep(c("Not evacuated", "Evacuated"), each=2), post=c("Before", "After"))
plot.data1$type <- relevel(plot.data1$type, "Not evacuated")

# Get hypotheetical and actual end points
hypo.end <- mean(noevac.2006) - (mean(noevac.2005) - mean(evac.2005))
evac.end <- mean(evac.2006)
evac.begin <- mean(evac.2005)

# Plot!
q4.p5 <- ggplot(plot.data, aes(x=post, y=estimate, fill=type, colour=type))
q4.p5 <- q4.p5 + geom_violin(position="identity", colour=NA, alpha=0.6) + 
  labs(x=NULL, y="Weeks worked\n", title="Number of weeks worked") + 
  annotate("segment", x=1, xend=2, y=evac.begin, yend=hypo.end, colour="#FC8D62", size=1, linetype=3) +
  geom_point(data=plot.data1, size=3) + geom_line(aes(group=type), data=plot.data1, size=1) +
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_fill_brewer(palette="Set2") + 
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  guides(colour="none", shape="none", fill=guide_legend(override.aes=list(shape=NA)))  # Remove extra guides and dot in fill guide
q4.p5


# Income
draw <- mvrnorm(1000, coef(did.income.full), vcov(did.income.full))
noevac.2005 <- draw %*% c(1, 0, 0, X, 0)  # Intercept, after, evacuated, X, after:evacuated
noevac.2006 <- draw %*% c(1, 1, 0, X, 0)  
evac.2005 <- draw %*% c(1, 0, 1, X, 0)
evac.2006 <- draw %*% c(1, 1, 1, X, 1)
plot.data <- data.frame(estimate=c(noevac.2005, noevac.2006, evac.2005, evac.2006), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac.2005)*2), post=rep(c("Before", "After"), each=length(noevac.2005)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")
plot.data$post <- relevel(plot.data$post, "Before")

plot.means <- c(mean(noevac.2005), mean(noevac.2006), mean(evac.2005), mean(evac.2006))
plot.data1 <- data.frame(estimate=plot.means, type=rep(c("Not evacuated", "Evacuated"), each=2), post=c("Before", "After"))
plot.data1$type <- relevel(plot.data1$type, "Not evacuated")

# Get hypotheetical and actual end points
hypo.end <- mean(noevac.2006) - (mean(noevac.2005) - mean(evac.2005))
evac.end <- mean(evac.2006)
evac.begin <- mean(evac.2005)

# Plot!
q4.p6 <- ggplot(plot.data, aes(x=post, y=estimate, fill=type, colour=type))
q4.p6 <- q4.p6 + geom_violin(position="identity", colour=NA, alpha=0.6) + 
  labs(x=NULL, y="Income\n", title="Income") + 
  annotate("segment", x=1, xend=2, y=evac.begin, yend=hypo.end, colour="#FC8D62", size=1, linetype=3) +
  geom_point(data=plot.data1, size=3) + geom_line(aes(group=type), data=plot.data1, size=1) +
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_fill_brewer(palette="Set2") + 
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  scale_y_continuous(labels=dollar) + 
  guides(colour="none", shape="none", fill=guide_legend(override.aes=list(shape=NA)))  # Remove extra guides and dot in fill guide
q4.p6


# Unemployment
draw <- mvrnorm(1000, coef(did.unemp.full), vcov(did.unemp.full))
noevac.2005 <- draw %*% c(1, 0, 0, X, 0)  # Intercept, after, evacuated, X, after:evacuated
noevac.2006 <- draw %*% c(1, 1, 0, X, 0)  
evac.2005 <- draw %*% c(1, 0, 1, X, 0)
evac.2006 <- draw %*% c(1, 1, 1, X, 1)
plot.data <- data.frame(estimate=c(noevac.2005, noevac.2006, evac.2005, evac.2006), type=rep(c("Not evacuated", "Evacuated"), each=length(noevac.2005)*2), post=rep(c("Before", "After"), each=length(noevac.2005)))
plot.data$type <- relevel(plot.data$type, "Not evacuated")
plot.data$post <- relevel(plot.data$post, "Before")

plot.means <- c(mean(noevac.2005), mean(noevac.2006), mean(evac.2005), mean(evac.2006))
plot.data1 <- data.frame(estimate=plot.means, type=rep(c("Not evacuated", "Evacuated"), each=2), post=c("Before", "After"))
plot.data1$type <- relevel(plot.data1$type, "Not evacuated")

# Get hypotheetical and actual end points
hypo.end <- mean(noevac.2006) - (mean(noevac.2005) - mean(evac.2005))
evac.end <- mean(evac.2006)
evac.begin <- mean(evac.2005)

# Plot!
q4.p7 <- ggplot(plot.data, aes(x=post, y=estimate, fill=type, colour=type))
q4.p7 <- q4.p7 + geom_violin(position="identity", colour=NA, alpha=0.6) + 
  labs(x=NULL, y="Unemployment compensation\n", title="Unemployment compensation") + 
  annotate("segment", x=1, xend=2, y=evac.begin, yend=hypo.end, colour="#FC8D62", size=1, linetype=3) +
  geom_point(data=plot.data1, size=3) + geom_line(aes(group=type), data=plot.data1, size=1) +
  theme_bw() + scale_colour_brewer(palette="Set2") + scale_fill_brewer(palette="Set2") + 
  theme(legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  annotate("errorbar", x=2.05, xend=2.05, ymin=evac.end, ymax=hypo.end, colour="darkorange4", width=0.03) + 
  scale_y_continuous(labels=dollar) + 
  guides(colour="none", shape="none", fill=guide_legend(override.aes=list(shape=NA)))  # Remove extra guides and dot in fill guide
q4.p7


# Predicted probabilities for actual betas and taus
predicted.probs <- function(x, X) {
  beta <- x[1:num.betas]
  tau <- x[(num.betas + 1):(num.betas + num.taus)]
  p1 <- plogis(tau[1] - X %*% beta)
  p2 <- plogis(tau[2] - X %*% beta) - plogis(tau[1] - X %*% beta)
  p3 <- plogis(tau[3] - X %*% beta) - plogis(tau[2] - X %*% beta)
  p4 <- plogis(tau[4] - X %*% beta) - plogis(tau[3] - X %*% beta)
  p5 <- 1 - plogis(tau[4] - X %*% beta)
  return(data.frame(p1, p2, p3, p4, p5))
}

beta.ologit <- coef(did.health.full)
tau.ologit <- did.health.full$zeta
num.betas <- length(beta.ologit)
num.taus <- length(tau.ologit)
coefs <- c(beta.ologit, tau.ologit)

noevac.2005 <- predicted.probs(coefs, c(0, 0, X, 0))  # After, evacuated, X, after:evacuated
noevac.2006 <- predicted.probs(coefs, c(1, 0, X, 0)) 
evac.2005 <- predicted.probs(coefs, c(0, 1, X, 0))
evac.2006 <- predicted.probs(coefs, c(1, 1, X, 1))

plot.means <- rbind(noevac.2005, noevac.2006, evac.2005, evac.2006)
plot.data1 <- data.frame(plot.means, type=rep(c("Not evacuated", "Evacuated"), each=2), post=c("Before", "After"))
plot.data1 <- melt(plot.data1, id.vars=c("post", "type"), variable.name="health.cat")
plot.data1$health.cat <- factor(plot.data1$health.cat, labels=health.labels)
plot.data1$type <- relevel(plot.data1$type, "Not evacuated")
plot.data1$post <- relevel(plot.data1$post, "Before")

# Actual simulation
draw <- mvrnorm(500, coefs, vcov(did.health.full))
beta.sim <- draw[,1:num.betas]
tau.sim <- draw[,(num.betas + 1):(num.betas + num.taus)]

# For some reason this:
# apply(draw, 1, FUN=predicted.probs, X=c(0, 0, X1, 0))
# doesn't work, but this does:
noevac.2005.sim <- apply(draw, 1, function(x) predicted.probs(x, X=c(0, 0, X, 0)))
noevac.2006.sim <- apply(draw, 1, function(x) predicted.probs(x, X=c(1, 0, X, 0)))
evac.2005.sim <- apply(draw, 1, function(x) predicted.probs(x, X=c(0, 1, X, 0)))
evac.2006.sim <- apply(draw, 1, function(x) predicted.probs(x, X=c(1, 1, X, 1)))


# Build data really messily
noevac.2005.data <- cbind(post="Before", type="Not evacuated", ldply(noevac.2005.sim))
noevac.2005.data <- melt(noevac.2005.data, id.vars=c("post", "type"), variable.name="health.cat")
noevac.2005.data$health.cat <- factor(noevac.2005.data$health.cat, labels=health.labels)

noevac.2006.data <- cbind(post="After", type="Not evacuated", ldply(noevac.2006.sim))
noevac.2006.data <- melt(noevac.2006.data, id.vars=c("post", "type"), variable.name="health.cat")
noevac.2006.data$health.cat <- factor(noevac.2006.data$health.cat, labels=health.labels)

evac.2005.data <- cbind(post="Before", type="Evacuated", ldply(evac.2005.sim))
evac.2005.data <- melt(evac.2005.data, id.vars=c("post", "type"), variable.name="health.cat")
evac.2005.data$health.cat <- factor(evac.2005.data$health.cat, labels=health.labels)

evac.2006.data <- cbind(post="After", type="Evacuated", ldply(evac.2006.sim))
evac.2006.data <- melt(evac.2006.data, id.vars=c("post", "type"), variable.name="health.cat")
evac.2006.data$health.cat <- factor(evac.2006.data$health.cat, labels=health.labels)

plot.data <- rbind(noevac.2005.data, noevac.2006.data, evac.2005.data, evac.2006.data)
plot.data$type <- relevel(plot.data$type, "Not evacuated")
plot.data$post <- relevel(plot.data$post, "Before")

# Plot everything, finally!
q4.p8 <- ggplot(plot.data, aes(x=post, y=value, fill=type, colour=type))
q4.p8 <- q4.p8 + facet_wrap(~ health.cat) + geom_violin(position="identity", colour=NA, alpha=0.6) + 
  labs(x=NULL, y="Probability of reporting\n", title="Health outcomes\n") + 
  geom_line(aes(group=type), data=plot.data1, size=1) + geom_point(data=plot.data1, size=3) + 
  scale_colour_brewer(palette="Set2") + scale_fill_brewer(palette="Set2") +
  theme_bw() + scale_y_continuous(labels=percent) +
  theme(legend.position=c(1, 0), legend.justification=c(1, 0), legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA), legend.title=element_blank()) + 
  guides(colour="none", shape="none", fill=guide_legend(override.aes=list(shape=NA)))
q4.p8

q4.control <- arrangeGrob(q4.p5, q4.p6, q4.p7, q4.p8, ncol=2)
q4.control
ggsave(q4.control, file="q4_control.pdf", width=6, height=4, scale=2)