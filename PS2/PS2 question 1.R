#-------------
# Question 1
#-------------
katrina.2006.evac <- subset(katrina, year==2006 & everevac==1)

# Weeks worked
t.test(katrina.2006.evac$wkswork)

# Earnings
t.test(katrina.2006.evac$earnings)

# Unemployment
t.test(katrina.2006.evac$unempinc)

# Health
(health.table <- xtabs(~ health.cat, data=katrina.2006.evac))
chisq.test(health.table)
prop.table(health.table)

# Plot stuff
# Regular variables
plot.data <- melt(katrina.2006.evac[,c("wkswork","earnings","unempinc")])
levels(plot.data$variable) <- c("Weeks worked", "Earnings", "Unemployment income")
q1.p1 <- ggplot(aes(x=value, y = ..scaled.., fill=variable), data=plot.data)
q1.p1 <- q1.p1 + geom_density() + facet_grid(. ~ variable, scales="free") +
  labs(x=NULL, y="Scaled density\n", title="Employment outcomes\n") + 
  theme_bw() + scale_fill_brewer(palette="Accent") + theme(legend.position="none")
q1.p1

# Health
plot.data <- data.frame(prop.table(health.table))
q1.p2 <- ggplot(plot.data, aes(health.cat, Freq))
q1.p2 <- q1.p2 + geom_bar(stat="identity", fill="#386CB0") + 
  labs(x=NULL, y="Frequency\n", title="Health status\n") + 
  theme_bw() + scale_y_continuous(labels=percent)
q1.p2

q1 <- arrangeGrob(q1.p1, q1.p2, ncol=1)
q1
ggsave(q1, file="q1.pdf", width=6, height=4, scale=2)