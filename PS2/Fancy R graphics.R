#------------
# Functions
#------------
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(grid)


ologit.spaghetti <- function(model, X, y.range, y.plot.label, legend.title, cat.labels, x.labels, x.plot.label, runs=500, y.limit=c(0, 0.75)) {
  library(ggplot2)
  library(reshape2)
  library(plyr)
  library(scales)
  
  # Get predicted probabilities for given taus and betas
  tau.beta <- function(x) {
    beta <- x[1:num.betas]
    tau <- x[(num.betas + 1):(num.betas + num.taus)]
    p1 <- plogis(tau[1] - X %*% beta)
    p2 <- plogis(tau[2] - X %*% beta) - plogis(tau[1] - X %*% beta)
    p3 <- plogis(tau[3] - X %*% beta) - plogis(tau[2] - X %*% beta)
    p4 <- plogis(tau[4] - X %*% beta) - plogis(tau[3] - X %*% beta)
    p5 <- 1 - plogis(tau[4] - X %*% beta)
    return(data.frame(p1, p2, p3, p4, p5))
  }
  
  num.betas <- length(coef(model))
  num.taus <- length(model$zeta)
  
  
  #--------------------------
  # Predicted probabilities
  #--------------------------
  # Extract betas and taus from model
  beta.ologit <- coef(model)
  tau.ologit <- model$zeta
  
  # Predicted probabilities for actual betas and taus
  single <- tau.beta(c(beta.ologit, tau.ologit))
  
  # Reshape data for ggplot
  single.plot <- cbind(xvar=y.range, single)
  single.plot <- melt(single.plot, id.vars="xvar", variable.name="category")
  single.plot$category <- factor(single.plot$category, labels=cat.labels)
  single.plot$xvar <- factor(single.plot$xvar, labels=x.labels)
  
  # Set up simulation and extract betas and taus
  draw <- mvrnorm(runs, c(beta.ologit, tau.ologit), vcov(model))
  beta.sim <- draw[,1:num.betas]
  tau.sim <- draw[,(num.betas + 1):(num.betas + num.taus)]
  
  # Predicted probabilities for simulated set of betas and taus
  simulation.list <- apply(draw, 1, FUN=tau.beta)
  
  # Reshape data for ggplot
  plot.data <- cbind(simulation=rep(1:runs, each=length(y.range)), xvar=rep(y.range, runs), ldply(simulation.list))
  plot.data <- melt(plot.data, id.vars=c("simulation", "xvar"), variable.name="category")
  plot.data$category <- factor(plot.data$category, labels=cat.labels)
  plot.data$xvar <- factor(plot.data$xvar, labels=x.labels)
  
  head(plot.data)
  # Spaghetti plot of predicted probabilities
  p <- ggplot()
  p <- p + geom_line(aes(x=xvar, y=value, group=category, colour=category), data=single.plot, size=2) + 
    geom_line(aes(x=xvar, y=value, group=interaction(simulation, category), colour=category), alpha=0.05, data=plot.data, size=1) + 
    scale_colour_brewer(palette="Set1") +
#     scale_colour_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1, size=2))) + 
    scale_y_continuous(labels=percent) + 
    coord_cartesian(ylim=y.limit) + 
    labs(y=y.plot.label, x=x.plot.label, colour=legend.title) + 
    theme_bw() + 
    #     theme(text=element_text(family="Source Sans Pro", size=13)) + 
    theme(plot.background=element_rect(fill=NA, colour=NA), legend.position="top", legend.key=element_blank(), legend.background=element_rect(fill=NA, colour=NA))
  p
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}