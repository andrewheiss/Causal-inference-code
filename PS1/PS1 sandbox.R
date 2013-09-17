library(foreign)
library(car)
library(plyr)
library(lubridate)
library(ggplot2)
star <- read.dta("~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 1/PS1.dta")

# Table 1, panel A
# Free lunch across kindergarten class types
prop.table(xtabs(~ gkclasst + gkfreelu, data=star), 1)[,1]
oneway.test(as.numeric(gkfreelu) ~ gkclasst, data=star, var.equal=TRUE)  # Force as.numeric for ANOVA magic

# White/Asian
summary(star$race)
star$race.fixed <- recode(star$race, "c('WHITE', 'ASIAN')='WHITE.ASIAN'; else='NOT WHITE'")
# Or the long way without recode()
# star$race.fixed <- star$race
# levels(star$race.fixed) <- c("WHITE.ASIAN", "NOT WHITE", "WHITE.ASIAN", "NOT WHITE", "NOT WHITE", "NOT WHITE")
prop.table(xtabs(~ gkclasst + race.fixed, data=star), 1)[,2]
oneway.test(as.numeric(race.fixed) ~ gkclasst, data=star, var.equal=TRUE)


# Age
star$birthmon.fixed <- revalue(star$birthmon, c("ARPIL"="APRIL"))  # Fix typo
star$birthday.actual <- mdy(paste(star$birthmon.fixed, star$birthday, star$birthyea, sep="-"))  # Create POSIX birthday
star$age1985 <- as.numeric(difftime(mdy("09-01-1985"), star$birthday.actual, units="days")/365.25)
ddply(star, ~ gkclasst, summarise, mean=mean(age1985, na.rm=TRUE))
oneway.test(age1985 ~ gkclasst, data=star, var.equal=TRUE)


# Attrition
star$attrition <- as.factor(ifelse(star$flagsgk=="YES" & star$flagsg1=="YES" & star$flagsg2=="YES" & star$flagsg3=="YES", "Stayed", "Dropped"))
prop.table(xtabs(~ gkclasst + attrition, data=star), 1)[,1]
oneway.test(as.numeric(attrition) ~ gkclasst, data=star, var.equal=TRUE)


# Class size
ddply(star, ~ gkclasst, summarise, mean=mean(gkclasss))
oneway.test(gkclasss ~ gkclasst, data=star, var.equal=TRUE)


# Divide by the NA-less length while keeping the NAs when ranking
perc.rank <- function(x) round(rank(x, na.last="keep")/length(na.omit(x)), 2)
star$reading.rank <- perc.rank(star$gktreads)
star$words.rank <- perc.rank(star$gkwordsk)
star$math.rank <- perc.rank(star$gktmaths)
star$avg.test <- (star$reading.rank + star$words.rank + star$math.rank)/3

ddply(star, ~ gkclasst, summarise, mean=mean(avg.test, na.rm=TRUE))
oneway.test(avg.test ~ gkclasst, data=star, var.equal=TRUE)


# Figure out distribution for regular group, get percentile for each score. then get percentile for smaller classes using the regular group distribution.
# Split scores into groups
reading.regular <- ifelse(star$gkclasst != "SMALL CLASS", star$gktreads, NA)
words.regular <- ifelse(star$gkclasst != "SMALL CLASS", star$gkwordsk, NA)
math.regular <- ifelse(star$gkclasst != "SMALL CLASS", star$gktmaths, NA)

reading.small <- ifelse(star$gkclasst == "SMALL CLASS", star$gktreads, NA)
words.small <- ifelse(star$gkclasst == "SMALL CLASS", star$gkwordsk, NA)
math.small <- ifelse(star$gkclasst == "SMALL CLASS", star$gktmaths, NA)

# Calculate the percentile ranks for the regular class scores
reading.regular.perc <- perc.rank(reading.regular)
words.regular.perc <- perc.rank(words.regular)
math.regular.perc <- perc.rank(math.regular)

mean(reading.regular.perc, na.rm=T)

# Take each small class score one at a time, insert it into the regular 
# distribution, calculate the percentile rank, then extract that one score 
# and use it for the small class score.  
acrobatics <- function(x, distribution, rank.function=perc.rank) {
  perc.rank <- function(x) round(rank(x, na.last="keep")/length(na.omit(x)), 2)  # Redefined because parSapply gets mad
  return(rank.function(c(x, distribution))[1])
}

# Use parallel workers to speed this up...
library(parallel)
cl <- makeCluster(10)  # Start parallel workers

# Run the acrobatics() function for each small class test score
reading.small.perc <- parSapply(reading.small, cl=cl, FUN=acrobatics, distribution=reading.regular)
words.small.perc <- parSapply(words.small, cl=cl, FUN=acrobatics, distribution=words.regular)
math.small.perc <- parSapply(math.small, cl=cl, FUN=acrobatics, distribution=math.regular)

stopCluster(cl)  # Stop the parallel workers

# Combine regular and small scores into one column
star$reading.score <- ifelse(is.na(reading.regular.perc), reading.small.perc, reading.regular.perc) 
star$words.score <- ifelse(is.na(words.regular.perc), words.small.perc, words.regular.perc) 
star$math.score <- ifelse(is.na(math.regular.perc), math.small.perc, math.regular.perc) 

# Calculate the average score
star$avg.score <- rowMeans(cbind(star$reading.score, star$words.score, star$math.score), na.rm=TRUE)
ddply(star, ~ gkclasst, summarise, mean=mean(avg.score, na.rm=TRUE))


h# Missing free/reduced lunch
star$lunch.present <- recode(star$gkfreelu, "c('FREE LUNCH', 'NON-FREE LUNCH')='YES'; else='MISSING'")
star$lunch.present
prop.table(xtabs(~ gkclasst + lunch.present, data=star), 1)
oneway.test(as.numeric(lunch.present) ~ gkclasst, data=star, var.equal=TRUE)


#-------------------
# Table 5, Panel A
#-------------------
# OLS function with robust and clustered standard errors
# Via http://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/
ols <- function(form, data, robust=FALSE, cluster=NULL,digits=3){
  r1 <- lm(form, data)
  if(length(cluster)!=0){
    data <- na.omit(data[,c(colnames(r1$model),cluster)])
    r1 <- lm(form, data)
  }
  X <- model.matrix(r1)
  n <- dim(X)[1]
  k <- dim(X)[2]
  if(robust==FALSE & length(cluster)==0){
    se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(r1))/(n-k))))
    res <- cbind(coef(r1),se)
  }
  if(robust==TRUE){
    u <- matrix(resid(r1))
    meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
    dfc <- n/(n-k)    
    se <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
    res <- cbind(coef(r1),se)
  }
  if(length(cluster)!=0){
    clus <- cbind(X,data[,cluster],resid(r1))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
    m <- dim(table(clus[,cluster]))
    dfc <- (m/(m-1))*((n-1)/(n-k))
    uclust  <- apply(resid(r1)*X,2, function(x) tapply(x, clus[,cluster], sum))
    se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)   
    res <- cbind(coef(r1),se)
  }
  res <- cbind(res,res[,1]/res[,2],(1-pnorm(abs(res[,1]/res[,2])))*2)
  res1 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res1) <- rownames(res)
  colnames(res1) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  return(res1)
}

# Clustered standard errors!
# Via http://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
# and http://people.su.se/~ma/clustering.pdf
# and https://stat.ethz.ch/pipermail/r-help/2010-June/242334.html (for handling NAs)
cl <- function(dat,fm, cluster){
  attach(dat, warn.conflicts = F)
  require(sandwich)
  require(lmtest)
  not <- attr(fm$model,"na.action")
  if( ! is.null(not)){   # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  with(dat,{
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL)
  }
  )
}


star$gkclasst <- relevel(star$gkclasst, "REGULAR CLASS")
model1 <- lm(avg.score ~ gkclasst, data=star)
summary(model1)

library(plm)
star$gkschid <- as.factor(star$gkschid)
# plm() chokes on huge datasets
model.data <- star[,c("gktreads", "gktmaths", "gktlists", "gkwordsk",
                      "gkclasst", "gkschid", "stdntid")]

# Model 1
# score ~ gkclasst
model1.form <- ~ gkclasst
model1.reading <- lm(update.formula(model1.form, gktreads ~ .), data=model.data)
model1.math <- lm(update.formula(model1.form, gktmaths ~ .), data=star)
model1.listening <- lm(update.formula(model1.form, gktlists ~ .), data=star)
model1.words <- lm(update.formula(model1.form, gkwordsk ~ .), data=star)

summary(model1.reading)
cl(model.data, model1.reading, model.data$gkschid)


# Model 2
# score ~ gkclasst + school fixed effects
model2.form <- ~ gkclasst
model2.reading <- plm(update.formula(model2.form, gktreads ~ .), 
                      data=model.data, 
                      index=c("gkschid", "stdntid"), 
                      model="within")
model2.math <- plm(update.formula(model2.form, gktmaths ~ .), 
                   data=model.data, 
                   index=c("gkschid", "stdntid"), 
                   model="within")
model2.listening <- plm(update.formula(model2.form, gktlists ~ .), 
                        data=model.data, 
                        index=c("gkschid", "stdntid"), 
                        model="within")
model2.words <- plm(update.formula(model2.form, gkwordsk ~ .), 
                    data=model.data, 
                    index=c("gkschid", "stdntid"), 
                    model="within")
summary(model2.reading)
summary(model2.math)
summary(model2.listening)
summary(model2.words)

# Robust clustered standard errors. Not exact, but HC0 gets the closest to Stata.
# See http://landroni.wordpress.com/2012/06/02/fama-macbeth-and-cluster-robust-by-firm-and-time-standard-errors-in-r/
coeftest(model2.reading, vcov=vcovHC(model2.reading, type="HC0", cluster="group"))
fixef(model2.reading)
pFtest(model2.reading, model1.reading)

coeftest(model2.reading, vcov=vcovHC(model2.reading, type="HC0", cluster="group"))
coeftest(model2.math, vcov=vcovHC(model2.reading, type="HC0", cluster="group"))
coeftest(model2.listening, vcov=vcovHC(model2.reading, type="HC0", cluster="group"))
coeftest(model2.words, vcov=vcovHC(model2.reading, type="HC0", cluster="group"))


# Model 3
# score ~ gkclasst + white + girl + free lunch + school fixed effects
model3.form <- ~ gkclasst + race.fixed + gender + gkfreelu
model3.reading <- plm(update.formula(model3.form, gktreads ~ .), data=star, index="gkschid", model="within", effect="individual", na.action=na.exclude)
model3.reading <- lm(update.formula(model3.form, gktreads ~ . + gkschid), data=star)


summary(model3.reading)

# Model 4
# score ~ gkclasst + white + girl + free lunch + white teacher + teacher experience + MA + school fixed effects
