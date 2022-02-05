library(ineq)
library(dplyr)
library(ggplot2)
setwd("/Users/yicanliu/Desktop/Study/2022 Spring/Econ 613/Homework/HW1")

#################### Exercise 1 ####################
rm(list = ls())
data.datind2009 <- subset(read.csv("Data/datind2009.csv", header = TRUE), select=c(age, wage) )     
data.datind2009 <- na.omit(data.datind2009)
X <- data.frame(data.datind2009$age, rep(1, length(data.datind2009$age) ) )
names(X) <- c("age", "cons")
y <- data.frame(data.datind2009$wage)
names(y) <- c("wage")

# Question 1.1
cor(x = X$age, y = y, use="complete.obs")

# Question 1.2
X.matrix <- as.matrix(X)
y.matrix <- as.matrix(y)
beta.est <- solve(t(X.matrix) %*% X.matrix ) %*% t(X.matrix) %*% y.matrix
print(beta.est)

# Question 1.3
# Standard errors of beta: using standard formulas of the OLS
y.res.matrix <- y.matrix - X.matrix %*% beta.est
N <- length(y.res.matrix) 
var.y.res.matrix <-  t(y.res.matrix) %*% y.res.matrix / ( N - 2 )
var.beta <- var.y.res.matrix[1, 1] * solve(t(X.matrix) %*% X.matrix)
sd.beta.age <- sqrt(var.beta[1, 1])
print(sd.beta.age)
sd.cons <- sqrt(var.beta[2, 2])
print(sd.cons)
# Bootstrap
## 1. first round
bs.beta.est <- c()
bs.cons.est <- c()
for( i in 1:49 ) {
  sample.data <- data.datind2009[sample(nrow(data.datind2009), 1000, replace = FALSE), ]
  sample.X <- data.frame(sample.data$age, rep(1, length(sample.data$age) ) )
  names(sample.X) <- c("age", "cons")
  sample.y <- data.frame(sample.data$wage)
  names(sample.y) <- c("wage")
  sample.X.matrix <- as.matrix(sample.X)
  sample.y.matrix <- as.matrix(sample.y)
  sample.beta.est <- solve(t(sample.X.matrix) %*% sample.X.matrix ) %*% t(sample.X.matrix) %*% sample.y.matrix
  #print(sample.beta.est)
  bs.beta.est <- append(bs.beta.est, sample.beta.est[1])
  bs.cons.est <- append(bs.cons.est, sample.beta.est[2])
}
summary(bs.beta.est)
sd(bs.beta.est)
summary(bs.cons.est)
sd(bs.cons.est)
## 2. second round
bs.beta.est <- c()
bs.cons.est <- c()
for( i in 1:499 ) {
  sample.data <- data.datind2009[sample(nrow(data.datind2009), 1000, replace = FALSE), ]
  sample.X <- data.frame(sample.data$age, rep(1, length(sample.data$age) ) )
  names(sample.X) <- c("age", "cons")
  sample.y <- data.frame(sample.data$wage)
  names(sample.y) <- c("wage")
  sample.X.matrix <- as.matrix(sample.X)
  sample.y.matrix <- as.matrix(sample.y)
  sample.beta.est <- solve(t(sample.X.matrix) %*% sample.X.matrix ) %*% t(sample.X.matrix) %*% sample.y.matrix
  #print(sample.beta.est)
  bs.beta.est <- append(bs.beta.est, sample.beta.est[1])
  bs.cons.est <- append(bs.cons.est, sample.beta.est[2])
}
summary(bs.beta.est)
sd(bs.beta.est)
summary(bs.cons.est)
sd(bs.cons.est)


#################### Exercise 2 ####################
rm(list = ls())
data.all.datind <- read.csv("Data/datind2005.csv", header = TRUE)
data.all.datind$year <- 2005
for (year in 2006:2018) {
  data.this.datind <- read.csv( paste0("Data/datind", as.character(year), ".csv"), header = TRUE)
  data.this.datind$year <- year
  data.all.datind <- rbind(data.all.datind, data.this.datind)
  rm(data.this.datind)
}
data.all.datind <- subset(data.all.datind, select=c(age, wage, year))
data.all.datind <- na.omit(data.all.datind)

# Question 2.1 
# Generate categorical variables
data.all.datind$cat.age <- ifelse(data.all.datind$age >= 18 & data.all.datind$age <= 25, "18to25", "")
data.all.datind$cat.age.var <- ifelse(data.all.datind$age >= 18 & data.all.datind$age <= 25, 1, 0)
data.all.datind$cat.age[data.all.datind$age >= 26 & data.all.datind$age <= 30] <- "26to30"
data.all.datind$cat.age.var[data.all.datind$age >= 26 & data.all.datind$age <= 30] <- 2
data.all.datind$cat.age[data.all.datind$age >= 31 & data.all.datind$age <= 35] <- "31to35"
data.all.datind$cat.age.var[data.all.datind$age >= 31 & data.all.datind$age <= 35] <- 3
data.all.datind$cat.age[data.all.datind$age >= 36 & data.all.datind$age <= 40] <- "36to40"
data.all.datind$cat.age.var[data.all.datind$age >= 36 & data.all.datind$age <= 40] <- 4
data.all.datind$cat.age[data.all.datind$age >= 41 & data.all.datind$age <= 45] <- "41to45"
data.all.datind$cat.age.var[data.all.datind$age >= 41 & data.all.datind$age <= 45] <- 5
data.all.datind$cat.age[data.all.datind$age >= 46 & data.all.datind$age <= 50] <- "46to50"
data.all.datind$cat.age.var[data.all.datind$age >= 46 & data.all.datind$age <= 50] <- 6
data.all.datind$cat.age[data.all.datind$age >= 51 & data.all.datind$age <= 55] <- "51to55"
data.all.datind$cat.age.var[data.all.datind$age >= 51 & data.all.datind$age <= 55] <- 7
data.all.datind$cat.age[data.all.datind$age >= 56 & data.all.datind$age <= 60] <- "56to60"
data.all.datind$cat.age.var[data.all.datind$age >= 56 & data.all.datind$age <= 60] <- 8
data.all.datind$cat.age[data.all.datind$age >= 61] <- "above60"
data.all.datind$cat.age.var[data.all.datind$age >= 61] <- 9

data.all.datind <- data.all.datind[data.all.datind$cat.age != "", ]


# Question 2.2
# Plot the wage of each age group
mean.wage.by.group <- aggregate(data.all.datind$wage, list(data.all.datind$year, data.all.datind$cat.age), FUN=mean)
mean.wage.by.group <- mean.wage.by.group[mean.wage.by.group$Group.2 != "", ]
names(mean.wage.by.group) = c("year", "age.group", "mean.wage")
pdf("Output/hw2_figure6_trend_meanwagebygroup.pdf")
ggplot(mean.wage.by.group, aes(x=year, y=mean.wage, group=age.group)) + geom_line(aes(color = age.group) ) + geom_point() + 
  labs(title="Mean Wage by Year of Different Age Groups", x = "Year", y = "Mean Wage", color = "Groups")
dev.off()

data.all.datind$year <- as.character(data.all.datind$year)
pdf("Output/hw2_figure7_trend_meanwagebygroup_boxplot.pdf")
ggplot(data.all.datind, aes(x=year, y=wage)) + geom_boxplot() + facet_wrap(~cat.age)  + scale_y_continuous(limits = c(NA, 100000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Question 2.3
# Regression with time FE
X <- subset(data.all.datind, select=c(age, year))
# I select 2015 as the base
for (year in 2006:2018) {
  X[paste0("dummy", as.character(year) )] = ifelse(X$year == year, 1, 0)
}
X$cons <- 1
X <- subset(X, select=-c(year))
y <- data.all.datind$wage
X.matrix <- as.matrix(X)
y.matrix <- as.matrix(y)
beta.est <- solve(t(X.matrix) %*% X.matrix ) %*% t(X.matrix) %*% y.matrix
print(beta.est)


#################### Exercise 3 ####################
rm(list = ls())
data.datind2007 <- read.csv("Data/datind2007.csv", header = TRUE)   
data.datind2007 <- data.datind2007[data.datind2007$empstat!="Inactive" & data.datind2007$empstat!="Retired", ]

# Question 3.1
data.datind2007$dummy.emp <- ifelse(data.datind2007$empstat == "Employed", 1, 0)

# Question 3.2
# We estimate the likelihood
likelihood <- function(beta, y, x) {
  x.beta <- beta[1] + beta[2]*x
  prob.y.est <- pnorm(x.beta)
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

# Question 3.3
# For age and employment, I write the function as below
mle.opt.age.and.emp <- optim(fn=likelihood, 
                             par= c(0, 0),
                             lower = c(-Inf, -Inf), 
                             upper = c(Inf, Inf),
                             x = data.datind2007$age,
                             y = data.datind2007$dummy.emp
                             )
print(mle.opt.age.and.emp$par)

# Question 3.4
# For wage and emploment , I only need to change the x in the above command...
# mle.opt.wage.and.emp <- optim(fn=likelihood, 
#                              par= c(1, 1),
#                              lower = c(-Inf, -Inf), 
#                              upper = c(Inf, Inf),
#                              x = data.datind2007$wage,
#                              y = data.datind2007$dummy.emp
#                              ) 


#################### Exercise 4 ####################
rm(list = ls())
# Question 4.1
data.all.datind <- read.csv("Data/datind2005.csv", header = TRUE)
data.all.datind$year <- 2005
for (year in 2006:2015) {
  data.this.datind <- read.csv( paste0("Data/datind", as.character(year), ".csv"), header = TRUE)
  data.this.datind$year <- year
  data.all.datind <- rbind(data.all.datind, data.this.datind)
  rm(data.this.datind)
}
data.all.datind <- subset(data.all.datind, select = c(age, year, empstat))
data.all.datind <- data.all.datind[data.all.datind$empstat!="Inactive" & data.all.datind$empstat!="Retired", ]
# I use 2005 as the benchmark for FE
for (year in 2006:2015) {
  data.all.datind[paste0("dummy", as.character(year) )] = ifelse(data.all.datind$year == year, 1, 0)
}
data.all.datind$dummy.emp <- ifelse(data.all.datind$empstat == "Employed", 1, 0)
data.all.datind <- subset(data.all.datind, select=-c(empstat))

# Question 4.2
probit.likelihood <- function(beta, x, y) {
  #print(y)
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  #print(x.beta)
  prob.y.est <- pnorm(x.beta)
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

logistic.likelihood <- function(beta, x, y) {
  #print(y)
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  #print(x.beta)
  prob.y.est <- exp(x.beta) / (1+exp(x.beta))
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

linear.likelihood <- function(beta, x, y) {
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  prob.y.est <- x.beta
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

x.with.time.fe = subset(data.all.datind, select=-c(dummy.emp, year))
x.with.time.fe$cons = 1
dummy.emp = data.all.datind$dummy.emp

mle.opt.age.and.emp.probit <- optim(fn=probit.likelihood, 
                                    par= rep(0, length(names(x.with.time.fe))),
                                    lower = c(-Inf, -Inf), 
                                    upper = c(Inf, Inf),
                                    x = x.with.time.fe,
                                    y = dummy.emp
)
names(x.with.time.fe)
print(mle.opt.age.and.emp.probit$par)

mle.opt.age.and.emp.logistic <- optim(fn=logistic.likelihood, 
                                      par= rep(0, length(names(x.with.time.fe))),
                                      lower = c(-Inf, -Inf), 
                                      upper = c(Inf, Inf),
                                      x = x.with.time.fe,
                                      y = dummy.emp
)
names(x.with.time.fe)
print(mle.opt.age.and.emp.logistic$par)

mle.opt.age.and.emp.linear <- optim(fn=linear.likelihood, 
                                    par= rep(0, length(names(x.with.time.fe))),
                                    lower = c(-Inf, -Inf), 
                                    upper = c(Inf, Inf),
                                    x = x.with.time.fe,
                                    y = dummy.emp
)
names(x.with.time.fe)
print(mle.opt.age.and.emp.linear$par)
# glm(dummy.emp ~ x.with.time.fe$age+x.with.time.fe$dummy2006+x.with.time.fe$dummy2007+x.with.time.fe$dummy2008+x.with.time.fe$dummy2009+x.with.time.fe$dummy2010+x.with.time.fe$dummy2011+x.with.time.fe$dummy2012++x.with.time.fe$dummy2013++x.with.time.fe$dummy2014+x.with.time.fe$dummy2015,family = binomial(link = "probit"))



#################### Exercise 5 ####################
rm(list = ls())
# Question 5.1


# Question 5.2
probit.likelihood <- function(beta, x, y) {
  #print(y)
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  #print(x.beta)
  prob.y.est <- pnorm(x.beta)
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

logistic.likelihood <- function(beta, x, y) {
  #print(y)
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  #print(x.beta)
  prob.y.est <- exp(x.beta) / (1+exp(x.beta))
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

linear.likelihood <- function(beta, x, y) {
  x.matrix <- as.matrix(x)
  x.beta <- x.matrix %*% beta
  prob.y.est <- x.beta
  likelihood.negative <- -sum( (y*log(prob.y.est)) + (1-y)*log(1-prob.y.est) )
  return(likelihood.negative) 
}

data.all.datind <- read.csv("Data/datind2005.csv", header = TRUE)
data.all.datind$year <- 2005
for (year in 2006:2015) {
  data.this.datind <- read.csv( paste0("Data/datind", as.character(year), ".csv"), header = TRUE)
  data.this.datind$year <- year
  data.all.datind <- rbind(data.all.datind, data.this.datind)
  rm(data.this.datind)
}
data.all.datind <- subset(data.all.datind, select = c(age, year, empstat))
data.all.datind <- data.all.datind[data.all.datind$empstat!="Inactive" & data.all.datind$empstat!="Retired", ]
# I use 2005 as the benchmark for FE
for (year in 2006:2015) {
  data.all.datind[paste0("dummy", as.character(year) )] = ifelse(data.all.datind$year == year, 1, 0)
}
data.all.datind$dummy.emp <- ifelse(data.all.datind$empstat == "Employed", 1, 0)
data.all.datind <- subset(data.all.datind, select=-c(empstat))


bs.marginal.effect.est.probit <- c()
for( i in 1:1000 ) {
  sample.data <- data.all.datind[sample(nrow(data.all.datind), 5000, replace = FALSE), ]
  #print(sample.data)
  sample.x.with.time.fe = subset(sample.data, select=-c(dummy.emp, year))
  sample.x.with.time.fe$cons = 1
  sample.dummy.emp = sample.data$dummy.emp
  sample.mle.opt.age.and.emp.probit <- optim(fn=probit.likelihood, 
                                      par= rep(0, length(names(sample.x.with.time.fe))),
                                      lower = c(-Inf, -Inf), 
                                      upper = c(Inf, Inf),
                                      x = sample.x.with.time.fe,
                                      y = sample.dummy.emp
  )
  bs.marginal.effect.est.probit <- append(bs.marginal.effect.est.probit, sample.mle.opt.age.and.emp.probit$par[1])
}
summary(bs.marginal.effect.est.probit)
sd(bs.marginal.effect.est.probit)

bs.marginal.effect.est.logistic <- c()
for( i in 1:1000 ) {
  sample.data <- data.all.datind[sample(nrow(data.all.datind), 5000, replace = FALSE), ]
  #print(sample.data)
  sample.x.with.time.fe = subset(sample.data, select=-c(dummy.emp, year))
  sample.x.with.time.fe$cons = 1
  sample.dummy.emp = sample.data$dummy.emp
  sample.mle.opt.age.and.emp.logistic <- optim(fn=logistic.likelihood, 
                                             par= rep(0, length(names(sample.x.with.time.fe))),
                                             lower = c(-Inf, -Inf), 
                                             upper = c(Inf, Inf),
                                             x = sample.x.with.time.fe,
                                             y = sample.dummy.emp
  )
  bs.marginal.effect.est.logistic <- append(bs.marginal.effect.est.logistic, sample.mle.opt.age.and.emp.logistic$par[1])
}
summary(bs.marginal.effect.est.logistic)
sd(bs.marginal.effect.est.logistic)











