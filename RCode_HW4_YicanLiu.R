library(ineq)
library(dplyr)
library(ggplot2)
library(panelr)

setwd("/Users/yicanliu/Desktop/Study/2022 Spring/Econ 613/Homework/HW4")

data.data4 <- read.csv("Data/dat_A4.csv", header = TRUE) 
data.data4 <- data.data4[!is.na(data.data4$YINC_1700_2019), ]
data.data4 <- data.data4[ !data.data4$YINC_1700_2019 < 0, ]
data.data4[is.na(data.data4)] <- 0
data.data4.panel <- read.csv("Data/dat_A4_panel.csv", header = TRUE)
data.data4.panel[is.na(data.data4.panel)] <- 0


transform_education <- function(education_list) {
  list.education.transformed <- c()
  for(edu in education_list) {
    if(edu == 1) {
      list.education.transformed <- append(list.education.transformed, 4)
    } else if(edu == 2) {
      list.education.transformed <- append(list.education.transformed, 12)
    } else if(edu == 3) {
      list.education.transformed <- append(list.education.transformed, 14)
    } else if(edu == 4) {
      list.education.transformed <- append(list.education.transformed, 16)
    } else if(edu == 5) {
      list.education.transformed <- append(list.education.transformed, 18)
    } else if(edu == 6) {
      list.education.transformed <- append(list.education.transformed, 23)
    } else if(edu == 7) {
      list.education.transformed <- append(list.education.transformed, 22)
    } else {
      list.education.transformed <- append(list.education.transformed, 0)
    }
  }
  return(list.education.transformed)
}




########################## Part 1 ##########################

# Exercise 1
# Question 1.1
data.data4$female <- data.data4$KEY_SEX_1997 - 1
data.data4$age <- 2019 - data.data4$KEY_BDATE_Y_1997 
data.data4$children <- data.data4$CV_BIO_CHILD_HH_U18_2019
data.data4$work_exp <- (data.data4$CV_WKSWK_JOB_DLI.01_2019 / 52) +
  data.data4$CV_WKSWK_JOB_DLI.02_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.03_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.04_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.05_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.06_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.07_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.08_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.09_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.10_2019 / 52 + 
  data.data4$CV_WKSWK_JOB_DLI.11_2019 / 52

# Question 1.2
# Transfer the education data
data.data4$total_edu <- transform_education(data.data4$YSCH.3113_2019)

# Question 1.3
# Plot the income data by different groups
data.data4.posinc <- data.data4[data.data4$YINC_1700_2019 > 0, ]

pdf("Output/figure1_income_age.pdf")
boxplot(YINC_1700_2019 ~ age, data=data.data4,  xlab="age", ylab="Income")
dev.off()

pdf("Output/figure2_income_gender.pdf")
boxplot(YINC_1700_2019 ~ KEY_SEX_1997, data=data.data4,  xlab="Gender", ylab="Income")
dev.off()

pdf("Output/figure3_income_child.pdf")
boxplot(YINC_1700_2019 ~ CV_BIO_CHILD_HH_U18_2019, data=data.data4,  xlab="number of children", ylab="Income")
dev.off()

# Table the share of zero incone by different groups
data.data4$dummy_zero.income <- 0

for (i in 1:nrow(data.data4)){
  if(data.data4$YINC_1700_2019[i] == 0) {
    data.data4$dummy_zero.income[i] = 1
  }
}

data.data4$dummy_pos.income <- 0

for (i in 1:nrow(data.data4)){
  if(data.data4$YINC_1700_2019[i] != 0) {
    data.data4$dummy_pos.income[i] = 1
  }
}

data.data4.por.zeroinc.age <- aggregate(data.data4$dummy_zero.income, list(data.data4$age), FUN=mean)
names(data.data4.por.zeroinc.age) <- c("age", "% of zero income")
print(data.data4.por.zeroinc.age)

data.data4.por.zeroinc.gender <- aggregate(data.data4$dummy_zero.income, list(data.data4$female), FUN=mean)
names(data.data4.por.zeroinc.gender) <- c("gender", "% of zero income")
print(data.data4.por.zeroinc.gender)

data.data4.por.zeroinc.mari.child <- aggregate(data.data4$dummy_zero.income, list(data.data4$CV_MARSTAT_COLLAPSED_2019, data.data4$CV_BIO_CHILD_HH_U18_2019), FUN=mean)
names(data.data4.por.zeroinc.mari.child) <- c("marital status", "number of children", "% of zero income")
print(data.data4.por.zeroinc.mari.child)

data.data4.por.zeroinc.child <- aggregate(data.data4$dummy_zero.income, list(data.data4$CV_BIO_CHILD_HH_U18_2019), FUN=mean)
names(data.data4.por.zeroinc.child) <- c( "number of children", "% of zero income")
print(data.data4.por.zeroinc.child)

data.data4.por.zeroinc.mari <- aggregate(data.data4$dummy_zero.income, list(data.data4$CV_MARSTAT_COLLAPSED_2019), FUN=mean)
names(data.data4.por.zeroinc.mari) <- c("marital status", "% of zero income")
print(data.data4.por.zeroinc.mari)


# Exercise 2
# Question 2.1
# Running OLS
ols.model <- lm(data.data4.posinc$YINC_1700_2019 ~ data.data4.posinc$age + 
                  data.data4.posinc$work_exp + data.data4.posinc$total_edu + data.data4.posinc$children + data.data4.posinc$female)
summary(ols.model)
# Question 2.2
# Question 2.3
# Heckman two-step estimator
heckman.glm.model <- glm( data.data4$dummy_pos.income ~ data.data4$age + 
                           data.data4$work_exp + data.data4$total_edu + data.data4$children + data.data4$female, 
                         family = binomial(link = "probit") )

heckman.glm.model.step1.pdf <- dnorm(heckman.glm.model$linear.predictors)
heckman.glm.model.step1.cdf <- pnorm(heckman.glm.model$linear.predictors)
heckman.glm.model.imr <- heckman.glm.model.step1.pdf / heckman.glm.model.step1.cdf

heckman.step2.ols <- lm(data.data4$YINC_1700_2019 ~ data.data4$age + 
                          data.data4$work_exp + data.data4$total_edu + data.data4$children + data.data4$female + 
                          heckman.glm.model.imr)
summary(heckman.step2.ols)
# Exercise 3
# Question 3.1
pdf("Output/figure4_hist_inc.pdf")
hist(data.data4$YINC_1700_2019)
dev.off()

# Question 3.2 and 3.3 
# The likelihood function of tobit model is 

likelihood_tobit <- function(beta0, y, X, upper_bound) {
  
  sigma <- exp(beta0[length(beta0)]) 
  beta  <- beta0[-length(beta0)]
  
  dummy_censor <- (y < upper_bound)
  fitted_y <- X %*% beta
  
  likelihood = sum(dummy_censor * log( (1/sigma)*dnorm((y-fitted_y)/sigma)) ) + 
    sum( (1-dummy_censor) * log(pnorm( (fitted_y-upper_bound)/sigma ) ) )
  
  return(-likelihood)
  
}

init.ols <- lm(data.data4.posinc$YINC_1700_2019 ~ data.data4.posinc$age + data.data4.posinc$work_exp + data.data4.posinc$total_edu + data.data4.posinc$female + data.data4.posinc$children)
init.for.optim <- c(coef(init.ols), log_sigma = log(summary(init.ols)$sigma))

tobit.model <- optim(
  par = as.matrix(init.for.optim),
  fn = likelihood_tobit,
  y  = as.matrix(data.data4.posinc$YINC_1700_2019),
  X  = as.matrix( data.frame(rep(1, length(data.data4.posinc$age) ), subset(data.data4.posinc, select=c(age, work_exp, total_edu, female, children) ) ) ),
  upper_bound = 100000.0,
  method  = 'BFGS',
  control = list(reltol = 1e-15)
)

print(tobit.model)

tobit.model.vglm <- vglm(data.data4.posinc$YINC_1700_2019 ~ data.data4.posinc$age + data.data4.posinc$work_exp + data.data4.posinc$total_edu + data.data4.posinc$female + data.data4.posinc$children,
                         tobit(Upper = 100000))

summary(tobit.model.vglm)

ols.model.new <- lm(data.data4.posinc$YINC_1700_2019 ~ data.data4.posinc$age + data.data4.posinc$work_exp + data.data4.posinc$total_edu + data.data4.posinc$female + data.data4.posinc$children)
summary(ols.model.new)
########################## Part 2 ##########################
# for(i in 1:19) {
#   print(data.data4.panel[paste("YINC.1700_", as.character(i)) ])
# }
for(year in 1997:2019) {
  work.var.list <- c()
  for(name in names(data.data4.panel) ) {
    if( (substr(name, nchar(name)-3, nchar(name))==as.character(year) ) & 
        (substr(name, 1, 16) == "CV_WKSWK_JOB_DLI") ) {
      work.var.list <- append(work.var.list, name)
    }
  }
  if(length(work.var.list) > 0) {
    data.data4.panel[paste("work_exp", as.character(year), sep="")] <- rowSums(data.data4.panel[work.var.list] ) 
  }
}

list.eduvar <- c()

for(year in 1997:2019) {
  for(name in names(data.data4.panel) ) {
    if( (substr(name, nchar(name)-3, nchar(name))==as.character(year) ) & (substr(name, 1, 17)=="CV_HIGHEST_DEGREE") )  {
      list.eduvar <- append(list.eduvar, name )
    }
  }
}

data.data4.edu <- data.data4.panel[append("X", list.eduvar)]
data.data4.edu <- select(data.data4.edu, select = -c("CV_HIGHEST_DEGREE_1011_2010", 
                                                     "CV_HIGHEST_DEGREE_1112_2011", 
                                                     "CV_HIGHEST_DEGREE_1314_2013"
                                                     ))
year.index <- c(1998:2011, 2013, 2015, 2017, 2019)
names(data.data4.edu) <- c("X", paste("Degree", year.index, sep=""))
data.data4.edu$Degree1998 <- transform_education(data.data4.edu$Degree1998)
data.data4.edu$Degree1999 <- transform_education(data.data4.edu$Degree1999)
data.data4.edu$Degree2000 <- transform_education(data.data4.edu$Degree2000)
data.data4.edu$Degree2001 <- transform_education(data.data4.edu$Degree2001)
data.data4.edu$Degree2002 <- transform_education(data.data4.edu$Degree2002)
data.data4.edu$Degree2003 <- transform_education(data.data4.edu$Degree2003)
data.data4.edu$Degree2004 <- transform_education(data.data4.edu$Degree2004)
data.data4.edu$Degree2005 <- transform_education(data.data4.edu$Degree2005)
data.data4.edu$Degree2006 <- transform_education(data.data4.edu$Degree2006)
data.data4.edu$Degree2007 <- transform_education(data.data4.edu$Degree2007)
data.data4.edu$Degree2008 <- transform_education(data.data4.edu$Degree2008)
data.data4.edu$Degree2009 <- transform_education(data.data4.edu$Degree2009)
data.data4.edu$Degree2010 <- transform_education(data.data4.edu$Degree2010)
data.data4.edu$Degree2011 <- transform_education(data.data4.edu$Degree2011)
data.data4.edu$Degree2013 <- transform_education(data.data4.edu$Degree2013)
data.data4.edu$Degree2015 <- transform_education(data.data4.edu$Degree2015)
data.data4.edu$Degree2017 <- transform_education(data.data4.edu$Degree2017)
data.data4.edu$Degree2019 <- transform_education(data.data4.edu$Degree2019)

# long_panel(data.data4.edu, prefix = "Degree", begin = 1998, end = 2019, label_location = "end")

list.edu <- c()
list.x <- c()
list.year <- c()
for(year in year.index) {
  list.x <- c(list.x, data.data4.edu$X)
  list.edu <- c(list.edu, data.data4.edu[, paste("Degree", as.character(year), sep="")  ])
  list.year <- c(list.year, rep(year, length(data.data4.edu$X) ) )
}

data.edu.panel <- data.frame(list.x, list.year, list.edu)
names(data.edu.panel) <- c("X", "year", "total.edu")


# reshape(data.data4.panel, 
#         varying  = c("total_work", "YINC.1700_", "CV_HIGHEST_DEGREE_0001_", "CV_MARSTAT_COLLAPSED_"),
#         direction = "long")

list.x <- c()
list.total_work <- c()
list.year <- c()
list.marstat <- c()
list.income <- c()

for(year in year.index) {
  list.x <- c(list.x, data.data4.panel$X)
  list.total_work <- c(list.total_work, data.data4.panel[, paste("work_exp", as.character(year), sep="")])
  list.year <- c(list.year, rep(year, length(data.data4.panel$X) ) )
  list.marstat <- c(list.marstat, data.data4.panel[, paste("CV_MARSTAT_COLLAPSED_", as.character(year), sep="")] )
  list.income <- c(list.income, data.data4.panel[, paste("YINC.1700_", as.character(year), sep="") ])
}
data.panel <- data.frame(list.x, list.year, list.total_work, list.marstat, list.income )
names(data.panel) <- c("X", "year", "total.work", "marstat", "income")

data.panel <- left_join(data.panel, data.edu.panel, by=c("X", "year"))
data.panel <- left_join(data.panel, subset(data.data4.panel, select = c("KEY_BDATE_Y_1997", "X")), by=c("X") )

data.panel$age <-  data.panel$year - data.panel$KEY_BDATE_Y_1997

# For within estimator 
data.mean.income <- aggregate(data.panel$income, by=list(data.panel$X), FUN=mean)
names(data.mean.income) <- c("X", "mean.income")

data.mean.edu <- aggregate(data.panel$total.edu, by=list(data.panel$X), FUN=mean)
names(data.mean.edu) <- c("X", "mean.total.edu")

data.mean.work <- aggregate(data.panel$total.work, by=list(data.panel$X), FUN=mean)
names(data.mean.work) <- c("X", "mean.total.work")

data.panel <- left_join(data.panel, data.mean.income, by = "X")
data.panel <- left_join(data.panel, data.mean.edu, by = "X")
data.panel <- left_join(data.panel, data.mean.work, by = "X")

data.panel$income.demeaned <- data.panel$income - data.panel$mean.income
data.panel$edu.demeaned <- data.panel$total.edu - data.panel$mean.total.edu
data.panel$work.demeaned <- data.panel$total.work - data.panel$mean.total.work

panel.lm.within <- lm(data.panel$income.demeaned ~ data.panel$edu.demeaned + data.panel$work.demeaned)
summary(panel.lm.within)

# For between estimator
data.panel.between <- left_join(data.mean.income, data.mean.edu, by = "X")
data.panel.between <- left_join(data.panel.between, data.mean.work, by = "X")
panel.lm.between <- lm(data.panel.between$mean.income ~ data.panel.between$mean.total.edu + data.panel.between$mean.total.work)
summary(panel.lm.between)

# For difference estimator
data.panel <- data.panel[order(data.panel$X, data.panel$year), ]
data.panel <- data.panel %>% group_by(X) %>% dplyr::mutate(lag.income = lag(income, n = 1, default = NA))
data.panel <- data.panel %>% group_by(X) %>% dplyr::mutate(lag.total.edu = lag(total.edu, n = 1, default = NA))
data.panel <- data.panel %>% group_by(X) %>% dplyr::mutate(lag.total.work = lag(total.work, n = 1, default = NA))

data.panel$income.diff <- data.panel$income - data.panel$lag.income
data.panel$total.edu.diff <- data.panel$total.edu - data.panel$lag.total.edu
data.panel$total.work.diff <- data.panel$total.work - data.panel$lag.total.work

panel.lm.diff <- lm(data.panel$income.diff ~ data.panel$total.edu.diff + data.panel$total.work.diff)
summary(panel.lm.diff)












