library(dplyr)
library(mlogit)
library(nnet)
setwd("/Users/yicanliu/Desktop/Study/2022 Spring/Econ 613/Homework/HW3")

data.datjss <- read.csv("datjss.csv", header = TRUE)
data.datsss <- read.csv("datsss.csv", header = TRUE)
data.datstu <- read.csv("datstu_v2.csv", header = TRUE)

data.datjss <- subset(data.datjss, select=-c(X))
names(data.datjss) <- c("jssdistrict", "jsslong", "jsslat")

# Clean data
sssdistrict.length <- c()
for(this.sssdistrict in data.datsss$sssdistrict) {
  sssdistrict.length <- append(sssdistrict.length, nchar(this.sssdistrict))
}
data.datsss["sssdistrict.length"] <- sssdistrict.length
data.schooldist.maxlen <- aggregate(data.datsss$sssdistrict.length, by=list(data.datsss$schoolcode), FUN=max)
names(data.schooldist.maxlen) <- c("schoolcode", "max.distname.len")
data.datsss <- left_join(data.datsss, data.schooldist.maxlen, by="schoolcode")
data.datsss <- data.datsss[data.datsss$sssdistrict.length == data.datsss$max.distname.len, ]
data.datsss <- unique(subset(data.datsss, select=-c(max.distname.len, sssdistrict.length, V1)) )
data.datsss <- na.omit(data.datsss)

########## Exercise 1 ##########
# Question 1.1
# Number of students
length(data.datstu$male)
# Number of schools
length(unique(data.datsss$schoolname) )
# Number of programs
length(unique(data.datstu$choicepgm1) )

# Question 1.2
# Number of choices
schoolcode <- c(data.datstu$schoolcode1, data.datstu$schoolcode2, data.datstu$schoolcode3, data.datstu$schoolcode4, data.datstu$schoolcode5, data.datstu$schoolcode6)
choicepgm <- c(data.datstu$choicepgm1, data.datstu$choicepgm2, data.datstu$choicepgm3, data.datstu$choicepgm4, data.datstu$choicepgm5, data.datstu$choicepgm6)
data.choices.schpgm <- unique(data.frame(schoolcode, choicepgm) )
length(data.choices.schpgm$schoolcode)

# Question 1.3 
# Initially we have 340823 datstu obs
#data.datstu.withsssdist <- data.datstu[c("V1", "schoolcode1", "schoolcode2", "schoolcode3", "schoolcode4", "schoolcode5", "schoolcode6")]
for (i in 1:6) {
  print(i)
  name.schoolcode <- paste("schoolcode", as.character(i), sep="")
  name.sssdistrict <- paste("sssdistrict", as.character(i), sep="")
  data.tmp.schoolinfo <- unique(data.datsss[c("schoolcode", "sssdistrict")] )
  names(data.tmp.schoolinfo) <- c(name.schoolcode, name.sssdistrict)
  data.datstu <- left_join(data.datstu, 
                          data.tmp.schoolinfo,
                          by= name.schoolcode ) 
}
data.datstu["same.high.junior.loc"] <- (data.datstu$jssdistrict==data.datstu$sssdistrict1) | 
  (data.datstu$jssdistrict==data.datstu$sssdistrict2) | (data.datstu$jssdistrict==data.datstu$sssdistrict3) |
  (data.datstu$jssdistrict==data.datstu$sssdistrict4) | (data.datstu$jssdistrict==data.datstu$sssdistrict5) 
length(data.datstu[data.datstu$same.high.junior.loc,]$V1)

# Question 1.4
data.school.admit <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(data.school.admit) <- c("V1", "schoolcode")
for(i in 1:6) {
  name.schoolcode <- paste("schoolcode", as.character(i), sep="")
  name.project <- paste("choicepgm", as.character(i), sep="")
  data.firstchoice <- data.datstu[(data.datstu$rankplace == i) & (!is.na(data.datstu$rankplace)), ]
  print(length(data.firstchoice$V1) )
  data.school.admit.thisrank <- data.firstchoice[c("V1", name.schoolcode, name.project)]
  print(length(data.school.admit.thisrank$V1))
  names(data.school.admit.thisrank) <- c("V1", "schoolcode", "choicepgm")
  data.school.admit <- rbind(data.school.admit, data.school.admit.thisrank)
}
names(data.school.admit) <- c("V1", "schoolcode.admit", "choicepgm.admit")
data.school.admit <- left_join(data.datstu, data.school.admit, by="V1")
data.numstu.school <- aggregate(data.school.admit$schoolcode.admit, by=list(data.school.admit$schoolcode.admit), FUN=length)
names(data.numstu.school) <- c("schoolcode", "num.students")
data.numstu.school

# Question 1.5
data.datstu.admitted <- data.school.admit[ (data.school.admit$rankplace!=99) & (!is.na(data.school.admit$rankplace)), ]
data.cutoff.schools <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$schoolcode.admit), FUN=min)
names(data.cutoff.schools) <- c("schoolcode", "cutoff")
data.cutoff.schools

# Question 1.6
data.quality.schools <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$schoolcode.admit), FUN=mean)
names(data.quality.schools) <- c("schoolcode", "quality")
data.quality.schools


########## Exercise 2 ##########
data.size.schpgm <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$schoolcode.admit, data.datstu.admitted$choicepgm.admit), FUN=length)
names(data.size.schpgm) <- c("schoolcode", "choicepgm", "size")
data.quality.schpgm <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$schoolcode.admit, data.datstu.admitted$choicepgm.admit), FUN=mean)
names(data.quality.schpgm) <- c("schoolcode", "choicepgm", "quality")
data.cutoff.schpgm <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$schoolcode.admit, data.datstu.admitted$choicepgm.admit), FUN=min)
names(data.cutoff.schpgm) <- c("schoolcode", "choicepgm", "cutoff")

data.schpgm <- left_join(data.size.schpgm, data.quality.schpgm, by=c("schoolcode", "choicepgm") )
data.schpgm <- left_join(data.schpgm, data.cutoff.schpgm, by=c("schoolcode", "choicepgm") )

#data.datsss.loc <- left_join(data.datsss, data.datjss, by=c("sssdistrict"="jssdistrict"))
data.schpgm <- left_join(data.schpgm, data.datsss[c("schoolcode", "sssdistrict", "ssslong", "ssslat")], by=c("schoolcode"))
data.schpgm <- unique(data.schpgm)
data.schpgm <- data.schpgm[order(data.schpgm$schoolcode, data.schpgm$choicepgm), ]
data.schpgm

########## Exercise 3 ##########
data.choice.dist <- data.datstu["V1"]
for (i in 1:6) {
  name.schoolcode <- paste("schoolcode", as.character(i), sep="")
  name.project <- paste("choicepgm", as.character(i), sep="")
  data.this.choice <- data.datstu[c("V1", name.schoolcode, name.project, "jssdistrict")]d
  names(data.this.choice) <- c("V1", "schoolcode", "choicepgm", "jssdistrict")
  data.this.choice <- left_join(data.this.choice, data.schpgm[c("schoolcode", "choicepgm", "ssslong", "ssslat")], by=c("schoolcode", "choicepgm"))
  data.this.choice <- left_join(data.this.choice, data.datjss, by=c("jssdistrict"))
  data.this.choice$distance <- sqrt(
    (69.172*(data.this.choice$ssslong - data.this.choice$jsslong) *cos(data.this.choice$jsslat / 57.3) )^2 +
      (69.172*(data.this.choice$ssslat - data.this.choice$jsslat)^2)
    )
  data.this.choice.dist <- data.this.choice[c("V1", "distance")]
  names(data.this.choice.dist) <- c("V1", paste("distance", as.character(i), sep="") )
  data.choice.dist <- left_join(data.choice.dist, data.this.choice.dist, by=c("V1"))
}
data.datstu <- left_join(data.datstu, data.choice.dist, by=c("V1"))
data.datstu 

########## Exercise 4 ##########
data.datstu$scode_rev1 = strtoi(substr(as.character(data.datstu$schoolcode1), 1, 3) )
data.datstu$scode_rev2 = strtoi(substr(as.character(data.datstu$schoolcode2), 1, 3) )
data.datstu$scode_rev3 = strtoi(substr(as.character(data.datstu$schoolcode3), 1, 3) )
data.datstu$scode_rev4 = strtoi(substr(as.character(data.datstu$schoolcode4), 1, 3) )
data.datstu$scode_rev5 = strtoi(substr(as.character(data.datstu$schoolcode5), 1, 3) )
data.datstu$scode_rev6 = strtoi(substr(as.character(data.datstu$schoolcode6), 1, 3) )

recode_program <- function(program) {
  if(is.na(program)) {
    return(NA)
  }
  if( (program == "General Arts") | (program == "Visual Arts")) {
    return("Arts")
  }
  else if( (program == "Business") | (program == "Home Economics") ) {
    return("Economics")
  }
  else if( program == "General Science") {
    return("Science")
  }
  else {
    return("Others")
  }
}

recode_program_list <- function(program_list) {
  pgm_rev <- c()
  for(program in program_list) {
    pgm_rev <- c(pgm_rev, recode_program(program))
  }
  return(pgm_rev)
}

data.datstu$pgm_rev1 <- recode_program_list(data.datstu$choicepgm1)
data.datstu$pgm_rev2 <- recode_program_list(data.datstu$choicepgm2)
data.datstu$pgm_rev3 <- recode_program_list(data.datstu$choicepgm3)
data.datstu$pgm_rev4 <- recode_program_list(data.datstu$choicepgm4)
data.datstu$pgm_rev5 <- recode_program_list(data.datstu$choicepgm5)
data.datstu$pgm_rev6 <- recode_program_list(data.datstu$choicepgm6)

data.datstu$choice_rev1 <- paste( data.datstu$scode_rev1, data.datstu$pgm_rev1)
data.datstu$choice_rev2 <- paste( data.datstu$scode_rev2, data.datstu$pgm_rev2)
data.datstu$choice_rev3 <- paste( data.datstu$scode_rev3, data.datstu$pgm_rev3)
data.datstu$choice_rev4 <- paste( data.datstu$scode_rev4, data.datstu$pgm_rev4)
data.datstu$choice_rev5 <- paste( data.datstu$scode_rev5, data.datstu$pgm_rev5)
data.datstu$choice_rev6 <- paste( data.datstu$scode_rev6, data.datstu$pgm_rev6)

data.datstu <- left_join(data.datstu, data.school.admit[c("V1", "schoolcode.admit", "choicepgm.admit")], by="V1")
data.datstu$scode_rev.admit <- strtoi(substr(as.character(data.datstu$schoolcode.admit), 1, 3) )
data.datstu$pgm_rev.admit <-  recode_program_list(data.datstu$choicepgm.admit)
data.datstu$choice_rev.admit <- paste( data.datstu$scode_rev.admit, data.datstu$pgm_rev.admit)

# Estimate the cutoff of different school-programs
data.datstu.admitted <- data.datstu[(data.datstu$rankplace!=99) & (!is.na(data.datstu$rankplace)), ]
data.cutoff.schpgm.rev <- aggregate(data.datstu.admitted$score, by=list(data.datstu.admitted$choice_rev.admit), FUN=min)
data.datstu.20000high <- data.datstu[order(-data.datstu$score), ][1:20000, ]


########## Exercise 5 ##########

# First, we estimate the maximun likelihood of each choice
maximum.likelihood <- function(y, x, beta.list) {
  #print(beta.list)
  beta.matrix <- t(matrix(beta.list, ncol = 2))
  #print(beta.matrix)
  y.matrix <- as.matrix(data.frame(y))
  x.matrix <- as.matrix(data.frame(rep(1, length(x)), x))
  
  pi.pred <- x.matrix %*% beta.matrix
  exp.pi.pred <- exp(pi.pred)
  exp.pi.pred.rowsum <- rowSums(exp.pi.pred) + 1 # I add one because I did not input the benchmark
  
  exp.pi.pred.selected <- rowSums(exp.pi.pred * y.matrix ) # The estimated exp(beta_n_0 + beta_n_1*x)
  exp.pi.pred.selected[rowSums(y.matrix) == 0] <- 1  # For those who choose benchmark (100 Arts)
  #print(rowSums(y.matrix))
  exp.pi.pred.prob <- exp.pi.pred.selected / exp.pi.pred.rowsum
  #print(length(exp.pi.pred.prob[exp.pi.pred.prob!=0]))
  #print(exp.pi.pred.prob)
  
  # For extreme values
  exp.pi.pred.prob[exp.pi.pred.prob>0.999999] = 0.999999
  exp.pi.pred.prob[exp.pi.pred.prob<0.000001] = 0.000001
  
  neg.likelihood <- -sum(log(exp.pi.pred.prob))
  print("neg likelihood: ")
  print(neg.likelihood)
  return(neg.likelihood)
}

marginal.effect <- function(beta, x) {
  
  names.beta <- rownames(beta)
  
  x.matrix <- as.matrix(data.frame(rep(1, length(x)), x))
  beta.matrix <- t(as.matrix(beta))
  pi.pred <- x.matrix %*% beta.matrix
  exp.pi.pred <- exp(pi.pred)
  exp.pi.pred.rowsum <- rowSums(exp.pi.pred) + 1
  
  pij <- data.frame(matrix(ncol = 0, nrow = length(x.matrix[, 1])))
  for(i in 1:length(beta[, 1])) {
    pij[names.beta[i]] <- exp.pi.pred[, i] / exp.pi.pred.rowsum
  }
  
  #print(pij)
  #print(exp.pi.pred)
  beta.coefficient.matrix <- as.matrix(beta[, 2])
  #print(beta.coefficient.matrix)
  beta.bar <- exp.pi.pred %*% beta.coefficient.matrix
  #print(beta.bar)
  #print(length(beta.bar))
  
  # Here we creat a matrix for (betaj - beta_bar_i)
  # For every observations,
  betai.minus.betabar <- data.frame(matrix(ncol = 0, nrow = length(x.matrix[, 1])))
  for(i in 1:length(beta[, 1])) {
    #print(i)
    betai.minus.betabar[names.beta[i]] <- - beta.bar + beta[i, 2]
  }
  return( as.matrix(pij) * as.matrix(betai.minus.betabar))
  
}

# I use a vector to record the value of beta
# I drop the students whose school code is NA
data.datstu.20000high <- data.datstu.20000high[! is.na(data.datstu.20000high$scode_rev1), ]

c.all.choice <- unique(data.datstu.20000high$choice_rev1)
c.all.choice <- c.all.choice[order(c.all.choice)]

# Here we construct the matrix for y:
# I choose the first variale as the benchmark.
y = data.frame(matrix(ncol = 0, nrow = length(data.datstu.20000high$score)))
for(i in 2:length(c.all.choice)) {
  c.this.line <- c()
  for(this.choice in data.datstu.20000high$choice_rev1){
    if(this.choice == c.all.choice[i]) {
      c.this.line <- c(c.this.line, 1)
    } else {
      c.this.line <- c(c.this.line, 0)
    }
  }
  y[c.all.choice[i]] <- c.this.line
}

x.score = data.frame(data.datstu.20000high$score)

multi.model1 <- multinom(formula = choice_rev1 ~ score, data = data.datstu.20000high)
multi.model1.coef <- coef(multi.model1)

#model1=optim(function(beta) maximum.likelihood(y=y, x=x.score, beta.list=beta, par= c(rep(0, (length(c.all.choice)-1)*2 ) ), method="CG" ) )
#model1 = optimize(maximum.likelihood, c(-1, 1), tol=0.0000001, )
#as.matrix(model1$par) 

#model1 <- optim(function(beta.list) maximum.likelihood(y=y, x=x.score, beta.list=beta.list), par=c(multi.model1.sum[, 1], multi.model1.sum[, 2] ), method="CG", control = list(maxit = 100))
multi.model1.coef <- matrix(model1$par, ncol = 2)

marginal.effect(multi.model1.coef, x.score)

########## Exercise 6 ##########

# I use the cutoff of score as the measurement for school quality

data.datstu.20000high["quality.choice_rev1"] <- left_join(data.datstu.20000high, data.schpgm[c("schoolcode", "quality", "choicepgm")], by=c("schoolcode1" = "schoolcode", "choicepgm1" = "choicepgm") )$quality

x.quality = data.frame(data.datstu.20000high$quality.choice_rev1)

multi.model2 <- multinom(formula = choice_rev1 ~ quality.choice_rev1, data = data.datstu.20000high)
multi.model2.coef <- coef(multi.model2)

#model2 <- optim(function(beta.list) maximum.likelihood(y=y, x=x.quality, beta.list=beta.list), par=c( multi.model2.sum[, 1], multi.model2.sum[, 2]), method="CG", control = list(maxit = 100))
matrix(model2$par, ncol = 2)

marginal.effect(multi.model2.coef, x.quality)




########## Exercise 7 ##########
choice.prob <- function(beta, x) {
  
  names.beta <- rownames(beta)
  
  x.matrix <- as.matrix(data.frame(rep(1, length(x)), x))
  beta.matrix <- t(as.matrix(beta))
  pi.pred <- x.matrix %*% beta.matrix
  exp.pi.pred <- exp(pi.pred)
  exp.pi.pred.rowsum <- rowSums(exp.pi.pred) + 1
  
  pij <- data.frame(matrix(ncol = 0, nrow = length(x.matrix[, 1])))
  for(i in 1:length(beta[, 1])) {
    pij[names.beta[i]] <- exp.pi.pred[, i] / exp.pi.pred.rowsum
  }
  
  return(as.matrix(pij))
  
}

data.datstu.20000high.noother <- data.datstu.20000high[!data.datstu.20000high$pgm_rev1=="Others", ]


y.noother <- data.frame(matrix(ncol = 0, nrow = length(data.datstu.20000high.noother$score)))

c.all.choice.noother <- unique(data.datstu.20000high.noother$choice_rev1)
c.all.choice.noother <- c.all.choice.noother[order(c.all.choice.noother)]

for(i in 2:length(c.all.choice.noother)) {
  c.this.line <- c()
  for(this.choice in data.datstu.20000high.noother$choice_rev1){
    if(this.choice == c.all.choice.noother[i]) {
      c.this.line <- c(c.this.line, 1)
    } else {
      c.this.line <- c(c.this.line, 0)
    }
  }
  y.noother[c.all.choice.noother[i]] <- c.this.line
}

multi.model3 <- multinom(formula = choice_rev1 ~ quality.choice_rev1, data = data.datstu.20000high.noother)
multi.model3.coef <- coef(multi.model3)

x.quality.noother <- data.datstu.20000high.noother$quality.choice_rev1


#model3 <- optim(function(beta.list) maximum.likelihood(y=y.noother, x=x.quality.noother, beta.list=beta.list), par=c(multi.model3.coef[, 1], multi.model3.coef[, 2] ), method="CG", control = list(maxit = 100))
#multi.model3.coef <- matrix(model3$par, ncol = 2)
choice.prob(multi.model3.coef, data.datstu.20000high.noother$score)

























