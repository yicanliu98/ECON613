########## Exercise 1 ##########
library(ineq)
library(dplyr)
library(ggplot2)
setwd("/Users/yicanliu/Desktop/Study/2022 Spring/Homework/HW1")

# Question 1.1
data.dathh2007 <- read.csv("Data/dathh2007.csv", header = TRUE)
# "Household of Survey in 2007: "
print(length(data.dathh2007$idmen))
rm(data.dathh2007)

# Question 1.2
data.dathh2005 <- read.csv("Data/dathh2005.csv", header = TRUE)
data.datah2005.coupwithkids <- data.dathh2005[ data.dathh2005$mstatus == "Couple, with Kids", ]
# "Household of Couple with kids in 2005
print(length(data.datah2005.coupwithkids$idmen))
rm(data.dathh2005)
rm(data.datah2005.coupwithkids)

# Question 1.3
data.datind2008 <- read.csv("Data/datind2008.csv", header = TRUE)
# "Household of individuals surveyed in 2008"
print(length(data.datind2008$idind))
rm(data.datind2008)

# Question 1.4
data.datind2016 <- read.csv("Data/datind2016.csv", header = TRUE)
data.datind2016.25to35 <- data.datind2016[data.datind2016$age >= 25 & data.datind2016$age <= 35, ]
# "Individual between 25 to 35 in 2016
print(length(data.datind2016.25to35$idind))
rm(data.datind2016)
rm(data.datind2016.25to35)

# Question 1.5
data.datind2009 <- read.csv("Data/datind2009.csv", header = TRUE)
table(data.datind2009$gender, data.datind2009$profession)
rm(data.datind2009)

# Question 1.6
data.datind2005 <- read.csv("Data/datind2005.csv", header = TRUE)
data.datind2019 <- read.csv("Data/datind2019.csv", header = TRUE)
summary(data.datind2005$wage)
sd(data.datind2005$wage, na.rm=TRUE)
quantile(data.datind2005$wage, probs = seq(0, 1, 0.1), na.rm = TRUE)
Gini(data.datind2005$wage)
summary(data.datind2019$wage)
sd(data.datind2019$wage, na.rm=TRUE)
quantile(data.datind2019$wage, probs = seq(0, 1, 0.1), na.rm = TRUE)
Gini(data.datind2019$wage)
rm(data.datind2005)
rm(data.datind2019)

# Question 1.7
data.datind2010 <- read.csv("Data/datind2010.csv", header = TRUE)
pdf("Output/figure1_histage_2010.pdf") 
hist(data.datind2010$age,  xlab = "Age, 2010", main = "Distribution of Age")
dev.off() 
pdf("Output/figure2_histagewomen_2010.pdf") 
hist(data.datind2010$age[data.datind2010$gender == "Female"],  xlab = "Women Age, 2010", main = "Distribution of Women Age")
dev.off() 
pdf("Output/figure2_histagemen_2010.pdf") 
hist(data.datind2010$age[data.datind2010$gender == "Male"],  xlab = "Men Age, 2010", main = "Distribution of Men Age")
dev.off() 
rm(data.datind2010)

# Question 1.8
data.datind2011 <- read.csv("Data/datind2011.csv", header = TRUE)
data.dathh2011 <- read.csv("Data/dathh2011.csv", header = TRUE)
summary(data.datind2011)
summary(data.dathh2011)
data.merged.2011  <- left_join(data.datind2011, data.dathh2011, by = "idmen")
length(data.merged.2011[data.merged.2011$location == "Paris", ]$idind)
rm(data.dathh2011)
rm(data.datind2011)
rm(data.merged.2011)



########## Exercise 2 ##########
# Question 2.1
data.all.datind <- read.csv("Data/datind2004.csv", header = TRUE)
for (year in 2005:2019) {
  data.this.datind <- read.csv( paste0("Data/datind", as.character(year), ".csv"), header = TRUE)
  data.all.datind <- rbind(data.all.datind, data.this.datind)
  rm(data.this.datind)
}

# Question 2.2
data.all.dathh <- read.csv("Data/dathh2004.csv", header = TRUE)
for (year in 2005:2019) {
  data.this.dathh <- read.csv( paste0("Data/dathh", as.character(year), ".csv"), header = TRUE)
  data.all.dathh <- rbind(data.all.dathh, data.this.dathh)
  rm(data.this.dathh)
}

# Question 2.3
data.all.datind.names <- colnames(data.all.datind)
data.all.dathh.names <- colnames(data.all.dathh)
intersect(data.all.datind.names, data.all.dathh.names)
setdiff(data.all.datind.names, data.all.dathh.names)
setdiff(data.all.dathh.names, data.all.datind.names)

# Question 2.4
data.all  <- unique(merge(subset(data.all.datind, select=-c(X)), subset(data.all.dathh, select=-c(X)), by.y = c("idmen", "year"), by.x = c("idmen", "year") ) )
length(data.all$idind)

# Question 2.5
data.family_members <- aggregate(data.all$idind, by=list(data.all$idmen, data.all$year), FUN=length)
data.family_members.over4 <- data.family_members[data.family_members$x > 4, ]
length(data.family_members.over4$x)
rm(data.family_members)
rm(data.family_members.over4)

# Question 2.6

data.all.unemployed <- data.all[data.all$empstat == "Unemployed", ]
data.all.unemployed <- unique(subset(data.all.unemployed, select=c("year", "idmen")) )
length(data.all.unemployed$idmen)
rm(data.all.unemployed)

# Question 2.7
data.all.employed <- data.all[data.all$profession != "", ]
#   For each household, I count the number of members within each industry
data.profession_num <- aggregate(data.all.employed$idind, by=list(data.all.employed$idmen, data.all.employed$year, data.all.employed$profession), FUN=length)
names(data.profession_num)[1] <- "idmen"
names(data.profession_num)[2] <- "year"
names(data.profession_num)[3] <- "profession"
names(data.profession_num)[4] <- "profession_num"
data.profession_num.over2 <- data.profession_num[data.profession_num$profession_num >= 2, ]
data.profession_num.over2.idmen <- subset(data.profession_num.over2, select=c(year, idmen) )
data.profession_num.over2.idmen <- unique(data.profession_num.over2.idmen)
count(data.profession_num.over2.idmen)
rm(data.all.employed)
rm(data.profession_num)
rm(data.profession_num.over2)
rm(data.profession_num.over2.idmen)

# Question 2.8
data.all.hcwithkids <- data.all[data.all$mstatus == "Couple, with Kids", ]
count(data.all.hcwithkids)
rm(data.all.hcwithkids)

# Question 2.9
data.all.paris <- data.all[data.all$location == "Paris", ]
count(data.all.paris)
rm(data.all.paris)

# Question 2.10
data.all.familymember <- count(data.all, year, idmen)
names(data.all.familymember)[3] = "family_member_num"
data.all.familymember <- left_join(data.all, data.all.familymember, by=c("year", "idmen"))
data.all.familymember.max <- data.all.familymember[data.all.familymember$family_member_num == max(data.all.familymember$family_member_num), ]
unique(as.character(data.all.familymember.max$idmen) )
rm(data.all.familymember)
rm(data.all.familymember.max)

# Question 2.11
data.all.2010and2011 <- data.all[data.all$year == 2011 | data.all$year == 2010, ]
data.all.2010and2011.idmen <- subset(data.all.2010and2011, select=c(year, idmen))
data.all.2010and2011.idmen <- unique(data.all.2010and2011.idmen)
count(data.all.2010and2011.idmen)


########## Exercise 3 ##########
# Question 3.1
#idmen <- c()
#startyear <- c()
#endyear <- c()
#idmen.set = unique(data.all$idmen)
#for (this.idmen in idmen.set) {
#  data.this.idmen <- data.all[data.all$idmen == this.idmen, ]
#  this.start.year <- min(data.this.idmen$year)
#  this.end.year <- max(data.this.idmen$year)
#  idmen <- append(idmen, this.idmen)
#  startyear <- append(startyear, this.start.year)
#  endyear <- append(endyear, this.end.year)
#}
#data.survey.year <- data.frame(idmen, startyear, endyear)
data.idmen.startyear <- aggregate(data.all$year, by=list(data.all$idmen), FUN=min)
names(data.idmen.startyear)[1] = "idmen"
names(data.idmen.startyear)[2] = "startyear"
data.idmen.endyear <- aggregate(data.all$year, by=list(data.all$idmen), FUN=max)
names(data.idmen.endyear)[1] = "idmen"
names(data.idmen.endyear)[2] = "endyear"
data.survey.time <- left_join(data.idmen.startyear, data.idmen.endyear, by=c("idmen") )
data.survey.time$surveyyear <- data.survey.time$endyear - data.survey.time$startyear + 1
summary(data.survey.time$surveyyear)
data.timefreq <- as.data.frame(table(data.survey.time$surveyyear))
names(data.timefreq)[1] <- "Year"
pdf("Output/figure4_histsurveytime.pdf") 
#hist(data.survey.time$surveyyear,  xlab = "Survey Period", main = "Distribution of Survey Time")
ggplot(data.timefreq, aes(x = Year, y = Freq)) +geom_bar(stat = "identity")
dev.off() 
rm(data.idmen.startyear)
rm(data.idmen.endyear)

# Question 3.2
data.all.withsurveytime <- left_join(data.all, data.survey.time, by=c("idmen"))
data.all.withsurveytime <- data.all.withsurveytime[!is.na(data.all.withsurveytime$datent), ]
data.all.withsurveytime$startwithsurvey <- data.all.withsurveytime$datent == data.all.withsurveytime$year
data.all.startwithsurvey <- data.all.withsurveytime[data.all.withsurveytime$startwithsurvey, ]
head(data.all.startwithsurvey, 10)
data.year.allind.3.2 <- count(data.all, year)
names(data.year.allind.3.2)[2] = "totalind"
data.year.startwithsurvey <- count(data.all.startwithsurvey, year)
data.year.allind.3.2$proportion <- data.year.startwithsurvey$n / data.year.allind.3.2$totalind
pdf("Output/figure5_trend_shareofind.pdf")
ggplot(data.year.allind.3.2, aes(x=year, y=proportion)) +
  geom_line() + 
  xlab("")
dev.off()

# Question 3.3
data.all.withmove <- data.all
data.all.withmove$migrated <- ( (data.all.withmove$myear == data.all.withmove$year) ) | (data.all.withmove$move == 2 )
data.all.withmove$migrated[is.na(data.all.withmove$migrated) ] = FALSE
head(data.all.withmove, 10)
data.all.withmove <- subset(data.all.withmove, select=c(migrated, idmen, year, idind))
data.all.migratedonyear <- data.all.withmove[data.all.withmove$migrated, ]
data.year.allind.3.3 <- count(data.all, year)
data.year.withmigration <- count(data.all.migratedonyear, year)
data.year.allind.3.3$proportion_2 <- data.year.withmigration$n/ data.year.allind.3.3$n

pdf("Output/figure6_trend_shareofind_2.pdf")
ggplot(data.year.allind.3.3, aes(x=year, y=proportion_2)) +
  geom_line() + 
  xlab("")
dev.off()

# Question 3.4
data.year.allind <- left_join(data.year.allind.3.2, data.year.allind.3.3, by=c("year"))
pdf("Output/figure7_trend_shareofind_combine.pdf")
ggplot(data.year.allind, aes(x=year)) + 
  geom_line(aes(y = proportion), color = "darkred") + 
  geom_line(aes(y = proportion_2), color="steelblue", linetype="twodash") 
dev.off()

# Question 3.5
data.withlagemp <- data.all
data.withlagemp_2 <- data.all
data.withlagemp_2$year <- data.withlagemp_2$year + 1
data.withlagemp_2 <- subset(data.withlagemp_2, select=c(year, idind, empstat))
names(data.withlagemp_2)[3] <- "lag.empstat"
data.withlagemp <- left_join(data.withlagemp, data.withlagemp_2, by=c("year", "idind"))
data.withlagemp <- data.withlagemp[ !is.na(data.withlagemp$empstat ) & !is.na(data.withlagemp$lag.empstat) , ]
data.changeemp <- data.withlagemp[data.withlagemp$empstat!=data.withlagemp$lag.empstat, ]
data.migrated.changeemp <- inner_join(
  subset(data.changeemp, select=c(year, idind, idmen, empstat, lag.empstat)), 
  subset(data.all.startwithsurvey, select=c(year, idind)), by=c("year", "idind"))
count(data.migrated.changeemp, year)

data.migrated.changeemp.household <- unique(subset(data.migrated.changeemp, select=c(year, idmen)) )
count(data.migrated.changeemp.household)

########## Exercise 4 ##########
# I use survey time to construct this variable
count(data.survey.time, endyear)





