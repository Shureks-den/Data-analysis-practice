## “Is there an association between college major category and income?”

install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)
dance_start(value = FALSE, contents = FALSE)
dim(college)
names(college)
head(college)
fit1<-lm(p75th ~ major_category, data = college) ## 75 percentile
fit2<-lm(median ~ major_category, data = college ) ## median income
summary(fit2)
fit3<-lm(median ~ major_category + perc_employed_fulltime, data =college[-17,])##NA in 17th row ## fulltime %
sum(is.na(college$perc_employed_fulltime))
summary(fit3)
fit2<-lm(median ~ major_category, data = college[-17,] )
anova(fit2,fit3) ## no strong corrlation
fit2<-lm(median ~ major_category, data = college )
fit4<-lm(median ~ major_category + perc_women, data =college)
anova(fit2,fit4) ## no strong correlation
fit4<-lm(median ~  perc_women, data =college)
summary(fit4)

library(ggplot2)
## ploting lm of all categories based on women %
g<-ggplot(data = college, aes(x=perc_women, y = median, col = major_category))
g<-g+geom_point()
g<-g+geom_smooth(aes(perc_women, median), method = lm, se = FALSE)
g
busi<-subset(college, college$major_category == "Business")
## mean by categories
sapply(split(college$median,college$major_category), mean)
##ploting lm of all categories based on fulltime %
g1<-ggplot(data = college[-17,], aes(x=perc_employed_fulltime, y = median, col = major_category))
g1<-g1+geom_point()
g1<-g1+geom_smooth(aes(perc_women, median), method = lm, se = FALSE)
g1
## no strong connection 
dance_save("~college_major_analysis.rds")
g<-ggplot(aes(major_category, p25th, col = major_category), data = college)
g<-g+geom_boxplot()
g
g<-ggplot(aes(major_category, p75th, col = major_category), data = college)
g<-g+geom_boxplot()
g
