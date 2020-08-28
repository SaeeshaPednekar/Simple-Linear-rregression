#x=years of experience  y =salary
salary_data<-read.csv("F:/Excelr/Assignments/dataset/assignmentno4/Salary_data.csv")
attach(salary_data)
View(salary_data)
str(salary_data)
sum(is.na(salary_data))
summary(salary_data)
library(lattice)
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)# 0.9782416

plot(Salary,YearsExperience)
cor(Salary,YearsExperience)

library(moments)
skewness(YearsExperience)
skewness(Salary)

hist(YearsExperience)
hist(Salary)

kurtosis(YearsExperience)
kurtosis(Salary)

dotplot(YearsExperience,main="YearsExperience")
dotplot(Salary,main="salary")

boxplot(YearsExperience,horizontal = T,col ="red")
boxplot(Salary,horizontal = T,col ="blue")
boxplot(salary)


qqnorm(YearsExperience,main="YearsExperience")

qqline(YearsExperience)

qqnorm(Salary,main="salary")
qqline(Salary)


#building model
reg<-lm(Salary~YearsExperience)
reg$coefficients
reg$residuals 
reg$fitted.values

mean(reg$residuals)
summary(reg)#R-squared:  0.957 p-value: < 2.2e-16
#rmse
sqrt(mean(reg$residuals^2))#5592.044

library(ggplot2)

ggplot(data=emp,aes(x=YearsExperience,y=Salary))+geom_point(color='blue')+
  geom_line(color='red',data=salary_data,aes(x=YearsExperience,y=reg$fitted.values))

cor(reg$fitted.values,Salary)
#sqrt
plot(sqrt(YearsExperience),Salary)
cor(sqrt(YearsExperience),Salary)# 0.9648839
reg_sqrt<-lm(Salary~sqrt(YearsExperience))
summary(reg_sqrt)#R-squared:  0.931 p-value: < 2.2e-16
#rmse
sqrt(mean(reg_sqrt$residuals^2))#7080.096
#logarithmic
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)# 0.9240611
reg_log<-lm(Salary~log(YearsExperience))
summary(reg_log)#R-squared:  0.8539 p-value: 3.25e-13

reg_log$fitted.values
#rmse
sqrt(mean(reg_log$residuals^2))#10302.89

#sqrt_y
plot(YearsExperience,sqrt(Salary))
cor(sqrt(Salary),YearsExperience)#0.974595
reg_sqrt_y<-lm(sqrt(Salary)~YearsExperience)
summary(reg_sqrt_y)#R-squared:  0.9498 p-value: < 2.2e-16
reg_sqrt_y$fitted.values
reg_sqrt_y$residuals

pred_sqrt_y<-(reg_sqrt_y$fitted.values)^2
err_pred_sqr_y<-Salary - pred_sqrt_y
mean(err_pred_sqr_y)
#rmse
sqrt(mean(err_pred_sqr_y^2))# 5926.009

#exponential

plot(YearsExperience,log(Salary))
cor(log(Salary),YearsExperience)
reg_exp<-lm(log(Salary)~YearsExperience)
summary(reg_exp)

reg_exp$fitted.values
reg_exp$residuals

pred_log_y<-exp((reg_exp$fitted.values))
err_pre_log_y<-Salary -pred_log_y
#rmse
sqrt(mean(err_pre_log_y^2))#34.26855

#hence this model has 95.7% accuray
