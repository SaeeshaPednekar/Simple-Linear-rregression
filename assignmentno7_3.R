#3) Emp_data -> Build a prediction model for Churn_out_rate

emp<-read.csv("F:/Excelr/Assignments/dataset/assignmentno4/emp_data.csv")
View(emp)
attach(emp)
str(emp)
sum(is.na(emp))
summary(emp)


#exploratory data analysis
library(lattice)
plot(Churn_out_rate,Salary_hike)
cor(Churn_out_rate,Salary_hike)

plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)

mean(Salary_hike)
mean(Churn_out_rate)

median(Salary_hike)
median(Churn_out_rate)

getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Salary_hike)
getmode(Churn_out_rate)


var(Salary_hike)
var(Churn_out_rate)

sd(Salary_hike)
sd(Churn_out_rate)


range(Salary_hike)
range(Churn_out_rate)



library(moments)
skewness(Churn_out_rate)
skewness(Salary_hike)

hist(Churn_out_rate)
hist(Salary_hike)

kurtosis(Churn_out_rate)
kurtosis(Salary_hike)


dotplot(Churn_out_rate,main="churn out rate")
dotplot(Salary_hike,main="salary hike")
boxplot(Churn_out_rate,horizontal = T,col ="red")
boxplot(Salary_hike,horizontal = T,col ="blue")
boxplot(emp)



qqnorm(Churn_out_rate,main="churn out rate")
qqline(Churn_out_rate)

qqnorm(Salary_hike,main="salary hike")
qqline(Salary_hike)

#building model
reg<-lm(Churn_out_rate~Salary_hike)
cor(Churn_out_rate,Salary_hike)# -0.9117216

reg$coefficients
#plot(reg$residuals) 
reg$residuals
reg$fitted.values
plot(reg$fitted.values)
mean(reg$residuals)
summary(reg)#R-squared:  0.8312  p-value: 0.0002386
#rmse
sqrt(mean(reg$residuals^2))#3.997528
 
library(ggplot2)

ggplot(data=emp,aes(x=Churn_out_rate,y=Salary_hike))+geom_point(color='blue')+
  geom_line(color='red',data=emp,aes(x=Churn_out_rate,y=reg$fitted.values))

cor(reg$fitted.values,Salary_hike)


#sqrt

plot(sqrt(Churn_out_rate),Salary_hike)
cor(Salary_hike,sqrt(Churn_out_rate))# -0.9235755

reg_sqrt<-lm(Salary_hike~sqrt(Churn_out_rate),data=emp)



summary(reg_sqrt)    #R-squared:  0.853  p-value: 0.000136
#rmse
sqrt(mean(reg_sqrt$residuals^2))# 33.4993
#logarithmic
plot(log(Churn_out_rate),Salary_hike)
cor(Salary_hike,log(Churn_out_rate))#-0.9346361



reg_log<-lm(Salary_hike~log(Churn_out_rate))


summary(reg_log)#R-squared:  0.8735  p-value: 7.377e-05
reg_log$fitted.values
#rmse
sqrt(mean(reg_log$residuals^2))#31.06952
#sqrt_y
plot(Churn_out_rate,sqrt(Salary_hike))
cor(sqrt(Salary_hike),Churn_out_rate)#-0.9165311

reg_sqrt_y<-lm(sqrt(Salary_hike)~Churn_out_rate)

summary(reg_sqrt_y)#R-squared:   0.84   p-value: 0.0001918

reg_sqrt_y$fitted.values
reg_sqrt_y$residuals

pred_sqrt_y<-(reg_sqrt_y$fitted.values)^2
err_pred_sqr_y<-Salary_hike - pred_sqrt_y
mean(err_pred_sqr_y)
#rmse
sqrt(mean(err_pred_sqr_y^2))#35.05767
#exponential
plot(Churn_out_rate,log(Salary_hike))
cor(log(Salary_hike),Churn_out_rate)
reg_exp<-lm(log(Salary_hike)~Churn_out_rate)
summary(reg_exp)

reg_exp$fitted.values
reg_exp$residuals

pred_log_y<-exp((reg_exp$fitted.values))
err_pre_log_y<-Salary_hike -pred_log_y
#rmse
sqrt(mean(err_pre_log_y^2))#34.26855

#Hence this model has accuracy of 83.12%






