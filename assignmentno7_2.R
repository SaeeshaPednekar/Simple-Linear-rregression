
#Predict delivery time using sorting time
#x=sorting time  y=delivery time
delivery_time<-read.csv("F:/Excelr/Assignments/dataset/assignmentno4/delivery_time.csv")
View(delivery_time)
attach(delivery_time)
str(delivery_time)
summary(delivery_time)
sum(is.na(delivery_time))
?ggplot

#exploratory data analysis
mean(Sorting.Time)
mean(Delivery.Time)

median(Sorting.Time)
median(Delivery.Time)


getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(Sorting.Time)
getmode(Delivery.Time)

sd(Sorting.Time)
sd(Delivery.Time)

var(Sorting.Time)
var(Delivery.Time)

rage(Sorting.Time)
range(Delivery.Time)


library(lattice)
plot(Sorting.Time,Delivery.Time)
cor(Sorting.Time,Delivery.Time)# 0.8259973

plot(Delivery.Time)
library(moments)
skewness(Sorting.Time)
skewness(Delivery.Time)

hist(Sorting.Time)
hist(Delivery.Time)

dotplot(Sorting.Time,main="sorting time")
dotplot(Delivery.Time,main="delivery time")

boxplot(Sorting.Time,horizontal = T,col ="red")
boxplot(Delivery.Time,horizontal = T,col ="blue")
boxplot(delivery_time)



qqnorm(Delivery.Time,main="delivery.time")
qqline(Delivery.Time)

qqnorm(Sorting.Time,main="sorting time")
qqline(Sorting.Time)

#x=sorting time  y=delivery time

#building model
reg<-lm(Delivery.Time~Sorting.Time)
cor(Delivery.Time,Sorting.Time)
reg$coefficients

reg$residuals 
plot(reg$residuals )

reg$fitted.values
plot(reg$fitted.values,Sorting.Time)

mean(reg$residuals)
summary(reg)# R-squared:  0.6823
#rmse
sqrt(mean(reg$residuals^2))# 2.79165
library(ggplot2)
?ggplot

cor(reg$fitted.values,Delivery.Time)

#sqrt
plot(sqrt(Sorting.Time),Delivery.Time)
cor(Delivery.Time,sqrt(Sorting.Time))#0.83415
reg_sqrt<-lm(Delivery.Time~sqrt(Sorting.Time),data=delivery_time)
summary(reg_sqrt)# R-squared:  0.6958 p-value: 2.611e-06

reg_sqrt$fitted.values
#rmse
sqrt(mean(reg_sqrt$residuals^2))#2.731543

pred_sqrt_sorting<-(reg_sqrt$fitted.values)^2


ggplot(data=delivery_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+
 geom_line(color='green',data = delivery_time,aes(x=Sorting.Time,y=sqrt(Delivery.Time)))



#logarithmic
plot(log(Sorting.Time),Delivery.Time)
cor(Delivery.Time,log(Sorting.Time))# 0.8339325
reg_log<-lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)# R-squared:  0.6954, p-value: 2.642e-06

reg_log$fitted.values
sum(reg_log$residuals)
#rmse
sqrt(mean(reg_log$residuals^2))# 2.733171


ggplot(data=delivery_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+
  geom_line(color='red',data=delivery_time,aes(x=Sorting.Time,y=reg$fitted.values))




#sqrt_deliverytime
plot(Sorting.Time,sqrt(Delivery.Time))
cor(sqrt(Delivery.Time),Sorting.Time)
reg_sqrt_y<-lm(sqrt(Delivery.Time)~Sorting.Time)
summary(reg_sqrt_y)#R-squared:  0.704 p-value: 2.001e-06
reg_sqrt_y$fitted.values
reg_sqrt_y$residuals

pred_sqrt_y<-(reg_sqrt_y$fitted.values)^2
err_pred_sqr_y<-Delivery.Time - pred_sqrt_y
mean(err_pred_sqr_y)

sum(err_pred_sqr_y)#2.31953
#rmse
sqrt(mean(err_pred_sqr_y^2))#2.849487


reg_sqrt_y<-lm(sqrt(Delivery.Time)~Sorting.Time)

ggplot(data=delivery_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red',
 data=delivery_time,aes(x=Sorting.Time,y=pred_sqrt_y))
#exponential
plot(Sorting.Time,log(Delivery.Time))
cor(log(Delivery.Time),Sorting.Time)#0.8431773
reg_exp<-lm(log(Delivery.Time)~Sorting.Time)
summary(reg_exp)# R-squared:  0.7109  p-value: 1.593e-06

reg_exp$fitted.values
reg_exp$residuals

pred_log_y<-exp((reg_exp$fitted.values))
err_pre_log_y<-Delivery.Time -pred_log_y
#rmse
sqrt(mean(err_pre_log_y^2))#2.94025


#sum
sum(reg$residuals)
sum(reg_sqrt$residuals)
sum(reg_log$residuals)
sum(err_pred_sqr_y)
sum(err_pre_log_y)

install.packages("iNEXT")
library(iNEXT)
?iNEXT
class(delivery_time)
iNEXT(Delivery.Time, q = 0.1, datatype = "abundance", size = NULL,
      endpoint = 15, knots = 40, se = TRUE, conf = 0.95,
      nboot = 50)
?hillnumber
#Hence sqrt transformation model is the best with r squared value of 0.6958 .