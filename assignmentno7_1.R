#predict weight gain using calorie consumed
#x=calorie_consumed  y=weight.gain
calories_consumed<-read.csv("F:/Excelr/Assignments/dataset/assignmentno4/calories_consumed.csv")
View(calories_consumed)
attach(calories_consumed)
str(calories_consumed)
sum(is.na(calories_consumed))
summary(calories_consumed)
#exploratory data analysis

mean(calories_consumed$Weight.gained..grams.)
mean(calories_consumed$Calories.Consumed)

median(calories_consumed$Weight.gained..grams.)
median(calories_consumed$Calories.Consumed)

getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(calories_consumed$Weight.gained..grams.)
getmode(calories_consumed$Calories.Consumed)



var(calories_consumed$Weight.gained..grams.)
var(calories_consumed$Calories.Consumed)


sd(calories_consumed$Weight.gained..grams.)
sd(calories_consumed$Weight.gained..grams.)




range(calories_consumed$Weight.gained..grams.)
range(calories_consumed$Calories.Consumed)


library(lattice)

plot(Calories.Consumed,Weight.gained..grams.)
cor(Calories.Consumed,Weight.gained..grams.)

plot(Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams.,Calories.Consumed)



library(moments)
skewness(calories_consumed)
skewness(Weight.gained..grams.)
skewness(Calories.Consumed)
hist(Weight.gained..grams.)
hist(Calories.Consumed)
kurtosis(Weight.gained..grams.)
kurtosis(Calories.Consumed)
dotplot(Weight.gained..grams.,main="Dot plot of weight gained grams")
dotplot(Calories.Consumed,main="Dot plot of calories consumed")
boxplot(Calories.Consumed,horizontal = T,col ="red")
boxplot(Weight.gained..grams.,horizontal = T,col ="blue")

boxplot(calories_consumed)

qqnorm(Calories.Consumed,main="calories.consumed")
qqline(Calories.Consumed)

qqnorm(Weight.gained..grams.,main="weight gained grams")
qqline(Weight.gained..grams.)

#BUILDING MODEL
#x=calorie_consumed  y=weight.gain
# first transformation
reg<-lm(Weight.gained..grams.~Calories.Consumed)
reg$coefficients
  reg$residuals
summary(reg)
mean(reg$residuals)
#sum
sum(reg$residuals)   #6.750156e-14
#rmse
sqrt(mean(reg$residuals^2))#103.3025
library(ggplot2)

ggplot(data=calories_consumed,aes(x=Calories.Consumed,y=Weight.gained..grams.))+geom_point(color='blue')+geom_line(color='red',
  data=calories_consumed,aes(x=Calories.Consumed,y=reg$fitted.values))

cor(reg$fitted.values,calories_consumed$Weight.gained..grams.)
#sqrt transformation
plot(sqrt(Calories.Consumed),Weight.gained..grams.)
reg_sqrt<-lm(Weight.gained..grams.~sqrt(Calories.Consumed))
cor(sqrt(Calories.Consumed),Weight.gained..grams.)
reg_sqrt$residuals
summary(reg_sqrt)
#sum
sum(reg_sqrt$residuals)#9.592327e-14
#rmse
sqrt(mean(reg_sqrt$residuals^2))#121.7122
#x=calorie_consumed  y=weight.gain

#logarithmic
plot(log(Calories.Consumed),Weight.gained..grams.)
reg_log<-lm(Weight.gained..grams.~log(Calories.Consumed),data=calories_consumed)
cor(log(Calories.Consumed),Weight.gained..grams.)
summary(reg_log)
#sum
sum(reg_log$residuals)#-4.973799e-14
#rmse
sqrt(mean(reg_log$residuals^2))#141.0054



#HENCE the first transormation gives r squared value as 0.89 and also p-value: 2.856e-07 .This model predicts the output.
