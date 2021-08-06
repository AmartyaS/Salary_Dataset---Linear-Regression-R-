library("lattice")

# Reading the data file
file <- read.csv(file.choose())

# Exploring the data file
summary(file)
attach(file)
dotplot(Salary,main="Dot Plot of Salary")
dotplot(YearsExperience,main="Dot Plot of Years of Experience")
range(Salary)
range(YearsExperience)
boxplot(Salary,col="dodgerblue4")
boxplot(YearsExperience,col="red", horizontal = T)
plot(Salary,YearsExperience)
hist(Salary,probability = T)
hist(YearsExperience,probability = T)
plot(Salary,YearsExperience ,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Salary", 
     ylab="Years Of Experience", pch=20) 

# First Model
reg <- lm(Salary~YearsExperience)
summary(reg)
confint(reg,level = 0.95)
pred <- as.data.frame(predict(reg,interval = "predict"))
pred <- pred$fit
cor(pred,Salary)

# Second Model
reg2 <- lm(Salary~YearsExperience+I(YearsExperience*YearsExperience))
summary(reg2)
confint(reg,level=0.95)
pred2 <- as.data.frame(predict(reg2,interval = "predict"))
pred2 <- pred2$fit
cor(pred2,Salary)

# Third Model
reg3 <- lm(Salary~log(YearsExperience))
summary(reg3)
confint(reg3,level=0.95)
pred3 <- as.data.frame(predict(reg3,interval = "predict"))
pred3 <- pred3$fit
cor(pred3,Salary)

# Fourth Model
reg4 <- lm(log(Salary)~YearsExperience)
summary(reg4)
confint(reg4,level = 0.95)
pred4 <- as.data.frame(predict(reg4,interval = "predict"))
pred4 <- exp(pred4$fit)
cor(pred4,Salary)
