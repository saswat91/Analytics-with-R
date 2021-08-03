getwd()
setwd("C:/Users/2/Desktop/DDDM")
# Read Data
data <- read.csv("01_.CSV", header = TRUE)
View(data)
summary(data)

# Data Partition
set.seed(1234)
datapart <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(0.7, 0.3))
training <- data[datapart==1,]
testing <- data[datapart==2,]

# Multiple Linear Regression
model1 <-lm(LC.50 ~ CIC0 +SM1_Dz.Z.+GATS1i+NdsCH+NdssC+MLOGP  , data=training)
model1
summary(model1)
model2 <-lm(LC.50 ~ CIC0 +SM1_Dz.Z.+GATS1i+NdsCH+MLOGP  , data=training)
model2
summary(model2)
anova(model1,model2)
AIC(model1)
AIC(model2)
plot(LC.50 ~ CIC0 +SM1_Dz.Z.+GATS1i+NdsCH+MLOGP,data=training)
abline(model2, col = "blue")

# Prediction
pred <- predict(model2, testing)
pred
actuals_preds <- data.frame(cbind(actuals=testing$LC.50, predicteds=pred)) 
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy 
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape
pred1<-predict(model2, data.frame(CIC0=0.204839, SM1_Dz.Z.=2.224165, GATS1i=0.049839, NdsCH=1.212222, MLOGP=1.045738,interval="confidence"))
pred1
