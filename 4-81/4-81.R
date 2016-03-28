##Define RMSE Function
rmse <- function(x,y)sqrt(mean((x-y)^2))

##Load Training dataset 
blogData_train81 <- read.csv("C:/Users/vanwu/Desktop/INFO 7390 ADS/Midterm/BlogFeedback/blogData_train81.csv", header=FALSE)

##Load testing dataset
blogData_test81 <- read.csv(file = "C:/Users/vanwu/Desktop/INFO 7390 ADS/Midterm/BlogFeedback/blog_data_test81.csv", header = FALSE)


#Training LR
lmFit81 <- lm(V81 ~ ., data = blogData_train81)
summary(lmFit81)

#Testing LR
rmseLm81 <- rmse(predict(lmFit81,blogData_test81),blogData_test81$V81)
rmseLm81

#Training CART
library(rpart)
cartFit81 <- rpart(V81 ~ ., method="anova", data=blogData_train81)
#Visualize the tree
install.packages("rattle")
install.packages("dynamicGraph")
library("rattle")					# Fancy tree plot
plot(cartFit81, uniform=TRUE, main="Regression Tree")
text(cartFit81, use.n=TRUE, all=TRUE, cex=.8)
install.packages("rpart.plot")
library(rpart.plot)
fancyRpartPlot(cartFit81)
#Test CART
rmseCart81 <-rmse(predict(cartFit81,blogData_test81),blogData_test81$V81)
rmseCart81

#Training Random forest
install.packages("randomForest")
library(randomForest)
rffit81 <- randomForest(V81 ~ ., data = blogData_train81, ntree = 100, mtry = 27  ,importance=TRUE, na.action = na.omit)
summary(rffit81)

#Test Random forest
rmseRF81 <-rmse(predict(rffit81,blogData_test81),blogData_test81$V81)
rmseRF81
