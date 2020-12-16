#install Library
library(rpart)
library(randomForest)
library(dplyr)
library(ggplot2)
library(funModeling)
library(caret)
library(corrplot)
library(tree)
library(caTools)
library(devtools)
library(car)
library(modelr)

#import the data and give the full input path 
house_data <-read.csv("C:\\Users\\aashi\\Desktop\\SafeJob\\Major Project\\House_Price_Data.csv", header = T, sep = ",")
View(house_data)

#summary of data
summary(house_data)

#names of data
names(house_data)

#dimensions of data
dim(house_data)

#structure of data
str(house_data)

#show null and NA values
colSums(is.na(house_data))
is.null(house_data)

#create new var for Omiting the na data from set
new_data <- na.omit(house_data)
dim(new_data)

#NA values after omit
colSums(is.na(new_data))
is.null(new_data)

#check the data frame for any more data cleaning 
View(new_data)

#we need to fill the blank values in "society" column
#filing the blank values in "society" column
new_data$Society[new_data$Society==""] <- "Not available"
View(new_data)

windows(15,15) # OPEN A NEW WINDOW & PLOT

#univariate graph for the area_type 
freq(new_data$Area_type)

#univariate graph for no of rooms
freq(new_data$No.of.rooms..BHK.)

#univariante graph for availibility
freq(new_data$Availability)

# graphical techniques
# histogram
# The No of rooms variables is positive skewed 
hist(new_data$No.of.rooms..BHK.,
     main = "Histogram of No of rooms",
     xlab = "No of Room (BHK)")

# histogram
# The Total_sqft variables is positive skewed 
hist(new_data$Total_sqft,
     main = "Histogram of Total Sqft",
     xlab = "Total Sqft")


# boxplot
boxplot(new_data$Total_sqft,
        main = toupper("Boxplot of Total Sqft"),
        ylab = "Total Sqft",
        col = "Orange")

boxplot(new_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Price",
        col = "blue")


#removing the outliers value
outlier_values <- boxplot.stats(new_data$Price..in.Lakhs.)$out
outlier_values <- boxplot.stats(new_data$Total_sqft)$out

print(outlier_values)

#create new var which do not contain outliers
new_house_data <- new_data[-which(new_data$Price..in.Lakhs. %in% outlier_values),]
new_house_data <- new_data[-which(new_data$Total_sqft %in% outlier_values),]

View(new_house_data)

#2nd boxplot
boxplot(new_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")

boxplot(new_house_data$Total_sqft,
        main = toupper("Boxplot of Total Sqft"),
        ylab = "Total Sqft",
        col = "orange")

#removing the outliers value
outlier_values <- boxplot.stats(new_house_data$Price..in.Lakhs.)$out
outlier_values <- boxplot.stats(new_house_data$Total_sqft)$out

print(outlier_values)

#create new var which do not contain outliers
Next_house_data <- new_house_data[-which(new_house_data$Price..in.Lakhs. %in% outlier_values),]
Next_house_data <- new_house_data[-which(new_house_data$Total_sqft %in% outlier_values),]

View(Next_house_data)

#3rd boxplot
boxplot(Next_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")

boxplot(Next_house_data$Total_sqft,
        main = toupper("Boxplot of Total Sqft"),
        ylab = "Total Sqft",
        col = "orange")


#removing the outliers value
outlier_values <- boxplot.stats(Next_house_data$Price..in.Lakhs.)$out
outlier_values <- boxplot.stats(Next_house_data$Total_sqft)$out

print(outlier_values)

#create new var which do not contain outliers
Last_house_data <- Next_house_data[-which(Next_house_data$Price..in.Lakhs. %in% outlier_values),]
Last_house_data <- Next_house_data[-which(Next_house_data$Total_sqft %in% outlier_values),]

View(Last_house_data)

#4th boxplot

boxplot(Last_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")


boxplot(Last_house_data$Total_sqft,
        main = toupper("Boxplot of TotalSqft"),
        ylab = "Total Sqft",
        col = "orange")

#removing the outliers value
outlier_values <- boxplot.stats(Last_house_data$Price..in.Lakhs.)$out

print(outlier_values)

#create new var which do not contain outliers
Pre_Final_house_data <- Last_house_data[-which(Last_house_data$Price..in.Lakhs. %in% outlier_values),]

View(Pre_Final_house_data)

#Pre Final boxplot
boxplot(Pre_Final_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")

#removing the outliers value
outlier_values <- boxplot.stats(Pre_Final_house_data$Price..in.Lakhs.)$out

print(outlier_values)

#create new var which do not contain outliers
Post_Final_house_data <- Pre_Final_house_data[-which(Pre_Final_house_data$Price..in.Lakhs. %in% outlier_values),]

View(Post_Final_house_data)

#Post Final boxplot
boxplot(Post_Final_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")

#removing the outliers value
outlier_values <- boxplot.stats(Post_Final_house_data$Price..in.Lakhs.)$out

print(outlier_values)

#create new var which do not contain outliers
Final_house_data <- Post_Final_house_data[-which(Post_Final_house_data$Price..in.Lakhs. %in% outlier_values),]

View(Final_house_data)

#Final boxplot
boxplot(Final_house_data$Price..in.Lakhs.,
        main = toupper("Boxplot of Price"),
        ylab = "Total Price",
        col = "blue")

#create a separate var for co-relation of data
cor_data <- Final_house_data[c("No.of.rooms..BHK.","Total_sqft","No.of.bathrooms","Balcony","Price..in.Lakhs.")]
View(cor_data)

#plot complete corr
corrplot(cor(cor_data),method = "number")

#complete plot of dataset
plot(cor_data)

#it is use to delete older plots
#dev.off()

#individual scatterplot of Dependent value (price) with other variables
scatterplot(Price..in.Lakhs.~ No.of.rooms..BHK., data = cor_data,grid = FALSE, frame = FALSE)
scatterplot(Price..in.Lakhs.~ Total_sqft, data = cor_data,grid = FALSE, frame = FALSE)
scatterplot(Price..in.Lakhs.~ No.of.bathrooms, data = cor_data,grid = FALSE, frame = FALSE)
scatterplot(Price..in.Lakhs.~ Balcony, data = cor_data,grid = FALSE, frame = FALSE)


#1) Decision Tree
#regression tree
regression_model <- tree(Price..in.Lakhs.~.,data = cor_data)

#summary
summary(regression_model)

#plot tree
plot(regression_model)
text(regression_model)

#predict the values
Price_predict <- predict(regression_model,newdata = cor_data)
print(Price_predict)

#show predition colunm and RMSE Column
cor_data['Predicted Price']<- Price_predict
cor_data['Residual Error']<- cor_data$Price..in.Lakhs.-cor_data$`Predicted Price`
View(cor_data)

#Mean of absolute error
RMSE(cor_data$Price..in.Lakhs.,Price_predict)
MAE(cor_data$Price..in.Lakhs.,Price_predict)


# 2)Random Forest

set.seed(100)

#create the split of dataset for 80% into new var
rowindex<-sample.split(Final_house_data$Price..in.Lakhs., SplitRatio = 0.80)
print(rowindex)

#create two sep var for train and test data
traindf<-subset(Final_house_data,rowindex==TRUE)
testdf<-subset(Final_house_data,rowindex==FALSE)

#View dataframe
View(traindf)
View(testdf)

#check no of rows and columns
dim(traindf)
dim(testdf)

#random forest regression
?randomForest

# mtry - Number of variables available for splitting at each tree node. 
# For regression models - it is the number of predictor variables divided by 3 (rounded down).
rfmodel <- randomForest(Price..in.Lakhs.~.,mtry=4,ntree=1000,data = traindf, importance = T)

#summary of model
rfmodel
summary(rfmodel)

#plot model
plot(rfmodel)

#importance
View(rfmodel$importance)

#predicting the price on test dataset
predicted_price<-predict(rfmodel,newdata = testdf)

testdf['Predict Price'] <- predicted_price
View(testdf)

#RMSE
RMSE(predicted_price,testdf$Price..in.Lakhs.)

#Mean of absolute error
MAE(predicted_price,testdf$Price..in.Lakhs.)

#plot importance
varImpPlot(rfmodel)


# 3) Lin Reg

#ploting the model
plot(Price..in.Lakhs. ~ .,data = cor_data)

#creating Linear regression model
# lm - LINEAR MODELS
# Dependent veriable/Col - Price
# Independent veriable/col - .
model <- lm(Price..in.Lakhs.~ ., data = cor_data)
summary(model)

# NORMAL Q-Q PLOT
# IF POINTS ARE PLOTTED ON DOTTED LINE THEN THIS IS 
# NORMAL DISTRIBUTION IF NOT THEN +ve OR -ve SKEWED
plot(model)

mean(model$residuals) # mean of error 0
hist(model$residuals) # normal distribution

#check col names
names(cor_data)

# plot for x and y
#ggplot(cor_data, aes(Price..in.Lakhs.,No.of.rooms..BHK.)) + geom_point() +
#stat_smooth(method = lm)

#Predict the values and store in 'Y_reg'
y_regression <- predict(model,newdata = cor_data)

#RMSE
RMSE(y_regression,cor_data$Price..in.Lakhs.)

summary(model)
