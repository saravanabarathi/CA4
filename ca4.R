#importing the dataset into Employment dataframe
Employment <- read.csv("Employment1.csv", header = TRUE)
Employment

#importing the crime dataset into crime dataframe
Crime <- read.csv("crime1.csv", header = TRUE)
Crime

#merging both the data frames
data_merge <- merge(Employment, Crime)
data_merge

#creating a linear model for both attributes
linear_model <- lm(Total.crime ~ Employment, data = data_merge) 
linear_model

#Plotting Employment and Total crime attributes
plot(data_merge$Employment, data_merge$Total.crime,
     xlab = "Employment",
     ylab = "Total crime",
     main = "Plot for Employment vs Total crimes")

#sketching a reference line to the plot with respect to linear model
abline(linear_model)

#summary of linear_model 
summary(linear_model)

#finding the correlation between Employment and total crime
cor(data_merge$Employment, data_merge$Total.crime)

#Examining 95% confidence intervals of the model
confint(linear_model)

#Examining the goodness of fit of the model
summary(linear_model)

#scatter plot for visualizing the linear relationship between the two variables
scatter.smooth(x = data_merge$Employment,
               y = data_merge$Total.crime,
               main = "Employment vs Total crimes",
               xlab = "Employment",
               ylab = "Total crimes")

#dividing the graph area into two columns
par(mfrow = c(1,2))
boxplot(data_merge$Employment, main = "Employment", 
        sub = paste("Outlier rows: ", boxplot.stats(data_merge$Employment)$out))

#using box plot to detect the outliers in Total crime
boxplot(data_merge$Total.crime, main = "Total crimes", 
        sub = paste("Outlier rows: ", boxplot.stats(data_merge$Total.crime)$out))

#now to detect outliers in Employment
boxplot(data_merge$Employment, main = "Employment", 
        sub = paste("Outlier rows: ", boxplot.stats(data_merge$Employment)$out))

#now using "e1071" library to calculate the outliers
install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))

#Plotting the density plot for determining the skewness for Employment variable
plot(density(data_merge$Employment), main = "Employment Density plot", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(data_merge$Employment), 2)))

#Filling the plot with brown color
polygon(density(data_merge$Employment), col = "Brown")

#Plotting the density plot for determining the skewness for Total crime variable
plot(density(data_merge$Total.crime), main = "Total crime Density plot", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(data_merge$Total.crime), 2)))

#Filling the plot with red color
polygon(density(data_merge$Total.crime), col = "Red")

#Finding correlation between the attributes
cor(data_merge$Employment, data_merge$Total.crime)

#summary
summary_linear <- summary(linear_model)
summary_linear

#storing model co-efficients in new dataframe
coefficient1 <- summary_linear$coefficients
coefficient1

# calculating beta estimate for Employment
beta.estimate <- coefficient1["Employment", "Estimate"]
beta.estimate

#calculating standard error for Employment
standard_error <- coefficient1["Employment", "Std. Error"] 
standard_error

#t-statistic value
t_value <- beta.estimate / standard_error

#p-value 
p_value <- 2 * pt(-abs(t_value), df = nrow(data_merge) - ncol(data_merge))

#f-statistics 
f_statistic <- linear_model$fstatistic[1]

#parameters for model p-value calculations
f <- summary(linear_model)$fstatistic

#model's p-value
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

#finding AIC and BIC values for linear regression model
AIC(linear_model)

BIC(linear_model)

#making training and testing datasets 

# Assuming 80% of the dataset as training dataset 
records <- sample(1:nrow(data_merge), 0.8 * nrow(data_merge))
training_sample <- data_merge[records,]
training_sample

#Assuming the remaining 20% of the dataset as testing dataset
testing_sample <- data_merge[-records,]
testing_sample

#Building the linear regression model on the training sample
linear_model_training <- lm(Total.crime ~ Employment, data = training_sample)
summary(linear_model_training)


prediction_mod <- predict(linear_model_training, testing_sample)

actual_prediction_mod <- data.frame(cbind(actuals = testing_sample$Total.crime, predicted = prediction_mod))
head(actual_prediction_mod)

correlation_accuracy <- cor(actual_prediction_mod)


min_max_accuracy <- mean(apply(actual_prediction_mod, 1, min) / apply(actual_prediction_mod, 1, max))
min_max_accuracy


mape <- mean(abs((actual_prediction_mod$predicted - actual_prediction_mod$actuals)) / actual_prediction_mod$actuals)
mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel1 <- gvlma(linear_model)
summary(gvmodel1)


# Polynomial Regression ------------

polynom_fit <- lm(Total.crime ~ Employment + I(Employment ^ 2), data = data_merge)
plot(polynom_fit)

#summary
summary(polynom_fit)


plot(data_merge$Employment, data_merge$Total.crime, xlab = "Employment", ylab = "Total crimes")
lines(data_merge$Employment, fitted(polynom_fit))


# Determining the AIC and BIC for the polynomial regression model
AIC(polynom_fit)
BIC(polynom_fit)

# training the polynomial regression model with training dataset
poly_model <- lm(Total.crime ~ Employment + I(Employment ^ 2), data = training_sample)

prediction_mod1 <- predict(poly_model, testing_sample)
prediction_mod1


actual_prediction_polynom <- data.frame(cbind(actuals = testing_sample$Total.crime, predicted = prediction_mod1))
actual_prediction_polynom


AIC(polynom_fit)

BIC(polynom_fit)

correlation_accuracy1 <- cor(actual_prediction_polynom)
correlation_accuracy1


min_max_accuracy_polynom <- mean(apply(actual_prediction_polynom, 1, min) / apply(actual_prediction_polynom, 1, max))
min_max_accuracy_polynom

mape_polynom <- mean(abs((actual_prediction_polynom$predicted - actual_prediction_polynom$actuals)) / actual_prediction_polynom$actuals)
mape_polynom

summary(prediction_mod1)

library(gvlma)
gvmodel <- gvlma(polynom_fit)
summary(gvmodel)

# Multi-linear ----------


multi_linear_fit <- lm(Total.crime ~ Employment + County, data = data_merge)
par(mfrow = c(2,2))
plot(multi_linear_fit)


# Determining the AIC and BIC for the Multi-linear regression model
AIC(multi_linear_fit)
BIC(multi_linear_fit)

# training the polynomial regression model with training dataset
multi_linear_model <- lm(Total.crime ~ Employment + County, data = training_sample)

multi_linear_prediction <- predict(multi_linear_model, testing_sample)
summary(multi_linear_prediction)

actual_prediction_multilinear <- data.frame(cbind(actuals = testing_sample$Total.crime, 
                                                  predicted = multi_linear_prediction))
head(actual_prediction_multilinear)

AIC(multi_linear_fit)
BIC(multi_linear_fit)

correlation_accuracy2 <- cor(actual_prediction_multilinear)
correlation_accuracy2

min_max_accuracy_multilinear <- mean(apply(actual_prediction_multilinear, 1, min) / 
                                       apply(actual_prediction_multilinear, 1, max))
min_max_accuracy_multilinear

mape_multilinear <- mean(abs((actual_prediction_multilinear$predicted - actual_prediction_multilinear$actuals)) / 
                           actual_prediction_multilinear$actuals)
mape_multilinear

par(mar= c(1,1,1,1))
library(gvlma)
gvmodel1 <- gvlma(multi_linear_fit)
summary(gvmodel1)
