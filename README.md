# R.-Project-Modeling-Total-Personal-Income-in-the-U.S.-Code
Please find the full code that i have used for my project on R.

#MY FINAL PROJECT.
# ACS Income Analysis Project


# The first thing is to install required packages

install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("caret")
install.packages("leaps")
install.packages("tidyr")


#we will load the required libraries
library(haven)
library(dplyr)
library(ggplot2)
library(caret)
library(leaps)
library(maps)
library(tidyr)


# Set seed for reproducibility 
set.seed(12345)

#set the directory where we will read the dataset from
setwd("C:/Users/mm00606/Desktop/misheck")

# we will read the dataset
my.data <- read_dta("acs_2023.dta")


# we will then start the Data preparation
# Handle missing values (9999999 represents missing)
my.data$inctot[my.data$inctot == 9999999] <- NA
my.data$hhincome[my.data$hhincome == 9999999] <- NA

# after that we will filter working age population (18-64) with non-missing income
my.data <- my.data[my.data$age >= 18 & my.data$age <= 64 & !is.na(my.data$inctot), ]

# Then we will Create dummy variables
# Gender
my.data$female <- ifelse(my.data$sex == 2, 1, 0)

# Education 
my.data$less_hs <- ifelse(my.data$educ <= 6, 1, 0)
my.data$high_school <- ifelse(my.data$educ >= 7 & my.data$educ <= 9, 1, 0)
my.data$some_college <- ifelse(my.data$educ >= 10 & my.data$educ <= 11, 1, 0)
my.data$associate <- ifelse(my.data$educ == 12, 1, 0)
my.data$bachelors <- ifelse(my.data$educ == 13, 1, 0)
my.data$graduate <- ifelse(my.data$educ >= 14, 1, 0)

# Race/ethnicity dummies
my.data$white <- ifelse(my.data$race == 1 & my.data$hispan == 0, 1, 0)
my.data$black <- ifelse(my.data$race == 2 & my.data$hispan == 0, 1, 0)
my.data$hispanic <- ifelse(my.data$hispan > 0, 1, 0)
my.data$asian <- ifelse((my.data$race == 4 | my.data$race == 5 | my.data$race == 6) & my.data$hispan == 0, 1, 0)

# Homeownership
my.data$homeowner <- ifelse(my.data$ownershp == 1, 1, 0)

# Labor force
my.data$in_labor_force <- ifelse(my.data$labforce == 2, 1, 0)

# Marital status
my.data$married <- ifelse(my.data$marst == 1, 1, 0)

# Step 3: Descriptive Analysis

# My Summary statistics
summary(my.data$inctot)
sd(my.data$inctot, na.rm = TRUE)

# Income histogram
hist(my.data$inctot, breaks = 50, main = "Distribution of Personal Income",
     xlab = "Income (USD)", col = "lightblue", xlim = c(0, 200000))

# Income by gender
boxplot(inctot ~ female, data = my.data, 
        names = c("Male", "Female"),
        main = "Income by Gender", 
        ylab = "Income (USD)")


# Income by education
# we will create a factor for education level to use in plots
my.data$educ_level <- NA
my.data$educ_level[my.data$less_hs == 1] <- "Less than HS"
my.data$educ_level[my.data$high_school == 1] <- "High School"
my.data$educ_level[my.data$some_college == 1] <- "Some College"
my.data$educ_level[my.data$associate == 1] <- "Associate"
my.data$educ_level[my.data$bachelors == 1] <- "Bachelor's" 
my.data$educ_level[my.data$graduate == 1] <- "Graduate"
my.data$educ_level <- factor(my.data$educ_level, 
                             levels = c("Less than HS", "High School", "Some College", 
                                        "Associate", "Bachelor's", "Graduate"))

#After that we will Plot income by education
boxplot(inctot ~ educ_level, data = my.data,
        main = "Income by Education Level",
        ylab = "Income (USD)",
        xlab = "Education Level",
        las = 2)

# Income by race/ethnicity
my.data$race_eth <- NA
my.data$race_eth[my.data$white == 1] <- "White"
my.data$race_eth[my.data$black == 1] <- "Black"
my.data$race_eth[my.data$hispanic == 1] <- "Hispanic"
my.data$race_eth[my.data$asian == 1] <- "Asian"
my.data$race_eth <- factor(my.data$race_eth)

boxplot(inctot ~ race_eth, data = my.data,
        main = "Income by Race/Ethnicity",
        ylab = "Income (USD)")

# Income by age groups
my.data$age_group <- cut(my.data$age, 
                         breaks = c(17, 24, 34, 44, 54, 64),
                         labels = c("18-24", "25-34", "35-44", "45-54", "55-64"))

boxplot(inctot ~ age_group, data = my.data,
        main = "Income by Age Group",
        ylab = "Income (USD)")

# Income by labor force status
boxplot(inctot ~ in_labor_force, data = my.data,
        names = c("Not in Labor Force", "In Labor Force"),
        main = "Income by Labor Force Status",
        ylab = "Income (USD)")

# Income by homeownership
boxplot(inctot ~ homeowner, data = my.data,
        names = c("Renter", "Homeowner"),
        main = "Income by Homeownership Status",
        ylab = "Income (USD)")


# After that we will start Predictive Modeling
# We will put our focus on working adults in the labor force
model.data <- my.data[!is.na(my.data$uhrswork) & my.data$in_labor_force == 1, ]

#we chose a random sample
if(nrow(model.data) > 10000) {
  selected <- sample(1:nrow(model.data), 10000)
  model.data <- model.data[selected, ]
}

# After choosing the random sample we will cCreate training and test partitions
set.seed(12345)
selected <- sample(1:nrow(model.data), size = nrow(model.data)*0.8, replace = FALSE)
training <- model.data[selected, ]
test <- model.data[-selected, ]

# Model 1: Simple linear model with age
model.1 <- lm(inctot ~ age, data = training)
summary(model.1)
rmse.1 <- sqrt(mean((model.1$residuals)^2))
print(paste("RMSE for Model 1:", rmse.1))

# Model 2: Multiple predictors
model.2 <- lm(inctot ~ age + female + uhrswork, data = training)
summary(model.2)
rmse.2 <- sqrt(mean((model.2$residuals)^2))
print(paste("RMSE for Model 2:", rmse.2))

# Model 3: Add education and race
model.3 <- lm(inctot ~ age + female + uhrswork + high_school + some_college + 
                associate + bachelors + graduate + white + black + hispanic + asian, 
              data = training)
summary(model.3)
rmse.3 <- sqrt(mean((model.3$residuals)^2))
print(paste("RMSE for Model 3:", rmse.3))

# we will then Generate predictions using validation dataset 
test$inctot_hat <- predict(model.3, newdata = test)

# Evaluation using RMSE on test data
rmse.test <- sqrt(1/nrow(test)*sum((test$inctot-test$inctot_hat)^2))
print(paste("Test RMSE:", rmse.test))

# K-fold cross validation CV 
model.4 <- train(inctot ~ age + female + uhrswork + high_school + some_college + 
                   associate + bachelors + graduate + white + black + hispanic + asian,
                 data = training,
                 method = "lm",
                 trControl = trainControl(method = "cv", number = 10)
)
print(model.4)

# Feature selection using stepwise forward selection
model.5 <- train(inctot ~ age + female + uhrswork + high_school + some_college + 
                   associate + bachelors + graduate + white + black + hispanic + 
                   asian + married + homeowner,
                 data = training,
                 method = "leapForward",
                 tuneGrid = data.frame(nvmax = 1:14),
                 trControl = trainControl(method = "cv", number = 10)
)
print(model.5)
plot(model.5)
summary(model.5$finalModel)

# Final model based on the selected features
final.model <- lm(inctot ~ age + female + uhrswork + bachelors + graduate + white + 
                    black + hispanic + married + homeowner, data = training)
summary(final.model)

# We will generate final predictions
test$inctot_hat_final <- predict(final.model, newdata = test)
final.rmse <- sqrt(1/nrow(test)*sum((test$inctot-test$inctot_hat_final)^2))
print(paste("Final Model Test RMSE:", final.rmse))

# Plot actual vs predicted income
plot(test$inctot, test$inctot_hat_final, 
     main = "Actual vs Predicted Income",
     xlab = "Actual Income", 
     ylab = "Predicted Income",
     pch = 20,
     col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)

# Lastly we will test the data
tss <- sum((test$inctot - mean(test$inctot))^2)
rss <- sum((test$inctot - test$inctot_hat_final)^2)
test.r.squared <- 1 - (rss/tss)
print(paste("R-squared on test data:", test.r.squared))

# Recursive Feature Elimination (RFE) approach 
predictors <- training[, c("age", "female", "uhrswork", "bachelors", "graduate", 
                           "white", "black", "hispanic", "married", "homeowner")]
outcome <- training$inctot

model.6 <- rfe(x = predictors, y = outcome,
               sizes = c(1:10),
               rfeControl = rfeControl(
                 functions = lmFuncs,
                 method = "cv",
                 number = 10,
                 verbose = TRUE
               )
)
print(model.6)
print(model.6$fit)




































































