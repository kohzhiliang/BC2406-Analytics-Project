# load necessary libraries
library(data.table)
library(car)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caTools)
library(pROC)
library(randomForest)
library(nnet)
library(caret)
options(scipen=999)

setwd('/Users/esmondchan/Downloads/Sem5Team7 Group Project')
data <- fread("ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors = TRUE)

# display summary  
summary(data)

# check for missing values
colSums(is.na(data))

# renaming of column names
data <- data %>%
  rename(
    high_caloric_food_frequenctly = FAVC, 
    veg_frequency = FCVC,
    num_of_meals = NCP,
    eat_between_meals = CAEC,
    smoker = SMOKE,  
    water_per_day = CH2O,  
    monitor_calories = SCC,  
    physical_activity_frequency = FAF,
    hrs_on_tech_device = TUE,
    alcohol_frequency = CALC,
    usual_mode_of_transport = MTRANS
  )

unique(data$Gender)

summary(data$Age)
data$Age <- round(data$Age)

summary(data$Height)  # note this wont be use in modeling
summary(data$Weight)  # note this wont be use in modeling

class(data$family_history_with_overweight)
unique(data$family_history_with_overweight)

class(data$high_caloric_food_frequenctly)
unique(data$high_caloric_food_frequenctly)

class(data$veg_frequency)
summary(data$veg_frequency) 
# rounding of decimal values 
data$veg_frequency <- ifelse(data$veg_frequency < 1.5, 1,
                                   ifelse(data$veg_frequency < 2.5, 2, 3))
unique(data$veg_frequency)
# rename levels according to original dataset source
data$veg_frequency <- factor(data$veg_frequency, levels = c(1, 2, 3), 
                             labels = c("Never", "Sometimes", "Always"))
data$veg_frequency <- relevel(data$veg_frequency, ref = "Never")

class(data$num_of_meals)
summary(data$num_of_meals)
# rounding of decimal values 
data$num_of_meals <- ifelse(data$num_of_meals < 1.5, 1,
                            ifelse(data$num_of_meals < 2.5, 2, 
                                   ifelse(data$num_of_meals < 3.5, 3, 4)))
unique(data$num_of_meals)
# rename levels according to original dataset source
data$num_of_meals <- factor(data$num_of_meals, levels = c(1, 2, 3, 4), 
                             labels = c("1", "2", "3", ">3"))
data$num_of_meals <- relevel(data$num_of_meals, ref = "1")

class(data$eat_between_meals)
unique(data$eat_between_meals)
data$eat_between_meals <- relevel(data$eat_between_meals, ref = "no")

class(data$smoker)
unique(data$smoker)

class(data$water_per_day)
summary(data$water_per_day)
# rounding of decimal values 
data$water_per_day <- ifelse(data$water_per_day < 1.5, 1,
                             ifelse(data$water_per_day < 2.5, 2, 3))
unique(data$water_per_day)
# rename levels according to original dataset source
data$water_per_day <- factor(data$water_per_day, levels = c(1, 2, 3), 
                             labels = c("<1L", "1-2L", ">2L"))
data$water_per_day <- relevel(data$water_per_day, ref = "<1L")

class(data$monitor_calories)
unique(data$monitor_calories)

class(data$physical_activity_frequency)
summary(data$physical_activity_frequency)
# rounding of decimal values 
data$physical_activity_frequency <- ifelse(data$physical_activity_frequency < 0.5, 0,
                                           ifelse(data$physical_activity_frequency < 1.5, 1, 
                                                  ifelse(data$physical_activity_frequency < 2.5, 2, 3)))
unique(data$physical_activity_frequency)
# rename levels according to original dataset source
data$physical_activity_frequency <- factor(data$physical_activity_frequency, levels = c(0, 1, 2, 3), 
                            labels = c("0", "1-2 days", "2-4 days", "4-5 days"))
data$physical_activity_frequency <- relevel(data$physical_activity_frequency, ref = "0")

class(data$hrs_on_tech_device)
summary(data$hrs_on_tech_device)
# rounding of decimal values 
data$hrs_on_tech_device <- ifelse(data$hrs_on_tech_device < 0.5, 0,
                             ifelse(data$hrs_on_tech_device < 1.5, 1, 2))
unique(data$hrs_on_tech_device)
# rename levels according to original dataset source
data$hrs_on_tech_device <- factor(data$hrs_on_tech_device, levels = c(0, 1, 2), 
                             labels = c("0-2 hrs", "3-5 hrs", ">5 hrs"))
data$hrs_on_tech_device <- relevel(data$hrs_on_tech_device, ref = "0-2 hrs")

class(data$alcohol_frequency)
unique(data$alcohol_frequency)
summary(data$alcohol_frequency)
data$alcohol_frequency <- relevel(data$alcohol_frequency, ref = "no")

class(data$usual_mode_of_transport)
unique(data$usual_mode_of_transport)
data$usual_mode_of_transport <- relevel(data$usual_mode_of_transport, ref = "Walking")

# Notable Findings ---------------------------------------------------------------------------
par(mar = c(4, 5, 4, 3))  
pie(summary(data$NObeyesdad), 
    col = rainbow(length(summary(data$NObeyesdad))), 
    main = "Distribution of NObeyesdad (y-Variable)",
    labels = paste(names(summary(data$NObeyesdad)), summary(data$NObeyesdad), sep = " : "))

par(mar = c(5, 10, 4, 3))  
boxplot(data$Age ~ data$NObeyesdad,
        horizontal = TRUE,
        col = rainbow(length(summary(data$NObeyesdad))), 
        las = 1,
        main = "NObeyesdad vs Age",
        xlab = "Age",
        ylab = "")

ggplot(data, aes(y = NObeyesdad, fill = family_history_with_overweight)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Family History with Overweight") 

ggplot(data, aes(y = NObeyesdad, fill = high_caloric_food_frequenctly)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Frequent Consumption of High Caloric Food") 

ggplot(data, aes(y = NObeyesdad, fill = veg_frequency)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Frequency of Veg Consumption") 

ggplot(data, aes(y = NObeyesdad, fill = num_of_meals)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Number of Main Meals") 

ggplot(data, aes(y = NObeyesdad, fill = eat_between_meals)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Consumption of Food Between Meals") 

ggplot(data, aes(y = NObeyesdad, fill = smoker)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Smoker") 

ggplot(data, aes(y = NObeyesdad, fill = water_per_day)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Daily Water Intake") 

ggplot(data, aes(y = NObeyesdad, fill = monitor_calories)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Whether Calorie Intake is Monitored") 

ggplot(data, aes(y = NObeyesdad, fill = hrs_on_tech_device)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Time Spent Using Tech Devices") 

ggplot(data, aes(y = NObeyesdad, fill = alcohol_frequency)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Frequency of Alcohol Consumption") 

ggplot(data, aes(y = NObeyesdad, fill = usual_mode_of_transport)) +
  geom_bar(position = "stack") +
  labs(y = "", 
       x = "Count", 
       title = "NObeyesdad vs Usual Mode of Transport") 


# Split the data into training and test sets -------------------------------------------------
# reference to normal weight for better comparison as normal weight shld be the benchmark
data$NObeyesdad <- relevel(data$NObeyesdad, ref = "Normal_Weight")

set.seed(123)
# 70/30 train test split 
train <- sample.split(Y = data$NObeyesdad, SplitRatio = 0.7)
trainset <- subset(data, train == TRUE)
testset <- subset(data, train == FALSE)

# Multi Category Y Logistic Regression -------------------------------------------------------
# create dummy model using glm to find vif among variables
dummy_model <- glm(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                     high_caloric_food_frequenctly + veg_frequency + num_of_meals + 
                     eat_between_meals + smoker + water_per_day + monitor_calories + 
                     physical_activity_frequency + hrs_on_tech_device + alcohol_frequency + 
                     usual_mode_of_transport, data = trainset, family = binomial())

vif(dummy_model)

# train multinomial logistic regression using multinom
multi_model <- multinom(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                          high_caloric_food_frequenctly + veg_frequency + num_of_meals + 
                          eat_between_meals + smoker + water_per_day + monitor_calories + 
                          physical_activity_frequency + hrs_on_tech_device + alcohol_frequency + 
                          usual_mode_of_transport, data = trainset)

summary(multi_model)

OR <- exp(coef(multi_model))
OR.CI <- exp(confint(multi_model))

multi_predictions <- predict(multi_model, newdata = testset)

# shows the confusion matrix & statistics consisting of precision, recall etc
multi_cm <- confusionMatrix(multi_predictions, testset$NObeyesdad)
multi_cm

# nicer plot for confusion matrix
ggplot(as.data.frame(multi_cm$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1, color = "black") +
  labs(title = "Confusion Matrix",
       x = "Actual Classes",
       y = "Predicted Classes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# accuracy of MLR
multi_accuracy <- mean(multi_predictions == testset$NObeyesdad)
multi_accuracy

# ROC curves
multi_probabilities <- predict(multi_model, newdata = testset, type = "probs")

roc_insufficient_logistic <- roc(testset$NObeyesdad == "Insufficient_Weight", multi_probabilities[, "Insufficient_Weight"])
roc_normal_logistic <- roc(testset$NObeyesdad == "Normal_Weight", multi_probabilities[, "Normal_Weight"])
roc_overweight1_logistic <- roc(testset$NObeyesdad == "Overweight_Level_I", multi_probabilities[, "Overweight_Level_I"])
roc_overweight2_logistic <- roc(testset$NObeyesdad == "Overweight_Level_II", multi_probabilities[, "Overweight_Level_II"])
roc_obesity1_logistic <- roc(testset$NObeyesdad == "Obesity_Type_I", multi_probabilities[, "Obesity_Type_I"])
roc_obesity2_logistic <- roc(testset$NObeyesdad == "Obesity_Type_II", multi_probabilities[, "Obesity_Type_II"])
roc_obesity3_logistic <- roc(testset$NObeyesdad == "Obesity_Type_III", multi_probabilities[, "Obesity_Type_III"])


# plot the ROC curves
plot(roc_insufficient_logistic, col = "red", main = "ROC Curves for Each Weight Category")
plot(roc_normal_logistic, col = "orange", add = TRUE)
plot(roc_overweight1_logistic, col = "yellow", add = TRUE)
plot(roc_overweight2_logistic, col = "green", add = TRUE)
plot(roc_obesity1_logistic, col = "blue", add = TRUE)
plot(roc_obesity2_logistic, col = "violet", add = TRUE)
plot(roc_obesity3_logistic, col = "purple", add = TRUE)
legend("bottomright", legend = c("Insufficient Weight", "Normal Weight", "Overweight1", "Overweight2", "Obesity1", "Obesity2", "Obesity3"),
       col = c("red", "orange", "yellow", "green", "blue", "violet", "purple"), lwd = 2)


# CART Model (Decision Tree) -----------------------------------------------------------------
# build a cart model using rpart
cart_model <- rpart(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                      high_caloric_food_frequenctly + veg_frequency + num_of_meals + 
                      eat_between_meals + smoker + water_per_day + monitor_calories + 
                      physical_activity_frequency + hrs_on_tech_device + alcohol_frequency + 
                      usual_mode_of_transport, data = trainset, method = "class", 
                      control = rpart.control(minsplit = 2, cp = 0.0001))

# Plot the decision tree
rpart.plot(cart_model)
print(cart_model)
printcp(cart_model)  # Cross-validation results
plotcp(cart_model)   # Plot cross-validation errors

# Prune the decision tree based on optimal complexity parameter (CP)

CVerror.cap <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xerror"] + cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart_model$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
i

# i = 30, take 30th and 29th cp value
optimal_cp <- sqrt(0.00071023 * 0.00081169)
# attempt to prune tree further but notice accuracy drops rather badly
alternate_cp <- sqrt(0.01136364 * 0.01461039)

# prune for visualisation, to be used in slides
temp_cp <- sqrt(0.04464286 * 0.06818182)
cart_model_temp <- prune(cart_model, cp = temp_cp)
rpart.plot(cart_model_temp, cex = .8)
plotcp(cart_model_temp)
print(cart_model_temp)

# pruning according to 1SE rule
cart_model_pruned <- prune(cart_model, cp = optimal_cp)
summary(cart_model_pruned)

# Plot the pruned tree
rpart.plot(cart_model_pruned)
plotcp(cart_model_pruned)
print(cart_model_pruned)

# Predict on test set using pruned CART model
cart_model_predict <- predict(cart_model_pruned, newdata = testset, type = 'class')

# shows the confusion matrix & statistics consisting of precision, recall etc
cart_cm <- confusionMatrix(cart_model_predict, testset$NObeyesdad)
cart_cm

# nicer plot for confusion matrix
ggplot(as.data.frame(cart_cm$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1, color = "black") +
  labs(title = "Confusion Matrix",
       x = "Actual Classes",
       y = "Predicted Classes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# accuracy of CART
cart_accuracy <- mean(cart_model_predict == testset$NObeyesdad)
cart_accuracy

# plot the variable importance of CART
cart_model_pruned$variable.importance
par(mfrow = c(1, 1), mar = c(7, 8, 4, 5) + 0.1)
barplot(cart_model_pruned$variable.importance / sum(cart_model_pruned$variable.importance) * 100, 
        main = "Variable Importance for CART", 
        horiz = TRUE, 
        col = rainbow(length(cart_model_pruned$variable.importance)),
        las = 1,
        cex.names = 0.6)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# ROC curves
cart_probabilities <- predict(cart_model_pruned, newdata = testset, type = "prob")

roc_insufficient_cart <- roc(testset$NObeyesdad == "Insufficient_Weight", cart_probabilities[, "Insufficient_Weight"])
roc_normal_cart <- roc(testset$NObeyesdad == "Normal_Weight", cart_probabilities[, "Normal_Weight"])
roc_overweight1_cart <- roc(testset$NObeyesdad == "Overweight_Level_I", cart_probabilities[, "Overweight_Level_I"])
roc_overweight2_cart <- roc(testset$NObeyesdad == "Overweight_Level_II", cart_probabilities[, "Overweight_Level_II"])
roc_obesity1_cart <- roc(testset$NObeyesdad == "Obesity_Type_I", cart_probabilities[, "Obesity_Type_I"])
roc_obesity2_cart <- roc(testset$NObeyesdad == "Obesity_Type_II", cart_probabilities[, "Obesity_Type_II"])
roc_obesity3_cart <- roc(testset$NObeyesdad == "Obesity_Type_III", cart_probabilities[, "Obesity_Type_III"])

# plot the ROC curves
plot(roc_insufficient_cart, col = "red", main = "ROC Curves for Each Weight Category")
plot(roc_normal_cart, col = "orange", add = TRUE)
plot(roc_overweight1_cart, col = "yellow", add = TRUE)
plot(roc_overweight2_cart, col = "green", add = TRUE)
plot(roc_obesity1_cart, col = "blue", add = TRUE)
plot(roc_obesity2_cart, col = "violet", add = TRUE)
plot(roc_obesity3_logistic, col = "purple", add = TRUE)
legend("bottomright", legend = c("Insufficient Weight", "Normal Weight", "Overweight1", "Overweight2", "Obesity1", "Obesity2", "Obesity3"),
       col = c("red", "orange", "yellow", "green", "blue", "violet", "purple"), lwd = 2)


# Random Forest ------------------------------------------------------------------------------
# build random forest using randomForest, ntree set to 500 default, mtry is sqrt of the number of predictors which is sqrt(14) round up to 4
rf_model <- randomForest(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                           high_caloric_food_frequenctly + veg_frequency + num_of_meals + 
                           eat_between_meals + smoker + water_per_day + monitor_calories + 
                           physical_activity_frequency + hrs_on_tech_device + alcohol_frequency + 
                           usual_mode_of_transport, 
                           data = trainset,
                           ntree = 500,
                           mtry = 4,
                           importance = TRUE)

rf_predictions <- predict(rf_model, newdata = testset)

# shows the confusion matrix & statistics consisting of precision, recall etc
rf_cm <- confusionMatrix(rf_predictions, testset$NObeyesdad)
rf_cm

# nicer plot for confusion matrix
ggplot(as.data.frame(rf_cm$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1, color = "black") +
  labs(title = "Confusion Matrix",
       x = "Actual Classes",
       y = "Predicted Classes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# accuracy for Random Forest
rf_accuracy <- mean(testset$NObeyesdad == rf_predictions)
rf_accuracy

# show and plot variable importance
importance(rf_model)
varImpPlot(rf_model)

# ROC curves
rf_probabilities <- predict(rf_model, newdata = testset, type = "prob")

roc_insufficient_rf <- roc(testset$NObeyesdad == "Insufficient_Weight", rf_probabilities[, "Insufficient_Weight"])
roc_normal_rf <- roc(testset$NObeyesdad == "Normal_Weight", rf_probabilities[, "Normal_Weight"])
roc_overweight1_rf <- roc(testset$NObeyesdad == "Overweight_Level_I", rf_probabilities[, "Overweight_Level_I"])
roc_overweight2_rf <- roc(testset$NObeyesdad == "Overweight_Level_II", rf_probabilities[, "Overweight_Level_II"])
roc_obesity1_rf <- roc(testset$NObeyesdad == "Obesity_Type_I", rf_probabilities[, "Obesity_Type_I"])
roc_obesity2_rf <- roc(testset$NObeyesdad == "Obesity_Type_II", rf_probabilities[, "Obesity_Type_II"])
roc_obesity3_rf <- roc(testset$NObeyesdad == "Obesity_Type_III", rf_probabilities[, "Obesity_Type_III"])

# plot the ROC curves
plot(roc_insufficient_rf, col = "red", main = "ROC Curves for Each Weight Category")
plot(roc_normal_rf, col = "orange", add = TRUE)
plot(roc_overweight1_rf, col = "yellow", add = TRUE)
plot(roc_overweight2_rf, col = "green", add = TRUE)
plot(roc_obesity1_rf, col = "blue", add = TRUE)
plot(roc_obesity2_rf, col = "violet", add = TRUE)
plot(roc_obesity3_rf, col = "purple", add = TRUE)
legend("bottomright", legend = c("Insufficient Weight", "Normal Weight", "Overweight1", "Overweight2", "Obesity1", "Obesity2", "Obesity3"),
       col = c("red", "orange", "yellow", "green", "blue", "violet", "purple"), lwd = 2)


# Comparison between models ------------------------------------------------------------------
# create a table for easy comparison
comparison_results <- data.frame(
  Model = c("Logistic Regression", "CART", "Random Forest"),
  Accuracy = c(multi_accuracy, cart_accuracy, rf_accuracy),
  AUC_Normal = c(auc(roc_normal_logistic), auc(roc_normal_cart), auc(roc_normal_rf)),
  AUC_Insufficient = c(auc(roc_insufficient_logistic), auc(roc_insufficient_cart), auc(roc_insufficient_rf)),
  AUC_Overweight1 = c(auc(roc_overweight1_logistic), auc(roc_overweight1_cart), auc(roc_overweight1_rf)),
  AUC_Overweight2 = c(auc(roc_overweight2_logistic), auc(roc_overweight2_cart), auc(roc_overweight2_rf)),
  AUC_Obesity1 = c(auc(roc_obesity1_logistic), auc(roc_obesity1_cart), auc(roc_obesity1_rf)),
  AUC_Obesity2 = c(auc(roc_obesity2_logistic), auc(roc_obesity2_cart), auc(roc_obesity2_rf)),
  AUC_Obesity3 = c(auc(roc_obesity3_logistic), auc(roc_obesity3_cart), auc(roc_obesity3_rf))
)

comparison_results


