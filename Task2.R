
#Task 2
strength_train<-imputed_data[imputed_data$isTrain=="yes",]
strength_test<-imputed_data[imputed_data$isTrain=="no", ]

#remove variable isTrainfrom both train and test
strength_train$isTrain<-NULL
strength_test$isTrain<-NULL


#installing the necessary packages
install.packages("caret")
install.packages("rattle")
install.packages("randomForest")
install.packages("Metrics")
install.packages("Metrics", dependencies = TRUE)
install.packages("kernlab")
install.packages("gbm")

#loading the necessary packages
library(caret)
library(rattle)
library(randomForest)
library(Metrics)
library(kernlab)
library(gbm)


########################
#decision tree algorithm
########################

fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

# Creating the model
dt_model <- train(Strength ~ .,
                  data = strength_train, # Data
                  method = 'rpart', 
                  trControl = fitControl) 


#importance variable
# Creating the object of importance of our variables 
dt_importance <- varImp(dt_model)

# Creating plot of importance of variables
ggplot(data = dt_importance, mapping = aes(x = dt_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Decision tree model") + 
  theme_light() 

#plotting decision tree
fancyRpartPlot(dt_model$finalModel, sub = '')


# Prediction and Evaluation
predictions <- predict(dt_model, newdata = strength_test)
mae <- MAE(predictions, strength_test$Strength)
mse <- mean((predictions - strength_test$Strength)^2)
rmse <- RMSE(predictions, strength_test$Strength)
r_squared <- R2(predictions, strength_test$Strength)


#number of observations and predictors
n <- nrow(strength_test)
p <- length(names(dt_model$finalModel$trainingData))
adjusted_r_squared <- 1 -((1-r_squared) * (n - 1)/(n - p - 1))

print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("R-squared:", r_squared))
print(paste("Adjusted R-squared:", adjusted_r_squared))




# Generate predictions and residuals
predictions <- predict(dt_model, newdata = strength_test)
residuals <- predictions - strength_test$Strength
residual_plot <- ggplot(data = data.frame(Actual = strength_test$Strength, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Actual Strength", y = "Residuals")

fitted_vs_original_plot <- ggplot(data = data.frame(Original = strength_test$Strength, Fitted = predictions), aes(x = Original, y = Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Original Plot", x = "Original Strength", y = "Fitted Strength")

# Arrange plots into one grid
grid.arrange(residual_plot, fitted_vs_original_plot, ncol = 2)


####################
#Random Forest 
####################

fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

rf_model <- train(
  Strength ~ ., 
  method = 'rf',  
  trControl = fitControl,  
  data = strength_train)  

rf_importance <- varImp(rf_model) 

# Create box plot of importance of variables
ggplot(data = rf_importance, mapping = aes(x = rf_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Random forest model") + 
  theme_light() # Theme




# Prediction and Evaluation
predictions <- predict(rf_model, newdata = strength_test)
mae_rf <- MAE(predictions, strength_test$Strength)
mse_rf <- mean((predictions - strength_test$Strength)^2)
rmse_rf <- RMSE(predictions, strength_test$Strength)
r_squared_rf <- R2(predictions, strength_test$Strength)


#number of observations and predictors
n <- nrow(strength_test)
p <- length(names(rf_model$finalModel$trainingData))
adjusted_r_squared_rf <- 1 -((1-r_squared_rf) * (n - 1)/(n - p - 1))

print(paste("Mean Absolute Error (MAE):", mae_rf))
print(paste("Mean Squared Error (MSE):", mse_rf))
print(paste("Root Mean Squared Error (RMSE):", rmse_rf))
print(paste("R-squared:", r_squared_rf))
print(paste("Adjusted R-squared:", adjusted_r_squared_rf))


# Generate predictions and residuals
predictions <- predict(rf_model, newdata = strength_test)
residuals <- predictions - strength_test$Strength
residual_plot <- ggplot(data = data.frame(Actual = strength_test$Strength, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Actual Strength", y = "Residuals")

fitted_vs_original_plot <- ggplot(data = data.frame(Original = strength_test$Strength, Fitted = predictions), aes(x = Original, y = Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Original Plot", x = "Original Strength", y = "Fitted Strength")

# Arrange plots into one grid
grid.arrange(residual_plot, fitted_vs_original_plot, ncol = 2)



#####################
#Support Vector Machine
#####################

fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

svm_model <- train(
  Strength ~ .,  
  method = 'svmLinear',  
  trControl = fitControl,  
  data = strength_train) 

svm_importance <- varImp(svm_model) 

# Create box plot of importance of variables
ggplot(data = svm_importance, mapping = aes(x = svm_importance[,1])) 
geom_boxplot() + 
  labs(title = "Variable importance: Support Vector Machine") + 
  theme_light()




# Prediction and Evaluation
predictions <- predict(svm_model, newdata = strength_test)
mae_svm <- MAE(predictions, strength_test$Strength)
mse_svm <- mean((predictions - strength_test$Strength)^2)
rmse_svm <- RMSE(predictions, strength_test$Strength)
r_squared_svm <- R2(predictions, strength_test$Strength)


#number of observations and predictors
n <- nrow(strength_test)
p <- ncol(strength_test) -1
adjusted_r_squared_svm <- 1 -((1-r_squared_svm) * (n - 1)/(n - p - 1))

print(paste("Mean Absolute Error (MAE):", mae_svm))
print(paste("Mean Squared Error (MSE):", mse_svm))
print(paste("Root Mean Squared Error (RMSE):", rmse_svm))
print(paste("R-squared:", r_squared_svm))
print(paste("Adjusted R-squared:", adjusted_r_squared_svm))


# Generate predictions and residuals
predictions <- predict(svm_model, newdata = strength_test)
residuals <- predictions - strength_test$Strength

residual_plot <- ggplot(data = data.frame(Actual = strength_test$Strength, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Actual Strength", y = "Residuals")

fitted_vs_original_plot <- ggplot(data = data.frame(Original = strength_test$Strength, Fitted = predictions), aes(x = Original, y = Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Original Plot", x = "Original Strength", y = "Fitted Strength")

# Arrange plots into one grid
grid.arrange(residual_plot, fitted_vs_original_plot, ncol = 2)


##################
#Linear Regression
##################
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

lm_model <- train(
  Strength ~ .,  
  method = 'lm', 
  trControl = fitControl,  
  data = strength_train)   

lm_importance <- varImp(lm_model) 

# Create box plot of importance of variables
ggplot(data = lm_importance, mapping = aes(x = lm_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: Linear Regression") + 
  theme_light() 


# Prediction and Evaluation
predictions <- predict(lm_model, newdata = strength_test)
mae_lm <- MAE(predictions, strength_test$Strength)
mse_lm <- mean((predictions - strength_test$Strength)^2)
rmse_lm <- RMSE(predictions, strength_test$Strength)
r_squared_lm <- R2(predictions, strength_test$Strength)


#number of observations and predictors
n <- nrow(strength_test)
p <- length(names(lm_model$finalModel$trainingData))
adjusted_r_squared_lm <- 1 -((1-r_squared_lm) * (n - 1)/(n - p - 1))

print(paste("Mean Absolute Error (MAE):", mae_lm))
print(paste("Mean Squared Error (MSE):", mse_lm))
print(paste("Root Mean Squared Error (RMSE):", rmse_lm))
print(paste("R-squared:", r_squared_lm))
print(paste("Adjusted R-squared:", adjusted_r_squared_lm))


# Generate predictions and residuals
predictions <- predict(lm_model, newdata = strength_test)
residuals <- predictions - strength_test$Strength

residual_plot <- ggplot(data = data.frame(Actual = strength_test$Strength, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Actual Strength", y = "Residuals")

fitted_vs_original_plot <- ggplot(data = data.frame(Original = strength_test$Strength, Fitted = predictions), aes(x = Original, y = Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Original Plot", x = "Original Strength", y = "Fitted Strength")

# Arrange plots into one grid
grid.arrange(residual_plot, fitted_vs_original_plot, ncol = 2)



###########################
#gradient boosting machine
###########################
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

gbm_model <- train(
  Strength ~ .,  
  method = 'gbm',  
  trControl = fitControl,  
  data = strength_train)   

gbm_importance <- varImp(gbm_model) 

# Create box plot of importance of variables
ggplot(data = gbm_importance, mapping = aes(x = gbm_importance[,1])) +
  geom_boxplot() + 
  labs(title = "Variable importance: Gradient Boosting Machines") + 
  theme_light() 




# Prediction and Evaluation
predictions <- predict(gbm_model, newdata = strength_test)
mae_gbm <- MAE(predictions, strength_test$Strength)
mse_gbm <- mean((predictions - strength_test$Strength)^2)
rmse_gbm <- RMSE(predictions, strength_test$Strength)
r_squared_gbm <- R2(predictions, strength_test$Strength)


#number of observations and predictors
n <- nrow(strength_test)
p <- length(names(gbm_model$finalModel$trainingData))
adjusted_r_squared_gbm <- 1 -((1-r_squared_gbm) * (n - 1)/(n - p - 1))

print(paste("Mean Absolute Error (MAE):", mae_gbm))
print(paste("Mean Squared Error (MSE):", mse_gbm))
print(paste("Root Mean Squared Error (RMSE):", rmse_gbm))
print(paste("R-squared:", r_squared_gbm))
print(paste("Adjusted R-squared:", adjusted_r_squared_gbm))


# Generate predictions and residuals
predictions <- predict(gbm_model, newdata = strength_test)
residuals <- predictions - strength_test$Strength

residual_plot <- ggplot(data = data.frame(Actual = strength_test$Strength, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Actual Strength", y = "Residuals")

fitted_vs_original_plot <- ggplot(data = data.frame(Original = strength_test$Strength, Fitted = predictions), aes(x = Original, y = Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Original Plot", x = "Original Strength", y = "Fitted Strength")

# Arrange plots into one grid
grid.arrange(residual_plot, fitted_vs_original_plot, ncol = 2)


#showing the result in table
evaluation_metrics<-matrix(NA,nrow=6,ncol=6)
evaluation_metrics[1,]<-c("Decision Tree",r_squared,adjusted_r_squared,mse,rmse,mae)
evaluation_metrics[2,]<-c("Random Forest",r_squared_rf,adjusted_r_squared_rf,mse_rf,rmse_rf,mae_rf)
evaluation_metrics[3,]<-c("Svm",r_squared_svm,adjusted_r_squared_svm,mse_svm,rmse_svm,mae_svm)
evaluation_metrics[4,]<-c("Linear Regression",r_squared_lm,adjusted_r_squared_lm,mse_lm,rmse_lm,mae_lm)
evaluation_metrics[5,]<-c("Gradient Boosting",r_squared_gbm,adjusted_r_squared_gbm,mse_gbm,rmse_gbm,mae_gbm)

evaluation_metrics_df <- as.data.frame(evaluation_metrics)
colnames(evaluation_metrics_df) <- c("Model","R-squared","Adjusted-R-squared","MSE","RMSE","MAE")

print(evaluation_metrics_df)


