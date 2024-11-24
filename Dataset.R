#installing the required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("mice")
install.packages("corrplot")
install.packages("gridExtra")

#loading the required packages

library(ggplot2)
library(dplyr)
library(mice)
library(corrplot)
library(gridExtra)


#loading the given datasets
strength_test <-read.csv("C:\\Users\\user\\Documents\\R L5\\ReportAssignment\\concrete_strength_test.csv")

strength_train <- read.csv("C:\\Users\\user\\Documents\\R L5\\ReportAssignment\\concrete_strength_train.csv")

#Structure of the given datasets
str(strength_train)
str(strength_test)


strength_train$isTrain<-"yes"
strength_test$isTrain<-"no"

#combine datasets
combined<- rbind(strength_train, strength_test)

#dimension
cat("The combined dataset has",nrow(combined), "rows and ",ncol(combined),"columns")


#summary of the combined dataset
summary(combined)



#EDA

#cement water ratio

data_ratio <- combined$Cement / combined$Water

ggplot(combined, aes(x = data_ratio)) +
  geom_histogram(binwidth=0.1, fill="grey", color="black") +
  labs(title = "Distribution of cement to water ratio",
       x = "Cement to water ratio",
       y = "Frequency")


#cement water ratio to strength
ggplot(combined, aes(x = data_ratio, y = Strength)) +
  geom_point(aes(color = "Data Points"), alpha = 0.5, size = 2) +  
  geom_smooth(aes(color = "Smooth Line"), method = "loess", se = FALSE) +  
  labs(title = "Ratio of Cement to Water vs. Strength",
       x = "Cement to Water Ratio",
       y = "Strength",
       color = "Legend") +
  scale_color_manual(values = c("black", "blue"), labels = c("Data Points", "Smooth Line")) + 
  theme(legend.position = "bottom")

#ratio between coarse and fine aggregate
aggregate_ratio = combined$Coarse.Aggregate / combined$Fine.Aggregate




ggplot() +
  geom_histogram(aes(x = aggregate_ratio), binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "Distribution of Coarse to Fine Aggregate Ratio",
       x = "Coarse to Fine Aggregate Ratio",
       y = "Frequency")


#correlation between aggregates and strength 
correlation_aggregate <- cor(aggregate_ratio, combined$Strength)
print(correlation_aggregate)



#relation between coarse-fine aggregate and water-cement ratio
coarse_fine_ratio = combined$Coarse.Aggregate / combined$Fine.Aggregate
water_cement_ratio = combined$Water / combined$Cement



ggplot(combined, aes(x = coarse_fine_ratio, y = water_cement_ratio, group = 1)) +
  geom_line(color = "blue") + 
  geom_point(color = "red", size = 2) +  
  labs(title = "Coarse-Fine Ratio vs. Water-Cement Ratio",
       x = "Coarse-Fine Ratio",
       y = "Water-Cement Ratio") +
  theme_minimal()  

#data pre processing

#calculating the missing percentage in the dataset in each column and overall
column_missing_percentage <- sapply(combined, function(x) mean(is.na(x))) * 100
overall_missing_percentage <- mean(column_missing_percentage)
print(column_missing_percentage)
cat(paste("Overall average percentage of missing values:", overall_missing_percentage, "%"))

#data imputing
impute_data <- mice(combined)
imputed_data <- complete(impute_data)


#outlier identification

boxplot_list <- lapply(imputed_data[, sapply(imputed_data, is.numeric)], boxplot.stats)

# Identifying the outliers based on boxplot statistics
outliers <- lapply(boxplot_list, function(x) x$out)

# Printing the outliers for each variable in the dataset
for (i in seq_along(outliers)) {
  if (length(outliers[[i]]) > 0) {
    cat("Outliers in", names(outliers)[i], ":\n")
    print(outliers[[i]])
  }
}
boxplot(outliers)

#correlation 

# Calculating the  correlation matrix
cor_matrix <- cor((imputed_data[, sapply(imputed_data, is.numeric)]), use = "complete.obs")
print(cor_matrix)

# Visualizing the correlation matrix
corrplot(cor_matrix, method = "circle")

ggplot(imputed_data, aes(Strength, Cement)) +
  geom_jitter(aes(col = Superplasticizer, size = Age)) + 
  labs(title = "Highest Correlated Variables on Strength", x= "Strength", y= 'Cement')+
  theme(plot.title = element_text(hjust = 0.5))


#investigating  variables
 
# Calculating variance for each numeric variable
variances <- sapply(imputed_data, var, na.rm = TRUE)
low_variance_vars <- names(variances[variances < 0.01])
# Removing low variance variables
imputed_data <- imputed_data[, !names(imputed_data) %in% low_variance_vars]



#scaling data
numeric_data <- imputed_data[sapply(imputed_data, is.numeric)]
scaled_data <- as.data.frame(scale(numeric_data))

#original data
p1 <- ggplot(data = numeric_data, aes_string(x = "Strength")) +
  geom_density(fill="skyblue", color = "black") + 
  ggtitle("Density Curve for Original Strength")
print(p1)

#scaled data
p2 <- ggplot(data = scaled_data, aes_string(x = "Strength")) +
  geom_density( fill = "darkblue", color = "black") +
  ggtitle(paste("Density Curve for Scaled Strength"))
print(p2)
grid.arrange(p1, p2, ncol = 2)



