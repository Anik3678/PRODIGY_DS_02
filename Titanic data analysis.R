#Prodigy Infotech DS Task-2

#Perform data cleaning & EDA on titanic dataset
#Explore the relationship between the variables & identify patterns and trend in data

#Load the data
Titanic_data<-read.csv("C:\\Users\\User\\Documents\\\\Data Science\\R for data science\\tested.csv")
Titanic_data

#Viewing the dataset
View(Titanic_data)

#Check for missing values
any(is.na(Titanic_data))
sum(is.na(Titanic_data))

#Remove the missing value
new_Titanic_data<-na.omit(Titanic_data)
new_Titanic_data

#Check again for any missing values
any(is.na(new_Titanic_data))
sum(is.na(new_Titanic_data))

#Check for duplicated values
any(duplicated(new_Titanic_data))

#Viewing the cleaned titanic dataset
View(new_Titanic_data)

#Viewing first 6 rows of the titanic dataet
head(new_Titanic_data)
View(head(new_Titanic_data))

#Viewing last 6 rows of the titanic dataet
tail(new_Titanic_data)
View(tail(new_Titanic_data))

#Plot the survival of gender
library(ggplot2)
ggplot(new_Titanic_data, aes(x = Sex, fill = factor(Survived))) + geom_bar(position = "dodge") + xlab("gender")+
  labs(title = "Survival by gender")


#Create the histogram
ggplot(new_Titanic_data, aes(x = Age)) +
  geom_histogram(bins = 30, aes(fill = ..density..), alpha = 0.5) +  # Adjust bins for better fit
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_bw()  # Set theme for a cleaner look


#Create scatterplot
ggplot(new_Titanic_data, aes(x = Age, y = Fare, color = as.factor(Age))) +
  geom_point() +  # Scatter points
  labs(title = "Scatter Plot of Age and Fare", 
       x = "Age", 
       y = "Fare", 
       color = "Age") +  # Legend title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "right")  # Position legend on the right

#Here we use glm as the response is binary

#Fit the data
plot(~Pclass + Sex + Age + SibSp + Parch + Fare,data=new_Titanic_data)
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, family = "binomial", data = new_Titanic_data)
model

#Summary of model
summary(model)

#Predicted values
predictions <- ifelse(predict(model) > 0.5, 1, 0)
predictions

observed<-new_Titanic_data$Survived
observed

#Accuracy of the model
accuracy<-mean(predictions==observed)
accuracy

#Create confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = new_Titanic_data$Survived)
conf_matrix
accuracy<-sum(diag(conf_matrix))/sum(conf_matrix)
accuracy

#Fitted value
Fitted_values<-fitted.values(model)
head(Fitted_values)
tail(Fitted_values)

head(observed)
tail(observed)

#Create a comparison table between fitted value & original value , error , residual

comparison_table <- data.frame(
  Actual = new_Titanic_data$Survived ,
  Fitted = Fitted_values,
  Error = new_Titanic_data$Survived - Fitted_values,
  Residual = resid(model)
)
comparison_table


residual<-resid(model)
Shapiro_test<-shapiro.test(residual)
p_value<-Shapiro_test$p.value

if(p_value<0.05)
{
  print("The data isnot normal")
}else
{
  print("The data is normal")
}

