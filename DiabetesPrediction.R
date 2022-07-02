#install required packages
install.packages("caret")    # this is used for data partitioning and model building
install.packages("/Users/greeshmakamal/Downloads/stringi",repos = NULL, type="source")
install.packages("stringi", dependencies = TRUE) 
library('caret') 
install.packages("pROC")  # this is for plotting roc curve
library('pROC')
#to add more than one plot at same time
library(patchwork)
install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library("ggplot2")

#Reading the dataset
diabetes <- read.csv("/Users/greeshmakamal/Documents/Sem1/Data Science Foundations/ICA/diabetes-dataset.csv",
                     sep = ",", header = TRUE)
#to check structure of dataset
str(diabetes)

# Factorization
diabetes$Outcome <- as.factor(diabetes$Outcome)

#for plotting the dataset
plot(diabetes)

#to check for any null values in dataset
anyNA(diabetes)
colSums(is.na(diabetes))

#for checking summary of dataset
summary(diabetes)

#Correlation Plot
numeric.var <- sapply(diabetes, is.numeric)
corr.matrix <- cor(diabetes[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", 
         order = "hclust", tl.col = "black", tl.srt=60, tl.cex=0.5, cl.cex=0.5)

#Univariate Analysis
plot(density(diabetes$Glucose)) 
plot(density(diabetes$Pregnancies)) 
plot(density(diabetes$BloodPressure)) 
plot(density(diabetes$SkinThickness)) 
plot(density(diabetes$Insulin)) 
plot(density(diabetes$BMI)) 
plot(density(diabetes$DiabetesPedigreeFunction)) 
plot(density(diabetes$Age))

#BiVariate Analysis

ggplot(diabetes, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")

ggplot(diabetes, aes(SkinThickness, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="SkinThickness Distribution by Outcome")

ggplot(diabetes, aes(BloodPressure, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="BloodPressure Distribution by Outcome")

ggplot(diabetes, aes(Insulin, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Insulin Distribution by Outcome")

ggplot(diabetes, aes(BMI, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="BMI Distribution by Outcome")

ggplot(diabetes, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

ggplot(diabetes, aes(DiabetesPedigreeFunction, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="DiabetesPedigreeFunction Distribution by Outcome")



#Class Distribution of Outcome
ggplot(data = diabetes) +
  geom_bar(mapping = aes(x = Outcome))

#Bivariate Analysis 
DiabetesPlot = ggplot(diabetes, aes(x = Outcome, y = Glucose)) +  geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes, aes(x=Outcome, y = BloodPressure)) + geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes, aes(x=Outcome, y = SkinThickness)) + geom_boxplot(aes(fill=Outcome))+
  
  ggplot(diabetes, aes(x=Outcome, y = Insulin)) + geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes, aes(x=Outcome, y = BMI)) +geom_boxplot(aes(fill=Outcome))  +
  
  ggplot(diabetes, aes(x=Outcome, y = DiabetesPedigreeFunction)) +geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes, aes(x=Outcome, y = Age)) +geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes, aes(x=Outcome, y = Pregnancies)) + geom_boxplot(aes(fill=Outcome))

DiabetesPlot

#InterquartileRange IQR function to find Interquartile range
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

#function to remove outliers
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

#removing outliers from the dataset
diabetes_outliers_removal <- remove_outliers(diabetes, c('Glucose', 'BloodPressure', 'SkinThickness','Insulin',
                                                 'BMI','DiabetesPedigreeFunction','Age','Pregnancies'))
#verifying dimension of dataset after removing outliers
dim(diabetes_outliers_removal)

#Bivariate Analysis post removing outliers
DiabetesPlot_without_outliers = ggplot(diabetes_outliers_removal, aes(x = Outcome, y = Glucose)) +  geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = BloodPressure)) + geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = SkinThickness)) + geom_boxplot(aes(fill=Outcome))+
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = Insulin)) + geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = BMI)) +geom_boxplot(aes(fill=Outcome))  +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = DiabetesPedigreeFunction)) +geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = Age)) +geom_boxplot(aes(fill=Outcome)) +
  
  ggplot(diabetes_outliers_removal, aes(x=Outcome, y = Pregnancies)) + geom_boxplot(aes(fill=Outcome))

DiabetesPlot_without_outliers

#summary of dataset post removing outliers
summary(diabetes_outliers_removal)

#splitting the data based on target variable

#holds instances where outcome is 1
diabetes_true <- diabetes_outliers_removal[(diabetes_outliers_removal$Outcome == 1), ]   

#holds instances where outcome is 0
diabetes_false <- diabetes_outliers_removal[(diabetes_outliers_removal$Outcome == 0), ]  

#checking the summary of the partitoned attributes
summary(diabetes_true)   
summary(diabetes_false)  

#UnderSampling the data for biasing the outcome
diabetes_false <-
  diabetes_false[sample(nrow(diabetes_false),765, replace = FALSE, prob = NULL),]

#Combining the dataframes, diabetes_true,diabetes_false
diabetes_final <- rbind(diabetes_true,diabetes_false)

#verifying the summary of the final dataset
summary(diabetes_final)

#Class Distribution of outcome post data pre-processing
ggplot(data = diabetes_final) +
  geom_bar(mapping = aes(x = Outcome))

#partitioning dataset to training(70%) and testing(30%)
set.seed(1)
intrain <- createDataPartition(y = diabetes_outliers_removal$Outcome,p = 0.70,list = FALSE)

#Assiging the 70% data for training
training <- diabetes_outliers_removal[intrain,]

#Remaining 30% is assigned for attribute, testing
testing <- diabetes_outliers_removal[-intrain,] 

#verifying the dimensions of training and testing
dim(training)
dim(testing)

#create objects x and y to holds the predictor and response variables 
x = training[,-9]
y = training$Outcome

#Training the model using method, nb and crossvalidation method in traincontrol function
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
print(model)
plot(model)

#to know the input variables impact on output prediction
X <- varImp(model)
plot(X)

#Model Evaluation before tuning
Predict <- predict(model,newdata = testing )
Predict

#Confusion Matrix
conf_matrix_naive <- table(Predict,  factor(testing$Outcome,ordered = TRUE))
confusionMatrix(conf_matrix_naive)

#Model Tuning
grid <- data.frame(fL=c(0.25,0.5,0.75,1.0), usekernel = TRUE, adjust=c(0.25,0.5,0.75,1.0))
model_tune = train(x,y,'nb',tuneGrid=grid, trControl=trainControl(method='cv',number=10))

#Testing with tuned model
Predict_tuned <- predict(model_tune,newdata = testing)
Predict_tuned
print(model_tune)
plot(model_tune)

#Model Evaluation post model tuning
conf_matrix_naive <- table(Predict_tuned,  factor(testing$Outcome,ordered = TRUE))
confusionMatrix(conf_matrix_naive)

#ROC Curve
rocCurve=roc(response=Predict_tuned, predictor= factor(testing$Outcome, 
                                                        ordered = TRUE), plot=TRUE)

plot(rocCurve, print.auc=TRUE,col="green",lwd =3,legacy.axes=TRUE)

#AUC
auc_qda<-auc(rocCurve)




