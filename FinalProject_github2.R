##Project: ANA625
##Author: Shashi Bala

#import file
Telcom <- read.csv("/Users/jill_jewelry/Documents/MSDA/ANA625/Dataset/WA_Fn-UseC_-Telco-Customer-Churn.csv")
#show the structure
str(Telcom)

## Check missing values
str(Telcom)
is.na(Telcom) # TRUE for NA values
complete.cases(Telcom) # FALSE for rows that have some NA values
table(is.na(Telcom)) #TRUE for NA values
# there are 11 missing values

# Omit missing values
Telcom2 <- Telcom[complete.cases(Telcom), ]
Telcom2 <- na.omit(Telcom)
table(is.na(Telcom2))
str(Telcom2)
# the obs decrease from 7043 to 7032 (11 obs were deleted).

#remove id column
Telcom2 <- Telcom2[,-1]

#change SeniorCitizen from Integer to Factor
Telcom2$SeniorCitizen <- as.factor(mapvalues(Telcom2$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))
#Collapse table levels
#change "No internet service" to "No"
cols_recode1 <- c(9:14) 
library(plyr)
for(i in 1:ncol(Telcom2[,cols_recode1])) {
  Telcom2[,cols_recode1][,i] <- as.factor(mapvalues
                                        (Telcom2[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}
#change "No phone service" to "No"
Telcom2$MultipleLines <- as.factor(mapvalues(Telcom2$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
str(Telcom2)

write.csv(Telcom2, file = "/Users/jill_jewelry/Documents/MSDA/ANA625/FinalProject/Telcom2.csv", row.names=F )
View(Telcom2)
###########################
Telcom2 <- read.csv("/Users/jill_jewelry/Documents/MSDA/ANA625/FinalProject/Telcom2.csv")

#Proportion of the Indenpendent Variable
prop.table(table(Churn))

summary(Telcom2)
# In summary output, it shows min, max, median, mean, 1st and 3rd quartiles in each numeric variables. 
# and for categorical variables, it shows counts for each level in each variable.

#interquartile range
IQR(Telcom2$tenure)
IQR(Telcom2$MonthlyCharges)
IQR(Telcom2$TotalCharges)

#Mode
Mode<- function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(Telcom2$tenure)
Mode(Telcom2$MonthlyCharges)
Mode(Telcom2$TotalCharges)

#standard diavation
sd(Telcom2$tenure)
sd(Telcom2$MonthlyCharges)
sd(Telcom2$TotalCharges)

#variance
var(Telcom2$tenure)
var(Telcom2$MonthlyCharges)
var(Telcom2$TotalCharges)

#Skewness and Kurtosis
library(moments)
skewness(Telcom2$tenure)
skewness(Telcom2$MonthlyCharges)
skewness(Telcom2$TotalCharges)
kurtosis(Telcom2$tenure)
kurtosis(Telcom2$MonthlyCharges)
kurtosis(Telcom2$TotalCharges)

#Histrogram
library(DataExplorer)
plot_histogram(Telcom2)
  #or
dev.off()
par(mfrow=c(1,3)) #display 3 graph in a row
hist(Telcom2$tenure, col= "pink", border = "red", 
     main = "Histogram of length of customer relationship", 
     xlab="tenure(months)", freq = FALSE, breaks = 20)
lines(density(Telcom2$tenure), col="Blue")
curve(dnorm(x, mean=mean(Telcom2$tenure), sd=sd(Telcom2$tenure)), add=TRUE, col="red")


hist(Telcom2$MonthlyCharges, col="orange", border = "purple", 
     main = "Histogram of Monthly Charges", 
     xlab="MonthlyCharges($)", freq = FALSE, breaks = 20)
lines(density(Telcom2$MonthlyCharges), col="Blue")
curve(dnorm(x, mean=mean(Telcom2$MonthlyCharges), sd=sd(Telcom2$MonthlyCharges)), add=TRUE, col="red")


hist(Telcom2$TotalCharges, col="lightblue", border = "blue", 
     main = "Histogram of Total Charges", 
     xlab="TotalCharges($)", freq = FALSE, breaks = 20)
lines(density(Telcom2$TotalCharges), col="Blue")
curve(dnorm(x, mean=mean(Telcom2$TotalCharges), sd=sd(Telcom2$TotalCharges)), add=TRUE, col="red")

#Density estimation
par(mfrow=c(1,3))
plot(density(Telcom2$tenure), col = "blue" , 
     main = "Kernel density plot of length of customer relationship" , 
     xlab="tenure(months)")
polygon(density(Telcom2$tenure), col="blue")

plot(density(Telcom2$MonthlyCharges), col = "pink" , 
     main = "Kernel density plot of Monthly Charges",
     xlab="MonthlyCharges($)")
polygon(density(Telcom2$MonthlyCharges), col="pink")

plot(density(Telcom2$TotalCharges), col = "purple" , 
     main = "Kernel density plot of Total Charges", 
     xlab="TotalCharges($)")
polygon(density(Telcom2$TotalCharges), col="purple")

#QQ-Plot
par(mfrow=c(1,3))
qqnorm(Telcom2$tenure, col = "orange", pch =18,
       main = "QQplot of length of customer relationship", 
       xlab="tenure(months)")
qqline(Telcom2$tenure, col = "blue", lty = "longdash")

qqnorm(Telcom2$MonthlyCharges, col = "steelblue1", pch =18,
       main = "QQplot of Monthly Charges", 
       xlab="MonthlyCharges($)")
qqline(Telcom2$MonthlyCharges, col = "red", lty = "longdash")

qqnorm(Telcom2$TotalCharges, col = "mediumseagreen", pch =18,
       main = "QQplot of Total Charges", 
       xlab="TotalCharges($)")
qqline(Telcom2$TotalCharges, col = "midnightblue", lty = "longdash")

#Correlation
library(DataExplorer)
tn_month_total <- subset(Telcom2, select = c("tenure", "MonthlyCharges","TotalCharges"))
cor(tn_month_total)
plot_correlation(tn_month_total)

#Library
library(ggplot2)
library(corrplot)
library(GGally)

# set new default colour for churn:
Churn_color <- c("royalblue", "tomato")

# To use for fills, add
scale_fill_manual(values=Churn_color)

# To use for line and point colors, add
scale_colour_manual(values=Churn_color)

#Scatterplot, Density and Correlation Matrix by Churn
ggscatmat(Telcom2, columns = 1:20, color = "Churn") + scale_colour_manual(values=Churn_color)

#Scatterplot between two variables
#Telcom2$MonthlyCharges and Telcom2$tenure
Monthly_ten <- ggplot(Telcom2, aes(x = Telcom2$MonthlyCharges, y = Telcom2$tenure, color = Churn)) + 
  geom_point(size = 1, shape = 16) + geom_rug()
Monthly_ten + labs(x = "Monthly Charges ($)", y = "Tenure (No. of Months)", title = "Scatterplot of Monthly Charges and Tenure") +
  scale_colour_manual(values=Churn_color)
#Telcom2$TotalCharges and Telcom2$tenure
Total_ten <- ggplot(Telcom2, aes(x = Telcom2$TotalCharges, y = Telcom2$tenure, color = Churn)) + geom_point(size = 1, shape = 16) + geom_rug() + scale_fill_manual(values=c("royalblue", "tomato"))
Total_ten + labs(x = "Total Charges ($)", y = "Tenure (No. of Months)", title = "Scatterplot of Total Charges and Tenure")+
  scale_colour_manual(values=Churn_color)
#Telcom2$MonthlyCharges and Telcom2$TotalCharges
Monthly_tot <- ggplot(Telcom2, aes(x = Telcom2$MonthlyCharges, y = Telcom2$TotalCharges, color = Churn)) + geom_point(size = 1, shape = 16) + geom_rug() + scale_fill_manual(values=c("royalblue", "tomato"))
Monthly_tot + labs(x = "Monthly Charges ($)", y = "Total Charges ($)", title = "Scatterplot of Monthly and Total Charges")+
  scale_colour_manual(values=Churn_color)

#Box plot between two variables
#Telcom2$Churn Vs. Telcom2$MonthlyCharges
box_Churn_Month <- ggplot(Telcom2, aes(x =  Telcom2$Churn,y = Telcom2$MonthlyCharges, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_Month + labs(x = "Churn", y = "Monthly Charges ($)", title = "Box plot of Customer Churn Vs. Monthly Charges")+ scale_fill_manual(values=Churn_color)

#Telcom2$Churn Vs. Telcom2$tenure
box_Churn_ten <- ggplot(Telcom2, aes(x =  Telcom2$Churn,y = Telcom2$tenure, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_ten + labs(x = "Churn", y = "Tenure (No. of Months)", title = "Box plot of Customer Churn Vs. Tenure") + scale_fill_manual(values=Churn_color)

#Telcom2$Churn Vs. Telcom2$TotalCharges
box_Churn_total <- ggplot(Telcom2, aes(x =  Telcom2$Churn,y = Telcom2$TotalCharges, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_total + labs(x = "Churn", y = "TotalCharges ($)", title = "Box plot of Customer Churn Vs. Total Charges") +  scale_fill_manual(values=Churn_color)

#Bar chart
library(cowplot)
library(stringi)
library(stringr)
theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
plot_grid(ggplot(Telcom2, aes(x=gender,fill=Churn))+ geom_bar()+ theme1 + scale_fill_manual(values=c("royalblue", "tomato")), 
          ggplot(Telcom2, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")), 
          ncol = 4)
         
plot_grid(ggplot(Telcom2, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato")),
          ggplot(Telcom2, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+theme1+ scale_fill_manual(values=c("royalblue", "tomato"))+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),ncol = 3)

#END EDA
################################
## Importing packages
library(tidyverse) 
library(ggcorrplot)
library(nnet)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)

#Modeling
model_1 <- glm(Churn ~ ., data = Telcom2, family = "binomial")
summary(model_1)
fitted(model_1)
head(model_1)

varImp(model_1)

#ANOVA
anova(model_1)

# predicting probabilities
predicted.class <- predict(model_1, Telcom2, "response")
head(predicted.class)

# "manually" convert the probabilities to classifications
# a midpoint value such as 0.5 is used to "categorize" the probabilities
trn_pred <- ifelse(predict(model_1, type = "response") > 0.5, "Yes", "No")
head(trn_pred)

# predicting probabilities
predicted.class <- predict(model_1, Telcom2, "response")
head(predicted.class)

# "manually" convert the probabilities to classifications
# a midpoint value such as 0.5 is used to "categorize" the probabilities
trn_pred <- ifelse(predict(model_1, type = "response") > 0.5, "Yes", "No")
head(trn_pred)

# Logistic Regression Model Evaluation using a cross-table: confusion matrix (CM)
trn_tab <- table(predicted = trn_pred, actual = Telcom2$Churn)
trn_tab

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(data = as.factor(trn_pred), reference = Telcom2$Churn, positive = "No")
levels(Telcom2$Churn)


########################## Data Pre-Processing ############################
###1)Scale & Center

#The scale transform calculates the standard deviation for an attribute and divides each value by that standard deviation.
#The center transform calculates the mean for an attribute and subtracts it from each value.

# summarize data
summary(Telcom2[,1:20])

# calculate the pre-process parameters from the dataset
Telcom2_preprocess_1 <- preProcess(Telcom2[,1:20], method=c("scale", "center"))

# summarize transform parameters
print(Telcom2_preprocess_1)

# transform the dataset using the parameters
transformed.Scale_1 <- predict(Telcom2_preprocess_1, Telcom2[,1:20])

# summarize the transformed dataset
summary(transformed.Scale_1)

###2)Normalize

#Data values can be scaled into the range of [0, 1] which is called normalization.

# calculate the pre-process parameters from the dataset
preprocessParams.Norm_1 <- preProcess(Telcom2[c(5, 18:19)], method=c("range"))

# summarize transform parameters
print(preprocessParams.Norm_1)

# transform the dataset using the parameters
transformed.Norm_1 <- predict(preprocessParams.Norm_1, Telcom2[c(5, 18:19)])

# summarize the transformed dataset (note pedigree and age)
summary(transformed.Norm_1)

###3)Box-Cox Transform
#When an attribute has a Gaussian-like distribution but is shifted, this is called a skew. The distribution of an attribute can be shifted to reduce the skew and make it more Gaussian. The BoxCox transform can perform this operation (assumes all values are positive).

# load libraries
library(mlbench)
library(caret)

# summarize Tenure, Monthly Income and Total Charges
summary(Telcom2$tenure, Telcom2$MonthlyCharges, Telcom2$TotalCharges)

# calculate the pre-process parameters from the dataset
Telcom2_preprocess_3 <- preProcess(Telcom2[ ,1:20], method=c("BoxCox"))

# summarize transform parameters
print(Telcom2_preprocess_3)

# transform the dataset using the parameters
transformed.BoxCox <- predict(Telcom2_preprocess_3, Telcom2[ ,1:20])

# summarize the transformed dataset (note Tenure, monthlyCharges and totalCharges)
summary(transformed.BoxCox)

#Histrogram after doing pre-processing
library(DataExplorer)
plot_histogram(transformed.BoxCox)

#or
dev.off()
par(mfrow=c(1,3)) #display 3 graph in a row

hist(transformed.BoxCox$tenure, col= "pink", border = "red", 
     main = "Histogram of length of customer relationship", 
     xlab="tenure(months)", freq = FALSE, breaks = 20)
lines(density(transformed.BoxCox$tenure), col="Blue")
curve(dnorm(x, mean=mean(transformed.BoxCox$tenure), sd=sd(transformed.BoxCox$tenure)), add=TRUE, col="red")


hist(transformed.BoxCox$MonthlyCharges, col="orange", border = "purple", 
     main = "Histogram of Monthly Charges", 
     xlab="MonthlyCharges($)", freq = FALSE, breaks = 20)
lines(density(transformed.BoxCox$MonthlyCharges), col="Blue")
curve(dnorm(x, mean=mean(transformed.BoxCox$MonthlyCharges), sd=sd(transformed.BoxCox$MonthlyCharges)), add=TRUE, col="red")


hist(transformed.BoxCox$TotalCharges, col="lightblue", border = "blue", 
     main = "Histogram of Total Charges", 
     xlab="TotalCharges($)", freq = FALSE, breaks = 20)
lines(density(transformed.BoxCox$TotalCharges), col="Blue")
curve(dnorm(x, mean=mean(transformed.BoxCox$TotalCharges), sd=sd(transformed.BoxCox$TotalCharges)), add=TRUE, col="red")

#Box plot between two variables after doing pre-processing
# set new default colour for churn:
Churn_color <- c("royalblue", "tomato")

# To use for fills, add
scale_fill_manual(values=Churn_color)

# To use for line and point colors, add
scale_colour_manual(values=Churn_color)

#Telcom2$Churn Vs. Telcom2$MonthlyCharges
box_Churn_Month <- ggplot(transformed.BoxCox, aes(x = transformed.BoxCox$Churn,y = transformed.BoxCox$MonthlyCharges, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_Month + labs(x = "Churn", y = "Monthly Charges ($)", title = "Box plot of Customer Churn Vs. Monthly Charges")+ scale_fill_manual(values=Churn_color)

#Telcom2$Churn Vs. Telcom2$tenure
box_Churn_ten <- ggplot(transformed.BoxCox, aes(x =  transformed.BoxCox$Churn,y = transformed.BoxCox$tenure, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_ten + labs(x = "Churn", y = "Tenure (No. of Months)", title = "Box plot of Customer Churn Vs. Tenure") + scale_fill_manual(values=Churn_color)

#Telcom2$Churn Vs. Telcom2$TotalCharges
box_Churn_total <- ggplot(transformed.BoxCox, aes(x =  transformed.BoxCox$Churn,y = transformed.BoxCox$TotalCharges, fill = Churn)) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, colour = "Black")
box_Churn_total + labs(x = "Churn", y = "TotalCharges ($)", title = "Box plot of Customer Churn Vs. Total Charges") +  scale_fill_manual(values=Churn_color)

#remove total charges
transformed.BoxCox$TotalCharges <- NULL

#################  Logistic Regression Model Creation
# Split into train/test splits first.
library(caTools)
set.seed(123)
sample <- sample.split(transformed.BoxCox$Churn, SplitRatio = 0.70)
Telcom2_train <- subset(transformed.BoxCox, sample == TRUE)
Telcom2_test <- subset(transformed.BoxCox, sample == FALSE)

#Model on Original traning dataset
#Modeling_2
model_glm_2 <- glm(Churn ~ ., family = "binomial"(link = "logit"), data = Telcom2_train)
summary(model_glm_2)

#ANOVA
anova(model_glm_2, test="Chisq")

#Assessing the predictive ability of the Logistic Regression model
Telcom2_test$Churn <- as.character(Telcom2_test$Churn)
Telcom2_test$Churn[Telcom2_test$Churn=="No"] <- "0"
Telcom2_test$Churn[Telcom2_test$Churn=="Yes"] <- "1"
fitted.results <- predict(model_glm_2,newdata=Telcom2_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Telcom2_test$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(Telcom2_test$Churn, fitted.results > 0.5)

# Logistic Regression Model Prediction
# return probabilities, we must specify type = "response"
head(predict(model_glm_2, type = "response")) # these predicted values are probabliliites, not classifications

# "manually" convert the probabilities to classifications
# a midpoint value such as 0.5 is used to "categorize" the probabilities
trn_pred_2 <- ifelse(predict(model_glm_2, type = "response") > 0.5, "Yes", "No")
head(trn_pred_2)

# Logistic Regression Model Evaluation using a cross-table: confusion matrix (CM)
# Making predictions on the train set.
trn_tab_2 <- table(predicted = trn_pred_2, actual = Telcom2_train$Churn)
trn_tab_2

# Making predictions on the test set.
tst_pred_2 <- ifelse(predict(model_glm_2, newdata = Telcom2_test, type = "response") > 0.5, "Yes", "No")
tst_tab_2 <- table(predicted = tst_pred_2, actual = Telcom2_test$Churn)
tst_tab_2

# Get the values of sensitivity, specificity, prevalance, etc.
library("caret")
confusionMatrix(trn_tab_2, positive = "No")

#  ROC curve (receiver operating characteristic curve) illustrates the sensitivity and 
# specificity for all possible cutoff values
library("pROC")
test_prob <- predict(model_glm_2, newdata = Telcom2_test, type = "response")
test_roc <- roc(Telcom2_test$Churn ~ test_prob, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)

################Feature Engineering###############
#Library
library(gridExtra)
library(ggthemes)
library(randomForest)
library(party)
#Decision Tree visualization
tree <- ctree(Churn~Contract+tenure+PaperlessBilling, Telcom2_train)
plot(tree)

#plotting Decision Trees, they are "Contract", "tenure_group" and "PaperlessBilling".
pred_tree <- predict(tree, Telcom2_test)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = Telcom2_test$Churn)

#Random Forest
#Random Forest Initial Model
rfModel <- randomForest(Churn ~., data = Telcom2_train)
print(rfModel)

