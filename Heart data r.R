
data=read.csv(file.choose(),header = TRUE)
data
head(data)
summary(data)
str(data)

age=data$age
age
anemia=data$anaemia
anemia
creatinine_phosphokinase=data$creatinine_phosphokinase
creatinine_phosphokinase
diabetes=data$diabetes
diabetes
ejection_fraction=data$ejection_fraction
ejection_fraction
high_blood_pressure=data$high_blood_pressure
high_blood_pressure
platelets=data$platelets
platelets
creatinine=data$serum_creatinine
creatinine
sodium=data$serum_sodium
sodium
sex=data$sex
sex
smoking=data$smoking
smoking
time=data$time
time
DEATH_EVENT=data$DEATH_EVENT
DEATH_EVENT





?data.frame
#correlation
df=data.frame(c("age","CPK","EF","platelets","creatinine","sodium"),data)
df
selected=df[,c("age","CPK","EF","platelets","creatinine","sodium")]
selected
COR_M=cor(selected,method = "spearman")
COR_M
library(corrplot)
corrplot(COR_M,method = "circle",type = "upper",t1.col="black",t1.srt=45,addCoef.col = "black",diag=F)

##########################################################
######################################################3
##non parametric test of data
# To check association of SMOING AND DEATH_EVENT 
# Convert relevant variables to factors
data$smoking = as.factor(data$smoking)
data$smoking
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT

# Create a contingency table for Smoking and Death Event
contingency_table= table(data$smoking, data$DEATH_EVENT)
contingency_table
# Perform the Chi-Square test
chi_square_test = chisq.test(contingency_table)
chi_square_test

####################################################################333

# To check association of anaemia AND DEATH_EVENT 
# Convert relevant variables to factors
data$anaemia = as.factor(data$anaemia)
data$anaemia
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT
# Create a contingency table for Anaemia and Death Event
contingency_table= table(data$anaemia,data$DEATH_EVENT)
contingency_table
# Perform the Chi-Square test
chi_square_test = chisq.test(contingency_table)
chi_square_test

########################################################################

# To check association of diabetes AND DEATH_EVENT 
# Convert relevant variables to factors
data$diabetes = as.factor(data$diabetes)
data$diabetes
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT
# Create a contingency table for diabetes and Death Event
contingency_table= table(data$diabetes,data$DEATH_EVENT)
contingency_table
# Perform the Chi-Square test
chi_square_test = chisq.test(contingency_table)
chi_square_test

##########################################################


# To check association of sex AND DEATH_EVENT 
# Convert relevant variables to factors
data$sex = as.factor(data$sex)
data$sex
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT
# Create a contingency table for sex and Death Event
contingency_table= table(data$sex,data$DEATH_EVENT)
contingency_table
# Perform the Chi-Square test
chi_square_test = chisq.test(contingency_table)
chi_square_test

###########################################################################

##########################################################


# To check association of HBP AND DEATH_EVENT 
# Convert relevant variables to factors
data$high_blood_pressure = as.factor(data$high_blood_pressure)
data$high_blood_pressure
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT
# Create a contingency table for sex and Death Event
contingency_table1= table(data$high_blood_pressure,data$DEATH_EVENT)
contingency_table1
# Perform the Chi-Square test
chi_square_test1 = chisq.test(contingency_table1)
chi_square_test1

###########################################################################
#load necessary library 
library(dplyr)
quartiles=quantile(age,probs = c(0,0.25,0.5,0.75,1),na.rm = T)
quartiles
Age_group=cut(data$age,breaks = quartiles,include.lowest = T,labels = 
                c("40-52","53-64","65-73","74-95"))
Age_group
print(data)
##########################################################


# To check association of AGE GROUP AND DEATH_EVENT 
# Convert relevant variables to factors
data$Age_group = as.factor(Age_group)
data$Age_group
data$DEATH_EVENT = as.factor(data$DEATH_EVENT)
data$DEATH_EVENT
# Create a contingency table for Age_group and Death Event
contingency_table1= table(data$Age_group,data$DEATH_EVENT)
contingency_table1
# Perform the Chi-Square test
chi_square_test1 = chisq.test(contingency_table1)
chi_square_test1

###########################################################################

summary(platelets)
summary(creatinine)
summary(sodium)
summary(time)

# Load necessary libraries
library(dplyr)       # For data manipulation
library(caret)       # For model training and evaluation
library(pROC)        # For ROC curve and AUC


##plit data into training/testing sets.
DEATH_EVENT=as.factor(data$DEATH_EVENT)
DEATH_EVENT
# Split data into training (80%) and testing (20%)
set.seed(123)
trainIndeX=createDataPartition(DEATH_EVENT, p = 0.8, list = FALSE)
trainIndeX
TrainData=data[trainIndeX, ]
TrainData
TestData=data[-trainIndeX, ]
TestData
TrainData$time=NULL

##logistic reggretion model

model=glm(TrainData$DEATH_EVENT~.,data=TrainData,family = binomial(link = "logit"))
model
summary(model)

model2=glm(TrainData$DEATH_EVENT ~ age  + 
             ejection_fraction + high_blood_pressure + serum_creatinine, 
           family = binomial(link = "logit"), data = TrainData)
model2
summary(model2)
#odds ratio
coef=model2$coefficients
coef
odds=exp(coef)
odds

# Predictions on test set
TestData$Predicted_Prob = predict(model, newdata = TestData, type = "response")
TestData$Predicted_Prob
TestData$Predicted_Class = ifelse(TestData$Predicted_Prob > 0.5, 1, 0)

conf_matrix=table(Predicted=TestData$Predicted_Class,Actual=TestData$DEATH_EVENT)
conf_matrix

accuracy=sum(diag(conf_matrix))/sum(conf_matrix)
accuracy


# ROC Curve and AUC
roc_curve = roc(TestData$DEATH_EVENT, TestData$Predicted_Prob)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value = auc(roc_curve)
auc_value



