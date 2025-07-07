data=read.csv(file.choose(),header = TRUE)
data
head(data)
install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)

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
summary(dada)

#load necessary library 
library(dplyr)
quartiles=quantile(age,probs = c(0,0.25,0.5,0.75,1),na.rm = T)
quartiles
Age_group=cut(data$age,breaks = quartiles,include.lowest = T,labels = 
                c("40-52","53-64","65-73","74-95"))
Age_group
print(data)
# Create a survival object 
surv_obj = Surv(time, DEATH_EVENT)
surv_obj




# 1) Fit survival curves for anemia vs. No_anemia
fit_ane = survfit(surv_obj ~ anemia, data = data)
fit_ane

# Plot the survival curves
plot(
  fit_ane ,
  col = c("green", "red"), 
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability",
  main = "Survival Curves: anemia vs No_anemia "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" No_anemia", "Yes_anemia"),
  col = c("green", "red"),
  lty = 1:2
)




# Perform the log-rank test for anemia
logrank_ane = survdiff(surv_obj ~ anemia, data = data)
logrank_ane

####################################################################


# 2) Fit survival curves for smokers vs. Non_smokers
fit_smo = survfit(surv_obj ~ smoking, data = data)
fit_smo

# Plot the survival curves
plot(
  fit_smo ,
  col = c("green", "red"), 
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability",
  main = "Survival Curves: Smoker vs Non_smoker "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" No_smoker", "Yes_smoker"),
  col = c("green", "red"),
  lty = 1:2
)




# Perform the log-rank test for smoking
logrank_smo = survdiff(surv_obj ~ smoking, data = data)
logrank_smo



###################################################################

# 3) Fit survival curves for high_blood_pressure vs. Normal_high_blood_pressure
fit_HBP = survfit(surv_obj ~ high_blood_pressure, data = data)
fit_HBP

# Plot the survival curves
plot(
  fit_HBP ,
  col = c("green", "red"), # Colors for the groups
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability",
  main = "Survival Curves: Normal_high_blood_pressure vs high_blood_pressure "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" Normal_high_blood_pressure", "high_blood_pressure"),
  col = c("green", "red"),
  lty = 1:2
)




# Perform the log-rank test for high_blood_pressure
logrank_HBP = survdiff(surv_obj ~ high_blood_pressure, data = data)
logrank_HBP



####################################################################


# 4) Fit survival curves for Male vs. Female
fit_sex = survfit(surv_obj ~ sex, data = data)
fit_sex

# Plot the survival curves
plot(
  fit_sex ,
  col = c("green", "red"), # Colors for the groups
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability",
  main = "Survival Curves: Female vs male "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" Female", "Male"),
  col = c("green", "red"),
  lty = 1:2
)


# Perform the log-rank test for Diabetes
logrank_sex = survdiff(surv_obj ~ sex, data = data)
logrank_sex

###################################################################

# 5) Fit survival curves for 
fit_Age_group = survfit(surv_obj ~ Age_group, data = data)
fit_Age_group

# Plot the survival curves Age_group
plot(
  fit_Age_group ,
  col = c("green", "blue","black","red"), # Colors for the groups
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability",
  main = "Survival Curves: Age_group "
)

# Add a legend
legend(
  "bottomleft",
  legend = c("40-52 ", "53-64","65-73","74-95"),
  col = c("green","blue","black","red" ),
  lty = 1:2
)




# Perform the log-rank test for Age_group
logrank_Age_group = survdiff(surv_obj ~ Age_group, data = data)
logrank_Age_group



##############



##################################################
# Test proportional hazards assumption
cox_model = coxph(surv_obj ~ Age_group+ diabetes+platelets + sex+high_blood_pressure+creatinine+sodium+smoking+anaemia+creatinine_phosphokinase+ejection_fraction, data = data)
cox_model
summary(cox_model)

test_ph=cox.zph(cox_model)  ##### # Schoenfeld residuals test
test_ph


cox_model1=coxph(surv_obj ~ Age_group + high_blood_pressure + creatinine + 
                   sodium + anaemia + creatinine_phosphokinase + ejection_fraction, 
                 data = data)
cox_model1
summary(cox_model1)


#odds ratio
coef=cox_model1$coefficients
coef
odds=exp(coef)
odds

# to check the model accuracy 
install.packages("caret")
library(caret)
predicted_value=predict(cox_model1,type = "response")
predicted_value
predicted_prob=ifelse(predicted_value>0.5,1,0)
predicted_prob
conf_matrix=table(predicted=predicted_prob,Actual=surv_obj)
conf_matrix
accuracy=sum(diag(conf_matrix))/sum(conf_matrix)
accuracy




