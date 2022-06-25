#---------------------------------------------
#           Prerequisite packages            #
#---------------------------------------------
install.packages("pROC", repos = "http://cran.us.r-project.org")
install.packages('caret', repos = "http://cran.us.r-project.org")

library("pROC")
library('plyr')
library('caret')

#---------------------------------------------
#     Loading, Checking and Cleaning Data     #
#---------------------------------------------

data.raw = read.table("data.csv", sep = ";", header = T)

head(data.raw)
str(data.raw)

data.raw[data.raw == ""] = NA
data.raw$City = as.factor(data.raw$City)
data.raw$Gender = as.factor(data.raw$Gender)
data.raw$Education = as.factor(data.raw$Education)

#-------- NA analysis -------
sum(is.na(data.raw))
data.raw[rowSums(is.na(data.raw)) > 0, ]

#-------- NA removal --------
data = data.raw[!rowSums(is.na(data.raw)) > 0, ]

#-------- Population Check -------
xtabs (~ DEFAULT + Loan.term, data = data)
xtabs (~ DEFAULT + Year.of.application, data = data)
xtabs (~ DEFAULT + Year.of.birth, data = data) #Birh dates in future!
xtabs (~ DEFAULT + City, data = data)
xtabs (~ DEFAULT + Gender, data = data)
xtabs (~ DEFAULT + Education, data = data)

#-----  Investication of problematic values (year.ofbirth) ------
hist(data$Year.of.birth, breaks = 20 , main = "Birth date Distribution", xlab = "Year", ylab = "Frequency", col = "lightblue")

data[which(data$Year.of.birth>2020),] 

# Seems as Y2K issue

data[which(data$Year.of.birth==2036),] 

# average life expectancy of women in CZ is 82 years
# thus Y2K issue seems as likely cause of anomaly in data

temp = data[which(data$Year.of.birth>2020),]
temp[order(temp$Year.of.birth),]

#----- Fix of problematic values ------
data$Year.of.birth[which(data$Year.of.birth>2020)] = data$Year.of.birth[which(data$Year.of.birth>2020)] - 100



#---------------------------------------------
#       1. [LOGIT] Age as single variable       #
#---------------------------------------------

#------------------------------------------------
# 1.1 [SINGLE VARIABLE] Age as QUANTITATIVE variable #
#------------------------------------------------

data$Age = max(data$Year.of.application) - data$Year.of.birth
data[order(data$Age),]

hist(data$Age[order(data$Age)], breaks = 20 , main = "Age Distribution", xlab = "Age", ylab = "Frequency", col = "lightblue")

# distribution of age seems to follow deviate from normal distributino
shapiro.test(data$Age)
#H0 confirms existence of normal distribution 
#H0 upon shapiro-wilk test we reject the hypothesis of the existence of normal distribution

#---- train test split 1 --------
set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 1 ----
logistic.age = glm(DEFAULT ~ Age, data = data.train, family = "binomial")
summary(logistic.age)
# according to the wald test, age is not sufficient predictor of the default

#---- generation prediction and AUC 1 ------
prediction.age = predict(logistic.age, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.age, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.age, plot = TRUE, print.auc = TRUE)

#------------------------------------------------
# 1.2a Preparing data for Age group default ration model #
#------------------------------------------------

hist(data$Age[which(data$DEFAULT == 1)])

age_group_default_likelihood = data.frame(
  Age_group = 20:90,
  DEFAULTS = 0,
  REPAYMENTS = 0,
  Likelihood = 0.1
)


#length(data$DEFAULT))
for (i in 1:70) {
  age_group_default_likelihood$DEFAULTS[i] = length(data$DEFAULT[which(data$Age == i+min(data$Age-1) & data$DEFAULT == 1)])
  age_group_default_likelihood$REPAYMENTS[i] = length(data$DEFAULT[which(data$Age == i+min(data$Age-1) & data$DEFAULT == 0)])
}

age_group_default_likelihood$Likelihood = age_group_default_likelihood$DEFAULTS / (age_group_default_likelihood$DEFAULTS + age_group_default_likelihood$REPAYMENTS)
age_group_default_likelihood$Likelihood[is.na(age_group_default_likelihood$Likelihood)] = 0

barplot(age_group_default_likelihood$Likelihood, age_group_default_likelihood$Age_group, names.arg = age_group_default_likelihood$Age_group, 
        space = 0, 
        width = 1,
        legend.text = FALSE
        )
abline(h = 0.5, col = 'red')

age_group_default_likelihood[which(age_group_default_likelihood$Likelihood >= 0.5), ]

#------------------------------------------------
# 1.2b [SINGLE VARIABLE] Age as QUALITATOVE variable #
#------------------------------------------------
 data$Risk.Age.Float = ifelse(data$Age < 30 | data$Age > 50, 1, 0)

#---- train test split 2  --------
set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 2 ----
logistic.age = glm(DEFAULT ~ Risk.Age.Float, data = data.train, family = "binomial")
summary(logistic.age)
# according to the wald test, age is not sufficient predictor of the default

#---- generation prediction and AUC 2 ------
prediction.age = predict(logistic.age, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.age, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.age, plot = TRUE, print.auc = TRUE)


#---------------------------------------------
#             [LOGIT] All variables          #
#---------------------------------------------

#------------------------------------------------
# 2.1 [ALL VARIABLE] Age as QUANTITATIVE variable #
#------------------------------------------------

set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 3 ----
logistic.all = glm(DEFAULT ~ .-Risk.Age.Float - Year.of.birth, data = data.train, family = "binomial")
summary(logistic.all)
#according to wald test, loan term, Year of application, Gender and age are not sufficient predictor of the default

#---- generation prediction and AUC 3 ------
prediction.all = predict(logistic.all, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.all, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.all, plot = TRUE, print.auc = TRUE)

#------------------------------------------------
# 2.2 [ALL VARIABLE] Age as QUALITATOVE variable #
#------------------------------------------------

set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 4 ----
logistic.all = glm(DEFAULT ~ .-Age - Year.of.birth, data = data.train, family = "binomial")
summary(logistic.all)
#according to wald test, loan term, Year of application, Gender and age are not sufficient predictor of the default

#---- generation prediction and AUC 4 ------
prediction.all = predict(logistic.all, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.all, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.all, plot = TRUE, print.auc = TRUE)

#------------------------------------------------
#        3 [LOGIT] Significant variables        #
#------------------------------------------------

#------------------------------------------------
#   3.1 [SIGNIFICANT VARIABLES] Identifying variables  #
#------------------------------------------------

#------- Qualitative testing ------
chisq.test(data$City, data$DEFAULT) # Significant (0hypo is independent)
chisq.test(data$Gender, data$DEFAULT) # Insignificant
chisq.test(data$Education, data$DEFAULT) # Significant (0hypo is independent)

#according to chi-square tests of factor variables, gender in not sufficient predictor of the default

#------------------------------------------------
#   3.2 [SIGNIFICANT VARIABLES] Using Age and City  #
#------------------------------------------------

set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 5 ----
logistic.sig = glm(DEFAULT ~ Loan.Amount + City + Education + Age, data = data.train, family = "binomial")
summary(logistic.sig)
#according to wald test, loan term, Year of application, Gender and age are not sufficient predictor of the default

#---- generation prediction and AUC 5 ------
prediction.sig = predict(logistic.sig, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.sig, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.sig, plot = TRUE, print.auc = TRUE)


#------------------------------------------------
#   3.3 [SIGNIFICANT VARIABLES] Using Age.Risk.Float and dropping City  #
#------------------------------------------------
# když zvolím seed 101 tak už vychází RISK AGE GROUP model lépe než AGE model podle AUC, takže otázka vybraných dat
set.seed(434)
split = sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
data.train = data[split == 1,]
data.test = data[split == 2,]

#---- logistic regression of Age and Default 6 ----
logistic.sig = glm(DEFAULT ~ Loan.Amount + City + Education + Risk.Age.Float, data = data.train, family = "binomial")
summary(logistic.sig)
#according to wald test, loan term, Year of application, Gender and age are not sufficient predictor of the default

#---- generation prediction and AUC 6 ------
prediction.sig = predict(logistic.sig, data.test, type = 'response')

#confusion matrix
confusionMatrix(as.factor(round(prediction.sig, 0)), reference = as.factor(data.test$DEFAULT))

#AUC indication
roc(data.test$DEFAULT ~ prediction.sig, plot = TRUE, print.auc = TRUE)

