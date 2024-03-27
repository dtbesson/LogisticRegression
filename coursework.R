# using the start.r file to load the titanic dataset
# please set the working directory to where this file is saved, along with 
# the start.r file

load("titanic.rdata")

n<-nrow(titanic.df)
n


head(titanic.df)

summary(titanic.df)

##

titanic.df$Pclass<-factor(titanic.df$Pclass)
titanic.df$Sex<-factor(titanic.df$Sex)
titanic.df$Age<-factor(titanic.df$Age)
titanic.df$Embarked<-factor(titanic.df$Embarked)


######## 
# Q1
########

# fit the model
model = glm(Survived ~ Pclass + Sex + Age + Parch + Embarked, 
              data = titanic.df, family = binomial)

# the glm() function automatically creates dummy variables for the
# multi-level categorical variables

# print the summary 
summary(model)

# a low p-value (given by Pr(>|Z|)) implies that the corresponding coefficient
# is significant, i.e. we have sufficient evidence to reject the null hypothesis
# that the coefficient is equal to 0

# the intercept, Pclass and Sex predictors are significant at the 0.001 level
# Age is significant at the 0.01 level
# the remaining predictors are less significant and would be candidates for 
# removal from the model



######## 
# Q2
########

# define the model
model2 = glm(Survived ~ Pclass + Sex + Age, data = titanic.df, family = binomial)

# perform analysis of deviance between our earlier full model and our new
# reduced model
anova(model2, model, test = 'Chisq')

# a p-value of 0.1076 > 0.05 (or whatever significance level we choose)
# suggests that there is insufficient evidence to reject the null hypothesis,
# (which is the hypothesis that removing Parch and Embarked does not
# significantly worsen model fit)

# the p-value can be calculated explicitly using the residual deviance too
1-pchisq(658.40 - 652.31, 3)


# see if we can further reduce this model

# check the summary of this model
summary(model2)

# the Age2 and Age3 are still less significant than the others
# see if removing these reduces deviance

# build a model without the Age predictor
model_noage = glm(Survived ~ Pclass + Sex, data = titanic.df, family = binomial)

# perform another analysis of deviance
anova(model_noage, model2, test = 'Chisq')






######## 
# Q3
########

# using our model, predict the probabilities for each data point
predicted_probabilities <- predict(model2, type = "response")

# create new binary variable pred.surv based on predicted probabilities
pred.surv <- ifelse(predicted_probabilities > 0.5, 1, 0)



# create the table / confusion matrix
confusion_matrix <- table(titanic.df$Survived, pred.surv)

# calculate the proportion of correctly classified sample cases
# correct predictions appear on the diagonals
# divide this by the total number of predictions made
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# output
print(confusion_matrix)
print(paste("The accuracy of the model:", accuracy))

# the model seems to predict cases where passengers did not survive (given by 0)
# more often than the cases where they did (given by 1)

# we could further analyse the confusion matrix by considering other metrics, 
# such as precision, recall and F1-score

# calculate precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

# calculate recall
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

# calculate F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# output  
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))






######## 
# Q4(i)
########

# for an adult female travelling 2nd class, we have 
# Pclass2 = Age2 = 1
# Pclass3 = Sexmale = Age3 = 0
# read the output of summary(model2) to obtain the coefficient values
summary(model2)

# the log odds are then given by 
log_odds = 3.0672 - 1.0673 - 0.7608

# odds are given by exponentiating
odds = exp(log_odds)
# observe the value
odds

# the odds is 3.452505
# this means that the probability of an adult female travelling 2nd class
# surviving is 3.452505 the probability of them not surviving




######## 
# Q4(ii)
########

# for an adult female travelling first class,
# Age2 = 1
# Pclass2 = Pclass3 = Sexmale = Age3 = 0

# again, read the output of summary(model2) to obtain the coefficients
log_odds_1 = 3.0672 - 0.7608
odds_1 = exp(log_odds_1)

# for a senior male travelling third class,
# Pclass3 = Sexmale = Age3 = 1
# Pclass2 = Age2 = 0

log_odds_2 = 3.0672 - 2.2334 - 2.4809 - 1.8734
odds_2 = exp(log_odds_2)

# calculate the odds ratio
odds_ratio = odds_1 / odds_2

# output
odds_ratio

# an odds ratio of 339.3052 implies that adult females travelling first class
# were 339.3052 times more likely to survive, compared to a senior male 
# travelling third class






######## 
# Q5
########

# we already know the odds of survival for an adult female travelling second
# class from Q4(i), given by the variable 'odds'
odds

# after rearranging, we can get the probability of survival by the formula 
# prob = odds/(1+odds)
prob = odds/(1+odds)
# output
prob

# we get the probability as 0.7754073


# we can calculate the standard error for this observation by
se_prob = sqrt(prob * (1-prob))
# the variance for a Bernoulli random variable is p(1-p)

# the critical value for N(0,1) at the 95% confidence level is 1.96

# the confidence interval bounds are therefore
lower = prob - 1.96 * se_prob
upper = prob + 1.96 * se_prob

# convert back to probabilities
lower_prob = 1/(1+exp(-lower))
upper_prob = 1/(1+exp(-upper))

confidence_interval = c(lower_prob, upper_prob)

# output
confidence_interval
