# libraries
library(tidyverse)
library(table1)
library(compareGroups)

###############################################################################################
### read data ###

d.train <- read.csv("/Users/ryan/test_analysis/titanic/data/train.csv")
d.test <- read.csv("/Users/ryan/test_analysis/titanic/data/test.csv")

# general summary of data
summary(d.train)

# load training data set
d.train <- read.csv("/Users/ryan/test_analysis/titanic/data/train.csv") %>%

  # convert variables to factors
  mutate(across(c(Pclass, Sex, Embarked), factor)) %>%

  # create factor variable for survival
  mutate(Surv_fac = factor(Survived))

# load training data set
d.test <- read.csv("/Users/ryan/test_analysis/titanic/data/test.csv") %>%

  # convert variables to factors
  mutate(across(c(Pclass, Sex, Embarked), factor))


###############################################################################################
### summary statistics ###

# create summary table of counts
table1(~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked | Surv_fac, overall=FALSE,
       topclass="Rtable1-zebra", data = d.train)

# Chi-square tests of association
chisq.test(d.train$Survived, d.train$Sex)
chisq.test(d.train$Survived, d.train$Pclass)
with(filter(d.train, Embarked != ""),
     chisq.test(Survived, Embarked))

# tables of counts and odds ratios
createTable(
  compareGroups(Survived ~ Sex + Pclass, data = d.train, method = 3),
show.ratio = TRUE)

createTable(
  compareGroups(Survived ~ Embarked, data = d.train, method = 3, subset = Embarked != ""),
show.ratio = TRUE)

# histograms for data distribution
hist(d.train$Fare, breaks = 20)


###############################################################################################
### logistic regression ###

# data set to use for logistic regression
d.logistic <- select(d.train, Survived, Pclass, Age, Sex, SibSp, Parch, Fare) %>%

  # remove any rows with missing values
  na.omit()

# logistic regression with all possible two-way interactions
logistic.1 <- glm(Survived ~ .^2,
                  family = "binomial",
                  data = d.logistic)

# step function to choose a logistic regression model
step(logistic.1, scope = Survived ~ 1, direction = "both", trace = FALSE)

# chosen logistic regression model
summary(
  logistic.2 <- glm(formula = Survived ~ Pclass + Age + Sex + SibSp + Parch +
    Fare + Pclass:Sex + Pclass:SibSp + Pclass:Parch + Age:Sex +
    Age:Parch + Age:Fare, family = "binomial", data = d.logistic)
)

# test if any elements should be removed
drop1(logistic.2)

# survival prediction for train data
pred.train.logistic2 <- if_else(predict(logistic.2, newdata = d.logistic, type = "response") > 0.5, 1, 0)

# compare actual and predicted survival for train data
table(pred.train.logistic2, d.logistic$Survived)

sum(pred.train.logistic2 == d.logistic$Survived) / nrow(d.logistic) * 100