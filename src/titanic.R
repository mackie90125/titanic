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
hist(d.train$Fare, breaks = 50)
hist(d.train$Age)
hist(d.train$SibSp)
hist(d.train$Parch)

###############################################################################################
### step logistic regression ###

# data set to use for logistic regression
d.logistic <- select(d.train, Survived, Pclass, Sex, SibSp, Parch, Fare) %>%

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
  logistic.2 <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare +
    Pclass:Sex + Pclass:SibSp + Pclass:Parch + Pclass:Fare +
    Sex:Parch + Parch:Fare, family = "binomial", data = d.logistic)
)

# test if any elements should be removed
drop1(logistic.2)

# function to return percent correct prediction from logistic regression model
logistic.accuracy <- function(model, new_data, cutoff){
  model.pred <- if_else(predict(model, newdata = new_data, type = "response") > cutoff, 1, 0)
  return(sum(model.pred == new_data$Survived) / nrow(new_data) * 100)
}

# accuracy of step logistic model at different cutoff values
cutoff.seq <- seq(0.05, 0.95, 0.05)
cutoff.pred <- NULL
for(i in 1:length(cutoff.seq)){
  cutoff.pred[i] <- logistic.accuracy(logistic.2, d.train, cutoff.seq[i])
}
plot(cutoff.seq, cutoff.pred, ylim = c(0,100))
max(cutoff.pred)
cutoff.seq[which(cutoff.pred == max(cutoff.pred))]

# original step model = 80.47138

###############################################################################################
### logistic regression with fare as a dichotomous variable ###

summary(d.train$Fare)

# try splitting Fare at median
d.train$Fare_dich <- cut(d.train$Fare, c(-Inf, median(d.train$Fare), Inf))
levels(d.train$Fare_dich) <- c("Low","High")

# logistic regression with all possible two-way interactions
logistic.3 <- glm(Survived ~ .^2,
                  family = "binomial",
                  data = select(d.train, Survived, Pclass, Sex, SibSp, Parch, Fare_dich))

# step function to choose a logistic regression model
step(logistic.3, scope = Survived ~ 1, direction = "both", trace = FALSE)

summary(
  logistic.4 <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare_dich +
    Pclass:Sex + Pclass:SibSp + Pclass:Parch + Pclass:Fare_dich +
    Sex:Parch + Parch:Fare_dich, family = "binomial", data = select(d.train,
    Survived, Pclass, Sex, SibSp, Parch, Fare_dich))
)

logistic.accuracy(logistic.4, d.train, 0.5)

# try different split values with same model
split.seq <- c(1, seq(5,100,5))
split.pred <- NULL

for(i in 1:length(split.seq)){

  # split Fare
  d.train$Fare_dich <- cut(d.train$Fare, c(-Inf, split.seq[i], Inf))
  levels(d.train$Fare_dich) <- c("Low","High")

  # logistic regression with all possible two-way interactions
  logistic.4 <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare_dich +
    Pclass:Sex + Pclass:SibSp + Pclass:Parch + Pclass:Fare_dich +
    Sex:Parch + Parch:Fare_dich, family = "binomial",
    data = select(d.train, Survived, Pclass, Sex, SibSp, Parch, Fare_dich)
  )

  split.pred[i] <- logistic.accuracy(logistic.4, d.train, 0.5)
}

plot(split.seq, split.pred, ylim = c(0,100))
max(split.pred)
split.seq[which(split.pred == max(split.pred))]

# model with Fare_dich = 81.14478

# split fare at 20
d.train$Fare_dich <- cut(d.train$Fare, c(-Inf, 20, Inf))
levels(d.train$Fare_dich) <- c("Low","High")

###############################################################################################
### logistic regression with SibSp and Parch as a dichotomous variables ###

# split both at 1
d.train$SibSp_dich <- cut(d.train$SibSp, c(-Inf, 0.99, Inf))
levels(d.train$SibSp_dich) <- c("None","Some")

d.train$Parch_dich <- cut(d.train$Parch, c(-Inf, 0.99, Inf))
levels(d.train$Parch_dich) <- c("None","Some")

# logistic regression with all possible two-way interactions
logistic.5 <- glm(Survived ~ .^2,
                  family = "binomial",
                  data = select(d.train, Survived, Pclass, Sex, SibSp_dich, Parch_dich, Fare_dich))

# step function to choose a logistic regression model
step(logistic.5, scope = Survived ~ 1, direction = "both", trace = FALSE)

summary(
  logistic.6 <- glm(formula = Survived ~ Pclass + Sex + Parch_dich + Fare_dich +
    Pclass:Sex + Pclass:Parch_dich + Pclass:Fare_dich + Sex:Parch_dich +
    Parch_dich:Fare_dich, family = "binomial", data = select(d.train,
    Survived, Pclass, Sex, SibSp_dich, Parch_dich, Fare_dich))
)

logistic.accuracy(logistic.6, d.train, 0.5)

# model with Fare_dich, Parch_dich and SibSp_dich = 81.25701

# accuracy of logistic model at different cutoff values
cutoff.seq <- seq(0.05, 0.95, 0.05)
cutoff.pred <- NULL
for(i in 1:length(cutoff.seq)){
  cutoff.pred[i] <- logistic.accuracy(logistic.6, d.train, cutoff.seq[i])
}
plot(cutoff.seq, cutoff.pred, ylim = c(0,100))
max(cutoff.pred)
cutoff.seq[which(cutoff.pred == max(cutoff.pred))]

###############################################################################################
### predict survival from model 6 ###

# create dichotomous variables in d.test data set
d.test$Fare_dich <- cut(d.test$Fare, c(-Inf, 20, Inf))
levels(d.test$Fare_dich) <- c("Low","High")

d.test$SibSp_dich <- cut(d.test$SibSp, c(-Inf, 0.99, Inf))
levels(d.test$SibSp_dich) <- c("None","Some")

d.test$Parch_dich <- cut(d.test$Parch, c(-Inf, 0.99, Inf))
levels(d.test$Parch_dich) <- c("None","Some")

# d.test model predictions
model.pred <- if_else(predict(logistic.6, newdata = d.test, type = "response") > 0.5, 1, 0)

# create submission data frame
d.sub <- data.frame(PassengerId = d.test$PassengerId,
                    Survived = model.pred)
write.csv(d.sub, "/Users/ryan/test_analysis/titanic/data/mackie_submission.csv", row.names = FALSE)