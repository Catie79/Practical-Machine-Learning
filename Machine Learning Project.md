---
title: "Predicting Exercise Type with Accelerometer Data"
author: "Catie Petersen"
date: "March 15, 2017"
output: html_document
---

##Summary
Dataset from:
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
Read more: http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz4XJIcS33E

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(fields)

#Read in data sets
pml.testing <- read.csv("C:/Users/IBM_ADMIN/Downloads/pml-testing.csv")
pml.training <- read.csv("C:/Users/IBM_ADMIN/Downloads/pml-training.csv")

#Evaluate data set
te <- as.data.frame(summary(pml.testing))
tr <- as.data.frame(summary(pml.training))
#output hidden to save space
#te
#tr
```

## Data Exploration

The testing data has several columns that aren't populated.  These columns won't be usable for predicting a classe and will need to be removed from the training set.


## Data Cleanup

The columns that are null in the testing set are removed from the training set.  The training set also has incomplete observations removed.  The user name and identifier are removed as they're not predictors of the exercise.

```{r}
#remove columns not used in testing data set
whichcolsneedtogo<- is.na(pml.testing[1,])
clean<-as.data.frame(pml.training[,!whichcolsneedtogo])
pml.training <- clean
pml.training$X <- NULL
pml.training$user_name <- NULL

#NAs
complete <- complete.cases(pml.training)
trainingcomplete <- pml.training[complete,]
```

##Model Building

The training data will be split to allow the model to be tested against data not used to build it.

```{r}
#Create testing, training, validation sets
set.seed(123)
trainIndex <- createDataPartition(trainingcomplete$classe, p = .6, 
                                  list = FALSE, 
                                  times = 1)
pmlTrain <- trainingcomplete[ trainIndex,]
pmlTest  <- trainingcomplete[-trainIndex,]
set.seed(123)
validIndex <- createDataPartition(pmlTest$classe, p = .5, 
                                  list = FALSE, 
                                  times = 1)
pmlValid <- trainingcomplete[ validIndex,]
pmlTest  <- trainingcomplete[-validIndex,]

```

Random Forest

With the number of variables and the nature of the data (classification), random forest seems and obvious possibility for predicting the classe.  It is a bit challenging when running on a machine with limited memory, so the count of trees will be limited to 100 in order to avoid memory errors.

```{r}
#Fit random forest model to predict classe
ctrl <- trainControl(method = "cv", number = 3)

set.seed(123)
fit.random <- train(classe ~ ., data = pmlTrain, method = "rf", trControl = ctrl, ntree = 100)

random <- predict(fit.random, pmlTest)
```

Gradient Boosted Tree

Ther performance of a gradient boosted tree is usually superior to a random forest in terms of speed and memory, but it needs to be properly tuned in order to outperform a random forest in terms of accuracy.  With the memory issues lessened, 200 trees will be built and the model will be built with two eta values to compare performance.

```{r}
#Fit boosted forest model to predict classe
xgbGrid <- expand.grid(eta = c(.1,.4), max_depth = 10, nrounds = 200, gamma = 0, colsample_bytree= 1, min_child_weight = 1)
ctrl <- trainControl(method = "cv", number = 3, classProbs = TRUE)

set.seed(123)
xgbtree.fit <- train(classe ~ ., data = pmlTrain, method = 'xgbTree', trControl = ctrl, tuneGrid = xgbGrid)

plot(xgbtree.fit)

boost <- predict(xgbtree.fit, pmlTest)
```

##Model Evaluation

```{r}
#Random forest
fit.random
confusionMatrix(random, pmlTest$classe)

#Gradient Boosted Tree
xgbtree.fit
confusionMatrix(boost, pmlTest$classe)
```

##Out of Sample Error

```{r}
oose <- predict(xgbtree.fit, pmlValid)
sum(oose == pmlValid$classe)/nrow(pmlValid)

```
##Model selection

Both models perform very well.  With an accuracy of .999 and an accuracy of .996, this is about as solid a performance as can be created.  The gradient boosted tree did benefit from the light tuning with an improvement with the lower shrinkage value.  Both were tested in caret using cross validation with three folds and both performed well with an out of sample error of 1%.

Since both models are performing with nearly perfect accuracy, I'll pick the one that's less demanding on my machine.  The gradient boosted tree is much more efficient.  I can build up to 400 trees with 8GB of memory while the random forest tops out at 150 trees (random forest builds with full trees, making it more demanding of resources).  While the random forest is slightly more accurate, it's within rounding error and the efficient performance is of more value then the slight uptick in accuracy.

##Test Data

The gradient boosted model is used to predict the classe given the measurements from the accelerometers.

```{r}
results <- predict(xgbtree.fit, pml.testing)
```


