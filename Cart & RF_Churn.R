#CART for 
?rpart
#setwd("D:/AP/baging and  RF")
#TELECOME
df1 = read.csv("Churn.csv")
head(df1)

names(df1)

df1 = df1[,-c(19:21)]

df1$Churn = ifelse(df1$Churn == 1, "Yes", "No")

table(df1$Churn)

prop.table(table(df1$Churn))

set.seed(1234)

ids = sample(nrow(df1), nrow(df1)*0.8)

train = df1[ids,]
test = df1[-ids,]

### cart model 
library(rpart)
churntree = rpart( Churn ~ ., data=train, method="class")

library(rpart)				        # Popular decision tree algorithm
#install.packages("rattle")
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
#install.packages("party")
library(party)					# Alternative decision tree algorithm
#install.packages("partykit")
library(partykit)				# Convert rpart object to BinaryTree
#install.packages("caret")
library(caret)

fancyRpartPlot(churntree)

test$pred = predict( churntree, newdata = test, type="class")
?predict.rpart
#type="class" - for classification 

## not adding the predictions to train dataset 
trainpred = predict( churntree, newdata = train, type="class")

### performance on train
table(train$Churn, trainpred)
### [erformance on test]
table(test$Churn, test$pred)

## train error is 0.046 
## test error is 0.067


precision = 67/78
precision
recall = 67/(67+32)
recall
f_1 = 2*precision*recall/(precision+recall)

### CP matrix to check the cp and relative error 
printcp(churntree)


### Pruning a tree 

prunedtree = prune(churntree, cp = 0.013021)

fancyRpartPlot(prunedtree)

printcp(prunedtree)


## performance of pruned tree 


test$pruned_pred = predict(prunedtree, newdata = test, type="class")


table(test$Churn, test$pruned_pred)


### randomforest 

head(train)

## ensemble techniques ## using re sampling methods
#install.packages("randomForest")
library(randomForest)

rftrees = randomForest(as.factor(Churn) ~ ., data=train ) #, ntree = 30, mtry = 6, classwt = c( 0.7, 0.3))
test$pred_rf = predict(rftrees, newdata = test )


table(test$Churn, test$pred_rf)
precision = 66/75
recall = 66/(99)

2*precision*recall/(precision+recall)

names(rftrees)
## number of trees by defult picked by randomforest 

rftrees$ntree


### parameter tuning of randomforest 
#mtry-  which is the number of variables that are selected at each split of each tree when you make a split
#nodesize	Minimum size of terminal nodes. Setting this number 
  #larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
#classwt - Need not add up to one. Ignored for regression.
rftree2 = randomForest(as.factor(Churn) ~ ., data=train, ntree=40, mtry=5, classwt = c(0.7, 0.3), nodesize = 20  )

pred = predict(rftree2, newdata = test)

## confusion matrix 

table(test$Churn, pred)

precision = 75/90
recall = 75/(99)

2*precision*recall/(precision+recall)

names(rftrees)

importance(rftrees, decreasing = T)

varImpPlot(rftrees)
  