# Multiple Discriminant Analysis.

data <-read.csv("/Users/ash/Desktop/RBS/SEM 2/Multivariate Analysis/Suicide Rate Analysis.csv") 

##########Exploring the Dataset###########
str(data)
library(data.table) 
setDT(data)
class(data)

library(caTools)
partn = floor(0.80*nrow(data))

set.seed(123) 
train_ind = sample(seq_len(nrow(data)),size = partn)

data_train = data[train_ind,]
data_test= data[-train_ind,]

dim(data_train)
dim(data_test)

data_train[is.na(data_train)] <- 0

as.factor(data_train$suicides_no)

#logreg_model <- glm(suicides_no ~.,family=binomial(link='logit'),na.action = na.exclude,data=data_train)

#since our dependent variable is a continuous variable, and even after converting it to factor, there were 1863 levels which cannot be reduced.
#Hence in such case, logistic regression isn't a good classification method.
