install.packages("e1071")
library(e1071)

setwd("C:/Users/Administrator/Google ‘∆∂À”≤≈Ã/backup 2015.2.14/spring 2015/246/midterm project")
load('digits.RData')

#label the data
num.class <- dim(training.data)[1] # Number of classes
num.training <- dim(training.data)[2] # Number of training data per class
d <- prod(dim(training.data)[3:4]) # Dimension of each training image (rowsxcolumns)
num.test <- dim(test.data)[2] # Number of test data
dim(training.data) <- c(num.class * num.training, d) # Reshape training data to 2-dim matrix
dim(test.data) <- c(num.class * num.test, d) # Same for test.
training.label <- rep(0:9, num.training) # Labels of training data.
test.label <- rep(0:9, num.test) # Labels of test data

#form training data and test data
train=cbind(training.data,label=training.label)
test=cbind(test.data,label=test.label)

##split test and train sample
sample_train<-function(train_obs){
  samples=list()
  length(samples)=10
  for (i in 1:10){
    samples[[i]]=train[train[,400+1]==i-1,][train_obs,]
  }
  result=do.call("rbind",samples)
  return(result)
}
sample_test<-function(test_obs){
  samples=list()
  length(samples)=10
  obs=1:500
  for (i in 1:10){
    samples[[i]]=train[train[,400+1]==i-1,][test_obs,]
  }
  result=do.call("rbind",samples)
  return(result)
}


##use package
SVM_1<-function(seed,c){
  set.seed(seed)
  s=sample(500)
  train_sample=sample_train(s[1:400])
  test_sample=sample_test(s[401:500])
  g1<-svm(label~., train_sample,cost=c,na.action=na.omit,scale=FALSE,type="C-classification",kernel="linear")
  svm.pred <- predict(g1, test_sample[,1:400])
  error=mean(svm.pred!=test_sample[,401])
  return(error)
}

#SVM_1(1,0.01)

param_C=seq(0.001,0.01,0.001)
t=0
n=length(param_C)
e=rep(0,n)
for (c in param_C){
  t=t+1
  for (i in 1:5){
    e[t]=e[t]+SVM_1(seed=i,c=c)
  }
  e[t]=e[t]/5
}
plot(param_C,e,xlab="cost",ylab="error rate",main="cross validation for c=[0.001,0.01]")

#Using C=0.07, fit SVM on whole training data
g<-svm(label~., train,cost=0.07,na.action=na.omit,scale=FALSE,type="C-classification",kernel="linear")
svm.pred <- predict(g, test[,1:400])
error=mean(svm.pred!=test[,401])
print(error)

#gradient descent cross-validation 
sgd<-function(ita0,lambda,S,seed){
  #first extract the training and test examples
  #ptm=proc.time()
  set.seed(seed)
  s=sample(500)
  train_obs=s[1:400]
  test_obs=s[401:500]
  train_sample=sample_train(train_obs)
  test_sample=sample_test(test_obs)
  #get the number of examples in training data
  N=nrow(train_sample)
  #initiate wk's to be a zero 400-vector
  W=matrix(0,nrow=400,ncol=10)
  #perform stochastic descent S times  
  for (iter in 1:S){
    #reset ita
    ita=ita0/iter
    #shuffule data with seed number set to iteration number
    set.seed(iter)
    train1=train_sample[sample(N),]
    #seperate x and y
    X=train1[,1:400]
    y=train1[,401]
    #vector of class numbers
    k=0:9
    #loop through all training examples
    for (i in 1:N){
      yn=(k==y[i])-(k!=y[i])
      W=(1-ita*lambda)*W+matrix(X[i,],ncol=10,nrow=400,byrow=FALSE)%*%diag(as.vector(((yn*(X[i,]%*%W))<=1)*ita*yn))
    }
  }
  #separate x and y in test examples
  X_test=test_sample[,1:400]
  y_test=test_sample[,401]
  #make prediction by choosing k which maximiza wk*x
  pred=max.col(X_test%*%W)-1
  #calculate error rate as #mistakes/#test examples
  error=mean(pred!=y_test)
  #print(proc.time()-ptm)
  return(list('W'=W,'error'=error))
}

#find best combination of parameters ita and lambda
param_lambda=seq(0.001,0.01,0.001)
param_ita0=seq(0.01,0.1,0.01)
j=0
e_1=vector()
e_all=list()
for (lambda in param_lambda){
  t=0
  j=j+1
  for (ita0 in param_ita0){
    t=t+1
    e=0
    for (i in 1:5){
      e=e+sgd(ita0,lambda,10,i)$error
    }
    e=e/5
    e_1[t]=e
  }
  e_all[[j]]=e_1
}

#choose lambda=0.001, ita=0.02

#do gradient descent on whole training data with lambda=0.001, ita=0.2
#calculate the prediction error using the whole test data
sgd_2<-function(lambda,ita,S){
  N=nrow(train)
  #initiate wk's to be a zero 400-vector
  W=matrix(0,nrow=400,ncol=10)
  #perform stochastic descent S times  
  for (iter in 1:S){
    #reset ita
    ita=ita0/iter
    #shuffule data with seed number set to iteration number
    set.seed(iter)
    train1=train[sample(N),]
    #seperate x and y
    X=train1[,1:400]
    y=train1[,401]
    #vector of class numbers
    k=0:9
    #loop through all training examples
    for (i in 1:N){
      yn=(k==y[i])-(k!=y[i])
      W=(1-ita*lambda)*W+matrix(X[i,],ncol=10,nrow=400,byrow=FALSE)%*%diag(as.vector(((yn*(X[i,]%*%W))<=1)*ita*yn))
    }
  }
  #separate x and y in test examples
  X_test=test[,1:400]
  y_test=test[,401]
  #make prediction by choosing k which maximiza wk*x
  pred=max.col(X_test%*%W)-1
  #calculate error rate as #mistakes/#test examples
  error=mean(pred!=y_test)
  #print(proc.time()-ptm)
  return(list('W'=W,'error'=error))
}

#error=0.1332 on the whole test data

