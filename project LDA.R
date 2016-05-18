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
train=cbind(training.data,label=training.label,obs=1:5000)
test=cbind(test.data,label=test.label)

#given the list of 400 obs for each class, we form the training set as follows
sample_train<-function(train_obs){
  samples=list()
  length(samples)=10
  for (i in 1:10){
    samples[[i]]=train[train[,d+1]==i-1,][train_obs[[i]],] #pick the observations included in the list
  }
  result=do.call("rbind",samples) #horizontally stack training set of all classes
  return(result)
}

#given the list of 100 obs used for testing for each class of training data, we form test set
sample_test<-function(test_obs){
  samples=list()
  length(samples)=10
  obs=1:500
  for (i in 1:10){
    samples[[i]]=train[train[,d+1]==i-1,][test_obs[[i]],] #pick the observations included in the list
  }
  result=do.call("rbind",samples) #horizontally stack test set of all classes
  return(result)
}
#get mu
compute_u<-function(train_sample){
  u=matrix(0,nrow=10,ncol=400)
  for (i in 1:10){
    u[i,]=colMeans(train_sample[train_sample[,d+1]==i-1,][,1:d]) #ith row of u is mean vector of ith class
  }
  return(u)
}
#get pooled covariance matrix by averaging covariance matrices of 10 classes
compute_sig<-function(train_sample,u){
  sig=0
  for (i in 1:10){
    s=cov(train_sample[train_sample[,d+1]==i-1,][,1:d])*399/400
    sig=sig+s/10 #pooled covariance = average of covariance matrix of each class
  }
  return(sig)
}

LDA<-function(seed,lambda){
  t_obs=list() #define the list of training observations to be chosen
  test_obs=list() #define the list of test observations to be chosem
  for (i in 1:10){
    set.seed(seed+i)
    s=sample(500,500,replace=FALSE) #permute 1:500 randomly
    #pick training set obs and test set obs for class i (true label=i-1)
    t_obs[[i]]=s[1:400] #pick the first 400 elements of the permuted 500 integers for training
    test_obs[[i]]=s[401:500] #pick the last 100 elements of the permuted 500 integers for test
  }
  train_sample=sample_train(t_obs)
  test_sample=sample_test(test_obs)
  u=compute_u(train_sample)
  sig=compute_sig(train_sample)
  
  #compute sigma_lambda
  sig1=(1-lambda)*sig+lambda*0.25*diag(400)
  #compute inverse of sigma_lamba
  inv=solve(sig1)
  
  a=matrix(0,nrow=10,ncol=400)
  b=matrix(0,nrow=10,ncol=1)
  #calculate the linear classifier for each class
  for (i in 1:10){
    a[i,]=2*t(u[i,])%*%inv
    b[i]=-t(u[i,])%*%inv%*%u[i,]
  }
  #transpose the test set 
  test.T=t(test_sample[,1:400])
  
  #calculate ai*x+b in matrix form, where ij element of result is ai*xj+bi
  result=a%*%test.T+matrix(rep(b,1000),nrow=10,ncol=1000,byrow=FALSE)
  
  #find the i that maximize ai*xj+bi for every j
  classes=cbind(max.col(t(result))-1,true_label=test_sample[,d+1])
  
  #calculate error = #wrong prediction/#test observations
  error=mean(classes[,1]!=classes[,2])
  return(error)
}

error=list()
ptm=proc.time()
#t records iterations and error list length
t=1
#for every lambda, run LDA five times and return its average as element in list of error rates
for (lambda in seq(0.01,0.4,0.01)){
  runs=rep(0,5)
  for (i in 1:5){
    runs[i]=LDA(i,lambda)
  }
  error[[t]]=mean(runs)
  t=t+1
}

time_used=proc.time()-ptm
#plot error rate against lambda values
plot(seq(0.01,0.4,0.01),do.call("rbind",error),type='l',lwd=2,xlab='lambda',ylab='error rate',main='LDA cross-validation error')
#get the lambda value that minimize error rate
lambda_min=seq(0.01,0.4,0.01)[which.min(error)]
#lambda_min=0.29
error_min=error[which.min(error)]
#error_min=0.1578

#calculate LDA error on test set
LDA_test<-function(lambda){
  train_sample=train
  test_sample=test
  u=compute_u(train_sample)
  sig=compute_sig(train_sample)
  sig1=(1-lambda)*sig+lambda*0.25*diag(400)
  inv=solve(sig1)
  a=matrix(0,nrow=10,ncol=400)
  b=matrix(0,nrow=10,ncol=1)
  for (i in 1:10){
    a[i,]=2*t(u[i,])%*%inv
    b[i]=-t(u[i,])%*%inv%*%u[i,]
  }
  test.T=t(test_sample[,1:400])
  result=a%*%test.T+matrix(rep(b,10000),nrow=10,ncol=10000,byrow=FALSE)
  classes=cbind(max.col(t(result))-1,true_label=test_sample[,d+1])
  error=mean(classes[,1]!=classes[,2])
  return(error)
}

LDA_test(0.29)
#0.1464
