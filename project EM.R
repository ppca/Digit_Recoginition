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


#form training and test data
train=cbind(training.data,label=training.label)
test=cbind(test.data,label=test.label)


#Using training examples for given class, we split it randomly into training set 
#and test set for cross_validation
samples<-function(class){
  #sample the index for training and test
  training.idx <- sample(500, 400)
  validation.idx <- setdiff(1:500, training.idx)
  #pick training set and test set according to the index
  train_sample=train[train[,d+1]==class,][training.idx,]
  test_sample=train[train[,d+1]==class,][validation.idx,]
  return(list("train"=train_sample,"test"=test_sample))
}


##calculate initial value
init<-function(M,train_sample){
  #randomly assign label to the training set
  labels=sample(M,nrow(train_sample),replace=TRUE)
  #column cancatenation of labels to the training set data
  train1=cbind(train_sample,labels)
  p=matrix(0,nrow=400,ncol=M)
  N=nrow(train_sample)
  pi=rep(0,M)
  for (m in 1:M){
    for (alpha in 1:400){
      p[alpha,m]=(1+sum((train1[,402]==m) & (train1[,alpha]==1)))/(2+sum(labels==m))
    }
  }
  for (m in 1:M){
    pi[m]=(1+sum(labels==m))/(N+M)
  }
  return(list("pi"=pi,"p"=p))
}

##compute responsibility
resp<-function(pi,p,train_sample,M){
  N=nrow(train_sample)
  X=train_sample[,1:400]
  pi_m=matrix(rep(log(pi),N),ncol=M,nrow=N,byrow=TRUE)
  l=X%*%log(p)+(1-X)%*%log(1-p)+pi_m
  l_max=apply(l,1,max)
  denom=rowSums(exp(l-matrix(rep(l_max,M),nrow=N,ncol=M,byrow=FALSE)))
  q=exp(l-matrix(rep(l_max,M),nrow=N,ncol=M,byrow=FALSE))/denom
  return(q)
}

##update p and pi
update<-function(q,train_sample,M){
  N=nrow(train_sample)
  p=t((1+t(q)%*%train_sample[,1:400])/matrix(rep(2+colSums(q),400),nrow=M,ncol=400,byrow=FALSE))
  pi=(1+colSums(q))/(N+M)
  return(list("pi"=pi,"p"=p))
}

#EM for M components of a given class
EM<-function(M,class,whole.data){
  #if whole.data=FALSE, we are doing cross-validation, so we need to select 400 training examples
  if (whole.data==FALSE){
    s=samples(class)
    train_sample=s$train
    test_sample=s$test
  }
  #if whole.data=TRUE, we are using all training example for the class
  else{
    train_sample=train[train[,401]==class,]
    test_sample=test[test[,401]==class,]
  }
  X=train_sample[,1:400]
  N=nrow(X)
  h=init(M,train_sample)
  p=h$p
  pi=h$pi
  d=1
  t=0
  while (d>0.1){
    p_0=p
    pi_0=pi
    q=resp(pi_0,p_0,X,M)
    hh=update(q,X,M)
    p=hh$p
    pi=hh$pi
    d=sum((p-p_0)^2)+sum((pi-pi_0)^2)
    t=t+1
    pi_m=matrix(rep(log(pi),N),ncol=M,nrow=N,byrow=TRUE)
    l=(X%*%log(p)+(1-X)%*%log(1-p)+pi_m)*q
    #l_lik=sum(log(p)+log(1-p))+sum(log(pi))+colSums(q,na.rm=FALSE,dims=1)%*%log(pi)+sum(l)
    l_lik=sum(l)+sum(-q*log(q))
    print(l_lik)
  }
  return(list("pi"=pi,"p"=p,"iter"=t,"test"=test_sample))
}

#draw the components of p
draw_p<-function(p){
  reshape=list()
  M=ncol(p)
  par(mfrow=c(3,as.integer(M/3)+1))
  for (m in 1:M){
    reshape[[m]]=array(p[,m],c(20,20))
    #windows()
    image(t(1 - reshape[[m]])[,20:1],
          col=gray(seq(0, 1, length.out=256)),
          axes=FALSE,
          asp=1)
  }
}

#gets all parameters and corresponding test examples for num components=M
#if whole.data=FALSE, we are doing cross-validation
EM_all<-function(M,whole.data){
  result=list()
  test=list()
  for (class in 0:9){
    result[[class+1]]=EM(M,class,whole.data)
    test[[class+1]]=result[[class+1]]$test
  }
  test_sample=do.call("rbind",test)
  return(list("result"=result,"test"=test_sample))
}

#calculate error of the model on test data
#if whole.data=FALSE, we are doing cross-validation, else, we are testing on test data
EM_error<-function(M,whole.data){
  hhh=EM_all(M,whole.data)
  result=hhh$result
  test_sample=hhh$test
  N=nrow(test_sample)
  l_lik=matrix(0,nrow=N,ncol=10)
  X=test_sample[,1:400]
  for (class in 0:9){
    p=result[[class+1]]$p
    pi=result[[class+1]]$pi
    pi_m=matrix(rep(log(pi),N),ncol=M,nrow=N,byrow=TRUE)
    l=X%*%log(p)+(1-X)%*%log(1-p)+pi_m
    l_lik[,class+1]=log(rowSums(exp(l)))
  }
  classification=cbind(apply(l_lik, 1, which.max)-1,true_label=test_sample[,401])
  error=mean(classification[,1]!=classification[,2])
  return(list("error"=error,"param"=result))
}

#do cross-validation t times and find out num of components that has lowest average error 
choose_M<-function(t){
  error=rep(0,10)
  for (i in 1:t){
    e=rep(0,10)
    for (M in 1:10){
      e[M] = EM_error(M,whole.data=FALSE)$error
    }
    error=error+e
  }
  error=error/t
  par(mfrow=c(1,1))
  plot(1:10,error,xlab="# of components",ylab="cross-validation error",main="cross_validation for # of components")
  index=which.min(error)
  min.error=error[[index]]
  return(list("components"=index,"min.error"=min.error,"error"=error))
}

#number of components chosen is 10, so M=10
#we do EM of M=10 on all training data, test the model on test data
h=EM_error(10,whole.data=TRUE)
#extract the parameters of all ten classes
param=h$param

#visualize the p vectors of a given class
visualize<-function(class,param){
  p=param[[class+1]]$p
  draw_p(p)
}
