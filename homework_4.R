datapath<-"E:/business analytics/Uchicago/subjects/machine learning/machine_learning_04_data/"
data<-read.csv(paste0(datapath,'test_sample.csv'))

set.seed(2001)
nEval = 20  # last nEval values - evaluation set
N=120
n = N - nEval # n values in train set
xTrain = data # reshuffle train set, keep test set unchanged
nFold = 10
testSize = floor(n/nFold)
resCV<-NULL


for(k in 1:6) 
{   # k is the degree of fitted polynomial
  if(k>1) 
  { 
    # only to create polynomial of degree k
    xTrain = cbind(xTrain,xTrain$X^k)  
    names(xTrain)[ncol(xTrain)] = paste0('x',k)
  }
  for(i in 1:nFold) 
  {
    # select train and test sets
    testInd = (1+(i-1)*testSize):(i*testSize)  #make test fold
    #This will go from 1 to 11 and then 12 to 22 and so on
    
    train = xTrain[(1:n)[-testInd],]  #exclude 10 values of test 
    test = xTrain[testInd,] #exclude 10 values for holdout
    model <- lm(Y~.,data=train)
    resCV[i] = sum((predict(model,test)-test$Y)^2)  
  }  
  cat(k,mean(resCV),'\n')  #like print, but more efficient
  #At every iteration it will print the value of the MSE obtd via CV
}

# xTest<-data[which(is.na(data$Y)==T),]
# xTest$'x2'<-xTest$X^2
# xTest$'x3'<-xTest$X^3
# xTest$'x4'<-xTest$X^4
# xTest$'x5'<-xTest$x^5

for_model<-xTrain[is.na(data$Y)==F,]
for_model<-lm(Y~(X+x2+x3),for_model)
predY<-predict.lm(for_model,newdata=xTrain[which(is.na(xTrain$Y)==T),])



fittedDegree<-3

res = matrix(c(fittedDegree,predY),ncol = 1,
             dimnames = list(c("Degree",paste0(data$X[is.na(data$Y)])),c())
) 
write.table(res,"W4answer.csv",quote=F,col.names = F,sep = ",")

