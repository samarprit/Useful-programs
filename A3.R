dataPath<-"E:/business analytics/Uchicago/subjects/machine learning/machine_learning_03_data"
data <- read.csv(paste(dataPath,"test_sample.csv",sep="/"),header=T)
lm_reg<-lm(Y~.,data=data.frame(Y=data[,1],data[,2:491]))
coeff_pr<-as.data.frame(summary(lm_reg)$coefficients) 
head(coeff_pr)
ind_sort<-sort(coeff_pr$`Pr(>|t|)`[which(coeff_pr[,4]>0.05,arr.ind = TRUE)],decreasing = TRUE)
ind_sort
Indices_excludedcoeff<-c(match(ind_sort,coeff_pr[,4]))
Indices_excludedcoeff
suppressWarnings(library(glmnet))
lasso490=glmnet(x=data.matrix(data[,2:491]),y=data[,1],alpha=1,nlambda=100,lambda.min.ratio=.0001)
summary(lasso490)$beta  
head(lasso490$beta)
plot(lasso490)
abline(h=0)
set.seed(1)
cv.out=cv.glmnet(x=data.matrix(data[,2:491]),y=data[,1],alpha=1)
plot(cv.out)
(bestlam =cv.out$lambda.min)
out=glmnet(x=data.matrix(data[,2:491]),y=data[,1],alpha=1,lambda=bestlam)
lassotemp.coef=predict(out,type="coefficients",s=bestlam)
removedsslope<-c(which(lassotemp.coef==0,arr.ind = TRUE)[,1])
remov
res = matrix(c("lasso","lm","",""),ncol=2)
colnames(res) <- c("model","removed_regressors")
res[,"removed_regressors"][1] = paste0(removedsslope,collapse = " ")
res[,"removed_regressors"][2] = paste0(Indices_excludedcoeff,collapse = " ")
head()
write.csv(res,"E:/business analytics/Uchicago/subjects/machine learning/W3answer.csv",quote=FALSE,row.names = F)
