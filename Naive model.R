library(caret)
library(rpart)
library(ranger)
library(foreach)

##7:3 train/valid
set.seed(1000) #reproducability setting
intrain<-createDataPartition(y=data_set$TARGET, p=0.7, list=FALSE) 
train<-data_set[intrain,-c(1)]
valid<-data_set[-intrain,-c(1)]


##logistic reg
logis_train=glm(TARGET~.,data=train,family=binomial)
logis_pred=predict.glm(logis_train,newdata = valid,type="response")

logis_bool<-ifelse(logis_pred>.2,0,1)
actual_target=1-as.numeric(valid$TARGET)

logis_conf<-confusionMatrix(actual_target,logis_bool)
logis_conf$byClass                                              #F1 0.40612468

##D tree
tree_train=rpart(TARGET~.,data=train)
tree_pred<-predict(tree_train,newdata=valid,type="vector")

tree_bool<-ifelse(tree_pred>.2,0,1)

tree_conf<-confusionMatrix(actual_target,tree_bool)
tree_conf$byClass                                               #F1 0.33678441


##rf
rf_train=ranger(TARGET~.,data=train,num.trees=500,mtry=4)
rf_pred=predict(rf_train,valid)

rf_bool<-ifelse(rf_pred$predictions>0.2,0,1)

rf_conf<-confusionMatrix(actual_target,rf_bool)
rf_conf$byClass[7]                                                 #F1 0.43237774


##para(rf)
rf.grid=expand.grid(ntree=c(200,300,400,500),mtry=c(3,4,5))

result=foreach(g=1:NROW(rf.grid),.combine = rbind) %do% {
  #training model
  m<-ranger(TARGET ~.,data=train,num.trees=rf.grid[g,"ntree"],mtry=rf.grid[g,"mtry"],importance='impurity')
  #prediction
  pred<-predict(m,valid)
  #f1
  rf_bool<-ifelse(pred$predictions>0.2,0,1)
  rf_conf<-confusionMatrix(actual_target,rf_bool)
  f1<-rf_conf$byClass[7]
  return(data.frame(g=g,num.trees=rf.grid[g,"ntree"],mtry=rf.grid[g,"mtry"],f1_acc=f1))
  cat(g)
}

result                                                          # n.tree 300    mtry 5 0.4393101

