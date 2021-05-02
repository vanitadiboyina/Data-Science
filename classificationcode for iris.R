library(datasets)
iris_data <-iris
summary(iris_data)

plot(iris_data)

#iris_data$species <- factor(iris_data$species)
iris_data$Species <- factor(iris_data$Species)

#splitting dataset into training and testing
#install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(iris_data$Species,SplitRatio=0.8)
training_set <- subset(iris_data,split==TRUE)
test_set <- subset(iris_data,split==FALSE)


#feature scaling
training_set[-5]<- scale(training_set[,-5])
test_set[-5]<-scale(test_set[-5])


#fitting knn to the training set and predicting the test set results
library(class)
y_pred <- knn(train = training_set[, -5],
              test = test_set[, -5],
              cl = training_set[, 5],
              k=5,
              prob=TRUE)


cm <- table(test_set[,5],y_pred)
cm

plot(iris_data)




#############################
iris_data_2 <- iris_data[-c(2)]


set.seed(123)
split <- sampe.split(iris_data_2$Species, SplitRatio=0.8)
training_set <- subset(iris_data_2,split==TRUE)
test_set <- subset(iris_data_2,split==FALSE)



training_set[-4]<- scale(training_set[,-4])
test_set[-4]<-scale(test_set[-4])




y_pred <-knn(train = training_set[,-4],
             test = test_set[,-4],
             cl = training_set[,4],
             k=5,
             prob=TRUE)
cm2 <- table(test_set[,4],y_pred)
cm2





##############################3
iris_data_2$Sum <- apply(iris_data_2[,c('Sepal.Length','Petal.Length','Petal.width')],
                         1. function(x) sum(x))

set.seed(123)
split <- sample.split(iris_data_2$Species,SplitRatio=0.8)
training_set <- subset(iris_data_2,split==TRUE)
test_set <- subset(iris_data_2,split==FALSE)

#feature scalling
training_set[-4] <- scale(training_set[-4])
test_set[-4]<-scale(test_set[-4])

y_pred <-knn(train = training_set[,-4],
             test = training_set[,-4],
             c1 = training_set[,4],
             k=5,
             prob=TRUE)
cm2 <- table(test_set[,4],y_pred)
cm2



####################3
#apply kfold cross validation
#install.packages('caret')
library(caret)
folds <- createFolds(training_set$Species,k=5)
cv <- lapply(folds,function(x){
  training_fold <- training_set[-x,]
  test_fold <- training_set[x,]
  
  y_pred <- knn(train=training_fold[,-4],
                test=test_fold[,-4],
                cl=training_fold[,4],
                k=5,
                prob=TRUE)
  
  cm2 <- table(test_fold[,4],y_pred)
  accuracy= (cm2[1,1]+ cm2[2,2]+cm2[3,3])/ (cm2[1,1]+cm2[2,2]+cm2[3,3]
                                            +cm2[1,2]+cm2[1,3]+cm2[2,1]
                                            +cm2[2,3]+cm2[3,1]+cm2[3,2])
  return (accuracy)
  
})

cv
accuracy <- mean(as.numeric(cv))
accuracy


















###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############
####CLUSTERING
#install.packages("stats")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

#View(iris)
#unsupervised learning-hence converting iris data to unlabeled
mydata=select(iris, c(1,2,3,4))



#WSS function
wssplot <- function(data,nc=15,seed=1234)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


#wss plot to chose maximum number of clusters
wssplot(mydata)

#Spotting the kink in the curve in order to choose the optimum 


#k-means cluster
km=kmeans(mydata,2)


#evaluating Cluster analysis



#cluster plot
autoplot(km,mydata,frame=TRUE) # graph or plot has no overlap

#cluster centres

km$centers





























#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

###dimensionality reduction
library(stats)
library(dplyr)


#unsupervised
mydata=select(iris,c(1,2,3,4))
#check PCA eligibility
#if average corelation is above 0.3 or below -0.3  then variables are highly correlated and eligible for PCA
cor(mydata)
mean(cor(mydata))

#pca

PCA=princomp(mydata)

#evaluating pca
#two ways:
#1. check wether pcs capture the essence of original variable
#2. check wether the pcs are independent

#pc loadings

PCA$loadings

#principal components
Pc=PCA$scores
View(Pc)
cor(Pc)









# 23 min video   https://www.youtube.com/watch?v=irtYynMb8kE&t=1s


####3 Random Forest
#install.packages("MLmetrics")
#install.packages("randomForest")
library(stats)
library(dplyr)
library(randomForest)
library(MLmetrics)


base_data=iris
View(base_data)
mydata=select(base_data,c(1,2,3,4))
str(base_data)
View(mydata)

mydata=iris
CM=cor(mydata)
CM

index=sample(2,nrow(mydata),replace=TRUE,prob=c(0.7,0.3))
index
table(index)
Training=mydata[index==1,]
Testing=mydata[index==2,]

RM=lm(Petal.Length~Petal.Width+Sepal.Width,data=Training)

summary(RM)


Petal_Length_Pred=predict(RM,Testing)
Testing$Petal_Length_Pred=Petal_Length_Pred
View(Testing)

Error=MAPE(Testing$Petal_Length_Pred,Testing$Petal.Length)

Error
1-Error#accuracy



###   14 min video   https://www.youtube.com/watch?v=acFviblzijU&t=314s



mydata=iris
index=sample(2,nrow(mydata),replace=TRUE,prob=c(0.7,0.3))
Training=mydata[index==1,]
Testing=mydata[index==2,]

RFM=randomForest(Species~.,data=Training)
Species_Pred=predict(RFM,Testing)
Testing$Species_Pred=Species_Pred
View(Testing)
CFM=table(Testing$Species,Testing$Species_Pred)
CFM


Classification_Accuracy=sum(diag(CFM)/sum(CFM))
Classification_Accuracy

