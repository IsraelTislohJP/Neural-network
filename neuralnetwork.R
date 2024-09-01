set.seed(123)

# Importing the data into R
data<-read.csv('wine_data.csv',sep=',')

# Converting the target variable datatype from character to factor
data$Class<-factor(data$Class,levels=c(1:3),labels=c('Low','Moderate','High'))

r<-runif(nrow(data))
data<-data[order(r),]

# Importing the required libraries

library(neuralnet)
library(caret)
library(nnet)

# Splitting the data into training and testing set

dp<-sample(2,nrow(data),replace=T,prob=c(.8,.2))
training<-cbind(as.data.frame(scale(data[dp==1,-1])),
class.ind(data[dp==1,]$Class))

testing<-cbind(as.data.frame(scale(data[dp==2,-1])),
class.ind(data[dp==2,]$Class))

# Fitting a neural network model to the data

model<-neuralnet(Low+Moderate+High~.,data=training,hidden=c(4,2),
threshold=.001,lifesign.step=20,rep=3,err.fct='sse')

# Plot the model

plot(model,rep=1)

# Checking the performance of the model

pred<-compute(model,rep=1,testing[,-c(14:16)])
pred<-pred$net.result
pred<-data.frame(pred)
names(pred)<-c('Low','Moderate','High')
prediction<-as.data.frame(colnames(pred)[apply(pred,1,which.max)])
names(prediction)<-'Predicted'
actual<-as.data.frame(colnames(testing[,14:16])[apply(testing[,14:16],1,
which.max)])
names(actual)<-'Actual'
tab<-cbind(actual,prediction)
table<-table(tab)
print(table)
View(tab)
confusionMatrix(table)