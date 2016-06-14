library(caret)
library(FSelector)
library(ROCR)
setwd('/home/hadoop/Documents/spotify_analysis/codes')
data<-read.table('data/user_song_combined_data1.csv',sep=',',header=TRUE)
data$Class[data$Class==1]<-'T'
data$Class[data$Class==0]<-'F'
weights <- chi.squared(Class~., data)
# print(weights)

#create train and test datasets
# 80% used for train and the rest for test
# Train <- createDataPartition(data$Class, times=10, p=0.8, list=TRUE)
flds<-createFolds(data$Class, k = 5, list = TRUE, returnTrain=FALSE)
training<-rbind(data[ flds[[1]],],data[ flds[[2]],],data[ flds[[3]],],data[ flds[[4]],])
testing <-data[ flds[[5]],]
print(dim(training))
print(dim(testing))
# mod_fit <- train(Class ~ ms_played+context+gender+age+country,data=training, method="glm", family="binomial")

mod_fit <- train(Class ~ ms_played+context+gender+age+country+acct_age+Mo+
                   No+Ev+Ni,  data=training, method="glm", family="binomial")
# gives a detailed result 
# print(summary(mod_fit))
# the results are log-odds ratio, to get the predicted probabiltiy we raise the fitted model with exponent
# print(exp(coef(mod_fit$finalModel)))
pred<-predict(mod_fit, newdata=testing)
# print(mod_fit)
# print(pred)
# plot the ROC curve for the predicted results
x<-c(pred)
y<-c(testing$Class)
y<-replace(y,y=='T',2)
y<-replace(y,y=='F',1)
y<-as.numeric(y)
fin_predic<-prediction(x, y)
# Recall-Precision curve             
rp_perf <- performance(fin_predic, "prec", "rec")
plot(rp_perf)
#ROC area under the curve
roc_auc<-performance(fin_predic, "tpr", "fpr")
plot (roc_auc)

auc.tmp <- performance(fin_predic,"auc");
auc <- as.numeric(auc.tmp@y.values)
print(auc)

