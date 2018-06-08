# Loading the libraries
library(readr)
library(caret)
library(VIM)
library(mice)
library(BaylorEdPsych)
library(mvnmle)

# Reading the data and checking the dimensions and summary of data
train <- read.csv("C:\\Fall_Semester\\EXST_7037_Multivariate\\Final_Report\\Marx_Project\\train.csv")
dim(train)
View(train)
summary(train)
prop.table(table(train$Loan_Status))

# Finding Missing data and replacing with NA
train <- train[,-1]
table(train$Gender)
table(train$Married)
table(train$Dependents)
table(train$Self_Employed)
train$Self_Employed[train$Self_Employed==""]<- NA
train$Dependents[train$Dependents==""]<- NA
train$Married[train$Married==""] <- NA
train$Gender[train$Gender==""] <-NA

# Created a final CSV for data to work with
dim(train)
str(train)
summary(train)
table(train$Loan_Status)
write.csv(train,"C:\\Fall_Semester\\EXST_7037_Multivariate\\Final_Report\\Marx_Project\\qq.csv",append = F)
train<-(read.csv("C:\\Fall_Semester\\EXST_7037_Multivariate\\Final_Report\\Marx_Project\\qq.csv"))
str(train)
train<-train[,-1]
summary(train)
names(train)

# Plotting data to understand missingness in the data structure
md.pattern(train)


mice_plot <- aggr(train, col=c('blue','yellow'),
                  numbers=F, sortVars=TRUE,combined=F,
                  labels=names(train), cex.lab =1.8,cex.axis = 0.5,
                  gap=2, ylab=c("Missing data","Pattern"))
                   

dmy <- dummyVars(~.,data=train,fullRank = T)
train_w_dummy <- data.frame(predict(dmy,newdata=train))
colnames(train_w_dummy)[15] <- 'Loan_Status'
names(train_w_dummy)
View(train_w_dummy)


# Conducting a Little's test to check the type of missingness
LittleMCAR(train[,c(1:12)])

# Calculating the missing percentage
row_miss_pct <-apply(train[,-12],1,function(x) sum(is.na(x))/11)
row_miss_pct
test <- cbind.data.frame(train,row_miss_pct)
test
test[c(test$row_miss_pct>=.2),]

#--------------------For t test----------------------
train$miss.Credit.History <- ifelse(is.na(train$Credit_History),1,0)
train$miss.Self.Emp <- ifelse(is.na(train$Self_Employed),1,0)
train$miss.Loan.Amt <- ifelse(is.na(train$LoanAmount),1,0)
train$miss.Dependents <- ifelse(is.na(train$Dependents),1,0)
train$miss.Loan.Amt_Term <- ifelse(is.na(train$Loan_Amount_Term),1,0)
train$miss.Gender <- ifelse(is.na(train$Gender),1,0)
train$miss.Married <- ifelse(is.na(train$Married),1,0)

#------------------Making Categorical variables into integer values------------------------
train$Gender.1.0 <- as.factor(ifelse(train$Gender=='Male',1,0))
train$Married.1.0 <- as.factor(ifelse(train$Married=='Yes',1,0))
train$Education.1.0 <- as.factor(ifelse(train$Education=='Graduate',1,0))
train$Self_Employed.1.0 <- as.factor(ifelse(train$Self_Employed=='Yes',1,0))
train$Property_Area.1.0 <- as.factor(ifelse(train$Property_Area=='Urban',1,ifelse(train$Property_Area=='Semiurban',2,0)))
train$Dependents.1.0 <- as.factor(ifelse(train$Dependents=='1',1,ifelse(train$Dependents=='2',2,ifelse(train$Dependents=='0',0,3))))
train$Credit_History <- as.factor(train$Credit_History)
str(train)
names(train)
train_for_LT <- train[,c(6,7,8,9,10,20,21,22,23,24,25)]
str(train_for_LT)
names(train_for_LT)

#--------------------Little's Test after making factors numeric------------------
LittleMCAR(train_for_LT)


#----------------
str(train_w_dummy)
names(train)
str(train)
df_for_imput <- train[,c(20,21,22,23,24,25,6,7,8,9,10,12)]
str(df_for_imput)
names(df_for_imput)
View(df_for_imput)

md.pattern(df_for_imput)


df_for_imput[,c(1,2,3,4,5,6,11,12)]<- data.frame(apply(df_for_imput[,c(1,2,3,4,5,6,11,12)],2,as.factor))
df_for_imput$Gender.1.0 <- as.factor(df_for_imput$Gender.1.0)
str(df_for_imput)
df_for_imput$Loan_Status <- ifelse(df_for_imput$Loan_Status =="Y",1,0)
df_for_imput$Loan_Status <- as.factor(df_for_imput$Loan_Status)
table(df_for_imput$Loan_Status)
str(df_for_imput)


mice_plot1 <- aggr(df_for_imput, col=c('blue','yellow'),
                   numbers=F, sortVars=TRUE,combined=F,
                   labels=names(df_for_imput), cex.axis=.5,
                   gap=4, ylab=c("Missing data","Pattern"))


#----------------IMPUTING_USING_MICE_FOR_2_FACTOR_VAR----------------

df_mice <- mice(df_for_imput[,c(1,2,3,4,11,12)], m=5,method="logreg",seed=100)
df_mice

df_mice_G.M.C.S<- complete(df_mice)
table(df_for_imput$Gender.1.0)
table(df_mice_G.M.C.S$Gender.1.0)
sum(is.na(df_mice_G.M.C.S$Gender.1.0))

table(df_for_imput$Self_Employed.1.0)
table(df_mice_G.M.C.S$Self_Employed.1.0)

table(df_for_imput$Married.1.0)
table(df_mice_G.M.C.S$Married.1.0)

table(df_for_imput$Credit_History)
table(df_mice_G.M.C.S$Credit_History)

table(df_for_imput$Self_Employed.1.0)
table(df_mice_G.M.C.S$Self_Employed.1.0)

#--------------IMPUTING_USING_MICE_FOR_MULTICATEGOROCAL_VAR--------------

df_mice <- mice(df_for_imput[,c(5,6)], m=5,method="polr",seed=100)
df_mice
df_mice_G.M.C.S.D<- complete(df_mice)

table(df_for_imput$Dependents.1.0)
table(df_mice_G.M.C.S.D$Dependents.1.0)

#--------------IMPUTING_USING_MICE_FOR_CONTINUOUS_VAR--------------
df_mice <- mice(df_for_imput[,c(7,8,9,10)], m=5,method="norm.predict",seed=100)
df_mice
df_mice_G.M.C.S.D.LA<- complete(df_mice)

mean(df_for_imput$LoanAmount,na.rm = T)
sd(df_for_imput$LoanAmount,na.rm = T)
mean(df_mice_G.M.C.S.D.LA$LoanAmount)
sd(df_mice_G.M.C.S.D.LA$LoanAmount)

mean(df_for_imput$Loan_Amount_Term,na.rm = T)
sd(df_for_imput$Loan_Amount_Term,na.rm = T)
mean(df_mice_G.M.C.S.D.LA$Loan_Amount_Term)
sd(df_mice_G.M.C.S.D.LA$Loan_Amount_Term)

mice_plot2 <- aggr(df_mice_G.M.C.S.D.LA, col=c('blue','yellow'),
                   numbers=F, sortVars=TRUE,combined=F,
                   labels=names(df_mice_G.M.C.S.D.LA), cex.axis=.5,
                   gap=4, ylab=c("Missing data","Pattern"))

#-------------------PREPARING NO MISSING DATASET--------------
names(df_mice_G.M.C.S)
names(df_mice_G.M.C.S.D)
names(df_mice_G.M.C.S.D.LA)
final_data <- cbind.data.frame(df_mice_G.M.C.S,df_mice_G.M.C.S.D,df_mice_G.M.C.S.D.LA)
dim(final_data)
str(final_data)
final_data[,c(1:8)] <- data.frame(apply(final_data[,c(1:8)],2,as.factor))

Loan_Status<-data.frame(Loan_Status=final_data[,6])
final_data <- final_data[,-6]
final_data <- cbind.data.frame(final_data,Loan_Status)
str(final_data)
names(final_data)

mice_plot2 <- aggr(final_data, col=c('blue','yellow'),
                   numbers=F, sortVars=TRUE,combined=F,
                   labels=names(final_data), cex.axis=.5,
                   gap=4, ylab=c("Missing data","Pattern"))

#---------------------CREATING PARTITIONs------------
table(final_data$Loan_Status)
prop.table(table(final_data$Loan_Status))
index<-createDataPartition(final_data$Loan_Status,times = 1,p=.70,list = F)
index
train.final<-final_data[index,]
valid.final<-final_data[-index,]

dim(train.final)
dim(valid.final)
str(train.final)
str(valid.final)
summary(train.final)
summary(valid.final)
table(train.final$Loan_Status)
prop.table(table(train.final$Loan_Status))

table(valid.final$Loan_Status)
prop.table(table(valid.final$Loan_Status))


#--------------------MODEL RANDOM FOREST---------------------------------
library(randomForest)
set.seed(12345)
rf.train <- randomForest(Loan_Status~.,data=train.final,ntree=500,nodesize=35,
                         strata=train.final$Loan_Status)
confusionMatrix(predict(rf.train,valid.final[,-12]),valid.final$Loan_Status,positive = '1')
predict <- predict(rf.train,valid.final[,-12])
class(predict)
predict <- as.numeric(predict)

importance(rf.train)
#-------------ROC--------------

library(pROC)
?pROC
plot.roc(roc(predict,as.numeric(valid.final$Loan_Status)),col='blue',legacy.axes=T,print.auc = T,
         grid=T,auc.polygon = T)
library(ROCR)
xxx<- prediction(predict,valid.final$Loan_Status)
roc <- performance(xxx,"tpr","fpr")
plot(roc)




#--------------DECISION TREE----------------------
library(rpart)
tree2 <- rpart(Loan_Status~.,data = train.final,method="class")
library(rpart.plot)
rpart.plot(tree2,type=3,extra=2,fallen.leaves = T)

predict(tree2,valid.final[,-12],type="class")
confusionMatrix(predict(tree2,valid.final[,-12],type="class"),valid.final$Loan_Status,positive = '1')

plot.roc(roc(predict(tree2,valid.final[,-12],type="class"),as.numeric(valid.final$Loan_Status)),col='blue',legacy.axes=T,print.auc = T,
         grid=T,auc.polygon = T)



#---------------LOGISTIC REGRESSION------------------
logistic_FULL <- glm(Loan_Status~.,data = train.final,family = "binomial")
summary(logistic_FULL)
confusionMatrix((ifelse((predict(logistic_FULL,newdata =valid.final[,-12],type = "response"))>0.5,1,0)),valid.final$Loan_Status,positive='1')


logistic_NULL <- glm(Loan_Status~1,data = train.final,family = "binomial")
summary(logistic_NULL)


step(logistic_NULL,scope=list(lower=logistic_NULL,upper=logistic_FULL),direction = "forward")

step(logistic_FULL,scope=list(lower=logistic_NULL,upper=logistic_FULL),direction = "backward")

step(logistic_FULL,scope=list(lower=logistic_NULL,upper=logistic_FULL),direction = "both")

logistic_best <- glm(Loan_Status~Credit_History+Married.1.0+Property_Area.1.0,data = train.final,family = "binomial")
summary(logistic_best)
# #-----------MODEL on raw data---------------------
# train11 <- read.csv("C:\\Fall_Semester\\EXST_7037_Multivariate\\Final_Report\\Marx_Project\\train.csv")
# 
# dim(train11)
# str(train11)
# train11 <- train11[,-1]
# train11$Loan_Status <- as.factor(train11$Loan_Status)
# i<-createDataPartition(train11$Loan_Status,times = 1,p=.70,list = F)
# train0000.final<-train11[i,]
# valid0000.final<-train11[-i,]
# 
# dim(train0000.final)
# dim(valid0000.final)
# prop.table(table(train0000.final$Loan_Status))
# prop.table(table(valid0000.final$Loan_Status))
# str(valid0000.final)
# set.seed(100)
# 
# rf.train111 <- randomForest(Loan_Status~.,data=train0000.final,ntree=500,na.action=na.roughfix)
# confusionMatrix(predict(rf.train111,valid0000.final[,-12]),valid0000.final$Loan_Status)
# 
# #------------KMEANS CLUSTERING------------------------------------
# 
str(final_data)
dim(final_data)
data_f_kmeans<-final_data
data_f_kmeans[,c(1:7)] <- data.frame(apply(final_data[,c(1:7)],2,as.numeric))
str(data_f_kmeans)
set.seed(1000)
table(data_f_kmeans$Loan_Status)
km_clus <- kmeans(data_f_kmeans[,-12],1,iter.max = 20)
table(km_clus$cluster)

#install.packages("flexclust")
library(flexclust)
k<-table(final_data$Loan_Status,km_clus$cluster)
k
randIndex(k)
#----------CLUSTER pLOT------------------------

clusplot(data_f_kmeans[,-12], km_clus$cluster, color=TRUE, shade=T, 
         labels=2,main='2 Clusters using Kmeans')
clus <- km_clus$cluster
clus <- data.frame(clus)
clus
df_w_clus <- cbind.data.frame(final_data,clus)
str(df_w_clus)


table(df_w_clus$Loan_Status)

index<-createDataPartition(df_w_clus$Loan_Status,times = 1,p=.70,list = F)
index
train.df.w.clus<-df_w_clus[index,]
valid.df.w.clus<-df_w_clus[-index,]

str(train.df.w.clus)
#--------------MODEL with cluster var------------------------
set.seed(12223)
rf.train.clus <- randomForest(Loan_Status~.,data=train.df.w.clus,ntree=500,nodesize=35,
                         strata=train.df.w.clus$Loan_Status)
confusionMatrix(predict(rf.train.clus,valid.df.w.clus[,-12]),valid.df.w.clus$Loan_Status,positive = '0')

# 
# #-------------ROSE-------------------------
# library(ROSE)
# data_rose<-ROSE(Loan_Status ~ ., data = train.final,N=1500,seed = 1)$data
# dim(data_rose)
# table(data_rose$Loan_Status)
# 
# rf.train.rose <- randomForest(Loan_Status~.,data=data_rose,ntree=500,nodesize=35,
#                          strata=data_rose$Loan_Status)
# confusionMatrix(predict(rf.train.rose,valid.final[,-12]),valid.final$Loan_Status,positive = '0')
# 
# #---------------------Oversample--------------
# 
# data_rose_over_0 <- ovun.sample(Loan_Status ~ ., data = train.final, method = "over",N = 1000)$data
# dim(data_rose_over_0)
# table(data_rose_over_0$Loan_Status)
# 
# rf.train.rose <- randomForest(Loan_Status~.,data=data_rose_over_0,ntree=500,nodesize=35,
#                               strata=data_rose_over_0$Loan_Status)
# confusionMatrix(predict(rf.train.rose,valid.final[,-12]),valid.final$Loan_Status,positive = '0')
# 
# #------------------------------------------------------

#-----------Logistic Regression Full--------------
str(train.final)
logistic_F <- glm(Loan_Status~.,data=train.final,family = 'binomial')
summary(logistic_F)

#------------LOGISTIC NULL-------------
logistic_N <- glm(Loan_Status~1,data =train.final,family = "binomial")
summary(logistic_N)

#--------------STEPWISE----------------

step(logistic_N,scope=list(lower=logistic_N,upper=logistic_F),direction = "forward")


step(logistic_F,scope=list(lower=logistic_N,upper=logistic_F),direction = "backward")


step(logistic_F,scope=list(lower=logistic_N,upper=logistic_F),direction = "both")

#------------LOGISTIC_FINAL_MODEL-----------------------

log_final <- glm(Loan_Status~Credit_History+Married.1.0+Property_Area.1.0,data=train.final,family = 'binomial')
summary(log_final)
str(valid.final)
names(valid.final)
valid.logistic.final <- valid.final[,c(5,2,6,12)]
confusionMatrix(ifelse(predict(log_final,valid.logistic.final[,-4],type = "response")>0.5,1,0),valid.logistic.final$Loan_Status,positive = '1')

#--------------Hierarchical cluster------------

cluster_hier <-  hclust(dist(data_f_kmeans[,-12]))
plot(cluster_hier)

