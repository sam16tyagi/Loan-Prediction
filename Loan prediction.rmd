---
title: "Loan Prediction"
---
##Creating training and testing datasets  
```{r}
lp_train<-read.csv("D:/Praxis/2nd Term/kaggle/loan pred/train.csv")
lp_test<-read.csv("D:/Praxis/2nd Term/kaggle/loan pred/test.csv")
head(lp_train,n=1)
head(lp_test,n=1)
lp_test$Loan_Status<-NA
lp_train<-lp_train[,-1]
lp_test<-lp_test[,-1]
lp<-rbind(lp_train,lp_test)
lp_temp1=read.csv("D:/Praxis/2nd Term/kaggle/loan pred/test.csv")
```
##Understanding the variables
```{r}
str(lp)
summary(lp)
##converting credit history to factor as it has only two levels
lp$Credit_History=as.factor(as.character(lp$Credit_History))
```
##Function for boxplot
```{r, message=FALSE, warning=FALSE}
library(grid)
library(gridExtra)
bplt=function(data,x,y)
{

for(i in y)
{
bp<-ggplot(data,aes(y=data[,i],x=data[,x]))+geom_boxplot(na.rm =T)+
xlab(colnames(data[x]))+ylab(colnames(data[i]))
return(bp)
}
}  

dbplots <- function(data,fun,ncol,x) 
{
col=ncol(data)
k=1
j<-numeric()
for(i in 1:col)
{
if(is.numeric(data[,i])| (is.integer(data[,i]))==TRUE)
{
j[k]<-i
k=k+1
}
}
plt <- list()
for (i in j) 
{
  p <- fun(data=data,y=i,x=x)
  plt<- c(plt, list(p))
}
  m1<-marrangeGrob(plt, ncol=ncol,nrow=2)
  m1

}
```
##Functions for histograms
```{r}
hplt=function(data,x)
{
for(i in x)
{
hp<-ggplot(data,aes(x=data[,x]))+geom_histogram(bins = 30,na.rm = T)+
xlab(colnames(data[x]))+ylab("Count")
return(hp)
}
}

dhplots <- function(data,fun,ncol) 
{
col=ncol(data)
k=1
j<-numeric()
for(i in 1:col)
{
if(is.numeric(data[,i])| (is.integer(data[,i]))==TRUE)
  {
j[k]<-i
k=k+1
  }
}
plt <- list()
for (i in j) 
  {
  p <- fun(data=data,x=i)
  plt<- c(plt, list(p))
}
m1<-marrangeGrob(plt, ncol=ncol,nrow=2)
m1
}
```

##Functions for density plots

```{r}
dplt=function(data,x)
{
for(i in x)
{
dp<-ggplot(data,aes(x=data[,x]))+geom_density(na.rm = T)+
xlab(colnames(data[x]))+ylab("Count")
return(dp)
} 
}

ddplots <- function(data,fun,ncol) 
{
col=ncol(data)
k=1
j<-numeric()
for(i in 1:col)
{
if(is.numeric(data[,i])| (is.integer(data[,i]))==TRUE)
{
j[k]<-i
k=k+1
}
}
plt <- list()
  for (i in j) {
    p <- fun(data=data,x=i)
    plt<- c(plt, list(p))
  }
m1<-marrangeGrob(plt, ncol=ncol,nrow=2)
m1
}

```


##Function for barplot

```{r}
brplt=function(data,x)
{
for(i in x)
{
bp<-ggplot(data,aes(x=data[,i]))+geom_bar(na.rm = T,stat = "count")+
xlab(colnames(data[i]))+ylab("Count")
return(bp)
}
}

dbrplots <- function(data,fun,ncol) 
{
col=ncol(data)
k=1
j<-numeric()
for(i in 1:col)
{
if(is.factor(data[,i])==TRUE)
  {
j[k]<-i
k=k+1
  }
}
plt <- list()
for (i in j) 
  {
  p <- fun(data=data,x=i)
  plt<- c(plt, list(p))
}
m1<-marrangeGrob(plt, ncol=ncol,nrow=2)
m1
}

```

##BoxPlots
```{r}
library(ggplot2)

dbplots(lp_train[,-1],fun=bplt,x=1,ncol=2)
dbplots(lp_test[,-1],fun=bplt,x=1,ncol=2)
```

##Histogram
```{r}
dhplots(fun=hplt,data = lp_train,ncol = 2)
dhplots(fun=hplt,data = lp_test,ncol = 2)
```

##Density plots
```{r}
ddplots(fun=dplt,data=lp,ncol = 2)
```
##Bar plots

```{r}
dbrplots(data = lp_train,fun = brplt,ncol = 2)
dbrplots(data = lp_test,fun = brplt,ncol = 2)
```

##correlation plot
```{r}
cplot=function(data)
{
library(corrplot)
ncol=ncol(data)
k=1
j<-numeric()

for(i in 1:ncol)
{
if(is.numeric(data[,i])| (is.integer(data[,i]))==TRUE)
{
j[k]<-i
k=k+1
}
}
correlations<-cor(x=data[j],y=NULL,use = "pairwise.complete.obs")
print(correlations)
return(corrplot(correlations,method = "circle",title = "Correlation Plot",
              tl.cex=0.5,cl.cex =0.5 ,mar = c(3,3,3,3)))
}
cplot(data=lp)


```
##Replacing the extra empty levels with NA

```{r}
levels(lp$Married)[1]<-NA
levels(lp$Gender)[1]<-NA
levels(lp$Dependents)[1]<-NA
levels(lp$Self_Employed)[1]<-NA
```

##Missing values and outliers

```{r}
which(rowSums(is.na(lp[,-12]))>2)
lp<-lp[-c(96,436),]
```

##Imputing values for loan_amout term

```{r}
library(scales)
lp[is.na(lp$Loan_Amount_Term),'Loan_Amount_Term']<-360

##some of the loan amount terms are below 50 like 12,36 and 6 which can be errors and it is replaced with 120,360 and 60 respectively

which(lp$Loan_Amount_Term<50)
lp[c(262,545,829),9]<-360
lp[c(496,757),9]<-120
lp[c(938),'Loan_Amount_Term']<-60
lp[lp$Loan_Amount_Term==84,9]<-480
lp[lp$Loan_Amount_Term==300,9]<-360
lp[lp$Loan_Amount_Term==350,9]<-360



##Married

##Married Males are approved more loans than females overall and single females are approved more loans

ggplot(na.omit(lp),aes(x=Loan_Status,group=Gender))+geom_bar(aes(y=..prop..,fill=Gender),
position ="dodge",stat="count")+facet_grid(.~Married)+
  scale_y_continuous(labels=percent_format())

ggplot(na.omit(lp),aes(x=Married,group=Gender))+geom_bar(aes(y=..prop..,fill=Gender),
position ="dodge",stat="count")+facet_grid(.~Dependents)+
  scale_y_continuous(labels=percent_format())

gm<-lp[lp$Married=="Yes",]
gs<-lp[lp$Married=="No",]
dim(gs)
dim(gm)
length(which(gs$CoapplicantIncome==0))
length(which(gm$CoapplicantIncome==0))

##Based on the median of the loan amount, and if coapplicant income is zero then married is "NO" and viceversa

which(is.na(lp$Married))
lp[104,'Married']<-"Yes"
lp[228,'Married']<-"No"
summary(lp)

prop.table(table(lp_train$Gender,lp_train$Married))

```


##Missing values many for factor variables take that as individual levels so model learns it

```{r}
levels(lp$Credit_History)[3]<-"MV"
levels(lp$Self_Employed)[3]<-"MV"
levels(lp$Dependents)[5]<-"MV"
levels(lp$Gender)[3]<-"MV"
```

##Credit history

```{r}

lp_train<-lp[!is.na(lp$Loan_Status),]
lp_test<-lp[is.na(lp$Loan_Status),]

lp_train[is.na(lp_train$Credit_History),'Credit_History']<-"MV"
lp_test[is.na(lp_test$Credit_History),'Credit_History']<-"MV"


```
##Gender

```{r}

lp_train[is.na(lp_train$Gender),'Gender']<-"MV"
lp_test[is.na(lp_test$Gender),'Gender']<-"MV"

```
##Dependents

```{r}
lp_train[is.na(lp_train$Dependents),'Dependents']<-"MV"
lp_test[is.na(lp_test$Dependents),'Dependents']<-"MV"

```
##Self-Employed

```{r}
lp_train[is.na(lp_train$Self_Employed),'Self_Employed']<-"MV"
lp_test[is.na(lp_test$Self_Employed),'Self_Employed']<-"MV"
```

##Loan Amount

```{r}

##train set
na_latr<-lp_train[is.na(lp_train$LoanAmount),c(1:11)]
nna_latr<-lp_train[!is.na(lp_train$LoanAmount),c(1:11)]

##test set
na_lats<-lp_test[is.na(lp_test$LoanAmount),c(1:11)]
nna_lats<-lp_test[!is.na(lp_test$LoanAmount),c(1:11)]

##predicting loan amount for imputation

library(caret)
tr_cntl=trainControl(method="cv",number=10)
imp_latr <- caret::train(y=nna_latr$LoanAmount,x=nna_latr[,-8],
            method = "rf",trControl=tr_cntl)
predicts_latr<-predict(imp_latr,newdata=na_latr,type="raw")
lp_train[is.na(lp_train$LoanAmount),8]<-predicts_latr

##test set
imp_lats <- train(y=nna_lats$LoanAmount,x=nna_lats[,-8],
            method = "rf",trControl=tr_cntl)
predicts_lats<-predict(imp_lats,newdata=na_lats,type="raw")
lp_test[is.na(lp_test$LoanAmount),8]<-predicts_lats


```

##Bringing back train and test to create new variables

```{r}

library(scorecard)
library(woe)

lp<-rbind(lp_train,lp_test)

```

## Loan amount/applicant income
```{r}

lp$tl_income<-rowSums(lp[,6:7])

##Creating a factor variable coapplicant available or not

for(i in 1:nrow(lp))
{
  if(lp$CoapplicantIncome[i]>0)
  {
    lp$coapp[i]<-"Yes"
  }
  if(lp$CoapplicantIncome[i]==0)
  {
    lp$coapp[i]<-"No"
  }
}


##converting the income to same units as loan amount

lp$tl_income<-sapply(1:nrow(lp),function(x){lp$tl_income[x]/1000})

##To avoid inf value while creating ratio

lp$coapp<-as.factor(lp$coapp)

for(i in 1:nrow(lp))
{
 if(lp$tl_income[i]==0 )
 {
   lp$tl_income[i]<-1
 }
}

lp$ratio=lp$LoanAmount/(lp$tl_income)

##To convert into approximate emi

lp$emi<-sapply(1:nrow(lp),function(x){lp$LoanAmount[x]/lp$Loan_Amount_Term[x]})

##Income to emi ratio

lp$rat_ei<-sapply(1:nrow(lp),function(x){(lp$emi[x]/lp$tl_income[x])*100})

##Income to dependents

##Lets assume every household has 

##20% expenses for 0 dependents
##40% expenses for 1 dependents
##60% expenses for 2 dependents
##80% expenses for 3 dependents

lp$Dependents<-as.numeric(lp$Dependents)
  for (x in 1:nrow(lp))
  {
  if(lp$Dependents[x]==1)
  {lp$dis_inc[x]<-(lp$tl_income[x]-lp$emi[x])-(lp$tl_income[x]*0.2)}
  if(lp$Dependents[x]==2)
  {lp$dis_inc[x]<-(lp$tl_income[x]-lp$emi[x])-(lp$tl_income[x]*0.4)}
  if(lp$Dependents[x]==3)
  {(lp$tl_income[x]-lp$emi[x])-(lp$tl_income[x]*0.6)}
  if(lp$Dependents[x]==4)
  {lp$dis_inc[x]<-(lp$tl_income[x]-lp$emi[x])-(lp$tl_income[x]*0.8)}
  if(lp$Dependents[x]==5)
  {lp$dis_inc[x]<-(lp$tl_income[x]-lp$emi[x])-(lp$tl_income[x]*0.5)}
  }

lp$Loan_Amount_Term<-as.factor(lp$Loan_Amount_Term)

```

##Woe and glm
 
```{r}
lp_woebin<-woebin(dt = lp,y = "Loan_Status",x=colnames(lp[,-c(12)]),
               positive ="Y")
lp_woe<-woebin_ply(lp,lp_woebin)
lp_woe_glm<-glm(data = lp_woe,formula =lp$Loan_Status~.,family = "binomial")

##logistic regression again with significant variables

lp_woe_glm_sig<-glm(data = lp_woe,
      Loan_Status~dis_inc_woe+ratio_woe+tl_income_woe+Property_Area_woe
+Credit_History_woe+Loan_Amount_Term_woe+LoanAmount_woe+ApplicantIncome_woe+Education_woe,family = "binomial")
summary(lp_woe_glm_sig)

##Scorecard

lp_scorecard<-scorecard(lp_woebin,lp_woe_glm_sig)
lp_scorecard_org<-scorecard_ply(lp,card = lp_scorecard,only_total_score = TRUE)

lp_temp_scard<-lp
lp_temp<-cbind(lp,lp_scorecard_org)


lp_train<-lp_temp[!is.na(lp_temp$Loan_Status),]
lp_test<-lp_temp[is.na(lp_temp$Loan_Status),]


tncnt3<-trainControl(method = "cv",number = 10,savePredictions = TRUE,classProbs = TRUE)

lp_scard_glm<-train(y=lp_train$Loan_Status,x=lp_train[,19],
                    method = "glm",maxit=100,trControl=tncnt3)
lp_scard_pre_glm<-predict(lp_scard_glm,newdata=lp_test)

submitscardglm <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = lp_scard_pre_glm)
write.csv(submitscardglm, "submitscardglm.csv")
table(lp_scard_pre_glm)


```


##Transformation variables
```{r}
##log transformation of all the numeric variables
for(i in 1:nrow(lp_temp))
  {
  if(lp_temp$ApplicantIncome[i]==0)
    {
    lp_temp$ApplicantIncome[i]<-1
    }
  if(lp_temp$CoapplicantIncome[i]==0)
    {
    lp_temp$CoapplicantIncome[i]<-1
    }

  }
lp_temp$ApplicantIncome<-sapply(1:nrow(lp_temp),function(x){lp_temp$ApplicantIncome[x]/1000})
lp_temp$CoapplicantIncome<-sapply(1:nrow(lp_temp),function(x){lp_temp$CoapplicantIncome[x]/1000})
str(lp_temp)
```

##Removing variables

```{r}
##Removing applicant,coapplicant and loan amount
lp_back<-lp_temp
lp_temp<-lp_temp[,-c(4,6,7,8,9,13,16)]

```

##Creating dummy variables using caret

```{r}
library(caret)
dmy_lp<-dummyVars(data=lp_temp[,-7],formula =~.,levelsOnly = FALSE,fullRank = TRUE)
lp_dmy_pre<-predict(dmy_lp,lp_temp)
lp_temp<-cbind(lp_temp[,7],lp_dmy_pre)
```

##Bringing the train and test datasets

```{r}
lp_train<-lp_temp[!is.na(lp_temp$Loan_Status),]
lp_test<-lp_temp[is.na(lp_temp$Loan_Status),]

```

##Model
```{r}
set.seed(290391)
library(caret)
tr_cntl=trainControl(method="cv",number=10)
model_rf_lp1 <- train(y=lp_train$Loan_Status,x=lp_train[,-c(1)],
            method = "rf",ntree=1000,trControl=tr_cntl,importance=T)

library(dplyr)
lp_imp<-as.data.frame(varImp(model_rf_lp1)$importance)
lp_imp$names<-row.names(lp_imp)
imp_idx<-lp_imp %>% arrange(desc(lp_imp[,1]))
imp_names<-imp_idx$names[1:7]

which(colnames(lp_train)%in%(imp_names))

model_rf_lp <- caret::train(y=lp_train$Loan_Status,
        x=lp_train[,c(8,9,10,13,14,15,16)],
          method = "rf",ntree=1000,trControl=tr_cntl)
predicts_lp<-predict(model_rf_lp,newdata=lp_test,type="raw")
table(predicts_lp)
```
##Submission
  
```{r}
submitrf <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predicts_lp)
write.csv(submitrf, "submitrf.csv")
	
```
##MOdel GLM
```{r}
lp_final_glm<-train(y=lp_train$Loan_Status,x=lp_train[,c(8,9,10,13,14,15,16)],
                    method = "glm",maxit=100)
predicts_lp_final_glm<-predict(lp_final_glm,newdata=lp_test)

submitglm <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predicts_lp_final_glm)
write.csv(submitglm, "submitglm.csv")
table(predicts_lp_final_glm)

```

##Model3
```{r}
##decision trees-c5.0
model_final_dt<-train(y=lp_train$Loan_Status,x=lp_train[,c(16)],method = "rpart",trControl=tr_cntl)
predicts_final_dt<-predict(model_final_dt,newdata=lp_test)

submitdt <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predicts_final_dt)
write.csv(submitdt, "submitdt.csv")
table(predicts_final_dt)
```
##Model4
```{r}
##svm
tr_cntl1<-trainControl(method="cv",number=10,classProbs = TRUE)
model_final_lda<-train(y=lp_train$Loan_Status,x=lp_train[,c(16)],method = "lda",trControl=tr_cntl1,tunelength=10)

predicts_final_lda<- stats::predict(model_final_lda, newdata=lp_test)
table(predicts_final_lda)

```

##Submission

```{r}
submitlda <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predicts_final_lda)
write.csv(submitlda, "submitlda.csv")
	

```

##XGBOOST
```{r}
library(xgboost)
ControlParamteres <- trainControl(method = "cv",
                  number = 5,savePredictions = TRUE,classProbs = TRUE)

parametersGrid <-  expand.grid(eta=c(0.5,0.05),colsample_bytree=c(0.5,0.7),
          max_depth=c(10),nrounds=500,gamma=15,
min_child_weight=c(2,10),subsample=c(0.4,0.7))

modelxgboost <- caret::train(y=lp_train$Loan_Status,
              x=lp_train[,-c(1)],method="xgbTree",
                             trControl=ControlParamteres,tuneGrid=parametersGrid)

predicts_final_xgb<- predict(modelxgboost, newdata=lp_test)
table(predicts_final_xgb)

submitxgb <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predicts_final_xgb)
write.csv(submitxgb, "submitxgb.csv")
```

##Stack modelling

```{r}
predicts_lp_st<-predict(model_rf_lp,newdata=lp_test,type="prob")
predicts_glm_st<-predict(lp_final_glm,newdata=lp_test,type="prob")
predicts_dt_st<-predict(model_final_dt,newdata=lp_test,type="prob")
predicts_xgb_st<- predict(modelxgboost, newdata=lp_test,type="prob")

predict_avg<-((predicts_lp_st$Y*0.35)+(predicts_glm_st$Y*0.20)+(predicts_xgb_st$Y*0.25)
              +(predicts_dt_st$Y*0.20)/4)
predict_avg_lp<-as.factor(ifelse(predict_avg>0.5,"Y","N"))
table(predict_avg_lp)

submit_preavg <- data.frame(Loan_ID=lp_temp1$Loan_ID,Loan_Status = predict_avg_lp)
write.csv(submit_preavg, "submitpreavg1.csv")

```
