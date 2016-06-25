
### IMPORT DATA

#Change working directory

setwd("~/Projects/Text-Mining-Project/analysis/data")

# LDA topic allocation
lda<-read.csv("LDA.csv", header=FALSE)
lda<-lda[-555,]


# Dictionary scores
dict<-read.csv("Dictionary_Scores.csv", header=FALSE)
dict<-t(dict)
dict<-apply(dict, 2, as.numeric)
dict<-scale(dict)
dict<-dict[-555,]


# TF-IDF weighting 
tfidf<-read.csv("Dictionary_Scores_tfidf.csv", header=FALSE)
tfidf<-t(tfidf)
tfidf<-scale(tfidf)

# Date of the speeches
date<-read.csv("dates.csv", header=FALSE)
date<-t(date)[,1]
date<-as.Date(date, "%d/%m/%Y")

 
# Risk Premium
rp <- read.csv("Risk_Premium.csv", sep="")


# Compute Target
# function that returns the risk premium of the days of the speeches
GetTarget<-function(data, country, date){
  
  # spread
  rp1<-data$values[rp$geo==country] - data$values[rp$geo=="DE"]
  
  # date of risk premium
  date1<-data[data$geo==country,3]
  
  # combine spread and date in data.frame
  X<-data.frame(time=date1, spread=rp1)
  X$time<-as.Date(X$time, "%Y-%m-%d")
  X<-X[X$time>"1998-01-1",]
  
  # find index of dates
  
  index<-rep(NA,length(date))
  
  for( i in 1:length(index)){
    
    if(date[i] %in% X$time){
      
      ind<-which(X$time==date[i])
      ind2<-which(!is.na(X$spread[ind:(ind+10)])==TRUE)[1]
      
      #index[i]<-which(X$time==date[i])
      index[i]<-ind+ind2-1
      
    } else {
      
      ind<-which(X$time %in% c(date[i]+1,date[i]+2, date[i]+3,date[i]+4, date[i]+5))[1]
      ind2<-which(!is.na(X$spread[ind:(ind+10)])==TRUE)[1]
      #index[i]<-which(X$time %in% c(date[i]+1,date[i]+2, date[i]+3,date[i]+4, date[i]+5))[1]
      index[i]<-ind+ind2-1
    }
    
  }
  
  # fill target
  target<-rep(NA,length(date))
  
  for( i in 1:length(target)){
    
    target[i]<-X$spread[index[i]]
  }
  
  return(list(target=target, index=index, X=X))
  
}

Result<-GetTarget(rp, "ES", date)
index<-Result$index
X<-Result$X
target<-Result$target

target<-target[-555]
target<-log(target+1)

# Data preparation
X<-cbind(lda, dict, tfidf)
dict_names<-c("Negative", "Positive", "Uncertainty", "Litigious", "Constraining", "Superfluous","Interesting", "Harvard_IV")

colnames(X)[1:20]<-paste("Topic_", 1:20, sep="")
colnames(X)[21:28]<-dict_names
colnames(X)[29:36]<-paste("tfidf_", dict_names, sep="")


# Model
model<-lm(target ~. , data=X[,-1])
summary(model)


# Stepwise Regression
library(MASS)
stepwise <- stepAIC(model, direction="both")
stepwise$anova # display results

model_step<-lm(target ~ Topic_5 + Topic_8 + Topic_9 + Topic_10 + Topic_11 + 
                 Topic_12 + Topic_14 + Topic_17 + Topic_20 + Positive + Litigious + 
                 Constraining + tfidf_Negative + tfidf_Harvard_IV, data=X[,-c(1,23)])
summary(model_step) # 13 variables



# Variable Selection with Leaps Package
library(leaps)
leaps<-regsubsets(target~. ,data=X[,-1],nbest=1)
summary(leaps)

# best leaps model 
model_leaps<-lm(target ~ Topic_5 + Topic_10 + Topic_11 + Topic_14 + Topic_20 + Positive + tfidf_Negative +tfidf_Harvard_IV, data=X[,-1])
summary(model_leaps)

### PLOTS

# Topic
topic<-apply(lda,1,which.max)
topic_score<-apply(lda,1,max)
topic_top<-topic[461]
topic_top


# Plot LDA scores
#png("/home/yaroslav/Dropbox/BGSE/3rd Term/Text Mining/Project/document/lda.png", width = 400, height = 350)
par(mfrow=c(3,2))
for( i in c(5,6,10,11,14,20)){  #4
  plot(date[-555],lda[,i], t="h", main=paste("Topic",i), ylab="",xlab="",col="darkblue")
  grid()
}
#dev.off()




# Plot Dictionary scores
#png("/home/yaroslav/Dropbox/BGSE/3rd Term/Text Mining/Project/document/dict_1.png", width = 400, height = 350)
par(mfrow=c(2,2))
for( i in 1:4){
  plot(date[-555],dict[,i], t="h", main=dict_names[i], col="darkblue", xlab="", ylab="")
  grid()
}
#dev.off()

#png("/home/yaroslav/Dropbox/BGSE/3rd Term/Text Mining/Project/document/dict_2.png", width = 400, height = 350)
par(mfrow=c(2,2))
for( i in 5:8){
  plot(date[-555],dict[,i], t="h", main=dict_names[i], col="darkblue", xlab="", ylab="")
  grid()
}
#dev.off()



# Plot risk premium
date_rp<-as.Date(rp$time[rp$geo=="ES"], "%Y-%m-%d")
rp_es<-rp$values[rp$geo=="ES"] - rp$values[rp$geo=="DE"]
ok<-4000:9503 # plot since 1995

par(mfrow=c(1,1))
#png("/home/yaroslav/Dropbox/BGSE/3rd Term/Text Mining/Project/document/risk_premium.png", width = 380, height = 300)
plot(date_rp[ok], rp_es[ok], t="l", main="Spain's risk premium", 
     col="darkblue", ylab="", xlab ="", lwd=1.2)
grid()
abline(v=as.Date("2012-27-07", "%Y-%d-%m"), lty=2, lwd=1.8)
#dev.off()

# md tables of models for the paper
 library(pander)
 pandoc.table(round(summary(model_step)$coefficients,3))
 pandoc.table(round(summary(model_leaps)$coefficients,3))

