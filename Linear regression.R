                      #  Project 2 - Linear Regression

#  Getting the data
library(readxl)
mydata<- read_xlsx("E:\\Analytixlabs\\Module 6 (Data science using R)\\Case Studies\\Case study 2 - Linear Regression\\Linear Regression Case.xlsx")
View(mydata)
dim(mydata)
str(mydata)
names(mydata)

mydata$spoused[mydata$spoused==-1]<- 0
mydata$carvalue[mydata$carvalue==-1]<-0

#  The first 83 variables of the dataset are the variables of interest. The remaining 
# variables are the teleco data which are of no use here
mydata<- mydata[,1:83]
names(mydata)
mydata$total_spend<- mydata$cardspent + mydata$card2spent
mydata$total_items<- mydata$carditems + mydata$card2items
mydata$cardspent<- NULL
mydata$card2spent<- NULL
mydata$carditems<- NULL
mydata$card2items<- NULL

    #  Step 1: Explanatory Data analysis and Data cleaning/preparation
#     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

#  Getting all the continuous variables
var_con<- c("age","ed","employ","income","lninc","debtinc","creddebt","lncreddebt",
            "othdebt","lnothdebt","spoused","reside","pets","pets_cats","pets_dogs",
            "pets_birds","pets_reptiles","pets_small","pets_saltfish","pets_freshfish",
            "address","cars","carvalue","commutetime","cardtenure","card2tenure",
            "total_items","total_spend")

# User defined function for calculating descriptives

udf<- function(x){
  n<-length(x)
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<- mean(a)
  max<- max(a)
  min<- min(a)
  p1<-quantile(a,0.01)
  p5<- quantile(a,0.05)
  p95<- quantile(a,0.95)
  p99<-quantile(a,0.99)
  
  return(c(count=n,nmiss=nmiss,mean=m,max=max,min=min,P1=p1,P5=p5,P95=p95,P99=p99))  
}
options(scipen = 999)
desc_stats<- data.frame(t(apply(mydata[var_con],2,udf)))
write.csv(desc_stats,"Stats.csv")

#  Outlier treatment:

udf2<- function(x){
  p5<- quantile(x,0.05,na.rm = T)
  p95<- quantile(x,0.95,na.rm = T)
  x[x<p5]<- p5   # Any value less than p5 are treated as Outlier
  x[x>p95]<- p95 # Any value greater than p95 are treated as Outlier
  return(x)
}
mydata[var_con]<- data.frame(apply(mydata[var_con],2,udf2))

# Missing value treatment:
mydata$lncreddebt[is.na(mydata$lncreddebt)]<- -0.130453522
mydata$lnothdebt[is.na(mydata$lnothdebt)]<- 0.69691526
mydata$commutetime[is.na(mydata$commutetime)]<- 25.34553822

#  Getting all the categorical variables:

var_cat<- (!names(mydata) %in% var_con)
View(var_cat)

udf3<- function(x){
  n<- length(x)
  nmiss<- sum(is.na(x))
  return(c(n=n,nmiss=nmiss))
}  
  
desc_stats_cat<- data.frame(t(apply(mydata[var_cat],2,udf3)))  
write.csv(desc_stats_cat,"stats_cat.csv") 
# We have 2 missing values in the 'townsize' variable. We'll impute the missing values
# with the most frequent value
prop.table(table(mydata$townsize))
mydata$townsize[is.na(mydata$townsize)]<- 1

######################################################################################
             #   Checking for normality
hist(mydata$total_spend)
mydata$lntotal_spend<- log(mydata$total_spend)
hist(mydata$lntotal_spend)
hist(mydata$income)
hist(mydata$lninc) # We take this variable because its distribution is more normal than 'income'
hist(mydata$debtinc)
hist(mydata$creddebt)
hist(mydata$lncreddebt) # We take this variable 
hist(mydata$othdebt)
hist(mydata$lnothdebt)  # We take this variable


           #  Step 2: Variable Reduction
#            _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

# Selection of significant categorical variables using ANOVA test:
var_con<- c(var_con,"lntotal_spend")
var_cat<- (!names(mydata) %in% var_con)
names(mydata[var_cat])

summary(aov(lntotal_spend ~ region,data = mydata)) # Significant
summary(aov(lntotal_spend ~ townsize,data = mydata))
summary(aov(lntotal_spend ~ gender,data = mydata)) # Significant
summary(aov(lntotal_spend ~ agecat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ birthmonth,data = mydata))
summary(aov(lntotal_spend ~ edcat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ jobcat,data = mydata))
summary(aov(lntotal_spend ~ union,data = mydata))
summary(aov(lntotal_spend ~ empcat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ retire,data = mydata)) # Significant
summary(aov(lntotal_spend ~ inccat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ default,data = mydata))
summary(aov(lntotal_spend ~ jobsat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ marital,data = mydata))
summary(aov(lntotal_spend ~ spousedcat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ homeown,data = mydata)) # Significant
summary(aov(lntotal_spend ~ hometype,data = mydata))
summary(aov(lntotal_spend ~ addresscat,data = mydata)) # Significant 
summary(aov(lntotal_spend ~ carown,data = mydata))  # Significant
summary(aov(lntotal_spend ~ cartype,data = mydata))
summary(aov(lntotal_spend ~ carcatvalue,data = mydata)) # Significant
summary(aov(lntotal_spend ~ carbought,data = mydata))
summary(aov(lntotal_spend ~ carbuy,data = mydata))
summary(aov(lntotal_spend ~ commute,data = mydata))
summary(aov(lntotal_spend ~ commutecat,data = mydata))
summary(aov(lntotal_spend ~ commutecar,data = mydata))
summary(aov(lntotal_spend ~ commutemotorcycle,data = mydata))
summary(aov(lntotal_spend ~ commutecarpool,data = mydata))
summary(aov(lntotal_spend ~ commutebus,data = mydata))
summary(aov(lntotal_spend ~ commuterail,data = mydata))
summary(aov(lntotal_spend ~ commutepublic,data = mydata))
summary(aov(lntotal_spend ~ commutebike,data = mydata)) # Significant
summary(aov(lntotal_spend ~ commutewalk,data = mydata))
summary(aov(lntotal_spend ~ commutenonmotor,data = mydata))
summary(aov(lntotal_spend ~ telecommute,data = mydata))
summary(aov(lntotal_spend ~ reason,data = mydata))
summary(aov(lntotal_spend ~ polview,data = mydata))
summary(aov(lntotal_spend ~ polparty,data = mydata))
summary(aov(lntotal_spend ~ polcontrib,data = mydata))
summary(aov(lntotal_spend ~ vote,data = mydata)) # Significant
summary(aov(lntotal_spend ~ card,data = mydata)) # Significant
summary(aov(lntotal_spend ~ cardtype,data = mydata)) 
summary(aov(lntotal_spend ~ cardbenefit,data = mydata))
summary(aov(lntotal_spend ~ cardfee,data = mydata))
summary(aov(lntotal_spend ~ cardtenurecat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ card2,data = mydata)) # Significant
summary(aov(lntotal_spend ~ card2type,data = mydata))
summary(aov(lntotal_spend ~ card2benefit,data = mydata))
summary(aov(lntotal_spend ~ card2fee,data = mydata))
summary(aov(lntotal_spend ~ card2tenurecat,data = mydata)) # Significant
summary(aov(lntotal_spend ~ active,data = mydata))
summary(aov(lntotal_spend ~ bfast,data = mydata))

#  The significant categorical variables we got after performing ANOVA test:

# region,gender,agecat,edcat,empcat,retire,inccat,jobsat,
# spousedcat,homeown,addresscat,carown,carcatvalue,commutebike,
# vote,card,cardtenurecat,card2,card2tenurecat

# The Potential Continuous variables are:

# age,ed,employ,lninc,debtinc,lncreddebt,lnothdebt,spoused,reside,pets,pets_cats,
# pets_dogs,pets_birds,pets_small,pets_freshfish,address,
# cars,carvalue,commutetime,cardtenure,card2tenure

fit1<- lm(lntotal_spend ~ age+ed+employ+lninc+debtinc+lncreddebt+lnothdebt+spoused+
            reside+pets+pets_cats+pets_dogs+pets_birds+pets_small+
            pets_freshfish+address+cars+carvalue+commutetime+cardtenure+
            card2tenure+region+gender+agecat+edcat+empcat+retire+inccat+jobsat+
            spousedcat+homeown+addresscat+carown+carcatvalue+commutebike+vote+card+
            cardtenurecat+card2+card2tenurecat,data = mydata)
summary(fit1)

#  Stepwise Linear regression:
library(MASS)
step<- stepAIC(fit1,direction = "both")
ls(step)
step$anova
#  The final model we get after applying stepAIC method
# lntotal_spend ~ age + lninc + lncreddebt + address + region + gender + 
#            jobsat + addresscat + commutebike + card + card2,data = mydata

#######################################################################################

        #  Final Model Building
#        _ _ _ _ _ _ _ _ _ _ _ _ _ _

#  Conevrting all categorical variables into factors
mydata$region<- as.factor(mydata$region)
mydata$gender<- as.factor(mydata$gender)
mydata$jobsat<- as.factor(mydata$jobsat)
mydata$addresscat<- as.factor(mydata$addresscat)
mydata$commutebike<- as.factor(mydata$commutebike)
mydata$card<- as.factor(mydata$card)
mydata$card2<- as.factor(mydata$card2)

#  Splitting the dataset into "Developement" and "validation" datasets

set.seed(222)
ind<- sample(2,nrow(mydata),replace = TRUE,prob = c(0.7,0.3))
dev<- mydata[ind==1,]
val<- mydata[ind==2,]

#  Building model on dev dataset

fit3<- lm(lntotal_spend ~ age + lninc + lncreddebt + address + region + gender + 
            jobsat + addresscat + commutebike + card + card2,data = dev)
summary(fit3)
library(car)
vif(fit3)

#  Building the model with significant variables

fit4<- lm(lntotal_spend ~ lninc + gender + addresscat + card + card2,data = dev)
summary(fit4)
vif(fit4)

#  Removing the Influential observations using Cook's D method

dev$cd<- cooks.distance(fit4)
dev1<- subset(dev,cd<(4/3500))

final_model<- lm(lntotal_spend ~ lninc + gender + addresscat + card + card2,
                 data = dev1)
summary(final_model)

dev1$cd1<-cooks.distance(final_model)
dev2<- subset(dev1,cd1<(4/3360))

final_model2<- lm(lntotal_spend ~ lninc + gender + addresscat + card + card2,
                  data = dev2)
summary(final_model2)
######################################################################################

     #  Predicting the Total spend in dev and val datasets:
#     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

pred1<- exp(predict(final_model2,newdata = dev))
dev<- cbind(dev,pred_spend=pred1)

pred2<- exp(predict(final_model2,newdata = val))
val<- cbind(val,pred_spend=pred2)

       #  Decile Analysis:
#       _ _ _ _ _ _ _ _ _ _ _ 

dec_loc<- quantile(dev$pred_spend,probs = seq(0.1,0.9,by=0.1))
dev$decile<- findInterval(dev$pred_spend,c(-Inf,dec_loc,+Inf))

library(sqldf)

dev_decile<- sqldf("select decile,count(decile) as Count,
                   avg(total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from dev
                   group by decile
                   order by decile desc")
write.csv(dev_decile,"dev_decile.csv")

dec_loc<- quantile(val$pred_spend,probs = seq(0.1,0.9,by=0.1))
val$decile<- findInterval(val$pred_spend,c(-Inf,dec_loc,Inf))

val_decile<- sqldf("select decile,count(decile) as Count,
                   avg(total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from val
                   group by decile
                   order by decile desc")
write.csv(val_decile,"val_decile.csv")


############+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++############











































