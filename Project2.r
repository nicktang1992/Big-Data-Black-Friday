library(plyr)
library(gmodels)
library(cluster)
library(class)

##
##read data
##
BF <- read.csv("BlackFriday.csv")

BF
BF['User_ID']
summary(BF)

un <- unique(BF['Occupation'])
sort(un$Occupation)

BF[which(BF$Product_ID == 'P00069042'),]
BF[which(BF$Occupation == 1),]

sapply(BF, typeof)

##
## cleaning and normalizing data
##

#mapping catagorical data columns and change them to numeric
#1. gender
BF['Gender'] <-mapvalues(BF$Gender, 
          from=c("M","F"), 
          to=c("1","0"))
#BF["Gender"]
BF["Gender"]<-as.numeric(BF$Gender)
#BF["Gender"]
x <-BF["Gender"]
BF["Gender"] <- {(x-min(x))/(max(x)-min(x))}
#BF["Gender"]

#2. age 
BF['Age'] <-mapvalues(BF$Age, 
                      from=c("0-17","18-25","26-35","36-45","46-50","51-55","55+"), 
                      to=c("0","1","2","3","4","5","6"))
BF["Age"]<-as.numeric(BF$Age)
x <- BF["Age"]
BF["Age"] <- {(x-min(x))/(max(x)-min(x))}
print(BF["Age"])

#3. stay in current city
BF['Stay_In_Current_City_Years'] <-mapvalues(BF$Stay_In_Current_City_Years, 
                      from=c("4+"), 
                      to=c("4"))
BF["Stay_In_Current_City_Years"]<-as.numeric(BF$Stay_In_Current_City_Years)
y <- BF["Stay_In_Current_City_Years"]
BF["Stay_In_Current_City_Years"] <- {(y-min(y))/(max(y)-min(y))}
print(BF["Stay_In_Current_City_Years"])

#4. normalization Purchase
BF["Purchase"]<-as.numeric(BF$Purchase)
z <- BF["Purchase"]
BF["Purchase"] <- {(z-min(z))/(max(z)-min(z))}
print(BF["Purchase"])

##
## pairwise plot on all data
##
#randomly sample 100 rows and create pairwise plot
plotdf = BF[sample(nrow(BF), 100), -c(0,1,2,9,10,11)]
summary(plotdf)
plot(plotdf)
## there is some level of dependency on purchase vs city, purchase vs gender, purchase vs marital status


#drop useless column 
print(BF)
BFTEST <- BF[, -c(2,9:11)]
print(BFTEST)
#BF <- BF[, -c("Product_ID","City_Category","Product_Category_1","Product_Category_2","Product_Category_3")]

#
# split by city then drop city from BF test
# do this first since we need to discard city category later
summary(BFTEST)
City_A_df = BFTEST[which(BFTEST$City_Category=='A'),]
City_B_df = BFTEST[which(BFTEST$City_Category=='B'),]
City_C_df = BFTEST[which(BFTEST$City_Category=='C'),]
BFTEST <- BFTEST[, -c(5)]


#create subset of data rows by:
#1. occupation
occupation_df <- list()
for(i in 1:20) {
  occupation_df[[i]] = BFTEST[which(BFTEST$Occupation==i),]
  
}

##
## this is an example of how to deal with one subset of data
##

occupation_df[1]
nrow(occupation_df[[1]])

current_df = occupation_df[[1]]

# Compute and plot wss for k 
#false unlist dont use it anynore
#df1<-as.numeric(unlist(occupation_df[1]))

current_df[,-c(0,1,4)]

# sample occupation df and print pairwise plot
plotdf = current_df[sample(nrow(current_df), 100), -c(0,1,4)]
summary(plotdf)
plot(plotdf)

#from k = 2 to 10, run k means clustering
k.max <- 10
wss <- sapply((2:k.max),function(k){kmeans(current_df[,-c(0,1,4,7)], k, nstart=20,iter.max = 20 )$tot.withinss})
plot(2:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

#using the elbow observation method, choose the number of clusters 
k.estimate = 4
#wss <- sapply(k.estimate-5:k.estimate+5,function(k){kmeans(current_df[,-c(0,1,4)], k, nstart=4,iter.max = 15 )$tot.withinss})
#plot(k.estimate-5:k.estimate+5, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

res <- kmeans(current_df[,-c(0,1,4,7)], k.estimate, nstart=20,iter.max = 20 )

# columns to include in the cluster plot. play with it to produce a good looking plot
plotcols <- c(2,3,5,6)
clusplot(current_df[,plotcols], res$cluster, main = 'Cusplot')

# split data into training and testing in order to train knn model
indecestrain <- sample(nrow(current_df), nrow(current_df)*0.7)
cols <-c(2,3,5,6,7)
knnTraining<-current_df[indecestrain,cols]
knnTesting<-current_df[-indecestrain,cols]
nrow(knnTraining)
nrow(knnTesting)
kmpred <- kmeans(knnTraining, k.estimate, nstart=20,iter.max = 20 )

#classification using training data, kmeans result on testing data
#although "purchase" is considered slightly dependent on other variables, its the only continuous variable that prevents "too many ties in knn" error.
knnres<-knn(knnTraining,knnTesting, kmpred$cluster,k=4)

#run kmeans on test data for evaluation of knn
kmres<-kmeans(knnTesting, k.estimate, nstart=20,iter.max = 20 )

#evaluate knn
CrossTable(x=kmres$cluster,y=knnres,prop.chisq = FALSE)
#the result here is kind of good, every km and knn prediction goes to the same cluster. although the number of cluster is somehow not correct

#Hierarchical clustering
#it requires too much space, sample rows 
cols <-c(2,3,5,6,7)
hres<-hclust(dist(current_df[sample(nrow(current_df),nrow(current_df)*0.1 ),cols]))
#clusters <- hclust(dist(iris[, 3:4]))
plot(hres)
##by observing the cluster diagram, we choose height 
groups <- cutree(hres, k=5)
groups


#2. gender
marital_a_df=BFTEST[which(BFTEST$Marital_Status==1),]
marital_b_df=BFTEST[which(BFTEST$Marital_Status==0),]
age_m_a_df = list()
age_m_b_df = list()
#divide each subset into 5 subsubset
for(i in 1:5){
  age_m_a_df[[i]]=marital_a_df[which(marital_a_df$Age<=(0.2*i)&marital_a_df$Age>(0.2*(i-1))),]
  age_m_b_df[[i]]=marital_b_df[which(marital_b_df$Age<=(0.2*i)&marital_b_df$Age>(0.2*(i-1))),]
}

#choose one subset
current_df = age_m_a_df[[1]]

#wholefunction
#start
plotdf = current_df[sample(nrow(current_df),100),-c(1,4)]
summary(plotdf)
plot(plotdf)
k.max <- 10
wss=sapply((2:k.max),function(k){kmeans(current_df[,-c(1,4,7)],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(2:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")
k.estimate = 4
res <- kmeans(current_df[,-c(0,1,4,7)], k.estimate, nstart=20,iter.max = 20 )
plotcols <- c(2,3,5,6)
clusplot(current_df[,plotcols], res$cluster, main = 'Cusplot')
indecestrain <- sample(nrow(current_df), nrow(current_df)*0.7)
cols <-c(2,3,5,6,7)
knnTraining<-current_df[indecestrain,cols]
knnTesting<-current_df[-indecestrain,cols]
nrow(knnTraining)
nrow(knnTesting)
kmpred <- kmeans(knnTraining, k.estimate, nstart=20,iter.max = 20 )
knnres<-knn(knnTraining,knnTesting, kmpred$cluster,k=4)
kmres<-kmeans(knnTesting, k.estimate, nstart=20,iter.max = 20 )
CrossTable(x=kmres$cluster,y=knnres,prop.chisq = FALSE)
cols <-c(2,3,5,6,7)
hres<-hclust(dist(current_df[sample(nrow(current_df),nrow(current_df)*0.1 ),cols]))
plot(hres)
groups <- cutree(hres, k=6)
plot(groups)
groups
#end

#start for lm and glm

#generate three trainingset and testset
indeces1 = sample(nrow(current_df), nrow(current_df)*0.5)
training_df1 = current_df[indeces1,]
testing_df1 = current_df[-indeces1,]

indeces2 = sample(nrow(current_df), nrow(current_df)*0.6)
training_df2 = current_df[indeces2,]
testing_df2 = current_df[-indeces2,]

indeces3 = sample(nrow(current_df), nrow(current_df)*0.7)
training_df3 = current_df[indeces3,]
testing_df3 = current_df[-indeces3,]

#pairwise ports to observe data dependency
plotdf = training_df1[sample(nrow(training_df1), 100),-c(1) ]
#summary(plotdf)
plot(plotdf)
#
#running lm on purchase
#

#gender, age and marital status on purchase
#model building
formula1_gen_age_ms_pur<-training_df1$Purchase ~ training_df1$Gender+training_df1$Age+training_df1$Marital_Status
fit_purchase<-lm(formula = formula1_gen_age_ms_pur,data=training_df1)
summary.lm(fit_purchase)

plot(fit_purchase$fitted.values, fit_purchase$residuals)
#prediction
pred_purchase<-vector()
pred_purchase$pred <- predict(fit_purchase, testing_df1)
summary(pred_purchase$pred)

pred_purchase$actuals_preds <- data.frame(cbind(actuals=testing_df1$Purchase, predicteds=pred_purchase$pred))
pred_purchase$correlation <- cor(pred_purchase$actuals_preds)
#correlation is incredibly low since low correlation in actual data

#fit only age on purchase
fit_age_purchase<-lm(formula = training_df1$Purchase ~ training_df1$Age,data=training_df1)
plot(fit_age_purchase$fitted.values, fit_age_purchase$residuals)
summary.lm(fit_age_purchase)

pred_age_purchase<-vector()
pred_age_purchase$pred <- predict(fit_age_purchase, testing_df1)
pred_age_purchase$actuals_preds <- data.frame(cbind(age=testing_df1$Age, predicteds=(pred_age_purchase$pred - testing_df1$Purchase)))
plot(pred_age_purchase$actuals_preds)

# residual plot looks bad

#
#running glm on gender
#

#gender on purchase
cor(training_df1)

fit_purchase_gender<-glm(formula = training_df1$Gender~training_df1$Purchase, data =training_df1,family=binomial())
plot(fit_purchase_gender$fitted.values, fit_purchase_gender$residuals)
plot(fit_purchase_gender)
#from here we can see the logit model does not fit well.
#however, I am struggling find a variable that has significant coefficient with gender

pred_purchase_gender<-vector()
pred_purchase_gender$pred <- predict(fit_purchase_gender, testing_df1)

#print prediction
plotting_df <-data.frame(cbind(testing_df1$Purchase,pred_purchase_gender$pred))
plot(plotting_df)

#print residual plot
plot(testing_df1$Purchase,pred_purchase_gender$pred - testing_df1$Gender)
#does not looks good either


#fit maritual status on age
fit_m_age<-glm(formula = training_df1$Marital_Status~training_df1$Age, data =training_df1,family=binomial())
plot(fit_m_age$fitted.values, fit_m_age$residuals)

pred_m_age<-vector()
pred_m_age$pred <- predict(fit_m_age, testing_df1)
plotting_df <-data.frame(cbind(testing_df1$Age,pred_m_age$pred))
plot(plotting_df)
#end

#3. City
#this split is already done in previous section
City_A_df
City_B_df  
City_C_df 

#then split by age
City_A_Age_0_17_df = City_A_df[which(City_A_df$Age==0/6),]
City_A_Age_18_25_df = City_A_df[which(City_A_df$Age==1/6),]
City_A_Age_26_35_df = City_A_df[which(City_A_df$Age==2/6),]
City_A_Age_36_45_df = City_A_df[which(City_A_df$Age==3/6),]
City_A_Age_46_50_df = City_A_df[which(City_A_df$Age==4/6),]
City_A_Age_51_55_df = City_A_df[which(City_A_df$Age==5/6),]
City_A_Age_55_M_df = City_A_df[which(City_A_df$Age==6/6),]

current_df = City_A_Age_0_17_df

current_df[,-c(1,3,5)]

#
#for each subset of data, do:
# plot attributes against each other
# drop columns
# run k-means or k-nearest neighbors
# run iclust to see the cluster plots and do some observations
#

# sample occupation df and print pairwise plot
plotdf = current_df[sample(nrow(current_df), 100), -c(1,3,5)]
summary(plotdf)
plot(plotdf)

#from k = 2 to 10, run k means clustering
k.max <- 10
wss <- sapply((2:k.max),function(k){kmeans(current_df[,-c(1,3,5)], k, nstart=20,iter.max = 20 )$tot.withinss})
plot(2:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

#using the elbow observation method, choose the number of clusters 
k.estimate = 3
#wss <- sapply(k.estimate-5:k.estimate+5,function(k){kmeans(current_df[,-c(0,1,4)], k, nstart=4,iter.max = 15 )$tot.withinss})
#plot(k.estimate-5:k.estimate+5, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

res <- kmeans(current_df[,-c(1,3,5)], k.estimate, nstart=20,iter.max = 20 )

# columns to include in the cluster plot. play with it to produce a good looking plot
plotcols <- c(2,4,6,8)
# if age > 18, keep 7, otherwise, remove 7
clusplot(current_df[,plotcols], res$cluster, main = 'Clusplot')

# split data into training and testing in order to train knn model
indecestrain <- sample(nrow(current_df), nrow(current_df)*0.7)
cols <-c(2,4,6,7,8)
knnTraining<-current_df[indecestrain,cols]
knnTesting<-current_df[-indecestrain,cols]
nrow(knnTraining)
nrow(knnTesting)
kmpred <- kmeans(knnTraining, k.estimate, nstart=20,iter.max = 20 )

#classification using training data, kmeans result on testing data
#although "purchase" is considered slightly dependent on other variables, its the only continuous variable that prevents "too many ties in knn" error.
knnres<-knn(knnTraining,knnTesting, kmpred$cluster,k=3)

#run kmeans on test data for evaluation of knn
kmres<-kmeans(knnTesting, k.estimate, nstart=20,iter.max = 20 )

#evaluate knn
CrossTable(x=kmres$cluster,y=knnres,prop.chisq = FALSE)
#the result here is kind of good, every km and knn prediction goes to the same cluster. although the number of cluster is somehow not correct

#Hierarchical clustering
#it requires too much space, sample rows 
cols <-c(2,4,6,7,8)
hres<-hclust(dist(current_df[sample(nrow(current_df),nrow(current_df)*0.1 ),cols]))
#clusters <- hclust(dist(iris[, 3:4]))
plot(hres)
##by observing the cluster diagram, we choose height 
groups <- cutree(hres, k=3)
groups

#
#split the data into training and test data on ratio of 50-50, 60-30, 70-30
#

indeces1 = sample(nrow(current_df[,-c(1,3,5)]), nrow(current_df[,-c(1,3,5)])*0.5)
training_df1 = current_df[,-c(1,3,5)][indeces1,]
testing_df1 = current_df[,-c(1,3,5)][-indeces1,]

indeces2 = sample(nrow(current_df[,-c(1,3,5)]), nrow(current_df[,-c(1,3,5)])*0.6)
training_df2 = current_df[,-c(1,3,5)][indeces2,]
testing_df2 = current_df[,-c(1,3,5)][-indeces2,]

indeces3 = sample(nrow(current_df[,-c(1,3,5)]), nrow(current_df[,-c(1,3,5)])*0.7)
training_df3 = current_df[,-c(1,3,5)][indeces3,]
testing_df3 = current_df[,-c(1,3,5)][-indeces3,]

#nrow(training_df1)
#nrow(testing_df1)
#nrow(training_df2)
#nrow(testing_df2)
#nrow(training_df3)
#nrow(testing_df3)

#TO-DO: run LM and GLM on all three data sets

#pairwise ports to observe data dependency
plotdf = training_df1[sample(nrow(training_df1), 100),-c(1) ]
#summary(plotdf)
plot(plotdf)
#
#running lm on purchase
#

#gender, occupation and marital status on purchase
#model building
formula1_gen_occ_ms_pur<-training_df1$Purchase ~ training_df1$Gender+training_df1$Occupation+training_df1$Marital_Status
fit_purchase<-lm(formula = formula1_gen_occ_ms_pur,data=training_df1)
summary.lm(fit_purchase)

plot(fit_purchase$fitted.values, fit_purchase$residuals)
#prediction
pred_purchase<-vector()
pred_purchase$pred <- predict(fit_purchase, testing_df1)
summary(pred_purchase$pred)

pred_purchase$actuals_preds <- data.frame(cbind(actuals=testing_df1$Purchase, predicteds=pred_purchase$pred))
pred_purchase$correlation <- cor(pred_purchase$actuals_preds)
#correlation is incredibly low since low correlation in actual data

#fit only occupation on purchase
fit_occ_purchase<-lm(formula = training_df1$Purchase ~ training_df1$Occupation,data=training_df1)
plot(fit_occ_purchase$fitted.values, fit_occ_purchase$residuals)
summary.lm(fit_occ_purchase)

pred_occ_purchase<-vector()
pred_occ_purchase$pred <- predict(fit_occ_purchase, testing_df1)
pred_occ_purchase$actuals_preds <- data.frame(cbind(occupation=testing_df1$Occupation, predicteds=(pred_occ_purchase$pred - testing_df1$Purchase)))
plot(pred_occ_purchase$actuals_preds)

# residual plot looks bad

#
#running glm on gender
#

#gender on purchase
cor(training_df1) 

fit_purchase_gender<-glm(formula = training_df1$Gender~training_df1$Purchase, data =training_df1,family=binomial())
plot(fit_purchase_gender$fitted.values, fit_purchase_gender$residuals)
plot(fit_purchase_gender)

#from here we can see the logit model does not fit well.
#however, I am struggling find a variable that has significant coefficient with gender

pred_purchase_gender<-vector()
pred_purchase_gender$pred <- predict(fit_purchase_gender, testing_df1)

#print prediction
plotting_df <-data.frame(cbind(testing_df1$Purchase,pred_purchase_gender$pred))
plot(plotting_df)

#print residual plot
plot(testing_df1$Purchase,pred_purchase_gender$pred - testing_df1$Gender)
#does not looks good either


#fit years stayed in current city on occupation
fit_year_occ<-glm(formula = training_df1$Stay_In_Current_City_Years~training_df1$Occupation, data =training_df1,family=binomial())
plot(fit_year_occ$fitted.values, fit_year_occ$residuals)

pred_year_occ<-vector()
pred_year_occ$pred <- predict(fit_year_occ, testing_df1)
plotting_df <-data.frame(cbind(testing_df1$Occupation,pred_year_occ$pred))
plot(plotting_df)
# not good. Am i not using glm correctly?


#for each subset of data, do:
# plot attributes against each other
# drop columns
# run k-means or k-nearest neighbors
# run iclust to see the cluster plots and do some observations


#
#reload the data, processing without normalization,
#split the data into training and test data on ratio of 50-50, 60-30, 70-30
#

BF <- read.csv("BlackFriday.csv")
#mapping gender
BF['Gender'] <-mapvalues(BF$Gender, 
                         from=c("M","F"), 
                         to=c("1","0"))
BF["Gender"]<-as.numeric(BF$Gender)
x <-BF["Gender"]
BF["Gender"] <- {(x-min(x))/(max(x)-min(x))}

#mapping age
BF['Age'] <-mapvalues(BF$Age, 
                      from=c("0-17","18-25","26-35","36-45","46-50","51-55","55+"), 
                      to=c("0","1","2","3","4","5","6"))
BF["Age"]<-as.numeric(BF$Age)
#stay in city years
BF['Stay_In_Current_City_Years'] <-mapvalues(BF$Stay_In_Current_City_Years, 
                                             from=c("4+"), 
                                             to=c("4"))
BF["Stay_In_Current_City_Years"]<-as.numeric(BF$Stay_In_Current_City_Years)

# drop columns
BF = BF[, -c(2,5,6,9:11)]
#index of the training data
BF

indeces1 = sample(nrow(BF), nrow(BF)*0.5)
training_df1 = BF[indeces1,]
testing_df1 = BF[-indeces1,]

indeces2 = sample(nrow(BF), nrow(BF)*0.6)
training_df2 = BF[indeces2,]
testing_df2 = BF[-indeces2,]

indeces3 = sample(nrow(BF), nrow(BF)*0.7)
training_df3 = BF[indeces3,]
testing_df3 = BF[-indeces3,]

#nrow(training_df1)
#nrow(testing_df1)
#nrow(training_df2)
#nrow(testing_df2)
#nrow(training_df3)
#nrow(testing_df3)

#TO-DO: run LM and GLM on all three data sets

#pairwise ports to observe data dependency
plotdf = training_df1[sample(nrow(training_df1), 100),-c(1) ]
#summary(plotdf)
plot(plotdf)
#
#running lm on purchase
#

#gender, age and marital status on purchase
#model building
formula1_gen_age_ms_pur<-training_df1$Purchase ~ training_df1$Gender+training_df1$Age+training_df1$Marital_Status
fit_purchase<-lm(formula = formula1_gen_age_ms_pur,data=training_df1)
summary.lm(fit_purchase)

plot(fit_purchase$fitted.values, fit_purchase$residuals)
#prediction
pred_purchase<-vector()
pred_purchase$pred <- predict(fit_purchase, testing_df1)
summary(pred_purchase$pred)

pred_purchase$actuals_preds <- data.frame(cbind(actuals=testing_df1$Purchase, predicteds=pred_purchase$pred))
pred_purchase$correlation <- cor(pred_purchase$actuals_preds)
#correlation is incredibly low since low correlation in actual data

#fit only age on purchase
fit_age_purchase<-lm(formula = training_df1$Purchase ~ training_df1$Age,data=training_df1)
plot(fit_age_purchase$fitted.values, fit_age_purchase$residuals)
summary.lm(fit_age_purchase)

pred_age_purchase<-vector()
pred_age_purchase$pred <- predict(fit_age_purchase, testing_df1)
pred_age_purchase$actuals_preds <- data.frame(cbind(age=testing_df1$Age, predicteds=(pred_age_purchase$pred - testing_df1$Purchase)))
plot(pred_age_purchase$actuals_preds)

# residual plot looks bad

#
#running glm on gender
#

#gender on purchase
cor(training_df1)

fit_purchase_gender<-glm(formula = training_df1$Gender~training_df1$Purchase, data =training_df1,family=binomial())
plot(fit_purchase_gender$fitted.values, fit_purchase_gender$residuals)
plot(fit_purchase_gender)
#from here we can see the logit model does not fit well.
#however, I am struggling find a variable that has significant coefficient with gender

pred_purchase_gender<-vector()
pred_purchase_gender$pred <- predict(fit_purchase_gender, testing_df1)

#print prediction
plotting_df <-data.frame(cbind(testing_df1$Purchase,pred_purchase_gender$pred))
plot(plotting_df)

#print residual plot
plot(testing_df1$Purchase,pred_purchase_gender$pred - testing_df1$Gender)
#does not looks good either


#fit maritual status on age
fit_m_age<-glm(formula = training_df1$Marital_Status~training_df1$Age, data =training_df1,family=binomial())
plot(fit_m_age$fitted.values, fit_m_age$residuals)

pred_m_age<-vector()
pred_m_age$pred <- predict(fit_m_age, testing_df1)
plotting_df <-data.frame(cbind(testing_df1$Age,pred_m_age$pred))
plot(plotting_df)
# not good. Am i not using glm correctly?
