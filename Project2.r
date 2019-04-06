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
BFTEST <- BF[, -c(2,6,9:11)]
print(BFTEST)
#BF <- BF[, -c("Product_ID","City_Category","Product_Category_1","Product_Category_2","Product_Category_3")]



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
#Elbow Method for finding the optimal number of clusters
set.seed(123)


#3. city

#for each subset of data, do:
# plot attributes against each other
# drop columns
# run k-means or k-nearest neighbors
# run iclust to see the cluster plots and do some observations

