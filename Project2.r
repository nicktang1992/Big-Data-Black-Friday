BF <- read.csv("BlackFriday.csv")

BF
BF['User_ID']
summary(BF)

un <- unique(BF['Occupation'])
sort(un$Occupation)

BF[which(BF$Product_ID == 'P00069042'),]
BF[which(BF$Occupation == 1),]

sapply(BF, typeof)

#mapping catagorical data columns and change them to numeric
#1. gender
BF['Gender'] <-mapvalues(BF$Gender, 
          from=c("M","F"), 
          to=c("1","0"))
BF["Gender"]<-as.numeric(BF$Gender)

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

#check result
summary(BF)
sapply(BF, mode)
#normalization Purchase
BF["Purchase"]<-as.numeric(BF$Purchase)
z <- BF["Purchase"]
BF["Purchase"] <- {(z-min(z))/(max(z)-min(z))}
print(BF["Purchase"])

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
occupation_df[1]

# Compute and plot wss for k 
df1<-as.numeric(unlist(occupation_df[1]))
k.max <- 50
wss <- sapply(10:k.max,function(k){kmeans(df1, k, nstart=10,iter.max = 15 )$tot.withinss})
plot(10:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")


#2. gender
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max,function(k){kmeans(BF, k, nstart=50,iter.max = 15)$gender})
wss
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")


#3. product_id?

#for each subset of data, do:
# plot attributes against each other
# drop columns
# run k-means or k-nearest neighbors
# run iclust to see the cluster plots and do some observations

#also do:
# split the subsets into training data and test data
# train a model using lm or glm on training data
# run predict on testing data with trained models
# plot test data residuals and do some observation