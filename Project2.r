BF <- read.csv("BlackFriday.csv")

BF
BF['User_ID']
summary(BF)

un <- unique(BF['Occupation'])
sort(un$Occupation)

BF[which(BF$Product_ID == 'P00069042'),]
BF[which(BF$Occupation == 1),]

sapply(BF, mode)

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

#3. stay in current city
BF['Stay_In_Current_City_Years'] <-mapvalues(BF$Stay_In_Current_City_Years, 
                      from=c("4+"), 
                      to=c("4"))
BF["Stay_In_Current_City_Years"]<-as.numeric(BF$Stay_In_Current_City_Years)
#check result
summary(BF)
sapply(BF, mode)


#create subset of data rows by:
#1. occupation
occupation_df <- list()
for(i in 1:20) {
  occupation_df[[i]] = BF[which(BF$Occupation==i),]
  
}
occupation_df[1]

#2. gender
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