BF <- read.csv("BlackFriday.csv")

BF
BF['User_ID']
summary(BF)

un <- unique(BF['Occupation'])
sort(un$Occupation)

BF[which(BF$Product_ID == 'P00069042'),]
BF[which(BF$Occupation == 1),]

sapply(BF, mode)

#cleaning data for clustering (replace them to numeric data and normalize using min-max)
#1. gender
#2. age 
#3. stay in current city

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