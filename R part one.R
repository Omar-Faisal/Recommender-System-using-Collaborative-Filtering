library("arules")
library("arulesViz")

transactions <- read.transactions("transactions.csv", format = "basket", sep=",",skip = 1)

summary(transactions)
inspect(head(transactions, 10))

#showing the top 10 transaction in the data

itemFrequencyPlot(transactions, topN = 10)


apriori(transactions)

# Generation of association rules using minimum support of 0.002, minimum confidence of 0.20, 
# and a maximum length of 3. Display the rules, sorted by descending lift value
rules <- apriori(transactions, parameter = list(support =
                                                         0.002, confidence = 0.2,maxlen=3))

rules
inspect(sort(rules, by = "lift"))
inspect(sort(rules, by = "lift")[1])

# Generation of association rules using minimum support of 0.002, minimum confidence of 0.20, 
# and a maximum length of 2. Display the rules, sorted by descending lift value

rules2 <- apriori(transactions, parameter = list(support =
                                                          0.002, confidence = 0.2,maxlen=2))

rules2
seond_rule <- inspect(sort(rules2, by = "lift")[1])

plot(rules)
plot(rules, method = "graph")








install.packages("lsa")
install.packages("matlib")
install.packages("recommenderlab")

library(stringi)
library(reshape2)


library(lsa)
library(matlib)
library("recommenderlab")





#Calculating the cosine similarity matrix
#install.packages("coop")
library(coop)
Ratings <-  read.delim("CF.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
Ratings[is.na(Ratings)] <- 0
tcosine(Ratings[,-1], use = "everything", inverse = FALSE)


library(Matrix)
library(arules)
#install.packages("recommenderlab", dependencies=TRUE)
library(recommenderlab)

Ratings <- read.csv("CF.csv")
ratingmat = as.matrix(Ratings[,-1])


#Convert ratings matrix to real rating matrx which makes it dense
ratingmat = as(ratingmat, "realRatingMatrix")


#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours
rec_mod = Recommender(ratingmat, method = "IBCF", param=list(method="Cosine")) 


#Obtain top 5 recommendations for 1st user entry in dataset
Top_5_pred = predict(rec_mod, ratingmat[4], n=1)



#Convert the recommendations to a list

Top_5_List = as(Top_5_pred, "list")
Top_5_List


