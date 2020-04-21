tn <- read.csv('Titanic.csv') # load the dataset into an object 
head(tn) # view the first six rows of the dataset 

tn[,4]

tnTree <- C5.0(tn[,-4], tn[,4]) 
plot(tnTree, main = 'Titanic decision tree') #view the tree  

summary(tnTree)

tnRules <- C5.0(tn[,-4], tn[,4], rules = TRUE)  
summary(tnRules) # view the ruleset   