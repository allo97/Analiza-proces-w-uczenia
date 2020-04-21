wine <- read.csv('Wine.csv') # read the dataset 

head(wine) # look at the 1st 6 rows 


wTree <- C5.0(wine[,-14], as.factor(wine[,14])) # train the tree  

summary(wTree) # view the model components  

plot(wTree, main = 'Wine decision tree') # view the model graphically  

wRules <- C5.0(wine[,-14], as.factor(wine[,14]), rules = TRUE) 
summary(wRules) # view the ruleset  