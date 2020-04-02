#install.packages('ISLR')
library(ISLR)

print(head(College))

College


# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))

# Check out results
print(head(scaled.data,2))


# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

# Print data with changed Private to numeric values
print(head(data,2))


library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)


feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)

# Convert to formula
f <- as.formula(f)

library(neuralnet)
nn <- neuralnet(f,train,hidden=c(5,5,5),linear.output=FALSE)

plot(nn)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])



# Check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

table(test$Private,predicted.nn.values$net.result)



