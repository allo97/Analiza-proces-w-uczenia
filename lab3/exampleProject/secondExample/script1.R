library("neuralnet")

# Generate training data

# RAM, HDD

traininginput <- as.data.frame(matrix(c(32, 16, 8, 64, 32, 64, 16, 8, 64, 32, 1000, 500, 256, 500, 500, 1000, 500, 256, 500, 500), 
                                      nrow=10, ncol=2))

trainingoutput <- c(4000, 3000, 1100, 2200, 2500, 4200, 3300, 1050, 800, 1470)

# Normalizacja danych

traininginput[ ,1:2]

maxs <- apply(traininginput[ ,1:2], 2, max)
mins <- apply(traininginput[ ,1:2], 2, min)

#skalowanie

scaled.traininginput <- as.data.frame(scale(traininginput[,1:2], center = mins, scale = maxs-mins))

print(head(scaled.traininginput, 10))

# polaczenie danych wejściowych i wyjściowych

trainingdata <- cbind(scaled.traininginput, trainingoutput)
colnames(trainingdata) <- c("RAM", "HDD", "Price")

print(trainingdata)

# Train the neural network
# c(3,2) hidden layers

net.price <- neuralnet(Price ~ RAM + HDD, trainingdata, hidden = c(3,2), threshold = 0.01)
print(net.price)

# plot the neural network
#plot(net.price)

# test the neural network on some training data

testdata <- as.data.frame(matrix(c(64, 32, 8, 1000, 500, 100), nrow=3, ncol=2))


scaled.testdata <- as.data.frame(scale(testdata[,1:2], center = mins, scale = maxs-mins))

net.results <- compute(net.price, scaled.testdata)

ls(net.results)

print(net.results$net.result)


