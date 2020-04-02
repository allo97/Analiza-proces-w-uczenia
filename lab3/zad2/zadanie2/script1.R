library("neuralnet")


# Prognozowanie ceny smartphonow

# Error < 100zl


# Generate training data



nazwa <- c("Samsung Galaxy S20","Samsung Galaxy A71","Samsung Galaxy A40","Samsung Galaxy A30s","Samsung Galaxy A70",
           "Samsung Galaxy A51","Samsung Galaxy S10","Samsung Galaxy A10","Samsung Galaxy S10 Lite","Samsung Galaxy Note10 Lite")
wyswietlacz <-c(6.7,6.7,5.9,6.4,6.7,6.5,6.1,6.2,6.7,6.7)
pamiec_RAM <-c(8,6,4,4,6,4,8,2,8,6)
pamiec_wbudowana <-c(128,128,64,64,128,128,128,32,128, 128)
aparat_foto <-c(64,64,16,25,32,48,16,13,48,12)
cena <-c(3949, 1999, 999, 849,1499, 1699, 3299,699,2799,2649)
liczba_opinii <-c(7,38,20,21,65,20,77,22,10,8)

traininginput <- data.frame(nazwa, wyswietlacz, pamiec_RAM, pamiec_wbudowana, aparat_foto, liczba_opinii)

row.names(traininginput) <- traininginput$nazwa
traininginput[1] <- NULL

# Normalizacja danych

maxs <- apply(traininginput[ ,1:5], 2, max)
mins <- apply(traininginput[ ,1:5], 2, min)

# skalowanie

scaled.traininginput <- as.data.frame(scale(traininginput[,1:5], center = mins, scale = maxs-mins))

# polaczenie danych wejściowych i wyjściowych

trainingdata <- cbind(scaled.traininginput, cena)

print(head(trainingdata,2))

# Train the neural network
# c(3,2) hidden layers

# Convert to formula
f <- as.formula("cena ~ wyswietlacz + pamiec_RAM + pamiec_wbudowana + aparat_foto + liczba_opinii")

print(f)


# za duzy error powstaje, nie wiem dlaczego
smartphones.price <- neuralnet(f, trainingdata, hidden = c(5,5,5))
print(smartphones.price)

# plot the neural network
plot(smartphones.price)

# test the neural network on some training data

testdata <- as.data.frame(matrix(c(5, 6, 64, 48, 50), nrow=1, ncol=5))


scaled.testdata <- as.data.frame(scale(testdata[,1:5], center = mins, scale = maxs-mins))

smartphones.results <- compute(smartphones.price, scaled.testdata)

ls(smartphones.results)

print(smartphones.results$smartphones.result)


