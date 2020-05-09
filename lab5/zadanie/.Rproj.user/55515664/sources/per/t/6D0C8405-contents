# Ocena klientow, pakiet mlr
library(mlr)

# Data of 11 Samsung Smartphones

nazwa <- c("Samsung Galaxy S20","Samsung Galaxy A71","Samsung Galaxy A40","Samsung Galaxy A30s","Samsung Galaxy A70",
           "Samsung Galaxy A51","Samsung Galaxy S10","Samsung Galaxy A10","Samsung Galaxy S10 Lite",
           "Samsung Galaxy Note10 Lite","Samsung Galaxy A20e")
wyswietlacz <-c(6.7,6.7,5.9,6.4,6.7,6.5,6.1,6.2,6.7,6.7, 5.8)
pamiec_RAM <-c(8,6,4,4,6,4,8,2,8,6, 3)
pamiec_wbudowana <-c(128,128,64,64,128,128,128,32,128, 128, 32)
aparat_foto <-c(64,64,16,25,32,48,16,13,48,12, 13)
cena <-c(3949, 1999, 999, 849,1499, 1699, 3299,699,2799,2649, 749)
liczba_opinii <-c(7,38,20,21,65,20,77,22,10,8, 42)
ocena_klientow <- c(3, 1, 2, 2, 2, 3, 3, 2, 4, 3, 2)

# Uwzględnia wyswietlacz, RAM, pamiec wbudowana, aparat i opinie

smartphones <- data.frame(wyswietlacz, pamiec_RAM, pamiec_wbudowana, aparat_foto, ocena_klientow)


## MAKE RESAMPLING STRATEGY
rdesc = makeResampleDesc(method = "CV", stratify = F)

#MAKE REGR TASK##############################################################################################################

regr.task = makeRegrTask(id = "Smartphones ocena klientow MSE",  data = smartphones, target = "ocena_klientow")
regr.task

#listLearners("regr")[c("class", "package")]

#Default values 
regr.learners = makeLearners(c("lm", "randomForest", "nnet", "ctree", "rpart"), type = "regr")
regr.comparison = benchmark(learners = regr.learners, tasks = regr.task, resamplings = rdesc)

## Results for regr

regr.comparison

getBMRAggrPerformances(regr.comparison)
getBMRPerformances(regr.comparison)
plotBMRBoxplots(regr.comparison)





