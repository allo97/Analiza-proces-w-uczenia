# Reguły klasyfikacyjne C5.0 Iris
library(C50)
data(iris)
mod1 <- C5.0(Species ~ ., data = iris, rules = TRUE)
library(C50)
plot(mod1)
summary(mod1)
