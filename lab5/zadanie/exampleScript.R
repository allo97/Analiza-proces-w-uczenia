library(magrittr)
library(tidyverse)

# Zrobic dla Iris
titanic <- "https://goo.gl/At238b" %>%
  read_csv %>% # read in the data
  select(survived, embarked, sex, sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))


# Rozbijanie danych dla nauczania i testowania
data <- c("training", "test") %>%
  sample(nrow(titanic), replace = TRUE) %>%
  split(titanic, .)

# Rozbicie rekursyjne

library(rpart)
rtree_fit <- rpart(survived ~ ., data = data$training)
par(xpd = NA)
library(rpart.plot)
rpart.plot(rtree_fit)
