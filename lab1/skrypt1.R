# 1 wariant
library("plotrix")
# a
a <- 2 * exp(5)
b <- 2 * a
a < b
a > b

# b
sum()
help(sum)

# c
vec <- 15:25
sum(vec)

# d
apropos("sum", mode = "function")

# e
getwd()
setwd("C:/Users/aslod/Documents/Wa?ne foldery/STUDIA/ATH/1 semestr/APU/lab1")
a <- "Smartfony Samsung"
save(a, file = "smartfony.RData")
remove(a)
a
ls()
load("smartfony.RData")
a

# f
head(mtcars,10)

# g
vec100 <- rev(seq(20,100,4))

# h
a <- c((9:5))
b <- c((11:16))
d <- c(b,a)
d

# i
nazwa <- c("tel1","tel2","tel3","tel4","tel5","tel6","tel7","tel8","tel9","tel10")
wyswietlacz <-c(5,6,5,5,4,3,2,4,5,6)
pamiec_RAM <-c(8,7,4,2,3,4,5,6,7,12)
pamiec_wbudowana <-c(8,16,32,64,128,256,512,1,2,3)
aparat_foto <-c(13,12,11,10,15,18,20,108,5,13)
cena <-c(500, 600, 700, 800,900, 1000, 1100,1200,1300,1400)
liczba_opinii <-c(1,2,3,4,5,6,8,7,9,10)

smartphone <- data.frame(nazwa, wyswietlacz, pamiec_RAM, pamiec_wbudowana, aparat_foto, cena, liczba_opinii)

mean_price <- mean(smartphone$cena)

# j
new_smartphone <- data.frame(nazwa="tel11", wyswietlacz=5, pamiec_RAM=8, pamiec_wbudowana=16, aparat_foto=20, cena=9075, liczba_opinii=5, stringsAsFactors = FALSE)

smartphone <- rbind(smartphone, new_smartphone)

mean_price <- mean(smartphone$cena)

# k
ocena_klientow <- seq(0,5,0.5)

smartphone <- cbind(smartphone, ocena_klientow)

mean_by_group <- aggregate(smartphone[, 6], list(smartphone$ocena_klientow), mean)

# l
new_smartphone <- data.frame(nazwa="tel11", wyswietlacz=5, pamiec_RAM=8, pamiec_wbudowana=16, aparat_foto=20, cena=9075, liczba_opinii=5, ocena_klientow=3.5, stringsAsFactors = FALSE)
for (i in 1:4) {
  smartphone <- rbind(smartphone, new_smartphone)
}

count <- table(smartphone$ocena_klientow)

barplot(count, main = "Liczebnosc oceny klientow", xlab = "Ocena", ylab = "Ilo??") 

# m
percentage <- table(smartphone$ocena_klientow) / length(smartphone$ocena_klientow)
pie(percentage)
fan.plot(percentage, labels = names(percentage), main = "Wykres wachlarzowy procentowego udzialu oceny klientow")

# n
status_opinii <- c("1 opinia", "wiecej niz 1 opinia", "wiecej niz 1 opinia", 
                   "wiecej niz 1 opinia", "5 opinii", "wiecej niz 5 opinii ale mniej ni? 10 opinii",
                   "wiecej niz 5 opinii ale mniej ni? 10 opinii","wiecej niz 5 opinii ale mniej ni? 10 opinii",
                   "wiecej niz 5 opinii ale mniej ni? 10 opinii", "conajmniej 10 opinii",
                   "5 opinii", "5 opinii","5 opinii","5 opinii","5 opinii")

smartphone <- cbind(smartphone, status_opinii)
count_status_opinii <- table(smartphone$status_opinii)
smartphone_percentage <- count_status_opinii / length(smartphone$status_opinii)
pie(smartphone_percentage)

# o
for(row in 1:nrow(smartphone)) {
  print(paste(smartphone[row, "nazwa"], "ma ocene klientow", smartphone[row, "ocena_klientow"], "bo ma liczbe opinii", smartphone[row, "liczba_opinii"] ))
}

# p
write.csv(smartphone, "smartphone.csv", row.names = TRUE)
next_smartphone <- read.csv2("smartphone.csv", header = TRUE, sep=",")





