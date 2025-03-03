#Opis zmiennych:
#country - kraj którego dana dotyczy
#year - rok którego dana dotyczy
#LifeExp - spodziewana długość życia osób urodzonych w tym roku wyrażona w latach, jeżeli nie zmienią się czynniki wpływające na nią
#sex_ratio - ilość mężczyzn w kraju podzielona przez ilość kobiet w kraju wymnożona przez 100
#pop_denst - gęstość zaludnienia kraju na km kwadratowy
#freedix - współczynnik wolności politycznej i wolności obywatelskich w skali od 1 do 7, przy czym 1 to kraje najbardziej wolne.
#myisw - średnia lat w szkole kobiet w wieku 15-24lat
#myism - średnia lat w szkole mężczyzn w wieku 15-24lat
#agricultural - procent powierzchni kraju przeznaczonego dla rolnictwa
#GDP_per_capita - Średnie PKB (uzyskane w ciągu danego roku) przypadające na mieszkańca kraju, z uwzględnioną inflacją


library(dplyr)
library(ggplot2)

#Wczytujemy wybrane pliki csv:
zbir1 <- read.csv("sex_ratio_all_age_groups.csv", sep=",")
zbir2 <- read.csv("population_density_per_square_km.csv", sep=",")
zbir3 <- read.csv("freedix_fh.csv", sep=",")
zbir4 <- read.csv("mean_years_in_school_women_15_to_24_years.csv", sep=",")
zbir5 <- read.csv("mean_years_in_school_men_15_to_24_years.csv", sep=",")
zbir7 <- read.csv("life_expectancy_years.csv", sep=",")
zbir8 <- read.csv("agricultural_land_percent_of_land_area.csv", sep=",")
zbir9 <- read.csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv", sep=",")

#Układamy te dane w kolejności alfabetycznej względem krajów:
zbir1 <- zbir1 %>%
  arrange(country)
zbir2 <- zbir2 %>%
  arrange(country)
zbir3 <- zbir3 %>%
  arrange(country)
zbir4 <- zbir4 %>%
  arrange(country)
zbir5 <- zbir5 %>%
  arrange(country)
zbir7 <- zbir7 %>%
  arrange(country)
zbir8 <- zbir8 %>%
  arrange(country)
zbir9 <- zbir9 %>%
  arrange(country)
#Tworzymy kolejne zbiory do których przypisujemy kraje, a następnie wyznaczamy dla wktora ‘kraiki’ kraje, które są we wszystkich plikach (csv):
zbior1 <- zbir1[,1]
zbior2 <- zbir2[,1]
zbior3 <- zbir3[,1]
zbior4 <- zbir4[,1]
zbior5 <- zbir5[,1]
zbior7 <- zbir7[,1]
zbior8 <- zbir8[,1]
zbior9 <- zbir9[,1]
kraiki <- intersect(zbior1,zbior2)
kraiki <- intersect(kraiki,zbior3)
kraiki <- intersect(kraiki,zbior4)
kraiki <- intersect(kraiki,zbior5)
kraiki <- intersect(kraiki,zbior7)
kraiki <- intersect(kraiki,zbior8)
kraiki <- intersect(kraiki,zbior9)
#Tworzymy dla tych plików wektor logiczny, którego długość odpowiada ilości krai odpowiadającemu mu pliku. Wektory te zawierają wartość TRUE jeżeli dany kraj jest w części współnej i FALSE jeśli dany kraj się w części wspólnej nie znajduje:
zbiorek1 <- data.frame(country=zbior1) %>%
  arrange(country)
zbiorek1 <- zbiorek1 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek1$country))
{
  if(is.element(zbiorek1$country[i],kraiki))
  {
    zbiorek1$prawda[i]=T
    next
  }
}
zbiorek2 <- data.frame(country=zbior2) %>%
  arrange(country)
zbiorek2 <- zbiorek2 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek2$country))
{
  if(is.element(zbiorek2$country[i],kraiki))
  {
    zbiorek2$prawda[i]=T
    next
  }
}
zbiorek3 <- data.frame(country=zbior3) %>%
  arrange(country)
zbiorek3 <- zbiorek3 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek3$country))
{
  if(is.element(zbiorek3$country[i],kraiki))
  {
    zbiorek3$prawda[i]=T
    next
  }
}
zbiorek4 <- data.frame(country=zbior4) %>%
  arrange(country)
zbiorek4 <- zbiorek4 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek4$country))
{
  if(is.element(zbiorek4$country[i],kraiki))
  {
    zbiorek4$prawda[i]=T
    next
  }
}
zbiorek5 <- data.frame(country=zbior5) %>%
  arrange(country)
zbiorek5 <- zbiorek5 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek5$country))
{
  if(is.element(zbiorek5$country[i],kraiki))
  {
    zbiorek5$prawda[i]=T
    next
  }
}
zbiorek7 <- data.frame(country=zbior7) %>%
  arrange(country)
zbiorek7 <- zbiorek7 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek7$country))
{
  if(is.element(zbiorek7$country[i],kraiki))
  {
    zbiorek7$prawda[i]=T
    next
  }
}
zbiorek8 <- data.frame(country=zbior8) %>%
  arrange(country)
zbiorek8 <- zbiorek8 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek8$country))
{
  if(is.element(zbiorek8$country[i],kraiki))
  {
    zbiorek8$prawda[i]=T
    next
  }
}
zbiorek9 <- data.frame(country=zbior9) %>%
  arrange(country)
zbiorek9 <- zbiorek9 %>%
  mutate(prawda = F)
for (i in 1:length(zbiorek9$country))
{
  if(is.element(zbiorek9$country[i],kraiki))
  {
    zbiorek9$prawda[i]=T
    next
  }
}
#Wybieram lata z zakresu 1972-2014 oraz kraje z wektorów robionych wyżej, po czym przekształcam niektóre dane żeby zamienić k na 1000:
zbir1 <- zbir1[zbiorek1$prawda,24:66]
zbir2 <- zbir2[zbiorek2$prawda,24:66]
zbir3 <- zbir3[zbiorek3$prawda,2:44]
zbir4 <- zbir4[zbiorek4$prawda,4:46]
zbir5 <- zbir5[zbiorek5$prawda,4:46]
zbir7 <- zbir7[zbiorek7$prawda,174:216]
zbir8 <- zbir8[zbiorek8$prawda,13:55]
zbir9 <- zbir9[zbiorek9$prawda,174:216]
for (i in 1:186) {
  zbir2[i,] <- as.numeric(sub("k", "e3", zbir2[i,], fixed = TRUE))
}
for (i in 1:186) {
  zbir9[i,] <- as.numeric(sub("k", "e3", zbir9[i,], fixed = TRUE))
}
#Tworzę wektor który posiada co trzeci rok w zakresie 1972-2014 a następnie powtarza je tyle razy ile jest różnych krai. Tworzymy także wektor zawierający 15 razy nazy krai z części wspólnej:
lata <- c(1972,1975,1978,1981,1984,1987,1990,1993,1996,1999,2002,2005,2008,2011,2014)
lata186 <- lata
for (i in 1:185) {
  lata186 <- c(lata186,lata)
}
l_lat <- c(kraiki)
for (i in 1:14) {
  l_lat <- c(l_lat,kraiki)
}
#Robię ramkę danych zawierającą powtarzane kraje a następnie układam ją w kolejności alfabetycznej. Po czym przypisuję do niej powtarzane lata. Dzięki temu że mam 15 razy ten sam kraj pod rząd i po 15 różnych lat poukładanych w sposób regularny, po połączeniu powstaje każdy kraj z każdym rokiem:
moje_dane<- data.frame(country=l_lat) %>%
  arrange(country)
moje_dane <- moje_dane %>%
  mutate(year=lata186)
#Następnie tworzę kolumny na kolejne dane i je przypisuje:
moje_dane <- moje_dane %>%
  mutate(LifeExp = "") %>%
  arrange(year,country)
moje_dane$LifeExp <- c(zbir7$X1972,zbir7$X1975,zbir7$X1978,zbir7$X1981,zbir7$X1984,zbir7$X1987,zbir7$X1990,zbir7$X1993,zbir7$X1996,zbir7$X1999,zbir7$X2002,zbir7$X2005,zbir7$X2008,zbir7$X2011,zbir7$X2014)
moje_dane <- moje_dane %>%
  mutate(sex_ratio = "")
moje_dane$sex_ratio <- c(zbir1$X1972,zbir1$X1975,zbir1$X1978,zbir1$X1981,zbir1$X1984,zbir1$X1987,zbir1$X1990,zbir1$X1993,zbir1$X1996,zbir1$X1999,zbir1$X2002,zbir1$X2005,zbir1$X2008,zbir1$X2011,zbir1$X2014)
moje_dane <- moje_dane %>%
  mutate(pop_denst = "")
moje_dane$pop_denst <- as.numeric(c(zbir2$X1972,zbir2$X1975,zbir2$X1978,zbir2$X1981,zbir2$X1984,zbir2$X1987,zbir2$X1990,zbir2$X1993,zbir2$X1996,zbir2$X1999,zbir2$X2002,zbir2$X2005,zbir2$X2008,zbir2$X2011,zbir2$X2014))
moje_dane <- moje_dane %>%
  mutate(freedix = "")
moje_dane$freedix <- c(zbir3$X1972,zbir3$X1975,zbir3$X1978,zbir3$X1981,zbir3$X1984,zbir3$X1987,zbir3$X1990,zbir3$X1993,zbir3$X1996,zbir3$X1999,zbir3$X2002,zbir3$X2005,zbir3$X2008,zbir3$X2011,zbir3$X2014)
moje_dane <- moje_dane %>%
  mutate(myisw = "")
moje_dane$myisw <- c(zbir4$X1972,zbir4$X1975,zbir4$X1978,zbir4$X1981,zbir4$X1984,zbir4$X1987,zbir4$X1990,zbir4$X1993,zbir4$X1996,zbir4$X1999,zbir4$X2002,zbir4$X2005,zbir4$X2008,zbir4$X2011,zbir4$X2014)
moje_dane <- moje_dane %>%
  mutate(myism = "")
moje_dane$myism <- c(zbir5$X1972,zbir5$X1975,zbir5$X1978,zbir5$X1981,zbir5$X1984,zbir5$X1987,zbir5$X1990,zbir5$X1993,zbir5$X1996,zbir5$X1999,zbir5$X2002,zbir5$X2005,zbir5$X2008,zbir5$X2011,zbir5$X2014)
moje_dane <- moje_dane %>%
  mutate(agricultural = "")
moje_dane$agricultural <- c(zbir8$X1972,zbir8$X1975,zbir8$X1978,zbir8$X1981,zbir8$X1984,zbir8$X1987,zbir8$X1990,zbir8$X1993,zbir8$X1996,zbir8$X1999,zbir8$X2002,zbir8$X2005,zbir8$X2008,zbir8$X2011,zbir8$X2014)
moje_dane <- moje_dane %>%
  mutate(GDP_per_capita = "")
moje_dane$GDP_per_capita <- as.numeric(c(zbir9$X1972,zbir9$X1975,zbir9$X1978,zbir9$X1981,zbir9$X1984,zbir9$X1987,zbir9$X1990,zbir9$X1993,zbir9$X1996,zbir9$X1999,zbir9$X2002,zbir9$X2005,zbir9$X2008,zbir9$X2011,zbir9$X2014))
#Następnie sprawdzam, czy jest jakaś dana pusta, a jeżeli tak, to czy jej kraj nie znajduje się w wektorze “usuwacz”, jeśli tak, to dodaje dany kraj do wektora “usuwacz”:
usuwacz <- c()
for (i in 1:length(moje_dane$year)) 
{
  if ((is.element(moje_dane$freedix[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$myisw[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$myism[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$sex_ratio[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$pop_denst[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$agricultural[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
  else if ((is.element(moje_dane$GDP_per_capita[i],NA)==T)&(is.element(moje_dane$country[i],usuwacz)==F)){usuwacz <- c(usuwacz,moje_dane$country[i])}
}
#Następnie usuwam z ramki danych kraje, w których wykryto brak jakiejś danej:
pomocnicza=1
for (i in 1:length(moje_dane$country)) {
  if (is.element(moje_dane$country[pomocnicza],usuwacz)==TRUE)
  {
    moje_dane<-moje_dane[-pomocnicza,]
    pomocnicza<-pomocnicza-1
  }
  pomocnicza<-pomocnicza+1
}
#Tutaj można zobaczyć wszystkie dane (ale przez ich ilość okomentowałem to)
#moje_dane

moje_dane$myis <- (moje_dane$myism + moje_dane$myisw)/2
moje_dane <- moje_dane[,-c(7,8)]

#podział na testowy i treningowy
set.seed(100)

kraje <- moje_dane[,1:2] %>%
  group_by(country) %>%
  summarise(year = mean(year))

krajetreningowe <- kraje %>%
  slice_sample(prop=0.8)

krajetestowe <- setdiff(kraje, krajetreningowe)

train <- moje_dane %>%
  inner_join(krajetreningowe, by = "country")

test <- moje_dane %>%
  inner_join(krajetestowe, by = "country")

rownames(train) <- 1:nrow(train)

#Metoda Hellwiga
moje_dane
m <- 6
2^m - 1

cor_matrix <- cor(train[,c(3:9)])
cor_matrix

Ry <- cor_matrix[-1,1]
Rx <- cor_matrix[-1,-1]


comb <- expand.grid(rep(list(c(T,F)), m))
comb

Max <- 0
K_max <- NULL
a <- c()
#duża pętla po wierszach comb
for(i in 1:nrow(comb)-1) {
  k <- c(1:6)[unlist(comb[i,])]
  wynik <- 0
  #pętla po zmiennych w kombinacji
  for(n in k){
    wynik <- wynik + Ry[n]^2/sum(abs(Rx[n,k]))
  }
  if(wynik>Max)
  {
    Max <- wynik
    K_max <- k
  }
}


model<-lm(LifeExp ~ freedix + myis + log(GDP_per_capita),train[-c(5,130, 140,232),] )
model
summary(model)
print(plot(old_model))

#korelacje
data <- train[,c(6,8,9)]
library(corrplot)
corrplot(cor(data))
#normalnosc
library(tseries)

jarque.bera.test(model$residuals)



shapiro.test(model$residuals)
hist(model$residuals)

#Niezależność od siebie
library(lmtest)
dwtest(model)
#28, 215, 401

library(car)
vif(model)



library(sandwich)
#test Breusch-Pagan
#heteroskedastycznosc
bptest(model)
#test Chowa 
library(strucchange)

median_GDP <- median(train$GDP_per_capita)
train$group <- ifelse(train$GDP_per_capita > median_GDP, "High", "Low")
# stworzenie modeli dla poszczególnych grup
modelchow <- lm(LifeExp ~ (freedix + myis + log(GDP_per_capita))*group, data = train)

sctest(modelchow, type = "Chow")
modelchow


predykcja <- predict(model, newdata = test)
test
error <- test$LifeExp - predykcja
MAE <- mean(abs(error))
MAE
MAPE <- mean(abs(error/test$LifeExp))
MAPE
RMSE <- sqrt(mean(error^2))
RMSE



