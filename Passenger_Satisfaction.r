install.packages("dplyr")
install.packages("caret")
library(dplyr)
library(caret)

DATA <- satisfaction_2015_1_
View(DATA)

str(DATA)
summary(DATA)

DATA <- DATA[-c(1)]
DATA

#Outlier Flight Distance | Verificar NA's' |  Departure Delay in Minutes Arrival Delay in Minutes

View(DATA)
par(mfrow = c(1,2))
boxplot(DATA$`Departure Delay in Minutes`,border = "darkblue")
hist(DATA$`Departure Delay in Minutes`, breaks = 25)

#Departure Delay in Minutes -> Outlier  mantido devido a inforação ser verdadeira e deve ser considerada na analise
View(DATA)

par(mfrow = c(1,2))
boxplot(DATA$`Flight Distance`,border = "darkblue")
hist(DATA$`Flight Distance`, breaks = 25)


DATA %>% filter(`Flight Distance` <= 4000) -> DATA 

summary(DATA)

DF_DATA <- DATA
summary(as.data.frame(DF_DATA))
str(DF_DATA)

View(DF_DATA)
DF_DATA$Satisfaction <- as.factor(DF_DATA$Satisfaction)
DF_DATA$Gender <- as.factor(DF_DATA$Gender)
DF_DATA$`Customer Type` <- as.factor(DF_DATA$`Customer Type`)
DF_DATA$`Type of Travel` <- as.factor(DF_DATA$`Type of Travel`)
DF_DATA$Class <- as.factor(DF_DATA$Class)

summary(DF_DATA)


#Removemos os missing values de Minutes Arrivel Delay in Minutes
DF_DATA %>% filter( (DATA$`Arrival Delay in Minutes` != is.na)) -> DF_DATA2
na.omit(DF_DATA, DF_DATA$`Arrival Delay in Minutes`) -> DF_DATA2
summary(DF_DATA2)

DATA <- DF_DATA2
View(unique(DATA$Satisfaction))

#Substituindo os espaços por _ no nome das colunas
DATA_BKP <- DATA
names(DATA) <- sub(" ","_",names(DATA))
View(DATA)

#Transformando as variaveis para Dummies 
View(DATA)
DATA_2 <- DATA

DATA_2<- as_tibble(DATA_2)
DF_DUMMY <- dummyVars('~.',data = DATA_2, sep = '_', fullRank = T)
DATA_2 <- as.data.frame(predict(DF_DUMMY, newdata = DATA_2))
View(DATA_2)

names(DATA_2) <- sub(" ","_",names(DATA_2))
View(DATA_2)

#Plotando os itens Histograma 

hist( DATA_2$Age,
      col = "Blue",
      border = "white",
      breaks = 25,
      xlim = c(0,100),
      xlab = "Age",
      ylim = c(0,18000),
      ylab = "Frequency",
      main = "HIST. AGE"
      )

hist( DATA_2$Flight_Distance,
      col = "Blue",
      border = "white",
      breaks = 25,
      xlim = c(0,5000),
      xlab = "Flight Distance",
      ylim = c(0,25000),
      ylab = "Frequency",
      main = "HIST. Flight Distance"
)

save(DATA,DATA_2,DATA_BKP, DF_DUMMY,satisfaction_2015_1_, file = "arq_14122020.RData")
