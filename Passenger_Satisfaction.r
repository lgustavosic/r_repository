
DATA <- satisfaction_2015_1_
View(DATA)

str(DATA)
summary(DATA)

DATA <- DATA[-c(1)]
DATA

#Outlier Flight Distance | Verificar NA's' |  Departure Delay in Minutes Arrival Delay in Minutes

View(DATA)
boxplot(DATA$`Departure Delay in Minutes`,border = "darkblue")
hist(DATA$`Departure Delay in Minutes`, breaks = 25)

#Departure Delay in Minutes -> Outlier  mantido devido a inforação ser verdadeira e deve ser considerada na analise
View(DATA)

par(mfrow = c(1,2))
boxplot(DATA$`Flight Distance`,border = "darkblue")
hist(DATA$`Flight Distance`, breaks = 25)

install.packages("dplyr")
library(dplyr)
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
