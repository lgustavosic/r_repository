install.packages("dplyr")
install.packages("caret")
install.packages("rpart")
install.packages("rattle")
install.packages("hmeasure")
install.packages("pROC")
library(dplyr)
library(caret)
library(rpart)
library(rattle)
library(hmeasure)
library(pROC)
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
View(DATA)


DATA <- DATA[-c(1)]

#Separando os data frames para que a coluna TARGET mantenha-se como AS IS 
DATA_2 %>% select(1) -> DATA_2
View(DATA_2)

#Transformando os itens em DUMMIES
DF_DUMMY <- dummyVars('~.',data = DATA, sep = '_', fullRank = T)
DATA <- as.data.frame(predict(DF_DUMMY, newdata = DATA))
View(DATA_2)

#Juntando os DataFrames
DATA <- cbind(DATA,DATA_2)
View(DATA)




save(DATA,DATA_2,DATA_BKP, DF_DUMMY,satisfaction_2015_1_, file = "arq_14122020.RData")
View(DATA_2)


#ARVORE DE DECISAO

#BKP

DATA_BKP <- DATA



set.seed(1234)
#Criando o particionamento
str(DATA)

View(DATA)

#removendo as colunas que nao fazem parte do modelo 

DATA_OK <- DATA [-c(8,9,10,11,12,13,14,15,16,17,18,19,20,21)]

View(DATA_OK)
names(DATA) <- sub(" ","_",names(DATA))



INDEX_TRAIN <- createDataPartition(DATA_OK$Satisfaction, p=0.7 , list = F)
TRAIN_SET <- DATA_OK[INDEX_TRAIN,]
TEST_SET <- DATA_OK[-INDEX_TRAIN,]

#Verificando a % de Satisfied e Nuetral/inssatisfied na base de teste e treino
prop.table(table(TRAIN_SET$Satisfaction));
prop.table(table(TEST_SET$Satisfaction))



MDL_FIT <- rpart(Satisfaction ~ .  , 
                 data = TRAIN_SET,
                 method = 'class',
                 control = rpart.control(minbucket = 10, cp = -1))


MDL_FIT
summary(MDL_FIT)
par(mfrow = c(1,1))
printcp(MDL_FIT)
plotcp(MDL_FIT)

MDL_FIT.PRUNE <- prune(MDL_FIT.PRUNE, cp = 5.8444e-04)

summary(MDL_FIT.PRUNE)
printcp(MDL_FIT.PRUNE)
plotcp(MDL_FIT.PRUNE)


rpart.plot::rpart.plot(
  MDL_FIT.PRUNE,
  main = "Classification Tree"
)


Y_PROB_TRAIN <- predict(MDL_FIT.PRUNE, type = 'prob')
Y_PROB_TEST <- predict(MDL_FIT.PRUNE, newdata = TEST_SET, type = 'prob')
head(Y_PROB_TRAIN)


#Verificando o GINI
HMeasure(TRAIN_SET$Satisfaction, Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics


#Definindo Modelo Final

MDL_FINAL <- MDL_FIT.PRUNE


#importancia das variaveis
round(MDL_FINAL$variable.importance,2)


#Valores Previstos x Realizado
Y_OBS <- TEST_SET$Satisfaction


Y_CLAs1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('NO', 'YES')
                  
                  
                  )

Y_CLAs2 <- factor(ifelse(Y_PROB_TEST > 0.3,1,0),
                  levels = c(0,1),
                  labels = c('NO', 'YES')
                  
                  
)

#boxplot()


#Curva ROC
roc1 <- roc(TRAIN_SET$Satisfaction, Y_PROB_TRAIN)


confusionMatrix(data = Y_CLAs1, reference = Y_OBS)


save( DATA,DATA_2, DATA_BKP, INDEX_TRAIN, MDL_FIT, TEST_SET, TRAIN_SET , file = "arq_14122020.RData")

