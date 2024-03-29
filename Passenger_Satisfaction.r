# install.packages("dplyr")
# install.packages("caret")
# install.packages("rpart")
# install.packages("rattle")
# install.packages("hmeasure")
# install.packages("pROC")
library(dplyr)
library(magrittr)
library(MASS)
library(car)
library(caret)
library(rpart)
library(rattle)
library(hmeasure)
library(pROC)
library(readxl)

filter <- dplyr::filter
select <- dplyr::select
satisfaction_2015_1_ <- read_excel("satisfaction_2015 (1).xlsx")
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
######DF_DATA %>% filter( (DATA$`Arrival Delay in Minutes` != is.na)) -> DF_DATA2
na.omit(DF_DATA, DF_DATA$`Arrival Delay in Minutes`) -> DF_DATA2
summary(DF_DATA2)

DATA <- DF_DATA2
View(unique(DATA$Satisfaction))

#Substituindo os espaços por _ no nome das colunas
DATA_BKP <- DATA
names(DATA) <- sub(" ","_",names(DATA))
View(DATA)
DATA_BKP <- DATA
names(DATA) <- sub(" ","_",names(DATA))
View(DATA)
DATA_BKP <- DATA
names(DATA) <- sub(" ","_",names(DATA))
View(DATA)
DATA_BKP <- DATA
names(DATA) <- sub("/","-",names(DATA))
View(DATA)


# Loop repetido 3 e 2 vezes para eliminar totalmente os espaços e acentos

#Transformando as variaveis para Dummies 
DATA_2 <- DATA
View(DATA)


DATA <- DATA[-c(1)]

#Separando os data frames para que a coluna TARGET mantenha-se como AS IS 
DATA_2[c(1)] -> DATA_2
View(DATA_2)

#Transformando os itens em DUMMIES
DF_DUMMY <- dummyVars('~.',data = DATA, sep = '_', fullRank = T)
DATA <- as.data.frame(predict(DF_DUMMY, newdata = DATA))
View(DATA)

#Juntando os DataFrames
DATA <- cbind(DATA,DATA_2)
View(DATA)

### Base apenas com features fora do questionário p/ REGLOG
DATA[-c(8:21)] -> DATA_REGLOG

## Linhas Executadas até aqui. Pular para linha 220 para regressão logística


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

MDL_FIT.PRUNE <- prune(MDL_FIT, cp = 5.8444e-04)

summary(MDL_FIT.PRUNE)
printcp(MDL_FIT.PRUNE)
plotcp(MDL_FIT.PRUNE)


rpart.plot::rpart.plot(
  MDL_FIT.PRUNE,
  main = "Classification Tree"
)


Y_PROB_TRAIN <- predict(MDL_FIT.PRUNE, type = 'prob')[,2]
Y_PROB_TEST <- predict(MDL_FIT.PRUNE, newdata = TEST_SET, type = 'prob')[,2]
head(Y_PROB_TRAIN)


#Verificando o GINI
HMeasure(TRAIN_SET$Satisfaction, Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics


#Definindo Modelo Final

MDL_FINAL <- MDL_FIT.PRUNE

DATA_OK$prob_predicted_tree  <- predict(MDL_FINAL, newdata = DATA_OK, type = 'prob')
cbind(DATA,DATA_OK$prob_predicted_tree) -> DATA

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


# confusionMatrix(data = Y_CLAs1, reference = Y_OBS)

######### Regressão Logística

### Linearizar variáveis.
##### Avaliar necessidades via histograma das quantitativas
##### Endereçar com mutate e funções
### Separar em treino e teste
### Gerar modelo full e stepwise e realizar treino
### Realizar predições
### Avaliar performance e selecionar finalista
### Analisar importância das variáveis
# 
par(mfrow = c(2,2))

hist(DATA_REGLOG$Age, breaks = 10,
     xlab = 'Age', main = ''
)
hist(DATA_REGLOG$Flight_Distance, breaks = 10,
     xlab = 'Flight Distance', main = ''
)
hist(DATA_REGLOG$Departure_Delay_in_Minutes, breaks = 500,
     xlab = 'Departure Delay (min)', main = ''
)
hist(DATA_REGLOG$Arrival_Delay_in_Minutes, breaks = 500,
     xlab = 'Arrival Delay (min)', main = ''
)

mtext('Frequency Distribution', side = 3, line = -2, outer = TRUE)

# Age ok. As demais precisam

# Linearizar flight distance

hist(DATA_REGLOG$Flight_Distance, breaks = 10,
     xlab = 'Original', main = ''
)
hist(log10(DATA_REGLOG$Flight_Distance), breaks = 10,
     xlab = 'Log10', main = ''
)
hist(log(DATA_REGLOG$Flight_Distance), breaks = 10,
     xlab = 'Ln', main = ''
)
hist(DATA_REGLOG$Flight_Distance**(1/4), breaks = 10,
     xlab = expression(sqrt('X',4)), main = ''
)

mtext('Linearization - Flight Distance', side = 3, line = -2, outer = TRUE)

#Vamos de raiz quarta

DATA_REGLOG %>%
  mutate(
    Flight_Distance =
      Flight_Distance**(1/4)
  ) -> DATA_REGLOG

# Linearizar Departure delay

hist(DATA_REGLOG$Departure_Delay_in_Minutes, breaks = 100,
     xlab = 'Original', main = ''
)
hist(log10(DATA_REGLOG$Departure_Delay_in_Minutes+0.1), breaks = 10,
     xlab = 'Log10', main = ''
)
hist(log(DATA_REGLOG$Departure_Delay_in_Minutes+0.1), breaks = 10,
     xlab = 'Ln', main = ''
)
hist((DATA_REGLOG$Departure_Delay_in_Minutes+0.1)**(1/4), breaks = 10,
     xlab = expression(sqrt('X',4)), main = ''
)

mtext('Linearization - Departure Delay (min)', side = 3, line = -2, outer = TRUE)

#Nada é muito bom.... raiz quarta serve

DATA_REGLOG %>%
  mutate(
    Departure_Delay_in_Minutes =
      Departure_Delay_in_Minutes**(1/4)
  ) -> DATA_REGLOG

#Linearizar Arrival delays

hist(DATA_REGLOG$Arrival_Delay_in_Minutes, breaks = 100,
     xlab = 'Original', main = ''
)
hist(log10(DATA_REGLOG$Arrival_Delay_in_Minutes+0.1), breaks = 10,
     xlab = 'Log10', main = ''
)
hist(log(DATA_REGLOG$Arrival_Delay_in_Minutes+0.1), breaks = 10,
     xlab = 'Ln', main = ''
)
hist((DATA_REGLOG$Arrival_Delay_in_Minutes+0.1)**(1/4), breaks = 10,
     xlab = expression(sqrt('X',4)), main = ''
)

mtext('Linearization - Arrival Delay (min)', side = 3, line = -2, outer = TRUE)

#Nada é muito bom.... raiz quarta serve

DATA_REGLOG %>%
  mutate(
    Arrival_Delay_in_Minutes =
      (Arrival_Delay_in_Minutes+0.1)**(1/4)
  ) -> DATA_REGLOG

#feito

## Separar em treino e teste e manter target separada

set.seed(9876) # garantindo reprodutibilidade da amostra

REGLOG_INDEX_TRAIN <- createDataPartition(DATA_REGLOG$Satisfaction, p = 0.7, list = F)
REGLOG_TRAIN_SET <- DATA_REGLOG[REGLOG_INDEX_TRAIN, ] # base de desenvolvimento: 70%
REGLOG_TEST_SET  <- DATA_REGLOG[-REGLOG_INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
prop.table(table(REGLOG_TRAIN_SET$Satisfaction));prop.table(table(REGLOG_TEST_SET$Satisfaction))

# Proporções permanecem parecidas (57/43)

# Realizar treino -> criar modelo full

REG_MDL_FIT <- glm(Satisfaction ~ ., data= REGLOG_TRAIN_SET, family = binomial(link='logit'))
REG_MDL_FIT

summary(REG_MDL_FIT)
vif(REG_MDL_FIT)

REG_MDL_FIT.STEP <- stepAIC(REG_MDL_FIT,
                            scope = list(lower = as.formula("Satisfaction ~ 1"),
                                         upper = as.formula("Satisfaction ~ .")),
                            direction = 'both', trace = TRUE)

# Realizar predições

REGLOG_Y_PROB_TRAIN <- predict(REG_MDL_FIT, type = 'response')
REGLOG_Y_PROB_TEST  <- predict(REG_MDL_FIT, newdata = REGLOG_TEST_SET, type = 'response')

REGLOG_Y_PROB_TRAIN.STEP <- predict(REG_MDL_FIT.STEP, type = 'response')
REGLOG_Y_PROB_TEST.STEP  <- predict(REG_MDL_FIT.STEP, newdata = REGLOG_TEST_SET, type = 'response')

# Avaliando performance

HMeasure(REGLOG_TRAIN_SET$Satisfaction,REGLOG_Y_PROB_TRAIN)$metrics
HMeasure(REGLOG_TEST_SET$Satisfaction, REGLOG_Y_PROB_TEST)$metrics

# Regressao com stepwise
HMeasure(REGLOG_TRAIN_SET$Satisfaction,REGLOG_Y_PROB_TRAIN.STEP)$metrics
HMeasure(REGLOG_TEST_SET$Satisfaction, REGLOG_Y_PROB_TEST.STEP)$metrics

# Definindo modelo final. Comentando as linhas de linerização (231 - 325), foi avaliado
# a qualidade do modelo para as variáveis cruas
REG_MDL_FINAL <- REG_MDL_FIT.STEP

anova(REG_MDL_FINAL)


DATA_REGLOG$prob_predicted_reglog  <- predict(REG_MDL_FINAL, newdata = DATA_REGLOG, type = 'response')

cbind(DATA,DATA_REGLOG$prob_predicted_reglog) -> DATA

## Análise top e bottom 25%
## Usando modelo de árvore como referência


DATA %>%
  filter(
    satisfied >= 0.75 | satisfied <= 0.25
  ) %>%
  mutate(
    case_when(
      satisfied >= 0.75 ~ '>75%',
      satisfied <= 0.25 ~ '<25%'
    )
  ) %>% 
  rename(quantile=`case_when(satisfied >= 0.75 ~ ">75%", satisfied <= 0.25 ~ "<25%")`
    
  ) %>%
  select(-c(25,27)) -> DATA_BIVAR


names(DATA_BIVAR) <- sub("`","",names(DATA_BIVAR))
names(DATA_BIVAR) <- sub("`","",names(DATA_BIVAR))
names(DATA_BIVAR) <- sub("-","",names(DATA_BIVAR))

## Partiu análise bivariada. Feature no eixo X, classe como legenda em coluna empilhada

par(mfrow = c(2,3))

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Gender_Male),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Gender_Male', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Customer_Type_Loyal_Customer),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Customer_Type_Loyal', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

boxplot(Age ~ quantile, data = DATA_BIVAR,
        main = '', ylab = 'Value', xlab = 'Age',
        col = c('dodgerblue','dodgerblue4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Type_of_Travel_Personal_Travel),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Personal_travel', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Class_Eco),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Class_Eco', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Class_Eco_Plus),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Class_Eco_Plus', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext('Bivariate Analysis - Bottom/Top 25% (1/4)', side = 3, line = -2, outer = TRUE)


boxplot(Flight_Distance ~ quantile, data = DATA_BIVAR,
        main = '', ylab = 'Value', xlab = 'Flight_Distance',
        col = c('dodgerblue','dodgerblue4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Inflight_wifi_service),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Inflight_wifi', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$DepartureArrival_time_convenient),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'time_convenient', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Ease_of_Online_booking),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Online_booking', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Gate_location),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Gate_location', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Food_and_drink),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Food_and_drink', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext('Bivariate Analysis - Bottom/Top 25% (2/4)', side = 3, line = -2, outer = TRUE)

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Online_boarding),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Online_boarding', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Seat_comfort),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Seat_comfort', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Inflight_entertainment),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Inflight_entertainment', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Onboard_service),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Onboard_service', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Leg_room_service),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Leg_room_service', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Baggage_handling),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Baggage_handling', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext('Bivariate Analysis - Bottom/Top 25% (3/4)', side = 3, line = -2, outer = TRUE)

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Checkin_service),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Checkin_service', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Inflight_service),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Inflight_service', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_BIVAR$quantile,DATA_BIVAR$Cleanliness),2),
        main = '',
        ylab = 'Frequency (%)', xlab = 'Cleanliness', 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

boxplot(Departure_Delay_in_Minutes ~ quantile, data = DATA_BIVAR,
        main = '', ylab = 'Value (lim at 50)', xlab = 'Departure_Delay_in_Minutes',
        ylim = c(0,50),
        col = c('dodgerblue','dodgerblue4'), border = 'gray20')

boxplot(Arrival_Delay_in_Minutes ~ quantile, data = DATA_BIVAR,
        main = '', ylab = 'Value (lim at 50)', xlab = 'Arrival_Delay_in_Minutes',
        ylim = c(0,50),
        col = c('dodgerblue','dodgerblue4'), border = 'gray20')

barplot(table(DATA_BIVAR$quantile),
        main = '',
        ylab = 'Quantity', xlab = 'Classification', 
        col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext('Bivariate Analysis - Bottom/Top 25% (4/4)', side = 3, line = -2, outer = TRUE)