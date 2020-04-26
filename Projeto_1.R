library(readr)
setwd('C:/Users/fmontaguti/Desktop/talkingdata-adtracking-fraud-detection')

#Carregando o dataset
#Esse dataset é uma amostra de 40M de registros do arquivo de treino
test = read_csv(file = 'train_newsample.csv')
test$X1 = NULL

head(test)
str(test)
summary(test)

source("utils_projeto1.R",encoding = "UTF-8")

#Verificando quantidade de distintos
sapply(test,function(x)length(unique(x)))

#Verificando quantidade de NA
sapply(test,function(x)sum(is.na(x)))

## Análise Exploratória ##
#Verificando balanceamento das classes
test$is_attributed = factor(test$is_attributed)
table(test$is_attributed)

library(dplyr)
library(ggplot2)
library(gridExtra)

#Evolução dos clicks por dia e se foi atribuido
test %>%
group_by(as.Date(click_time),is_attributed) %>%
  summarise(Freq=n()) %>%
  ggplot(aes(x=`as.Date(click_time)`,Freq)) + 
  geom_bar(stat = "identity",fill = "steelblue", size = 1) +
  labs(title = "Clicks gerados por Data",subtitle = "Número 1 Corresponde que o Click gerou Download")+
  facet_wrap(~is_attributed,nrow=1, scales = 'free_y')+
  xlab('')+
  ylab('Frequência')

library(lubridate)

#Evolução dos clicks por hora e se foi atribuido
test %>%
group_by(hour(click_time),is_attributed) %>%
  summarise(Freq=n()) %>%
  ggplot(aes(x=`hour(click_time)`,Freq)) + 
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") +
  labs(title = "Clicks gerados por Hora",subtitle = "Número 1 Corresponde que o Click gerou Download")+
  facet_wrap(~is_attributed,nrow=2, scales = 'free_y')+
  scale_x_continuous(breaks=seq(0,23,2))+
  xlab('Horário do Click')+
  ylab('Frequência')

#Verificando principais IP
plot.bars(test,'ip',10)

#Verificando principais app
plot.bars(test,'app',20)

#Verificando principais Device
plot.bars(test,'device',5)

#Verificando principais OS
plot.bars(test,'os',10)

#Verificando principais Canais
plot.bars(test,'channel',8)

## Pré-Processamento dos Dados

#Coletando o dia da semana / hora e minuto do click
test$weekday = factor(weekdays(test$click_time))
test$hour = hour(test$click_time)
test$minute = minute(test$click_time)
test$click_time = NULL  

str(test)

#Adicionando um contador para novas variáveis
testn = test %>%
  select(-attributed_time) %>%
  add_count(ip,weekday) %>% rename('u_ip'=n) %>%
  add_count(ip,weekday,app) %>% rename('u_ip_app'=n) %>%
  add_count(ip,weekday,device) %>% rename('u_ip_device'=n) %>%
  add_count(ip,weekday,channel) %>% rename('u_ip_channel'=n) %>%
  add_count(ip,weekday,os) %>% rename('u_ip_os'=n) %>%
  add_count(ip,weekday,hour) %>% rename('u_ip_hour'=n) 

summary(testn)

library(ROSE)
#Balanceando o Dataset por undersample
testn = ovun.sample(formula = is_attributed ~.,data = testn,method = 'under')$data

table(testn$is_attributed)

summary(testn)

#Verificando quantidade de NA
sapply(testn,function(x)sum(is.na(x)))

#Realizando o Split
library(caTools)
sample = sample.split(testn$is_attributed,SplitRatio = 0.7)
train_s = subset(testn,sample==TRUE)
test_s = subset(testn,sample==FALSE)

#Verificando proporção
table(train_s$is_attributed)
table(test_s$is_attributed)

#Treinando o modelo e verificando a importância das variáveis pelo Random Forest
library(randomForest)
rforest = randomForest(is_attributed~.,data = train_s,importance = TRUE) 
varImpPlot(rforest,main = "Importância das Variáveis")

labels = test_s$is_attributed

library(ROCR)

predicted = as.numeric(predict(rforest,newdata = test_s))
predictions = prediction(predicted,labels)

#Plotando as curvas ROC
plot.roc.curve(predictions,title.text = "Curva ROC - Modelo Random-Forest")
plot.pr.curve(predictions,title.text = "Curva Precision/Recall - Modelo Random-Forest")

#Random Forest
library(caret)
confusionMatrix(table(data=predict(rforest,newdata = test_s),reference=test_s$is_attributed),positive = '1')

#Testando modelo de regressão logistica
lr = glm(is_attributed~.,data = train_s,family = "binomial")
summary(lr)

#Regressão Logística
confusionMatrix(table(data=round(predict(lr,newdata = test_s,type = "response")),reference=test_s$is_attributed),positive = '1')

predicted2 = as.numeric(predict(lr,newdata = test_s))
predictions2 = prediction(predicted2,labels)

#Plotando as curvas ROC
plot.roc.curve(predictions2,title.text = "Curva ROC - Modelo Regressão Logística")
plot.pr.curve(predictions2,title.text = "Curva Precision/Recall - Modelo Regressão Logística")
