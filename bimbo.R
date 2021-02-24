library(readr)
getwd()
setwd('C:/Users/fmontaguti/Desktop/Bimbo')

#Carregando os arquivos
client = read_csv('cliente_tabla.csv')
product = read_csv('producto_tabla.csv')
town = read_csv('town_state.csv')
#data = read_csv('train.csv')
data = read_csv('sample.csv')
data$X1 = NULL

#Observando primeiras linhas dos datasets
head(client)
head(data)
head(product)
head(town)
str(data)

#Coletando a quantidade de semanas
s=length(unique(data$Semana))

#Verificando Registros Duplicados
table(duplicated(client$Cliente_ID))
table(duplicated(product$Producto_ID))
table(duplicated(town$Agencia_ID))

#Removendo os Registros Duplicados
doubleclient = duplicated(client$Cliente_ID)
client = subset(client,doubleclient==FALSE)

#Verificando quantidade de distintos e missing
sapply(data,function(x)(length(unique(x))))
sapply(data,function(x)(sum(is.na(x))))

summary(data)

library(dplyr)
library(ggplot2)
library(scales)

##Pre Processamento Produtos
library(stringr)

#Colocando em minúsculo o nome dos produtos
names = tolower(product$NombreProducto)
#Removendo digitos seguidos por letras
names = str_remove_all(names,'[:digit:]+[:alpha:]+')
#Removendo números iguais ou maiores que 3 digitos
names = str_remove_all(names,'[:digit:]{3,}')

library(tm)
#Carregando Stopwords 
stopwords = stopwords('es')
#Removendo Stopwords dos produtos
names = removeWords(names,stopwords)
#Realizando o TRIM
names = str_trim(names,side = 'both')
#Fazendo o split por dois espaços
newnames = str_split(names,'  ',simplify = TRUE)
#Coletando o nome dos produtos pela primeira parte do split
names = newnames[,1]
#Removendo padrões no nome dos produtos
remove = c('kc','sp','rb','sn','mm','gh','bk','rn','rw','ca','mg','cu','cj','fs','gv','jr','sc','wb')
#Removendo os padrões
names = removeWords(names,remove)

##Lidando com as marcas
#Realiza o split total do nome por ' '
split = str_split(tolower(product$NombreProducto),' ')
#Coleta as marcas dos produtos do split
brands = unique((sapply(split, function(x)x[length(x)-1])))
#Removendo as marcas 'kg e 'identificado'
removebrands = brands[c(1,17)]
brands = brands[-c(1,17)]

#Removendo as marcas dos nomes dos produtos
names = removeWords(names,brands)
#Realizando o TRIm dos nomes
names = str_trim(names,'both')
#Criando a coluna com o nome
product$Name = str_to_title(names)

##Lidando com Categorias
#Removendo números dos nomes
category = str_remove_all(names,'[:digit:]')
#Criando palavras para remover
removewords = c('super','mini','duo','medio','unico','healthy','classica','pdq')
#Removendo as palavras das categorias
category = removeWords(category,removewords)
#Realizando o TRIM
category = str_trim(category,'both')
#Realizando o split
nsplit = str_split(category,' ',simplify = TRUE)
#Coletando apenas a primeira coluna do split
category = nsplit[,1]
#Removendo o s do final de categoria
category = str_remove(category,'s$')
#Ajustando categoria Não identificada
category = sapply(category,function(x)ifelse(x=='identificado','N.A',x))
#Criando a coluna de categoria
product$Category = toupper(category)

##Lidando com o conteúdo e as unidades
#Encontra o valor e unidade do conteudo [g,kg e ml]
splitdata = str_match(split, "(\\d+)(kg|g|ml)")
#Cria as colunas 
product$Content = as.numeric(splitdata[,2])
product$Unit=splitdata[,3]

#Mapeando os dados que vieram como kg
p=sapply(product[,'Unit'],function(x)(ifelse(is.na(x),FALSE,ifelse(x=='kg',TRUE,FALSE))))
#Alterando os dados de kg para g
product[p,'Content'] = sapply(product[p,'Content'],function(x)(x*1000))
#Alterando unidade para g
product[p,'Unit'] = sapply(product[p,'Unit'],function(x)(x='g'))

#Coletando quantidade de Peças
pieces = str_match(split,'[:digit:]+(?=p\\b)')
#Inserindo 1 para peças que não tem o valor
pieces = sapply(pieces,function(x)ifelse(is.na(x),1,x))
#Criando a coluna
product$Pieces = pieces

#Adicionando a marca do produto
product$Brand = sapply(split, function(x)x[length(x)-1])
#Ajustando as marcas que vieram como KG e Identificado
product$Brand = sapply(product$Brand,function(x)ifelse(x %in% removebrands,'N.A',toupper(x)))
#Verificando Quantidade de NA da tabela Produto
sapply(product,function(x)(sum(is.na(x))))
#Unidades encontradas no dataset
unique(product$Unit)

#Verificando o tipo dos dados
str(product)

##Pre processamento das Cidades
#Removendo México para o DF
town$State = str_replace(town$State,'MÉXICO,','')
#Removendo Segundo ponto do DF
town$State = str_replace(town$State,'(?<=F)\\.','')
#Realizando o TRIM do estado e Maiúsculo
town$State = str_trim(toupper(town$State),'both')

#Removendo Ag. e Cd, das cidades e Passando pra minúsculo
town$Tname = str_replace(tolower(town$Town),'(ag\\.|cd\\.)','')
#Trocando _ por espaço em branco
town$Tname = str_replace(town$Tname,'_',' ')
#Removendo o Número da Ag
town$Tname = str_replace((town$Tname),'([:digit:]+)','')  
#Removendo Números adicionais
town$Tname = str_replace((town$Tname),'[:digit:]','') 
#Removendo I no final das palavras
town$Tname = str_replace(town$Tname,'(?<=\\s)i+','')
#Realizando o TRIM
town$Tname = str_trim(toupper(town$Tname),side = 'both')
#Buscando o Número
town$Number = as.numeric(str_extract(town$Town,'[:digit:]{4}'))

#Análise Exploratória
data %>%
  group_by(Semana) %>%
  summarise(Unidades=sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=Semana,y=Unidades,group=1))+
  geom_line(color = "steelblue", size = 1)+
  geom_point(color = "steelblue")+
  labs(title = 'Demanda por Semana')+
  scale_y_continuous(labels=label_number_si(accuracy = .01))+
  scale_x_continuous(breaks = seq(3:11),minor_breaks = 1)

#Principais Estados
topstates = data %>%
  left_join(town,by = 'Agencia_ID') %>%
  group_by(State) %>%
  summarise(Clients=length(unique(Cliente_ID)),
            Units=sum(Venta_uni_hoy),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100))%>%
  arrange(desc(Net_U))

#Demanda por Estado
ggplot(topstates,aes(x=reorder(State,Net_U),y=Net_U/s))+
  geom_bar(stat = "identity",aes(fill = Return_rate))+
  coord_flip()+
  scale_y_continuous(labels=label_number_si())+
  scale_fill_gradient(low="deepskyblue4", high="darkorange")+
  labs(title='Demanda por Estado', fill="Taxa de\nRetorno %")+
  ylab('Unidades por Semana')+
  xlab('Estado')

#Principais Agências  
topagencia = data %>%
  group_by(Agencia_ID) %>%
  summarise(Clients=length(unique(Cliente_ID)),
            Products=length(unique(Producto_ID)),
            Units=sum(Venta_uni_hoy),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100))%>%
  arrange(desc(Net_U)) %>%
  left_join(town,by = 'Agencia_ID') %>%
  select(-Town)

#Histograma Demanda Agencias
ggplot(topagencia,aes(x=Net_U/s))+
  geom_histogram(fill='steelblue',colour='black')+
  labs(title='Histograma Demanda das Agências')+
  ylab('')+
  scale_x_continuous(labels=label_number_si())+
  xlab('Unidades por Semana')

#Histograma Produtos Agencias
ggplot(topagencia,aes(x=Products))+
  geom_histogram(fill='steelblue',colour='black')+
  labs(title='Histograma Nº Produtos por Agência')+
  ylab('')+
  xlab('Produtos')

library(treemap)
#Top 50 Agencias
treemap(topagencia[1:50,],index = c('Agencia_ID'),vSize = 'Net_U',vColor = 'Return_rate',
        type="value",title = 'Top 50 Agencias',palette = 'Spectral',title.legend = 'Taxa de Retorno %')

#Principais Produtos 
topproducts = data %>%
  group_by(Producto_ID) %>%
  summarise(Clients=length(unique(Cliente_ID)),
            Units=sum(Venta_uni_hoy),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100),
         AVG_Price=Pesos/Units)%>%
  arrange(desc(Net_U))%>%
  left_join(product,by = 'Producto_ID')

#Demanda principais produtos por Nome
topproducts %>%
  group_by(Name) %>%
  summarise(Units=sum(Units),
            Return_U=sum(Return_U),
            Net_U=sum(Net_U)) %>%
  mutate(Return_rate=((Return_U/(Units+Return_U))*100)) %>%
  arrange(desc(Net_U)) %>%
  head(20) %>%
  ggplot(aes(x=reorder(Name,Net_U)))+
  geom_bar(stat = "identity",aes(y=Net_U/s,fill=Return_rate))+
  coord_flip()+
  scale_y_continuous(labels=label_number_si())+
  scale_fill_gradient(low="deepskyblue4", high="darkorange")+
  labs(title='Demanda Top 20 Produtos', fill="Taxa de\nRetorno %")+
  ylab('Unidades por Semana')+
  xlab('')

#Histograma Preço Médio dos Produtos
ggplot(topproducts,aes(x=AVG_Price))+
  geom_histogram(fill='steelblue',colour='black')+
  labs(title='Histograma Preço Médio dos Produtos')+
  ylab('')+
  scale_x_continuous(limits = c(0,150))+
  xlab('Pesos')

#Demanda Semanal por Marcas
topproducts %>%
  group_by(Brand) %>%
  summarise(Demand=sum(Net_U)) %>%
  arrange(desc(Demand)) %>%
  head(10) %>%
  ggplot(aes(x=reorder(Brand,-Demand)))+
  geom_bar(stat = 'identity',aes(y=Demand/s),fill='skyblue4')+
  scale_y_continuous(labels=label_number_si())+
  ylab('Unidades')+
  xlab('Marca')+
  labs(title='Demanda Semanal Top 10 Marcas')

#Principais Clientes 
topclients = data %>%
  group_by(Cliente_ID) %>%
  summarise(Units=sum(Venta_uni_hoy),
            Products=length(unique(Producto_ID)),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100))%>%
  arrange(desc(Net_U))%>%
  left_join(client,by = 'Cliente_ID') 

#Top 30 Clientes
treemap(topclients[1:30,],index = c('NombreCliente'),vSize = 'Net_U',vColor = 'Return_rate',
        type="value",title = 'Top 30 Clientes',palette = 'Spectral',title.legend = 'Taxa de Retorno %',fontsize.labels = 8)

#Principais Canais
topchanels = data %>%
  group_by(Canal_ID) %>%
  summarise(Clients=length(unique(Cliente_ID)),
            Agencies = length(unique(Agencia_ID)),
            Units=sum(Venta_uni_hoy),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100),
         Percent=percent(Net_U/sum(Net_U)))%>%
  arrange(desc(Net_U))

#Distribuição da demanda por Canal
treemap(topchanels,index = c('Canal_ID','Percent'),vSize = 'Net_U',type = 'index',title = 'Demanda por Canal',
        align.labels = list(c("center", "center"),c("center", "bottom")))

#Agencias por Canal
ggplot(topchanels,aes(x=reorder(Canal_ID,-Agencies),y=Agencies))+
  geom_bar(stat = "identity",fill='skyblue4')+
  labs(title='Agências por Canal')+
  ylab('QTD')+
  xlab('')

#Principais Rotas
toproute = data %>%
  group_by(Ruta_SAK) %>%
  summarise(Clients=length(unique(Cliente_ID)),
            Agencies = length(unique(Agencia_ID)),
            Products=length(unique(Producto_ID)),
            Units=sum(Venta_uni_hoy),
            Pesos=sum(Venta_hoy),
            Return_U=sum(Dev_uni_proxima),
            Return_P=sum(Dev_proxima),
            Net_U=sum(Demanda_uni_equil)) %>%
  mutate(Net_P=Pesos-Return_P,
         Return_rate=((Return_U/(Units+Return_U))*100))%>%
  arrange(desc(Net_U))

#Histograma Demandas Rotas
ggplot(toproute,aes(x=Net_U/s))+
  geom_histogram(fill='steelblue',colour='black')+
  labs(title='Histograma Demanda das Rotas')+
  ylab('')+
  scale_x_continuous(labels=label_number_si())+
  xlab('Unidades por Semana')

#Histograma Agências Rotas
ggplot(toproute,aes(x=Agencies))+
  geom_histogram(fill='steelblue',colour='black')+
  labs(title='Histograma Nº Agências por Rota')+
  ylab('')+
  xlab('Agências')

#Principais Rotas
ggplot(toproute[1:20,],aes(x=reorder(Ruta_SAK,Net_U)))+
  geom_bar(stat = "identity",aes(y=Net_U/s,fill=Return_rate))+
  coord_flip()+
  scale_y_continuous(labels=label_number_si())+
  scale_fill_gradient(low="deepskyblue4", high="darkorange")+
  labs(title='Demanda Top 20 Rotas', fill="Taxa de\nRetorno %")+
  ylab('Unidades por Semana')+
  xlab('Número da Rota')

#Removendo colunas que não existem nos dados de teste e criando a Mediana para Produto e Mediana para Produto-CLiente
data = data %>%
  select(-Venta_uni_hoy,-Venta_hoy,-Dev_uni_proxima,-Dev_proxima) %>%
  group_by(Producto_ID) %>%
  mutate(Pmedian = median(Demanda_uni_equil)) %>%
  group_by(Producto_ID,Cliente_ID) %>%
  mutate(PCmedian = median(Demanda_uni_equil)) %>%
  ungroup()

#Verificando a distribuição
table(data$Semana)

#Criando split pelas semanas
train = subset(data,Semana<=7)
test = subset(data,Semana>7)

#Removendo o dataset 
rm(data)

#Criando a matrix de correlação
cor = cor(train)
library(corrplot)
#Plotando a correlação das variáveis
corrplot.mixed(cor, lower.col = "black", number.cex = .7, tl.cex=0.7)

#Treinando Regressão Linear
library(caret)
lm=train(Demanda_uni_equil~.,data=train,method='lm',metric="RMSE")
lm

#Importância das variáveis - LM
plot(varImp(lm),main = 'Importância das Variáveis - LM')

#Criando Previsões pelo LM
plm = predict(lm,newdata = test)
#Verificando nos dados de teste
postResample(plm,test$Demanda_uni_equil)

#Treinando Modelo GBM
library(gbm)
gbm = train(Demanda_uni_equil~.,data=train,method='gbm',metric="RMSE",verbose=FALSE)
gbm

plot(gbm,main='GBM')

#Importância das variáveis - GBM
plot(varImp(gbm),main = 'Importância das Variáveis - GBM')

#Criando previsões pelo GBM
pgbm = predict(gbm,newdata = test)
#Veficando nos dados de teste
postResample(pgbm,test$Demanda_uni_equil)

