## Modelo Preditivo para demanda de estoque baseada no hist�rico de vendas

# Os datasets est�o dispon�veis em: https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data

# train.csv - dados de treinamento
# test.csv - dados de teste

# A proposta deste script � criar um modelo preditivo para atender com precis�o a demanda semanal de produtos de panifica��o 
# com base em dados hist�ricos de vendas. 

# A m�trica de avalia��o � o rmsle(Root Mean Squared Logarithmic Error). Ocorre que o rmsle n�o est� dispon�vel no algoritmo XGBoost. Para criar o modelo no XGBoost foi utilizado o rmse (Root-mean-square deviation) e convertido para a m�trica desejada, conforme abaixo:
  
#  RSMLE (x) = RMSE (log (x) +1)


# O modelo foi treinado usando uma amostra de 27% dos dados de treino, devido � limita��o de mem�ria, e obteve rmse= 0,471. O 
# desktop utilizado possui 16GB de RAM. Para melhoria do resultado do treinamento, recomenda-se executar o script em um 
# computador com maior capacidade de armazenamento em mem�ria RAM.



# Carregando as bibliotecas
library(dplyr)
library(data.table)
library(xgboost)
library(highcharter)
library(viridisLite)




# Carregando os arquivos de treino, teste e a distribui��o de frequ�ncia da coluna semana.
train <- fread('train.csv',select = c("Semana",'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'),
               showProgress = F)
test <- fread('test.csv', select = c("Semana",'id','Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'), showProgress = F)
invisible(gc())
table(train$Semana)
table(test$Semana)



# Renomeando a coluna Demanda_uni_equil para target e combinando os dados de treino e teste por linhas com a fun��o rbind.
# Os datasets test, train s�o removidos com a fun��o rm e a fun��o gc � utilizada para limpeza da mem�ria.
test$target <- 0
train[,target:=Demanda_uni_equil]
train[,Demanda_uni_equil:=NULL]
r_train <- as.data.table(rbind(train, test, fill= TRUE))
rm(test, train)
invisible(gc())



# Calculando as m�dias da coluna target, considerando as colunas Semana, Cliente_ID e Producto_ID 
# A fun��o merge combina o dataset e o subset com as m�dias armazenadas nas colunas targel1 a targetl5. Adicionalmente,
# a fun��o merge cria colunas com sufixos .x e .y para diferenciar as colunas dos datasets combinados. Em seguida, as 
# colunas .y e .x s�o renomeadas. 
r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Semana=Semana+1,Cliente_ID,Producto_ID),
                                 .SDcols= colnames(r_train[, 'target'])],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
r_train[,targetl1:=target.y][,target.y:=NULL][, target:= target.x][,target.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Semana=Semana+2,Cliente_ID,Producto_ID),
                                 .SDcols= colnames(r_train[,'target'])],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
r_train[,targetl2:=target.y][,target.y:=NULL][, target:= target.x][,target.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Semana=Semana+3,Cliente_ID,Producto_ID),
                                 .SDcols= colnames(r_train[,'target'])],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
r_train[,targetl3:=target.y][,target.y:=NULL][, target:= target.x][,target.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Semana=Semana+4,Cliente_ID,Producto_ID),
                                 .SDcols= colnames(r_train[,'target'])],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
r_train[,targetl4:=target.y][,target.y:=NULL][, target:= target.x][,target.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Semana=Semana+5,Cliente_ID,Producto_ID),
                                 .SDcols= colnames(r_train[,'target'])],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
r_train[,targetl5:=target.y][,target.y:=NULL][, target:= target.x][,target.x:=NULL]
invisible(gc())



# Coleta de amostra: 27% dos dados de treino combinados com os dados de teste
# A mem�ria RAM dispon�vel � 16GB. A amostra de 27% dos dados de treino � coletada com a fun��o sample_frac e s�o combinados
# com os dados de teste atrav�s da fun��o rbind.
r_train <- as.data.table(rbind(sample_frac(r_train[Semana<10], size = 0.27), r_train[Semana>9, ] ))
invisible(gc())



# Criando distribui��es de frequ�ncia com o par�metro by, extraindo e combinando as m�dias  
# O par�metro by cria a distribui��o de frequ�ncia para nove vari�veis. As m�dias s�o extra�das do subset com o par�metro .SD/mean e 
# a fun��o merge combina o dataset com as m�dias do subset. Em seguida a coluna .y � renomeada e a coluna .x � exclu�da.
r_train  [, nAgencia_ID1 :=      .N, by = "Agencia_ID,Semana"
         ][, nRuta :=            .N, by = "Ruta_SAK,Semana"
         ][, nCliente_ID1 :=     .N, by = "Cliente_ID,Semana"
         ][, nProducto_ID1 :=    .N, by = "Producto_ID,Semana"
         ][, nRuta_Cli_ID :=     .N, by = "Ruta_SAK,Cliente_ID,Semana"
         ][, nRuta_Prod :=       .N, by = "Ruta_SAK,Producto_ID,Semana"
         ][, nRuta_Agen :=       .N, by = "Ruta_SAK,Agencia_ID,Semana"
         ][, nProd_Agen :=       .N, by = "Producto_ID,Agencia_ID,Semana"  
         ][, nProd_Ruta_Agen :=  .N, by = "Producto_ID,Ruta_SAK,Agencia_ID,Semana"] 

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Agencia_ID),
                                 .SDcols= colnames(r_train[,'nAgencia_ID1'])],by='Agencia_ID',all.x=T)
r_train[,nAgencia_ID1:=nAgencia_ID1.y][,nAgencia_ID1.y:=NULL][,nAgencia_ID1.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Ruta_SAK),
                                 .SDcols= colnames(r_train[,'nRuta'])],by='Ruta_SAK',all.x=T)
r_train[,nRuta:=nRuta.y][,nRuta.y:=NULL][,nRuta.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Cliente_ID),
                                 .SDcols= colnames(r_train[,'nCliente_ID1'])],by='Cliente_ID',all.x=T)
r_train[,nCliente_ID1:=nCliente_ID1.y][,nCliente_ID1.y:=NULL][,nCliente_ID1.x:=NULL]
invisible(gc()) 

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =.(Producto_ID),
                                 .SDcols= colnames(r_train[,'nProducto_ID1'])],by='Producto_ID',all.x=T)
r_train[,nProducto_ID1:=nProducto_ID1.y][,nProducto_ID1.y:=NULL][,nProducto_ID1.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =c('Ruta_SAK,Cliente_ID'),
                                 .SDcols= colnames(r_train[,'nRuta_Cli_ID'])],by=c('Ruta_SAK', 'Cliente_ID'),all.x=T)
r_train[,nRuta_Cli_ID:=nRuta_Cli_ID.y][,nRuta_Cli_ID.y:=NULL][,nRuta_Cli_ID.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =c('Ruta_SAK,Producto_ID'),
                                 .SDcols= colnames(r_train[,'nRuta_Prod'])],by=c('Ruta_SAK','Producto_ID'),all.x=T)
r_train[,nRuta_Prod:=nRuta_Prod.y][,nRuta_Prod.y:=NULL][,nRuta_Prod.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =c('Ruta_SAK,Agencia_ID'),
                                 .SDcols= colnames(r_train[,'nRuta_Agen'])],by=c('Ruta_SAK','Agencia_ID'),all.x=T)
r_train[,nRuta_Agen:=nRuta_Agen.y][,nRuta_Agen.y:=NULL][,nRuta_Agen.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =c('Producto_ID,Agencia_ID'),
                                 .SDcols= colnames(r_train[,'nProd_Agen'])],by=c('Producto_ID','Agencia_ID'),all.x=T)
r_train[,nProd_Agen:=nProd_Agen.y][,nProd_Agen.y:=NULL][,nProd_Agen.x:=NULL]
invisible(gc())

r_train <- merge(r_train,r_train[, lapply(.SD, mean), by =c('Producto_ID,Ruta_SAK,Agencia_ID'),
                                 .SDcols= colnames(r_train[,'nProd_Ruta_Agen'])],by=c('Producto_ID','Ruta_SAK','Agencia_ID'),all.x=T)
r_train[,nProd_Ruta_Agen:=nProd_Ruta_Agen.y][,nProd_Ruta_Agen.y:=NULL][,nProd_Ruta_Agen.x:=NULL]
invisible(gc())



# Convers�o da coluna target com a fun��o log, divis�o de dados de treino e teste
# A m�trica usada para treinamento do modelo no XGBoost � o rmse. A fun��o log � usada para converter o rmse para a m�trica 
# rmsle na coluna target. Os dados de treino e teste s�o divididos com a fun��o rbind. As semanas 10 e 11 est�o no dados de
# teste e as semanas de 3 a 9 est�o nos dados de treino. As vari�veis id e target s�o exclu�das dos dados de treino.
r_train$target <- log(r_train$target+1)
data_train <- rbind(r_train[Semana<10, ])
data_test <- rbind(r_train[Semana>9, ])
invisible(gc())
features <- names(data_train)[!(names(data_train) %in% c('id','target'))] 



# Criando dados de valida��o com o par�metro watchlist
# Removendo o dataset r_train, limpando a men�ria com a fun��o gc() e coletando amostra de 30 mil observa��es para o 
# watchlist. Em seguida, criando duas matrizes e armazenando na vari�vel watchlist.
rm(r_train)
invisible(gc())
wltst <- sample(nrow(data_train),3e4)  
dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
      label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist <- list(dval=dval)



# Preparando o modelo XGBoost
# Ajuste dos par�metros do XGboost para treinamento do modelo.
m_xgb <- xgb.train(params=list(  objective="reg:linear", 
                   booster = "gbtree",
                   nthread = 6,
                   eta= 0.56, 
                   max_depth= 10, 
                   subsample= 1,
                   colsample_bytree= 0.4),
                   data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                   label= data.matrix(data_train[-wltst,target]),missing=NA), 
                   nrounds= 150, 
                   verbose= 1,
                   print_every_n= 5,
                   early_stopping_rounds= 10,
                   watchlist= watchlist,
                   maximize= FALSE,
                   eval_metric= 'rmse'
)



# Visualiza��es do modelo criado
# Gr�ficos criados para visualiza��es de ganho entre as vari�veis preditoras
imp <- xgb.importance(features, model=m_xgb)
v_ganho <- highchart() %>%
           hc_title(text = "Ganho no modelo XGBoost") %>%
           hc_xAxis(categories = imp$Feature) %>% hc_legend(enabled = FALSE) %>% 
           hc_add_series(name = "Gain", data = imp, type = "pie", hcaes(x = Feature, y = Gain)) %>%
           hc_add_theme(hc_theme_economist())  
 v_ganho 

v_model <- highchart() %>% 
  hc_xAxis(categories = imp$Feature) %>% 
  hc_add_series(name = "Cover", data = imp$Cover) %>% 
  hc_add_series(name = "Gain", data = imp$Gain) %>% 
  hc_add_series(name = "Frequency",data = imp$Frequency) %>% 
  hc_chart(type = "column",options3d = list(enabled = TRUE, beta = 15, alpha = 15))
cols <- viridis(3)
cols <- substr(cols, 0,7)
v_model %>% hc_chart(borderColor = '#EBBA95',
                borderRadius = 10,
                borderWidth = 2,
                backgroundColor = list(
                  linearGradient = c(10, 10, 10, 500),
                  stops = list(
                    list(0, 'rgb(255, 240, 240)'),
                    list(1, 'rgb(240, 255, 255)') 
                  ))) %>%
  hc_colors(cols)




# Testando o modelo, criando previs�es para a semana 10. A convers�o dos dados, realizada com a fun��o exp, � necess�ria pois a
# fun��o log foi empregada antes do treinamento do modelo. As m�dias por produto_ID e cliente_ID na semana 10 s�o armazenadas na
# coluna targetl1 do dataset data_test_sem10.
data_test_sem10 <- data_test[Semana==10,]
pred <- predict(m_xgb,xgb.DMatrix(data.matrix(data_test_sem10[,features,with=FALSE]),missing=NA))
pred <- exp(round(pred,5))-1
res_sem10 <- data.table(id=data_test_sem10$id,Demanda_uni_equil=pred)
data_test_sem10 <- data_test_sem10[,.(Cliente_ID,Producto_ID, targetl1=pred)]
data_test_sem10 <- data_test_sem10[,.(targetl1=mean(targetl1)), by=.(Cliente_ID,Producto_ID)]




# Criando previs�es para as demandas nas semanas 10 e 11
# Subset de dados para a semana 11. Mesclando as m�dias por cliente, produto e fazendo a previs�es com a fun��o predict. Em
# seguida, convertendo os dados com a fun��o exp, combinando com a fun��o rbind e ordenando por id.
data_test_sem11 <- data_test[Semana==11,][,targetl1:=NULL]
data_test_sem11 <- merge(data_test_sem11,data_test_sem10,all.x=T,by=c('Cliente_ID','Producto_ID'))
pred <- predict(m_xgb,xgb.DMatrix(data.matrix(data_test_sem11[,features,with=FALSE]),missing=NA))
pred <- exp(round(pred,5))-1



# Salvando as previs�es criadas
# As previs�es negativas s�o zeradas e a fun��o round atribui uma casa decimal para os dados da vari�vel target. Em seguida, os
# dados s�o salvos com a fun��o fwrite.
res_sem11 <- data.table(id=data_test_sem11$id,Demanda_uni_equil=pred)
r_final <- rbind(res_sem10, res_sem11)[order(id)]
r_final[Demanda_uni_equil<0, 2]=0
r_final[,2] <- round(r_final[,2],1)
fwrite(r_final,paste0("xgb_", m_xgb$best_score, ".csv"))