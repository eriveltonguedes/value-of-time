##### Modelo de escolha discreta ####
install.packages("mlogit")
library("mlogit")


#### BASE GERAL - ARQUIVO BASE_FINAL.CSV ####
train <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")

#MODELO MIXED LOGIT BASE COMPLETA
completa<- mlogit.data(train, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
basecompleta<- mlogit(choice~tempo+atraso+valor, data=completa,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
comp <- summary(basecompleta)


#POR RENDA
macro1<-function(var1,j){
  resultados <- list()
  n <- 1
  for(i in 1:j){
      b1=subset(train, train[,var1]==i)
      b2=mlogit.data(b1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
      b3=mlogit(choice~tempo+atraso+valor, data=b2, rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=FALSE, R=30, print.level=0)
      b4= summary(b3)
      
      b5=coef(b3)["atraso"]/coef(b3)["valor"]*60
      b6=coef(b3)["tempo"]/coef(b3)["valor"]*60
      
      resultados[[n]] <- list(b4, b5, b6)
      n <- n+1
    } return(resultados)}


a <- macro1(17,4)


#POR DIST?NCIA

b <- macro1(16,3)

#POR MOTIVO

c <- macro1(13,2)


#POR RENDA E MOTIVO

macro2<-function(var1, var2,q,w){
  resultados <- list()
  n <- 1
  for(i in 1:q){
    for (j in 1:w){
      
      b1=subset(train, train[,var1]==i & train3[,var2]==j)
      b2=mlogit.data(b1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
      b3=mlogit(choice~tempo+atraso+valor, data=b2, rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=FALSE, R=30, print.level=0)
      b4= summary(b3)
      
      b5=coef(b3)["atraso"]/coef(b3)["valor"]*60
      b6=coef(b3)["tempo"]/coef(b3)["valor"]*60
      
      resultados[[n]] <- list(b4, b5, b6)
      n <- n+1
    }  }  return(resultados)}

d <- macro2(17,13,4,2)

#POR RENDA E DIST?NCIA

e <-macro2(17,16,4,3)


#MODELO POR RENDA DECLARADA

#### BASE DECLAROU RENDA - ARQUIVO DECLAROURENDA.CSV ####

train2 <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")

#GERAL
declarourenda<- mlogit.data(train2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
declarou<- mlogit(choice~tempo+atraso+valor, data=declarourenda,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
f<- summary(declarou)

#MODELO DOS QUE MUDARAM DE OPINI?O (627 - 1254 RESPOSTAS)

#### BASE MUDARAM - ARQUIVO BASE_MUDARAM.CSV ####
train3 <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")

#GERAL
mudaramop<- mlogit.data(train3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudaram<- mlogit(choice~tempo+atraso+valor, data=mudaramop,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
g <- summary(mudaram)

#POR RENDA

macro3<-function(var1,j){
  resultados <- list()
  n <- 1
  for(i in 1:j){
      b1=subset(train3, train3[,var1]==i)
      b2=mlogit.data(b1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
      b3=mlogit(choice~tempo+atraso+valor, data=b2, rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=FALSE, R=30, print.level=0)
      b4= summary(b3)
      
      b5=coef(b3)["atraso"]/coef(b3)["valor"]*60
      b6=coef(b3)["tempo"]/coef(b3)["valor"]*60
      
      resultados[[n]] <- list(b4, b5, b6)
      n <- n+1
    }   return(resultados)}


h <- macro3(17,4)

#POR MOTIVO

k <- macro3(13,2)