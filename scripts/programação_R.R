
##### Modelo de escolha discreta ####
install.packages("mlogit")
library("mlogit")


#### BASE GERAL - ARQUIVO BASE_FINAL.CSV ####
train <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")


#MODELO MIXED LOGIT BASE COMPLETA
# Print.level=0 - não printa informações sobre o processo de otimização
completa<- mlogit.data(train, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
basecompleta<- mlogit(choice~tempo+atraso+valor, data=completa,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(basecompleta)

#method = bfgs (updates at each iteration the estimation of the hessian. It is often more robust and may performs well in cases where the first one doesn't work.)


#MODELO POR RENDA

brenda1=subset(train, renda==1)
baserenda1<- mlogit.data(brenda1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1<- mlogit(choice~tempo+atraso+valor, data=baserenda1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1)

brenda2=subset(train, renda==2)
baserenda2<- mlogit.data(brenda2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2<- mlogit(choice~tempo+atraso+valor, data=baserenda2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2)

brenda3=subset(train, renda==3)
baserenda3<- mlogit.data(brenda3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3<- mlogit(choice~tempo+atraso+valor, data=baserenda3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3)

brenda4=subset(train, renda==4)
baserenda4<- mlogit.data(brenda4, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4<- mlogit(choice~tempo+atraso+valor, data=baserenda4,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4)

#MODELO POR DISTANCIA

bdistancia1=subset(train, distancia==1)
basedistancia1<- mlogit.data(bdistancia1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
distancia1<- mlogit(choice~tempo+atraso+valor, data=basedistancia1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(distancia1)

bdistancia2=subset(train, distancia==2)
basedistancia2<- mlogit.data(bdistancia2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
distancia2<- mlogit(choice~tempo+atraso+valor, data=basedistancia2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(distancia2)

bdistancia3=subset(train, distancia==3)
basedistancia3<- mlogit.data(bdistancia3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
distancia3<- mlogit(choice~tempo+atraso+valor, data=basedistancia3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(distancia3)


#MODELO POR MOTIVO

bmotivo1=subset(train, motivo==1)
basemotivo1<- mlogit.data(bmotivo1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
motivo1<- mlogit(choice~tempo+atraso+valor, data=basemotivo1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(motivo1)

bmotivo2=subset(train, motivo==2)
basemotivo2<- mlogit.data(bmotivo2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
motivo2<- mlogit(choice~tempo+atraso+valor, data=basemotivo2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(motivo2)

#MODELO POR RENDA E DISTANCIA
brenda1dist1=subset(train, renda==1 & distancia==1)
baserenda1dist1<- mlogit.data(brenda1dist1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1dist1<- mlogit(choice~tempo+atraso+valor, data=baserenda1dist1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1dist1)

brenda1dist2=subset(train, renda==1 & distancia==2)
baserenda1dist2<- mlogit.data(brenda1dist2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1dist2<- mlogit(choice~tempo+atraso+valor, data=baserenda1dist2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1dist2)

brenda1dist3=subset(train, renda==1 & distancia==3)
baserenda1dist3<- mlogit.data(brenda1dist3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1dist3<- mlogit(choice~tempo+atraso+valor, data=baserenda1dist3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1dist3)



brenda2dist1=subset(train, renda==2 & distancia==1)
baserenda2dist1<- mlogit.data(brenda2dist1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2dist1<- mlogit(choice~tempo+atraso+valor, data=baserenda2dist1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2dist1)

brenda2dist2=subset(train, renda==2 & distancia==2)
baserenda2dist2<- mlogit.data(brenda2dist2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2dist2<- mlogit(choice~tempo+atraso+valor, data=baserenda2dist2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2dist2)

brenda2dist3=subset(train, renda==2 & distancia==3)
baserenda2dist3<- mlogit.data(brenda2dist3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2dist3<- mlogit(choice~tempo+atraso+valor, data=baserenda2dist3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2dist3)



brenda3dist1=subset(train, renda==3 & distancia==1)
baserenda3dist1<- mlogit.data(brenda3dist1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3dist1<- mlogit(choice~tempo+atraso+valor, data=baserenda3dist1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3dist1)

brenda3dist2=subset(train, renda==3 & distancia==2)
baserenda3dist2<- mlogit.data(brenda3dist2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3dist2<- mlogit(choice~tempo+atraso+valor, data=baserenda3dist2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3dist2)

brenda3dist3=subset(train, renda==3 & distancia==3)
baserenda3dist3<- mlogit.data(brenda3dist3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3dist3<- mlogit(choice~tempo+atraso+valor, data=baserenda3dist3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3dist3)


brenda4dist1=subset(train, renda==4 & distancia==1)
baserenda4dist1<- mlogit.data(brenda4dist1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4dist1<- mlogit(choice~tempo+atraso+valor, data=baserenda4dist1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4dist1)


brenda4dist2=subset(train, renda==4 & distancia==2)
baserenda4dist2<- mlogit.data(brenda4dist2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4dist2<- mlogit(choice~tempo+atraso+valor, data=baserenda4dist2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4dist2)


brenda4dist3=subset(train, renda==4 & distancia==3)
baserenda4dist3<- mlogit.data(brenda4dist3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4dist3<- mlogit(choice~tempo+atraso+valor, data=baserenda4dist3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4dist3)

#MODELO POR MOTIVO E RENDA

brenda1mot1=subset(train, renda==1 & motivo==1)
baserenda1mot1<- mlogit.data(brenda1mot1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1mot1<- mlogit(choice~tempo+atraso+valor, data=baserenda1mot1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1mot1)

brenda1mot2=subset(train, renda==1 & motivo==2)
baserenda1mot2<- mlogit.data(brenda1mot2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda1mot2<- mlogit(choice~tempo+atraso+valor, data=baserenda1mot2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda1mot2)



brenda2mot1=subset(train, renda==2 & motivo==1)
baserenda2mot1<- mlogit.data(brenda2mot1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2mot1<- mlogit(choice~tempo+atraso+valor, data=baserenda2mot1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2mot1)

brenda2mot2=subset(train, renda==2 & motivo==2)
baserenda2mot2<- mlogit.data(brenda2mot2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda2mot2<- mlogit(choice~tempo+atraso+valor, data=baserenda2mot2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda2mot2)




brenda3mot1=subset(train, renda==3 & motivo==1)
baserenda3mot1<- mlogit.data(brenda3mot1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3mot1<- mlogit(choice~tempo+atraso+valor, data=baserenda3mot1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3mot1)

brenda3mot2=subset(train, renda==3 & motivo==2)
baserenda3mot2<- mlogit.data(brenda3mot2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda3mot2<- mlogit(choice~tempo+atraso+valor, data=baserenda3mot2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda3mot2)



brenda4mot1=subset(train, renda==4 & motivo==1)
baserenda4mot1<- mlogit.data(brenda4mot1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4mot1<- mlogit(choice~tempo+atraso+valor, data=baserenda4mot1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4mot1)

brenda4mot2=subset(train, renda==4 & motivo==2)
baserenda4mot2<- mlogit.data(brenda4mot2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
renda4mot2<- mlogit(choice~tempo+atraso+valor, data=baserenda4mot2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(renda4mot2)



############ JÉSSICA VAI RODAR ################


#MODELO POR RENDA DECLARADA

#### BASE DECLAROU RENDA - ARQUIVO DECLAROURENDA.CSV ####

train2 <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")

#GERAL
declarourenda<- mlogit.data(train2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
declarou<- mlogit(choice~tempo+atraso+valor, data=declarourenda,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(declarou)

#MODELO DOS QUE MUDARAM DE OPINIÃO (627 - 1254 RESPOSTAS)

#### BASE MUDARAM - ARQUIVO BASE_MUDARAM.CSV ####
train3 <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")


#GERAL
mudaramop<- mlogit.data(train3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudaram<- mlogit(choice~tempo+atraso+valor, data=mudaramop,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudaram)


#POR RENDA
renda1=subset(train3, renda==1)
mudaramrenda1<- mlogit.data(renda1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudrenda1<- mlogit(choice~tempo+atraso+valor, data=mudaramrenda1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudrenda1)

renda2=subset(train3, renda==2)
mudaramrenda2<- mlogit.data(renda2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudrenda2<- mlogit(choice~tempo+atraso+valor, data=mudaramrenda2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudrenda2)

renda3=subset(train3, renda==3)
mudaramrenda3<- mlogit.data(renda3, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudrenda3<- mlogit(choice~tempo+atraso+valor, data=mudaramrenda3,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudrenda3)

renda4=subset(train3, renda==4)
mudaramrenda4<- mlogit.data(renda4, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudrenda4<- mlogit(choice~tempo+atraso+valor, data=mudaramrenda4,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudrenda4)

#POR MOTIVO


motivo1=subset(train3, motivo==1)
mudarammotivo1=mlogit.data(motivo1, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudmotivo1<- mlogit(choice~tempo+atraso+valor, data=mudarammotivo1,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudmotivo1)


motivo2=subset(train3, motivo==2)
mudarammotivo2=mlogit.data(motivo2, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))
mudmotivo2<- mlogit(choice~tempo+atraso+valor, data=mudarammotivo2,  rpar=c(valor='t', atraso='t', tempo='t'), halton=NA, panel=TRUE, R=700, print.level=0)
summary(mudmotivo2)


#PROPENSÃO A PAGAR

coef(mudaram)["atraso"]/coef(mudaram)["valor"]*60
coef(mudaram)["tempo"]/coef(mudaram)["valor"]*60

plot(rpar(declarou,'tempo'))
plot(rpar(declarou,'atraso'))
plot(rpar(declarou,'valor'))

Corr.mxl <- update(declarou, correlation = TRUE)
summary(Corr.mxl)

cor.mlogit(Corr.mxl)


#### Vetores Aleatorios 

library(truncnorm)

custo = rtruncnorm( 3000 , a = 0 , b = Inf , mean = 0.1568735 , sd = 0.0647084)
morte = rtruncnorm( 3000 , a = 0 , b = Inf , mean = 0.3586975 , sd = 0.3240246)
tempo = rtruncnorm( 3000 , a = 0 , b = Inf , mean = 0.0722171 , sd = 0.0214030)


razao1 = morte/custo
razao2 = (tempo/custo)*60

plot( density( razao1[razao1 < 100] ) , col = "blue")


final2 <- mlogit.data(finaldata, choice = "choice", shape = "wide",
                     alt.levels = c("A", "B"), sep = "_",
                     opposite = c("valor", "tempo", "atraso")
)



