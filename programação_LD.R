
##### Modelo de escolha discreta ####
install.packages("mlogit")
library("mlogit")


#### BASE GERAL - ARQUIVO BASE_FINAL.CSV ####
train <- read.csv(file.choose(T), header = TRUE, sep=";", dec=",")


#MODELO MIXED LOGIT BASE COMPLETA
# Print.level=0 - n?o printa informa??es sobre o processo de otimiza??o
completa<- mlogit.data(train, choice="choice", shape="wide", varying=4:9, id="id", alt.levels=c("A","B"), sep="_", opposite = c("tempo","atraso", "valor"))

# Completa #
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



# exportar resultados pro excel
write.csv(coef(summary(renda1)), file= "res_dist1.csv")
write.csv(coef(summary(renda1)), file= "res_dist2.csv")
write.csv(coef(summary(renda1)), file= "res_dist3.csv")

write.csv(coef(summary(renda1)), file= "res_mot1.csv")
write.csv(coef(summary(renda1)), file= "res_mot2.csv")

write.csv(coef(summary(renda1)), file= "res_ren1.csv") 
write.csv(coef(summary(renda1)), file= "res_ren2.csv")
write.csv(coef(summary(renda1)), file= "res_ren3.csv") 
write.csv(coef(summary(renda1)), file= "res_ren4.csv") 





