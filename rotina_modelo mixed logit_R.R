
##### Rotina para dados finais do experimento - parte R - Modelo de escolha discreta ####
### elaborado por: Tatiana Kolodin Ferrari

#### Para a base final ver: Etapas no Stata ####

library(foreign)
finaldata <- read.dta("C:/Users/b3944459/Documents/Modal_passageiros/Dados_FInal/base_mixed.dta")
summary(finaldata)


#### Mixed Logit Model - Modelo Geral - distribui????o normal truncada ####
library("gmnl")
library("mlogit")

Hbasic <- mlogit.data(finaldata, shape="long", choice="choice", alt.var = "rota", id="id", opposite = c('custo','morte', 'tempo'))

mxl.basic500<- mlogit(choice~custo+morte+tempo, Hbasic, rpar=c(custo='cn', morte='cn', tempo='cn'), R=500, halton=NA, print.level=0, panel=TRUE)
summary(mxl.basic500)


coef(mxl.basic500)["morte"]/coef(mxl.basic500)["custo"]
coef(mxl.basic500)["tempo"]/coef(mxl.basic500)["custo"] 

plot(rpar(mxl.basic500,'morte'))
plot(rpar(mxl.basic500,'tempo'))
plot(rpar(mxl.basic500,'custo'))

Corr.mxl <- update(mxl.basic500, correlation = TRUE)
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



