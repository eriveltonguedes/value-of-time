
##### Rotina para dados finais do experimento - parte R - Expans??o da base ####
### elaborado por: Tatiana Kolodin Ferrari

# instalar pacotes

install.packages("reshape2")
install.packages("mlogit")
install.packages("gmnl")

# libray

library(reshape2)

completo <- read.csv(file="C:/Users/b3944459/Documents/Modal_passageiros/Dados_FInal/17out_completas.csv", header=TRUE, sep=",")
summary(completo)

### retirando variaveis de controle de question??rio (Ex. data da resposta, pagina, endereco, idioma)

completo<-completo[,-c(2:8)]

### definir diret??rio de trabalho ####
getwd()
setwd("C:/Users/b3944459/Documents/Modal_passageiros/Dados_FInal")

##### Organizando a base ####

colnames(completo)

base_jogo<-melt(completo, id.vars = c("id","DSE01","DSE02","DSE03","HAB01","HAB02", "HAB03", "HAB04.1.",     
                                      "HAB04.2.","HAB04.3.","HAB04.4.","HAB04.5.", "HAB04.6.","ULV01","ULV02","ULV03","UVL04",
                                      "ULV05","ULV06","ULV07","ULV08","ULV09","RAN01","ACD1", "RAN02","RAN03","CAP101.SQ001.","CAP102.SQ001.",
                                      "CAP103.SQ001.","CAP104.SQ001.","CAP105.SQ001.","CAP106.SQ001.", "CAP107.SQ001.", "CAP108.SQ001.",
                                      "CAP109.SQ001.","CAP201.SQ001.","CAP202.SQ001.","CAP203.SQ001.", "CAP204.SQ001.", "CAP205.SQ001.",
                                      "CAP206.SQ001.","CAP207.SQ001.","CAP208.SQ001.","CAP209.SQ001.","CAP301.SQ001.","CAP302.SQ001.",
                                      "CAP303.SQ001.","CAP304.SQ001.","CAP305.SQ001.","CAP306.SQ001.","CAP307.SQ001.","CAP308.SQ001.",
                                      "CAP309.SQ001.", "DEB01","DEB02", "DEB03", "COM01.1.","COM01.2.","COM01.3.","COM01.4." , "COM01.5.",
                                      "COM01.6.", "COM01.7.","COM02.1.","COM02.2.", "COM02.3." , "COM02.4." ,"COM02.5.","COM03.SQ001.",
                                      "ACD02","DSE04","DSE05","DSE06", "DSE07", "DSE08","DSE09","DSE10","FNT01","interviewtime"), 
                variable.name = "jogo", value.name = "escolha")

base_jogo<- base_jogo[which(base_jogo$escolha != ""), ]
base_jogo$choice<- ifelse(base_jogo$escolha == "A2", 1, 2)


## DUPLICANDO A BASE ###

rota <- 1
jogo1 <- base_jogo
jogo1$rota <- rota

rota <- 2
jogo2<- base_jogo
jogo2$rota <- rota

jogo_expand <- rbind(jogo1,jogo2)

write.csv(jogo_expand, file = "jogo_expand.csv")

