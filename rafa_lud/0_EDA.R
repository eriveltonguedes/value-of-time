'
2022-07 - Value of Time 
EDA 
'

library(tidyverse)
library(readr)


# import and consolidade data ---------------------------------------------

files <- list.files(path = './data', pattern = '*.csv', full.names = T)

dados1 <- read_csv2(files[1], locale=locale(encoding="latin1"))
dados2 <- read_csv2(files[2], locale=locale(encoding="latin1"))
dados3 <- read_csv2(files[3], locale=locale(encoding="latin1"))

falta <- names(dados2)[!names(dados2) %in% names(dados1)]

table(dados1$Opção_escolhida)
hist(dados1$`Tempo_Av-TAV`, 50)


d = dados1 %>% 
  select(Opção_escolhida, SbjNum) %>% 
  mutate(ind = 1) %>% 
  group_by(SbjNum, Opção_escolhida) %>% 
  summarise(FREQ = n())

prop.table(table(d$FREQ))
names(dados1)

mudou_ideia <- unlist(unique(d[d$FREQ==1,'SbjNum']))

dados1 <- dados1 %>% 
  # select(`R$ Av-TAV`) %>% 
  mutate(dif_av_tav = as.numeric(gsub(',00','', gsub("R\\$ ","",`R$ Av-TAV`))))

library(ggplot2)
g <- ggplot(dados1, aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=4)  
  # geom_line(aes(group = SbjNum))
g

dados1 %>%  
  filter(SbjNum %in% mudou_ideia) %>% 
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=2) + 
  geom_line(aes(group = SbjNum))

length(mudou_ideia)/5802



