'
2022-07 - Value of Time 
EDA 
'

library(tidyverse)
library(readr)




# import and consolidade data ---------------------------------------------

files <- list.files(path = './data', pattern = '*.csv', full.names = T)

dados1 <- read_csv2(files[1], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_1')
dados2 <- read_csv2(files[2], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_2')
dados3 <- read_csv2(files[3], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_3')

dados2 <- dados2 %>% 
  mutate('Tempo_Av-TAV' = Tempo_avião - Tempo_TAV)

dados3 <- dados3 %>% 
  mutate('Tempo_Av-TAV' = Tempo_avião - Tempo_TAV)

falta <- names(dados2)[!names(dados2) %in% names(dados1)]

colunas <- c('fonte', 'SbjNum', 'Opção_escolhida', 
             'Tempo_Av-TAV', 'Tempo_avião', 'Tempo_TAV',
             'Valor_avião',	'Valor_TAV')

convert_money <- function(x){
  y <- as.numeric(gsub(',00','', gsub("R\\$ ","", x)))
  return(y)
}

dados_all <- dados1 %>% select(colunas) %>% 
  bind_rows(dados2 %>% select(colunas)) %>% 
  bind_rows(dados3 %>% select(colunas)) %>% 
  mutate(Opção_escolhida = ifelse(Opção_escolhida == 'Não sabe/ Não respondeu'
                                  ,'NSR', Opção_escolhida)
         ,dif_av_tav = convert_money(Valor_avião) - convert_money(Valor_TAV)
         )


choices <- dados_all %>% 
  group_by(SbjNum) %>% 
  mutate(choice = row_number()
         ,opcao_num = case_when(Opção_escolhida == "Avião"  ~ 1
                               ,Opção_escolhida == "NSR"  ~ 0
                               ,Opção_escolhida == "Trem de alta velocidade"  ~ -1)
         ) %>% 
  ungroup() %>% 
  select(fonte, SbjNum, `Tempo_Av-TAV`, dif_av_tav, Opção_escolhida, opcao_num, choice) 
  
choices <- choices %>% filter(choice == 1) %>% 
  left_join(choices %>% filter(choice == 2), suffix = c(".1", ".2"), by = c("SbjNum" = "SbjNum", 'fonte' = 'fonte'))

choices <- choices %>% 
  mutate(d_av_tav = dif_av_tav.1 - dif_av_tav.2
         ,d_t_av_tav = `Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`
         ,racional = case_when( (opcao_num.1 - opcao_num.2) > 0 &  
                               ( 
                                 (dif_av_tav.1 - dif_av_tav.2) > 0 &
                                 (`Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`) <= 0
                                 ) ~ 'Efeito_TAV_mais_caro_e_rapido'

                               ,(opcao_num.1 - opcao_num.2) < 0 &
                                 (
                                   (dif_av_tav.1 - dif_av_tav.2) < 0 &
                                     (`Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`) <= 0
                                 ) ~ 'Efeito_Avião_mais_caro'
                               ,TRUE ~ 'OUTROS'
                               ))


choices <- choices %>% 
  mutate(d_av_tav = dif_av_tav.1 - dif_av_tav.2
         ,d_t_av_tav = `Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`
         ,racional = case_when( (opcao_num.1 - opcao_num.2) > 0 &  
                                  ( 
                                    # (dif_av_tav.1 - dif_av_tav.2) > 0 &
                                      (`Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`) > 0
                                  ) ~ 'Efeito_TAV_mais_caro_e_rapido'
                                
                                ,(opcao_num.1 - opcao_num.2) < 0 &
                                  (
                                    (dif_av_tav.1 - dif_av_tav.2) < 0 &
                                      (`Tempo_Av-TAV.1` - `Tempo_Av-TAV.2`) <= 0
                                  ) ~ 'Efeito_Avião_mais_caro'
                                ,TRUE ~ 'OUTROS'
         ))

plot(choices$d_av_tav, choices$d_t_av_tav)

table(choices$racional)

choices %>% filter(racional == 'Efeito_TAV_mais_caro_e_rapido') %>% View()
check <- choices[choices$racional == 'Efeito_TAV_mais_caro_e_rapido',]$SbjNum
check <- choices[choices$racional == 'Efeito_Avião_mais_caro',]$SbjNum
check <- choices[choices$racional == 0,]$SbjNum

dados_all %>%  
  filter(SbjNum %in% check) %>% 
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=2) + 
  geom_line(aes(group = SbjNum))


table(dados_all$Opção_escolhida)
hist(dados_all$`Tempo_Av-TAV`, 50)
hist(dados_all$dif_av_tav, 50)

d = dados_all %>% 
  select(Opção_escolhida, SbjNum) %>% 
  mutate(ind = 1) %>% 
  group_by(SbjNum, Opção_escolhida) %>% 
  summarise(FREQ = n())

prop.table(table(d$FREQ))

mudou_ideia <- unlist(unique(d[d$FREQ==1,'SbjNum']))

length(mudou_ideia); length(unique(d$SbjNum))
length(mudou_ideia)/length(unique(d$SbjNum))


library(ggplot2)
g <- ggplot(dados_all, aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=4)  
  # geom_line(aes(group = SbjNum))
g

dados_all %>%  
  filter(SbjNum %in% mudou_ideia) %>% 
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=2) + 
  geom_line(aes(group = SbjNum))



# Testes ------------------------------------------------------------------

data("TravelMode",package="AER")

library(mlogit)
library(AER)
TravelMode <- mlogit.data(TravelMode,choice="choice",shape="long",
                          alt.var="mode",chid.var="individual")

TravelMode$avinc <- with(TravelMode,(mode=='air')*income)
ml.TM <- mlogit(choice ~ wait + gcost + avinc, TravelMode,
                reflevel = "car")
hl.TM <- mlogit(choice ~ wait + gcost + avinc, TravelMode,
                reflevel = "car", heterosc = TRUE)
summary(hl.TM)

summary(ml.TM)



TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
head(TM)


# estimate with mlogit
ml.TM <- mlogit(choice ~ gcost +wait +travel, TM, reflevel = "car")
#show results
summary(ml.TM)


#how fitted choice probability match with data
apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability




########### 
Hbasic <- mlogit.data(finaldata, shape="long", choice="choice", alt.var = "rota", id="id", opposite = c('custo','morte', 'tempo'))

mxl.basic500<- mlogit(choice~custo+morte+tempo, Hbasic, 
                      rpar=c(custo='cn', morte='cn', tempo='cn'), 
                      R=500, halton=NA, print.level=0, panel=TRUE)
summary(mxl.basic500)
##############

TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
head(TM)

ml.TM <- mlogit(choice ~ 1 +  gcost +wait +travel, TM, reflevel = "car"
                ,rpar=c(gcost='cn', wait = 'n'),
                R=100, halton=NA, print.level=0)
#show results
summary(ml.TM)


#how fitted choice probability match with data
apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability







data("Fishing", package = "mlogit")

rpl <- mlogit(mode ~ price + catch | income, Fishing, varying = 2:9,
              rpar = c(price= 'n', catch = 'n'), correlation = TRUE,
              alton = NA, R = 50)
summary(rpl)
rpar(rpl)
cor.mlogit(rpl)
cov.mlogit(rpl)
rpar(rpl, "catch")
summary(rpar(rpl, "catch"))




(-0.072*60)/-0.157
(-.359)/-0.157


