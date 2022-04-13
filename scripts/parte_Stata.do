

insheet using "D:\Users\B3944459\Documents\Passageiros_final\jogo_expand.csv", comma

drop v1
sort id

***** Encode, replace e outras modificações necessárias *******

encode dse02, generate(DSE02)
encode hab02, generate(HAB02)
replace HAB02=0 if HAB02==5
encode hab03, generate(HAB03)

encode hab041, generate(HAB041)
encode hab042, generate(HAB042)
encode hab043, generate(HAB043)
encode hab044, generate(HAB044)
encode hab045, generate(HAB045)
encode hab046, generate(HAB046)

replace HAB041=0 if HAB041==. & HAB02~=0
replace HAB042=0 if HAB042==. & HAB02~=0
replace HAB043=0 if HAB043==. & HAB02~=0
replace HAB044=0 if HAB044==. & HAB02~=0
replace HAB045=0 if HAB045==. & HAB02~=0
replace HAB046=0 if HAB046==. & HAB02~=0

encode ulv01, generate(ULV01)
encode ulv02, generate(ULV02)
encode ulv03, generate(ULV03)

replace ULV02=. if ULV02==28
replace ULV03=. if ULV03==28

encode uvl04, generate(ULV04)
replace ULV04=. if ULV04==7

encode ulv05, generate(ULV05)
replace ULV05=. if ULV05==11

encode ulv06, generate(ULV06)
encode ulv07, generate(ULV07)
encode ulv08, generate(ULV08)
encode ulv09, generate(ULV09)

drop hab02 hab03 hab041 hab042 hab043 hab044 hab045 hab046 ulv01 ulv02 ulv03 uvl04 ulv05 ulv06 ulv07 ulv08 ulv09
drop dse02

encode cap101sq001, generate(CAP101)
replace CAP101=. if CAP101==11
encode cap102sq001, generate(CAP102)
replace CAP102=. if CAP102==11
encode cap103sq001, generate(CAP103)
replace CAP103=. if CAP103==11
encode cap104sq001, generate(CAP104)
replace CAP104=. if CAP104==11
encode cap105sq001, generate(CAP105)
replace CAP105=. if CAP105==11
encode cap106sq001, generate(CAP106)
replace CAP106=. if CAP106==11
encode cap107sq001, generate(CAP107)
replace CAP107=. if CAP107==10
encode cap108sq001, generate(CAP108)
replace CAP108=. if CAP108==11
encode cap109sq001, generate(CAP109)
replace CAP109=. if CAP109==11

encode cap201sq001, generate(CAP201)
replace CAP201=. if CAP201==11
encode cap202sq001, generate(CAP202)
replace CAP202=. if CAP202==11
encode cap203sq001, generate(CAP203)
replace CAP203=. if CAP203==11
encode cap204sq001, generate(CAP204)
replace CAP204=. if CAP204==11
encode cap205sq001, generate(CAP205)
replace CAP205=. if CAP205==11
encode cap206sq001, generate(CAP206)
replace CAP206=. if CAP206==11
encode cap207sq001, generate(CAP207)
replace CAP207=. if CAP207==11
encode cap208sq001, generate(CAP208)
replace CAP208=. if CAP208==11
encode cap209sq001, generate(CAP209)
replace CAP209=. if CAP209==11

encode cap301sq001, generate(CAP301)
replace CAP301=. if CAP301==11
encode cap302sq001, generate(CAP302)
replace CAP302=. if CAP302==11
encode cap303sq001, generate(CAP303)
replace CAP303=. if CAP303==11
encode cap304sq001, generate(CAP304)
replace CAP304=. if CAP304==11
encode cap305sq001, generate(CAP305)
replace CAP305=. if CAP305==11
encode cap306sq001, generate(CAP306)
replace CAP306=. if CAP306==11
encode cap307sq001, generate(CAP307)
replace CAP307=. if CAP307==11
encode cap308sq001, generate(CAP308)
replace CAP308=. if CAP308==11
encode cap309sq001, generate(CAP309)
replace CAP309=. if CAP309==11

drop cap101sq001 cap102sq001 cap103sq001 cap104sq001 cap105sq001 cap106sq001 cap107sq001 cap108sq001 cap109sq001 cap201sq001 cap202sq001 cap203sq001 cap204sq001 cap205sq001 cap206sq001 cap207sq001 cap208sq001 cap209sq001 cap301sq001 cap302sq001 cap303sq001 cap304sq001 cap305sq001 cap306sq001 cap307sq001 cap308sq001 cap309sq001

encode com011, generate(COM011)
encode com012, generate(COM012)
encode com013, generate(COM013)
encode com014, generate(COM014)
encode com015, generate(COM015)
encode com016, generate(COM016)
encode com017, generate(COM017)

drop com011 com012 com013 com014 com015 com016 com017

encode dse10, generate(DSE10)
replace DSE10=. if DSE10==3
drop dse10

encode acd1, generate(ACD1)
encode acd02, generate(ACD02)
replace ACD1=. if ACD1==3
replace ACD02=. if ACD02==3
gen rand_acid=1 if ACD02==.
replace rand_acid=2 if ACD1==.
drop acd1 acd02

encode ran03, generate(RAN03)
replace RAN03=. if RAN03==3
drop ran03

rename dse01 DSE01
rename dse03 DSE03
rename hab01 HAB01
rename dse04 DSE04
rename dse05 DSE05
rename dse06 DSE06
rename dse07 DSE07
rename dse08 DSE08
rename dse09 DSE09

drop choice

save "D:\Users\B3944459\Documents\Passageiros_final\base_arrumada.dta", replace

** criando grup e questões **
clear all

use "D:\Users\B3944459\Documents\Passageiros_final\base_arrumada.dta", clear

encode jogo, generate(question)
sort id question rota

order id jogo question rota escolha

egen nun_question = seq(), f(1) t(9) b(2)

gen grup=nun_question
replace grup = 10 if ran02==2&grup==1
replace grup = 11 if ran02==2&grup==2
replace grup = 12 if ran02==2&grup==3
replace grup = 13 if ran02==2&grup==4
replace grup = 14 if ran02==2&grup==5
replace grup = 15 if ran02==2&grup==6
replace grup = 16 if ran02==2&grup==7
replace grup = 17 if ran02==2&grup==8
replace grup = 18 if ran02==2&grup==9
replace grup = 19 if ran02==3&grup==1
replace grup = 20 if ran02==3&grup==2
replace grup = 21 if ran02==3&grup==3
replace grup = 22 if ran02==3&grup==4
replace grup = 23 if ran02==3&grup==5
replace grup = 24 if ran02==3&grup==6
replace grup = 25 if ran02==3&grup==7
replace grup = 26 if ran02==3&grup==8
replace grup = 27 if ran02==3&grup==9


**** tirando randomização*****
** No caso das questões 2, 4, 6 e 9 da rota 1; 11, 13, 15 e 18 da rota 2; 20, 22, 24 e 27 da rota 3
** se a pessoa escolheu a rota 1, na verdade escolheu a rota2 e vice-versa. 

encode escolha, generate (resposta)

gen choice = 1 if resposta==2
replace choice=2 if resposta==3

replace choice=4 if choice==1 & (grup==2|grup==4|grup==6|grup==9|grup==11|grup==13|grup==15|grup==18|grup==20|grup==22|grup==24|grup==27)
replace choice=3 if choice==2 & (grup==2|grup==4|grup==6|grup==9|grup==11|grup==13|grup==15|grup==18|grup==20|grup==22|grup==24|grup==27)
replace choice=1 if choice==3
replace choice=2 if choice==4

drop escolha
drop if choice==0
drop if choice==.

rename choice escolha

gen choice=0
replace choice=1 if escolha==1&rota==1
replace choice=1 if escolha==2&rota==2



***input variables***

*** Valores de Custo****

gen custo = 0
replace custo=20 if rota==1&(grup==1|grup==6|grup==8|grup==10|grup==15|grup==17|grup==19|grup==24|grup==26)
replace custo=10 if rota==1&(grup==3|grup==4|grup==9|grup==12|grup==13|grup==18|grup==21|grup==22|grup==27)
replace custo=5 if rota==1&(grup==2|grup==5|grup==7|grup==11|grup==14|grup==16|grup==20|grup==23|grup==25)
replace custo=20 if rota==2&(grup==2|grup==5|grup==7|grup==11|grup==14|grup==16|grup==20|grup==23|grup==25)
replace custo=25 if rota==2&(grup==1|grup==6|grup==8|grup==10|grup==15|grup==17|grup==19|grup==24|grup==26)
replace custo=40 if rota==2&(grup==3|grup==4|grup==9|grup==12|grup==13|grup==18|grup==21|grup==22|grup==27)

*** Valores de Acidente****

gen morte = 0
replace morte=20 if rota==1&(grup==1|grup==4|grup==5|grup==7|grup==8|grup==9|grup==10|grup==13|grup==14|grup==16|grup==17|grup==18|grup==19|grup==22|grup==23|grup==25|grup==26|grup==27)
replace morte=15 if rota==1&(grup==2|grup==3|grup==6|grup==11|grup==12|grup==15|grup==20|grup==21|grup==24)
replace morte=15 if rota==2&(grup==1|grup==5|grup==9|grup==10|grup==14|grup==18|grup==19|grup==23|grup==27)
replace morte=14 if rota==2&(grup==2|grup==3|grup==6|grup==11|grup==12|grup==15|grup==20|grup==21|grup==24)
replace morte=11 if rota==2&(grup==4|grup==7|grup==8|grup==13|grup==16|grup==17|grup==22|grup==25|grup==26)

*** Valores de tempo****

gen tempo=0
replace tempo=210 if rota==1&(grup==1|grup==2|grup==3|grup==4|grup==5|grup==8|grup==10|grup==11|grup==13|grup==15|grup==16|grup==18|grup==21|grup==23|grup==24|grup==25|grup==26|grup==27)
replace tempo=165 if rota==1&(grup==6|grup==7|grup==9|grup==12|grup==14|grup==17|grup==19|grup==20|grup==22)
replace tempo=180 if rota==2&(grup==1|grup==2|grup==4|grup==15|grup==16|grup==18|grup==21|grup==23|grup==26)
replace tempo=150 if rota==2&(grup==6|grup==7|grup==9|grup==12|grup==14|grup==17|grup==19|grup==20|grup==22)
replace tempo=165 if rota==2&(grup==3|grup==5|grup==8|grup==10|grup==11|grup==13|grup==24|grup==25|grup==27)

order choice escolha id  nun_question grup rota custo morte tempo

**** retirando os lexicográficos ****
sort id
by id: egen sumescolha = total(escolha)

drop if sumescolha==18|sumescolha==36

*** organizando variáveis categoricas para o mixlogit***
gen optA=1 if rota==1
replace optA=0 if optA==.

gen optB=1 if rota==2
replace optB=0 if optB==.

gen arenda = optA*DSE07
gen afreq = optA*HAB01
gen agenero = optA*DSE02
gen aetaria = optA*DSE03
gen aescola = optA*DSE04
gen acivil = optA*DSE06
gen afilhos = optA*DSE09
gen amotivo= optA*ULV01
gen aacid = optA*rand_acid
gen arisco = optA*deb03
gen asozinho = optA*deb02
gen arand03=optA*RAN03

gen acidente = ACD1 if rand_acid==1
replace acidente = ACD02 if rand_acid==2

gen aacidente=optA*acidente


order id question jogo nun_question grup rota choice resposta custo morte tempo optA arand03 arenda afreq agenero aetaria aescola acivil afilhos amotivo aacid arisco asozinho aacidente
drop jogo

save "D:\Users\B3944459\Documents\Passageiros_final\base_mixed.dta", replace
 
 