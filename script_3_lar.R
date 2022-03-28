
#Parte 3 #
# p.53 ####

#TABELA 15 - RECEITA MÉDIA DE CAMPANHA E VOTOS POR SEXO (DISTRIBUIÇÃO DAS MEDIDAS CENTRAIS)

#library(tidyverse)

#2012####
Mulher <- Banco2012Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
save(Mulher, file = "Mulher.RData")


Homem1 <- Banco2008Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
summary(Homem1$total_votos_turno_1)
save(Homem1, file = "Homem1.RData")

#2012####
Mulher2012 <- Banco2012Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
save(Mulher2012, file = "Mulher2012.RData")


Homem2012 <- Banco2012Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
save(Homem2012, file = "Homem2012.RData")

#2016 ####
Mulher2016 <- Banco2016Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
save(Mulher2016, file = "Mulher2016.RData")


Homem2016 <- Banco2016Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
save(Homem2016, file = "Homem2016.RData")

# Teste T ** 

options(scipen = 1000)

#TesteTReceitas2008####
t.test(Mulher$receitas_total, Homem1$receitas_total)


#Welch Two Sample t-test

#data:  Mulher$receitas_total and Homem1$receitas_total
#t = -23.308, df = 90623, p-value < 0.00000000000000022
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -334.0401 -282.2180
#sample estimates:
#  mean of x mean of y 
#1290.564  1598.693 

summary(Mulher$receitas_total)
# > summary(Mulher$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0      85     450    1291    1474  186540 

summary(Homem1$receitas_total)
# > summary(Homem1$receitas_total)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     0     150     678    1599    1939  279990 

#TesteTVotoss2008####
t.test(Mulher$total_votos_turno_1, Homem1$total_votos_turno_1)

# Welch Two Sample t-test
# 
# data:  Mulher$total_votos_turno_1 and Homem1$total_votos_turno_1
# t = -66.063, df = 99871, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -59.25935 -55.84438
# sample estimates:
#   mean of x mean of y 
# 113.0653  170.6172 

summary(Mulher$total_votos_turno_1)
# > summary(Mulher$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    21.0    55.0   113.1   134.0  2921.0 

summary(Homem1$total_votos_turno_1)
# > summary(Homem1$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    42.0   102.0   170.6   219.0  3483.0 

#TesteTReceitas2012####
t.test(Mulher2012$receitas_total, Homem2012$receitas_total)

# Welch Two Sample t-test
# 
# data:  Mulher2012$receitas_total and Homem2012$receitas_total
# t = -77.411, df = 251075, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1198.290 -1139.109
# sample estimates:
#   mean of x mean of y 
# 1589.803  2758.503 

summary(Mulher2012$receitas_total)

# > summary(Mulher2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0       0     332    1590    1690  151800 

boxplot(Mulher2012$receitas_total) #Muitos casos fora da curva

summary(Homem2012$receitas_total)

# > summary(Homem2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     265    1198    2758    3472  224738 

#TesteTVotos2012####

t.test(Mulher2012$total_votos_turno_1, Homem2012$total_votos_turno_1)
# 
# Welch Two Sample t-test
# 
# data:  Mulher2012$total_votos_turno_1 and Homem2012$total_votos_turno_1
# t = -155.87, df = 265956, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -100.18921  -97.70079
# sample estimates:
#   mean of x mean of y 
#73.24452 172.18953 


summary(Mulher2012$total_votos_turno_1)
# > summary(Mulher2012$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.00   24.00   73.24   79.00 3152.00 

summary(Homem2012$total_votos_turno_1)
# > summary(Homem2012$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    42.0   104.0   172.2   224.0  4051.0 

#TesteTReceitas2016####
t.test(Mulher2016$receitas_total, Homem2016$receitas_total)

# Welch Two Sample t-test
# 
# data:  Mulher2016$receitas_total and Homem2016$receitas_total
# t = -77.424, df = 249356, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -919.9942 -874.5654
# sample estimates:
#   mean of x mean of y 
# 1661.366  2558.646 

summary(Mulher2016$receitas_total)
# > summary(Mulher2016$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     249     690    1661    2030  184684 

summary(Homem2016$receitas_total)
# > summary(Homem2016$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     477    1450    2559    3450  196777

#TesteTVotos2016####
t.test(Mulher2016$total_votos_turno_1, Homem2016$total_votos_turno_1)

# Welch Two Sample t-test
# 
# data:  Mulher2016$total_votos_turno_1 and Homem2016$total_votos_turno_1
# t = -161.36, df = 278976, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -102.13989  -99.68842
# sample estimates:
#   mean of x mean of y 
# 72.19523 173.10938 

summary(Mulher2016$total_votos_turno_1)
# > summary(Mulher2016$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    5.00   23.00   72.19   74.00 3236.00 

summary(Homem2016$total_votos_turno_1)

# > summary(Homem2016$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    42.0   103.0   173.1   226.0  3498.0 

#p.54 ####
#Com os mesmos dados que rodamos para a tabela de cima, fazemos 
#essa tabela da p.54

#p. 55####
#TABELA 17 - COMPARAÇÃO DE MÉDIAS DE RECEITAS (R$) ENTRE ELEITOS(AS) E NÃO ELEITOS

table(Banco2008Vereadores$desc_sit_tot_turno_agreg)
#ELEITOS NÃO ELEITOS 
#46153      198954 

#Eleitos 2008####
BancoEle <- Banco2008Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "ELEITOS") 
save(BancoEle, file = "BancoEle.RData")

#Nâo eleitos 2008####
BancoNaoEle <- Banco2008Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "NÃO ELEITOS") 
save(BancoNaoEle, file = "BancoNaoEle.RData")


#TesteT #### 
#Eleito e não eleito 2008 ####


t.test(BancoEle$receitas_total, BancoNaoEle$receitas_total)

# Welch Two Sample t-test
# 
# data:  BancoEle$receitas_total and BancoNaoEle$receitas_total
# t = 92.804, df = 50829, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1970.473 2055.501
# sample estimates:
#   mean of x mean of y 
# 3166.692  1153.705 

summary(BancoEle$receitas_total)

# > summary(BancoEle$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0     830    1901    3167    3890  279990 

summary(BancoNaoEle$receitas_total)
# > summary(BancoNaoEle$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0      88     450    1154    1376  106361 

#Eleitos 2012####
BancoEle2012 <- Banco2012Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "ELEITOS") 
save(BancoEle2012, file = "BancoEle2012.RData")

#Nâo eleitos 2012####
BancoNaoEle2012 <- Banco2012Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "NÃO ELEITOS") 
save(BancoNaoEle2012, file = "BancoNaoEle2012.RData")

#TesteT #### 
#Eleito e não eleito 2012 ####

t.test(BancoEle2012$receitas_total, BancoNaoEle2012$receitas_total)

# Welch Two Sample t-test
# 
# data:  BancoEle2012$receitas_total and BancoNaoEle2012$receitas_total
# t = 114.18, df = 54539, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3521.008 3644.005
# sample estimates:
#   mean of x mean of y 
# 5364.275  1781.768 

summary(BancoEle2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1435      3399    5364    6706  152733  

summary(BancoNaoEle2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     100     560    1782    2172     224738 

#Eleitos 2016####
BancoEle2016 <- Banco2016Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "ELEITOS") 
save(BancoEle2016, file = "BancoEle2016.RData")

#Nâo eleitos 2016####
BancoNaoEle2016 <- Banco2016Vereadores %>%
  filter(desc_sit_tot_turno_agreg == "NÃO ELEITOS") 
save(BancoNaoEle2016, file = "BancoNaoEle2016.RData")

#TesteT #### 
#Eleito e não eleito 2016 ####

t.test(BancoEle2016$receitas_total, BancoNaoEle2016$receitas_total)
# Welch Two Sample t-test
# 
# data:  BancoEle2016$receitas_total and BancoNaoEle2016$receitas_total
# t = 127.58, df = 56651, p-value < 0.00000000000000022
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2749.169 2834.961
# sample estimates:
#   mean of x mean of y 
# 4602.971  1810.906  

summary(BancoEle2016$receitas_total)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0    1689    3460    4603    5983  139711   

summary(BancoNaoEle2016$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0     305     886    1811    2380  196777 


#p.56####
#TABELA 18 - SITUAÇÃO DAS CANDIDATURAS POR SEXO

#2008####
tab1 <- table(Banco2008Vereadores$descricao_sexo, 
              Banco2008Vereadores$desc_sit_tot_turno_agreg)
tab1

#           ELEITOS NÃO ELEITOS
# FEMININO     5903       46556
# MASCULINO   40250      152398

chisq.test(tab1)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab1
# X-squared = 2506.5, df = 1, p-value < 0.00000000000000022

options(digits = 3)
tab1 <- prop.table(tab1, margin = 1)
tab1 * 100

#          ELEITOS NÃO ELEITOS
#FEMININO     11.3        88.7
#MASCULINO    20.9        79.1

#2012####
tab2 <- table(Banco2012Vereadores$descricao_sexo, 
              Banco2012Vereadores$desc_sit_tot_turno_agreg)
tab2
#          ELEITOS NÃO ELEITOS
#FEMININO     6858       90159
#MASCULINO   42933      158760

chisq.test(tab2)
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab2
# X-squared = 9532, df = 1, p-value <0.0000000000000002

options(digits = 2)
tab2 <- prop.table(tab2, margin = 1)
tab2 * 100

#          ELEITOS NÃO ELEITOS
# FEMININO      7.1        92.9
# MASCULINO    21.3        78.7

#2016####

tab3 <- table(Banco2016Vereadores$descricao_sexo, 
              Banco2016Vereadores$desc_sit_tot_turno_agreg)
tab3
#           ELEITOS NÃO ELEITOS
# FEMININO     6956       95209
# MASCULINO   43191      164490

chisq.test(tab3)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab3
# X-squared = 9877, df = 1, p-value <0.0000000000000002

options(digits = 2)
tab3 <- prop.table(tab3, margin = 1)

tab3 * 100
#ELEITOS NÃO ELEITOS
#FEMININO      6.8        93.2
#MASCULINO    20.8        79.2


#p.57####
# TABELA 19 - QUARTIL SUPERIOR RECEITAS
#CANDIDATOS E CANDIDATAS ELEITOS (AS) NAS ELEIÇÕES DE 2008, 2012 E 2016

#Para analisar o sexo (feminino e masculino) dentro
#do banco de eleitos, teremos que criar 2 sub bancos 
#sendo 1 para cada sexo

#MUlheres 2008####
table(BancoEle$descricao_sexo)
#FEMININO MASCULINO 
# 5903     40250 
freq(BancoEle$descricao_sexo)

BancoEle$descricao_sexo 
# Frequência Percentual
# FEMININO        5903      12.79
# MASCULINO      40250      87.21
# Total          46153     100.00

BancoEleF <- BancoEle %>%
  filter(descricao_sexo == "FEMININO") 

save(BancoEleF, file = "BancoEleF.RData")
 
summary(BancoEleF$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     854    1978    3300    4084  186540 


#Homens 2008####
table(BancoEle$descricao_sexo)
#FEMININO MASCULINO 
# 5903     40250 

BancoEleM <- BancoEle %>%
  filter(descricao_sexo == "MASCULINO") 

save(BancoEleM, file = "BancoEleM.RData")

summary(BancoEleM$receitas_total)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0     827    1896    3147    3855  279990

#MUlheres 2012####


BancoEleF2012 <- BancoEle2012 %>%
  filter(descricao_sexo == "FEMININO") 

save(BancoEleF2012, file = "BancoEleF2012.RData")

summary(BancoEleF2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0    1500    3641    5654    7222  151800

#Homens 2012####
table(BancoEle2012$descricao_sexo)
# FEMININO MASCULINO 
#6858     42933

freq(BancoEle2012$descricao_sexo)

# BancoEle2012$descricao_sexo 
#           Frequência Percentual
# FEMININO        6858      13.77
# MASCULINO      42933      86.23
# Total          49791     100.00

BancoEleM2012 <- BancoEle2012 %>%
  filter(descricao_sexo == "MASCULINO") 

save(BancoEleM2012, file = "BancoEleM2012.RData")

summary(BancoEleM2012$receitas_total)

# > summary(BancoEleM2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1424    3358    5318    6630      152733 


#MUlheres 2016####

table(BancoEle2016$descricao_sexo)
# FEMININO MASCULINO 
# 6956     43191 

freq(BancoEle2016$descricao_sexo)

# BancoEle2016$descricao_sexo 
# Frequência Percentual
# FEMININO        6956      13.87
# MASCULINO      43191      86.13
# Total          50147     100.00

BancoEleF2016 <- BancoEle2016 %>%
  filter(descricao_sexo == "FEMININO") 

save(BancoEleF2016, file = "BancoEleF2016.RData")

summary(BancoEleF2016$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0    1800    3770    4919    6426   95695 

#Homens 2016####

BancoEleM2016 <- BancoEle2016 %>%
  filter(descricao_sexo == "MASCULINO") 

save(BancoEleM2016, file = "BancoEleM2016.RData")

summary(BancoEleM2016$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1670    3412    4552    5900  139711 

#Teste t ####
#Para todos os anos (2008,12 e 2016) para analisar 
#se existe diferença de médias entre os homens e mulheres
#eleitos

#2008####
t.test(BancoEleF$receitas_total, BancoEleM$receitas_total)
# Welch Two Sample t-test
# 
# data:  BancoEleF$receitas_total and BancoEleM$receitas_total
# t = 2, df = 7435, p-value = 0.02
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   20 285
# sample estimates:
#   mean of x mean of y 
# 3300      3147 

#2012####
t.test(BancoEleF2012$receitas_total, BancoEleM2012$receitas_total)

# Welch Two Sample t-test
# 
# data:  BancoEleF2012$receitas_total and BancoEleM2012$receitas_total
# t = 4, df = 9208, p-value = 0.0002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   162 510
# sample estimates:
#   mean of x mean of y 
# 5654      5318 

#2016####
t.test(BancoEleF2016$receitas_total, BancoEleM2016$receitas_total)
# Welch Two Sample t-test
# 
# data:  BancoEleF2016$receitas_total and BancoEleM2016$receitas_total
# t = 6, df = 9130, p-value = 0.000000007
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   242 491
# sample estimates:
#   mean of x mean of y 
# 4919      4552 

#Todos os testes menores 
#q 0.05


#Calculos de porcentagem para os valores
#Cal 1
279990 + 186540 
#[1] 466530
466530 -100
279990 -x

#466530x= 27999000

27999000/466530
x= 60

#Cal2
151800 + 152733
#[1] 304533

#304533x = 15180000
15180000/304533
#[1] 49.8

#Cal 3
95695 + 139711

#[1] 235406
#235406x = 9569500

9569500/235406
x= 40.7

100-40.7
y= 59.3


#APÊNDICE p.59 ####
#Seria apêndice 1,2,3
#Fica apêndice 1 


# Qui-quadrado do total de receitas pelas v:
# 1 - Sexo, 2-Grau de instrução, 3- Estado Civil, 4-Ocupação, 
# 5- Carreira Política, 6- Incumbente, 7- Resultado da Eleição
# 8-Total de votos

#2008####

tab.1 <- table(Banco2008Vereadores$codigo_sexo, 
               Banco2008Vereadores$receitas_total)
chisq.test(tab.1)

#Pearson's Chi-squared test

# 
# data:  tab.1
# X-squared = 12339, df = 12341, p-value = 0.5
# 
# Warning message:
#   In chisq.test(tab.1) : Aproximação do qui-quadrado pode estar incorreta

#library(Kendall)
Kendall(Banco2008Vereadores$codigo_sexo, 
         Banco2008Vereadores$receitas_total)

#tau = -0.0632, 2-sided pvalue =<0.0000000000000002

tab.2 <- table (Banco2008Vereadores$cod_grau_instrucao, 
                Banco2008Vereadores$receitas_total)
chisq.test(tab.2)
# Pearson's Chi-squared test
# 
# data:  tab.2
# X-squared = 95222, df = 86387, p-value <0.0000000000000002

tab.3 <- table(Banco2008Vereadores$codigo_estado_civil,
               Banco2008Vereadores$receitas_total)
chisq.test(tab.3)
# 
# Pearson's Chi-squared test
# 
# data:  tab.3
# X-squared = 52304, df = 49364, p-value <0.0000000000000002

tab.4 <- table(Banco2008Vereadores$descricao_ocupacao,
               Banco2008Vereadores$receitas_total)
chisq.test(tab.4)

# Pearson's Chi-squared test
# 
# data:  tab.4
# X-squared = 3036199, df = 2937158, p-value <0.0000000000000002

tab.5 <- table(Banco2008Vereadores$descricao_ocupacao_agreg1,
               Banco2008Vereadores$receitas_total)
chisq.test(tab.5)

#Pearson's Chi-squared test

#data:  tab.5
#X-squared = 31288, df = 12341, p-value <0.0000000000000002

tab.6 <- table(Banco2008Vereadores$incumb.xdesaf.,
               Banco2008Vereadores$receitas_total)
chisq.test(tab.6)

# Pearson's Chi-squared test
# 
# data:  tab.6
# X-squared = 35290, df = 12341, p-value <0.0000000000000002

tab.7 <- table(Banco2008Vereadores$desc_sit_tot_turno_agreg,
               Banco2008Vereadores$receitas_total)
chisq.test(tab.7)

# Pearson's Chi-squared test
# 
# data:  tab.7
# X-squared = 50351, df = 12341, p-value <0.0000000000000002


#2012####
tab.11 <- table(Banco2012Vereadores$codigo_sexo, 
               Banco2012Vereadores$receitas_total)
chisq.test(tab.11)

# Pearson's Chi-squared test
# 
# data:  tab.11
# X-squared = 31856, df = 18144, p-value <0.0000000000000002


tab.12 <- table (Banco2012Vereadores$cod_grau_instrucao, 
                Banco2012Vereadores$receitas_total)
chisq.test(tab.12)
# Pearson's Chi-squared test

# data:  tab.12
# X-squared = 123970, df = 127008, p-value = 1

Kendall(Banco2012Vereadores$cod_grau_instrucao, 
        Banco2012Vereadores$receitas_total)

#tau = -0.0171, 2-sided pvalue =<0.0000000000000002

tab.13 <- table(Banco2012Vereadores$codigo_estado_civil,
               Banco2012Vereadores$receitas_total)
chisq.test(tab.13)
# Pearson's Chi-squared test
# 
# data:  tab.13
# X-squared = 73248, df = 72576, p-value = 0.04
# 

tab.14 <- table(Banco2012Vereadores$descricao_ocupacao,
               Banco2012Vereadores$receitas_total)
chisq.test(tab.14)

# # Pearson's Chi-squared test
# 
# data:  tab.14
# X-squared = 4358771, df = 4318272, p-value <0.0000000000000002

tab.15 <- table(Banco2012Vereadores$descricao_ocupacao_agreg1,
               Banco2012Vereadores$receitas_total)
chisq.test(tab.15)

#Pearson's Chi-squared test

# data:  tab.15
# X-squared = 51496, df = 18144, p-value <0.0000000000000002


tab.16 <- table(Banco2012Vereadores$incumb.xdesaf.,
               Banco2012Vereadores$receitas_total)
chisq.test(tab.16)

# Pearson's Chi-squared test
# 
# data:  tab.16
# X-squared = 53512, df = 18144, p-value <0.0000000000000002

tab.17 <- table(Banco2012Vereadores$desc_sit_tot_turno_agreg,
               Banco2012Vereadores$receitas_total)
chisq.test(tab.17)


# Pearson's Chi-squared test
# 
# data:  tab.17
#X-squared = 72456, df = 18144, p-value <0.0000000000000002


#2016####
tab.21 <- table(Banco2016Vereadores$codigo_sexo, 
                Banco2016Vereadores$receitas_total)
chisq.test(tab.21)

#PPearson's Chi-squared test

# data:  tab.21
# X-squared = 24842, df = 15122, p-value <0.0000000000000002

tab.22 <- table (Banco2016Vereadores$cod_grau_instrucao, 
                 Banco2016Vereadores$receitas_total)
chisq.test(tab.22)
# Pearson's Chi-squared test

# data:  tab.22
# X-squared = 99682, df = 105854, p-value = 1

Kendall(Banco2016Vereadores$cod_grau_instrucao, 
        Banco2016Vereadores$receitas_total)

#tau = -0.0171, 2-sided pvalue =<0.0000000000000002

tab.23 <- table(Banco2016Vereadores$codigo_estado_civil,
                Banco2016Vereadores$receitas_total)
chisq.test(tab.23)
# Pearson's Chi-squared test

# data:  tab.23
# X-squared = 58688, df = 60488, p-value = 1

# 

tab.24 <- table(Banco2016Vereadores$descricao_ocupacao,
                Banco2016Vereadores$receitas_total)
chisq.test(tab.24)

# # Pearson's Chi-squared test
# 
#data:  tab.24
#X-squared = 3997187, df = 3629280, p-value <0.0000000000000002

tab.25 <- table(Banco2016Vereadores$descricao_ocupacao_agreg1,
                Banco2016Vereadores$receitas_total)
chisq.test(tab.25)

#Pearson's Chi-squared test

# data:  tab.25
#X-squared = 48437, df = 15122, p-value <0.0000000000000002


tab.26 <- table(Banco2016Vereadores$incumb.xdesaf.,
                Banco2016Vereadores$receitas_total)
chisq.test(tab.26)

# Pearson's Chi-squared test
# 
# data:  tab.26
#X-squared = 54013, df = 15122, p-value <0.0000000000000002

tab.27 <- table(Banco2016Vereadores$desc_sit_tot_turno_agreg,
                Banco2016Vereadores$receitas_total)
chisq.test(tab.27)


# Pearson's Chi-squared test
# 
#data:  tab.27
#X-squared = 66991, df = 15122, p-value <0.0000000000000002

#GRÁFICO 6####

#MULHERES 2008
summary(BancoEleF$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     854    1978    3300    4084  186540 

#HOMENS 2008
summary(BancoEleM$receitas_total)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0     827    1896    3147    3855  279990

#MULHERES 2012
summary(BancoEleF2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0    1500    3641    5654    7222  151800

#HOMENS 2012
summary(BancoEleM2012$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1424    3358    5318    6630      152733

#MULHERES 2016
summary(BancoEleF2016$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0    1800    3770    4919    6426   95695 

#HOMENS 2016
summary(BancoEleM2016$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1670    3412    4552    5900  139711 


library(tidyr)
library(ggplot2)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1670    3412    4552    5900  139711 


Objc <- data.frame (Quartis = c("1ºQ", "2ºQ", "3ºQ", "4ºQ"), 
                    Masculino2008=	c(827,	1896 ,	3855,	279990),
                    Feminino2008=	c(854, 1978, 4084, 186540),
                    Masculino2012=	c(1424, 3358, 6630, 152733),
                    Feminino2012=	c(1500, 3641, 7222, 151800),
                    Masculino2016= c(1670, 3412, 5900, 139711),
                    Feminino2016= c(1800, 3770, 6426, 95695)) 


Objc.1 <- Objc %>% gather(Sexo, Valores, -Quartis)

Gra3 <- ggplot(data = Objc.1, aes(x= Quartis, y= Valores, 
                                  group = Sexo))+
  geom_line(aes(colour = Sexo), size = 2) + 
  geom_point(aes(shape = Sexo))

Gra3

Gra3 + theme_bw()+ theme(legend.position = "left")


#APÊNDICE p.60####

#Seria apêndice 1,2,3
#Fica apêndice 1 

# Qui-quadrado do sucesso eleitoral pelas v:
# 1 - Sexo, 2-Grau de instrução, 3- Estado Civil, 4-Ocupação, 
# 5- Carreira Política, 6- Incumbente
# 7-Total de votos

#2008####
tab1 <- table(Banco2008Vereadores$descricao_sexo, 
         Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab1)

# Pearson's Chi-squared test
# 
# data:  tab1
# X-squared = 2535, df = 5, p-value <0.0000000000000002

tab2 <- table(Banco2008Vereadores$descricao_grau_instrucao, 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab2)
# Pearson's Chi-squared test
# 
# data:  tab2
# X-squared = 1187, df = 35, p-value <0.0000000000000002

tab3 <- table(Banco2008Vereadores$descricao_estado_civil, 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab3)

# Pearson's Chi-squared test
# 
# data:  tab3
# X-squared = 2085, df = 20, p-value <0.0000000000000002

tab4 <- table(Banco2008Vereadores$descricao_ocupacao, 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab4)

# Pearson's Chi-squared test
# 
# data:  tab4
# X-squared = 20251, df = 1190, p-value <0.0000000000000002

tab5 <- table(Banco2008Vereadores$descricao_ocupacao_agreg1, 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab5)

# Pearson's Chi-squared test
# 
# data:  tab5
# X-squared = 12567, df = 5, p-value <0.0000000000000002

tab6 <- table(Banco2008Vereadores$incumb.xdesaf., 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab6)

# Pearson's Chi-squared test
# 
# data:  tab6
# X-squared = 33869, df = 5, p-value <0.0000000000000002

tab7 <- table(Banco2008Vereadores$receitas_total, 
              Banco2008Vereadores$desc_sit_tot_turno)
chisq.test(tab7)
# Pearson's Chi-squared test
# 
# data:  tab7
# X-squared = 94105, df = 61705, p-value <0.0000000000000002

#2012####
tab11 <- table(Banco2012Vereadores$descricao_sexo, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab11)

# Pearson's Chi-squared test
# 
# data:  tab11
#X-squared = 9600, df = 3, p-value <0.0000000000000002

tab12 <- table(Banco2012Vereadores$descricao_grau_instrucao, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab12)
# Pearson's Chi-squared test
# 
# data:  tab12
#X-squared = 1882, df = 21, p-value <0.0000000000000002

tab13 <- table(Banco2012Vereadores$descricao_estado_civil, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab13)

# Pearson's Chi-squared test
# 
# data:  tab13
#X-squared = 2451, df = 12, p-value <0.0000000000000002

tab14 <- table(Banco2012Vereadores$descricao_ocupacao, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab14)

# Pearson's Chi-squared test
# 
# data:  tab14
#X-squared = 29399, df = 714, p-value <0.0000000000000002

tab15 <- table(Banco2012Vereadores$descricao_ocupacao_agreg1, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab15)

# Pearson's Chi-squared test
# 
# data:   tab15
#X-squared = 19516, df = 3, p-value <0.0000000000000002


tab16 <- table(Banco2012Vereadores$incumb.xdesaf., 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab16)

# Pearson's Chi-squared test
# 
# data:  tab16
#X-squared = 45401, df = 3, p-value <0.0000000000000002

tab17 <- table(Banco2012Vereadores$receitas_total, 
              Banco2012Vereadores$desc_sit_tot_turno)
chisq.test(tab17)
# Pearson's Chi-squared test
# 
# data:  tab17
#X-squared = 129674, df = 54432, p-value <0.0000000000000002

#2016####
tab21 <- table(Banco2016Vereadores$descricao_sexo, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab21)

# Pearson's Chi-squared test
# 
# data:  tab21
#X-squared = 9945, df = 3, p-value <0.0000000000000002

tab22 <- table(Banco2016Vereadores$descricao_grau_instrucao, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab22)
# Pearson's Chi-squared test
# 
# data:  tab22
#X-squared = 3009, df = 21, p-value <0.0000000000000002

tab23 <- table(Banco2016Vereadores$descricao_estado_civil, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab23)

# Pearson's Chi-squared test
# 
# data: tab23
#X-squared = 2841, df = 12, p-value <0.0000000000000002

tab24 <- table(Banco2016Vereadores$descricao_ocupacao, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab24)

# Pearson's Chi-squared test
# 
# data:  tab24
#X-squared = 34973, df = 720, p-value <0.0000000000000002

tab25 <- table(Banco2016Vereadores$descricao_ocupacao_agreg1, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab25)

# Pearson's Chi-squared test
# 
# data:   tab25
#X-squared = 23652, df = 3, p-value <0.0000000000000002

tab26 <- table(Banco2016Vereadores$incumb.xdesaf., 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab26)

# Pearson's Chi-squared test
# 
# data:  tab26
#X-squared = 53622, df = 3, p-value <0.0000000000000002

tab27 <- table(Banco2016Vereadores$receitas_total, 
               Banco2016Vereadores$desc_sit_tot_turno)
chisq.test(tab27)
# Pearson's Chi-squared test
# 
# data:  tab27
#X-squared = 123919, df = 45366, p-value <0.0000000000000002