#Salvar tudo em R.data 
#Função pra abrir em CSV 
Banco2016Vereadores = read.csv2("Banco2016Veradores.csv")
#Observações 434.045
Banco2012Vereadores = read.csv2("Banco2012Veradores.csv")
#Observações 412.198
Banco2008Vereadores = read.csv2("Banco2008Veradores.csv")
#Obervações 245.107

#Os bancos estão ok, todos com 90 variáveis.
#Somando as observações:
434045 + 412198 + 324598
#[1] 1170841
#Total de vereadores (APTOS E INAPTOS) entre 2008 e 2026 =1.170.841 

save(Banco2016Vereadores, file = "Banco2016Vereadores.RData")
save(Banco2012Vereadores, file = "Banco2012Vereadores.RData")
save(Banco2008Vereadores, file = "Banco2008Vereadores.RData")


freq(Banco2008Vereadores$descricao_ue)

#OK.



#Primeira tabela word Região do País e municípios

##1ªTabela####
####Sexo####
freq(Banco2008Vereadores$descricao_sexo)
#             Frequência Percentual
#FEMININO       52459       21.4
#MASCULINO     192648       78.6
#Total         245107      100.0

freq(Banco2012Vereadores$descricao_sexo)
#             Frequência Percentual
#FEMININO       97017      32.48
#MASCULINO     201693      67.52
#Total         298710     100.00

freq(Banco2016Vereadores$descricao_sexo)
#             Frequência Percentual
#FEMININO      102165      32.97
#MASCULINO     207681      67.03
#Total         309846     100.00

####Estado civil####
freq(Banco2008Vereadores$descricao_estado_civil)
#                             Frequência Percentual
#CASADO(A)                     154431     63.006
#DIVORCIADO(A)                  10796      4.405
#SEPARADO(A) JUDICIALMENTE       6248      2.549
#SOLTEIRO(A)                    69260     28.257
#VIÚVO(A)                        4372      1.784
#Total                         245107    100.000

# recodificar essa variável, vamos usar o pac. memisc
#função recod
library(memisc)

Banco2008Vereadores$EstCiv <- recode(Banco2008Vereadores$descricao_estado_civil, 
                      "Casado" <- "CASADO(A)", "Solteiro" <- "SOLTEIRO(A)", 
                      "Divorciado" <- c("DIVORCIADO(A)", "SEPARADO(A) JUDICIALMENTE",
                                        "VIÚVO(A)")) 
freq(Banco2008Vereadores$EstCiv)
#            Frequência Percentual
#Casado         154431     63.006
#Solteiro        69260     28.257
#Divorciado      21416      8.737
#Total          245107    100.000

 
Banco2012Vereadores$EstCiv <- recode(Banco2012Vereadores$descricao_estado_civil, 
                                     "Casado" <- "CASADO(A)", "Solteiro" <- "SOLTEIRO(A)", 
                                     "Divorciado" <- c("DIVORCIADO(A)", "SEPARADO(A) JUDICIALMENTE",
                                                       "VIÚVO(A)")) 
freq(Banco2012Vereadores$EstCiv)
#           Frequência Percentual
#Casado         173207     57.985
#Solteiro        97004     32.474
#Divorciado      28499      9.541
#Total          298710    100.000

Banco2016Vereadores$EstCiv <- recode(Banco2016Vereadores$descricao_estado_civil, 
                                     "Casado" <- "CASADO(A)", "Solteiro" <- "SOLTEIRO(A)", 
                                     "Divorciado" <- c("DIVORCIADO(A)", "SEPARADO(A) JUDICIALMENTE",
                                                       "VIÚVO(A)")) 
freq(Banco2016Vereadores$EstCiv)
#           Frequência Percentual
#Casado         167610     54.095
#Solteiro       112379     36.269
#Divorciado      29857      9.636
#Total          309846    100.000

####Escolaridade####

freq(Banco2008Vereadores$descricao_grau_instrucao_agreg)
#               Frequência Percentual
#FUNDAMENTAL     109480      44.67
#MÉDIO            91906      37.50
#SUPERIOR         43721      17.84
#Total           245107     100.00

freq(Banco2012Vereadores$descricao_grau_instrucao_agreg)
#            Frequência Percentual
#FUNDAMENTAL     117645      39.38
#MÉDIO           122435      40.99
#SUPERIOR         58630      19.63
#Total           298710     100.00

freq(Banco2016Vereadores$descricao_grau_instrucao_agreg)
#                Frequência Percentual
#FUNDAMENTAL     113844      36.74
#MÉDIO           134405      43.38
#SUPERIOR         61597      19.88
#Total           309846     100.00


####Ocupacao####

freq(Banco2008Vereadores$descricao_ocupacao_agreg)
#                           Frequência Percentual
#ESPECIALIZADO FUNDAMENTAL     126728     51.703
#ESPECIALIZADO MÉDIO            58014     23.669
#ESPECIALIZADO SUPERIOR         43560     17.772
#NÃO ESPECIALIZADO              16805      6.856
#Total                         245107    100.000

freq(Banco2012Vereadores$descricao_ocupacao_agreg)
#                              Frequência Percentual
#ESPECIALIZADO FUNDAMENTAL     137122     45.905
#ESPECIALIZADO MÉDIO            78457     26.265
#ESPECIALIZADO SUPERIOR         56369     18.871
#NÃO ESPECIALIZADO              26762      8.959
#Total                         298710    100.000

freq(Banco2016Vereadores$descricao_ocupacao_agreg)
#                           Frequência Percentual
#ESPECIALIZADO FUNDAMENTAL     138402     44.668
#ESPECIALIZADO MÉDIO            86105     27.790
#ESPECIALIZADO SUPERIOR         56620     18.274
#NÃO ESPECIALIZADO              28719      9.269
#Total                         309846    100.000

####Carreira pol.####
freq(Banco2008Vereadores$descricao_ocupacao_agreg1)
#                           Frequência Percentual
#NÃO POLÍTICO DE CARREIRA     230842      94.18
#POLÍTICO DE CARREIRA          14265       5.82
#Total                        245107     100.00

freq(Banco2012Vereadores$descricao_ocupacao_agreg1)
#                            Frequência Percentual
#NÃO POLÍTICO DE CARREIRA     283455     94.893
#POLÍTICO DE CARREIRA          15255      5.107
#Total                        298710    100.000

freq(Banco2016Vereadores$descricao_ocupacao_agreg1)
#                           Frequência Percentual
#NÃO POLÍTICO DE CARREIRA     292180     94.298
#POLÍTICO DE CARREIRA          17666      5.702
#Total                        309846    100.000

####Politico####
 freq(Banco2008Vereadores$incumb.xdesaf.)
#             Frequência Percentual
#DESAFIANTE     211971      86.48
#INCUMBENTE      33136      13.52
#Total          245107     100.00

freq(Banco2012Vereadores$incumb.xdesaf.)
#             Frequência Percentual
#DESAFIANTE     266766      89.31
#INCUMBENTE      31944      10.69
#Total          298710     100.00

freq(Banco2016Vereadores$incumb.xdesaf.)
#             Frequência Percentual
#DESAFIANTE     274564      88.61
#INCUMBENTE      35282      11.39
#Total          309846     100.00

####Resultado####
freq(Banco2008Vereadores$desc_sit_tot_turno_agreg)

#              Frequência Percentual
#ELEITOS          46153      18.83
#NÃO ELEITOS     198954      81.17
#Total           245107     100.00

freq(Banco2012Vereadores$desc_sit_tot_turno_agreg)
#               Frequência Percentual
#ELEITOS          49791      16.67
#NÃO ELEITOS     248919      83.33
#Total           298710     100.00

freq(Banco2016Vereadores$desc_sit_tot_turno_agreg)

#               Frequência Percentual
#ELEITOS          50147      16.18
#NÃO ELEITOS     259699      83.82
#Total           309846     100.00



####Receitas####
summary(Banco2008Vereadores$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     130     620    1533    1837  279990 

summary(Banco2012Vereadores$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     150     836    2379    2945  224738

summary(Banco2016Vereadores$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     380    1130    2263    3022  196777 

####Votos####
options(scipen = 1000)
summary(Banco2008Vereadores$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.0    36.0    91.0   158.3   202.0  3483.0  

summary(Banco2012Vereadores$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.0    22.0    72.0   140.1   182.0  4051.0 

summary(Banco2016Vereadores$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.0    22.0    70.0   139.8   181.0  3498.0 

####PCRM####
#Participação do candidato no total de receitas do município

#Onde R Candidato é igual ao total de receitas 
#do candidato e (dividio por) R Munícipio é igual ao total 
#de receitas dos candidatos no distrito/ município.

####PCVM####
#Participação do candidato no total de votos do município
#Onde V Candidato é igual ao total de votos do candidato 
#e (divido por) V Munícipio é igual ao total de 
#votos dos candidatos no distrito/ município.


summary(Banco2008Vereadores$total_de_receita_no_municipio)


####INAPTOS####
##2ªe3ª tabela####
#Teremos que agora trabalhar tbém com os inaptos pq a tabela 2 
#Apresenta dados sobre todas as pessoas que se inscreveram.

library(readr)
library(readxl)

X2008INAPTOS = read.csv2("2008INAPTOS.csv")

X2012INAPTOS = read.csv2("2012INAPTOS.csv")

X2016INAPTOS = read.csv2("2016INAPTOS.csv")




freq(X2008INAPTOS$descricao_sexo)
#           Frequência Percentual
#FEMININO        4133      21.24
#MASCULINO      15330      78.76
#Total          19463     100.00
#Total          

freq(X2012INAPTOS$descricao_sexo)
#           Frequência Percentual
#FEMININO        9865      42.33
#MASCULINO      13439      57.67
#Total          23304     100.00

freq(X2016INAPTOS$descricao_sexo)
X2016INAPTOS$descricao_sexo 
#           Frequência Percentual
#FEMININO        7358      44.05
#MASCULINO       9345      55.95
#Total          16703     100.00

###Para montar a tabela somar os valor dos APTOS 
#Que estão lá em cima, com os INAPTOS

####Correlação#####
#Qui quadrado entre sexo e situação da candidatura (Variável: desc_sit_can_superior)
# e também uma tabela de relação
#Como essa variável tem a categoria APTA E INAPTA você precisa juntar 
#Os dois bancos, então antes do teste fazer essa junção, criando outros
#3 bancos através da função MERGE 

##Banco <- merge(Banco 1, Banco2, all = T)
#2008
APTOS.INAPTOS <- merge(X2008INAPTOS, Banco2008Vereadores, all = T)

table(APTOS.INAPTOS$desc_sit_cand_superior)
APTOS.INAPTOS$desc_sit_cand_superior <- recode(APTOS.INAPTOS$desc_sit_cand_superior,
                        "APTO" <- c("#NULO#", "APTO"), "INAPTO" <- "INAPTO")
save(APTOS.INAPTOS, file = "APTOS.INAPTOS.RData")

#2012
APTOS.INAPTOS2 <- merge(X2012INAPTOS, Banco2012Vereadores, all = T)

table(APTOS.INAPTOS2$desc_sit_cand_superior)

save(APTOS.INAPTOS2, file = "APTOS.INAPTOS2.RData")

#2016
APTOS.INAPTOS3 <- merge(X2016INAPTOS, Banco2016Vereadores, all = T)

table(APTOS.INAPTOS3$desc_sit_cand_superior)
APTOS.INAPTOS3$desc_sit_cand_superior <- recode(APTOS.INAPTOS3$desc_sit_cand_superior,
                                               "APTO" <- c("#NULO#", "APTO"), "INAPTO" <- "INAPTO")
save(APTOS.INAPTOS3, file = "APTOS.INAPTOS3.RData")


#####X² 2008####

#Criamos uma tab1 e rodamos o teste:

options(scipen = 999, digits = 5)

table(APTOS.INAPTOS$desc_sit_cand_superior)

tab1 <- table(APTOS.INAPTOS$descricao_sexo, APTOS.INAPTOS$desc_sit_cand_superior)
chisq.test(tab1) 

#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab1
#X-squared = 0.29036, df = 1, p-value = 0.59

tab1
#           APTO INAPTO
#FEMININO   52459   4133
#MASCULINO 192648  15330

#Porcentagem
tab1 <- prop.table (tab1, margin = 1)
tab1 * 100
#          APTO    INAPTO
#FEMININO  92.696848  7.303152
#MASCULINO 92.629028  7.370972


#####X² 2012####

#Criamos uma tab1 e rodamos o teste:

options(scipen = 999, digits = 2)

tab2 <- table(APTOS.INAPTOS2$descricao_sexo, APTOS.INAPTOS2$desc_sit_cand_superior)
chisq.test(tab2) 

#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab2
#X-squared = 945.67, df = 1, p-value < 0.00000000000000022


tab2
#           APTO INAPTO
#FEMININO   97017   9865
#MASCULINO 201692  13440

#Porcentagem
tab2 <- prop.table (tab2, margin = 1)
tab2 * 100
#           APTO    INAPTO
#FEMININO  90.770195  9.229805
#MASCULINO 93.752673  6.247327

#####X² 2016####

#Criamos uma tab1 e rodamos o teste:

options(scipen = 999, digits = 2)

tab3 <- table(APTOS.INAPTOS3$descricao_sexo, APTOS.INAPTOS3$desc_sit_cand_superior)
chisq.test(tab3) 

#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab3
#X-squared = 872, df = 1, p-value <0.0000000000000002

tab3
#          APTO INAPTO
#FEMININO  102165   7358
#MASCULINO 207680   9346

#Porcentagem
tab3 <- prop.table (tab3, margin = 1)
tab3 * 100
#           APTO INAPTO
#FEMININO  93.3    6.7
#MASCULINO 95.7    4.3

##4ªtabela####
#Qui-quadrado entre o sexo e a escolaridade, no banco de INAPTO E APTO
# e a tabela de relação

#APTAS 2008
table(Banco2008Vereadores$descricao_grau_instrucao_agreg)

tab4 <- table(Banco2008Vereadores$descricao_sexo, 
              Banco2008Vereadores$descricao_grau_instrucao_agreg)
chisq.test(tab4)
#Pearson's Chi-squared test

#data:  tab4
#X-squared = 8015, df = 2, p-value <0.0000000000000002

tab4
#          FUNDAMENTAL  MÉDIO SUPERIOR
#FEMININO        15663 21705    15091
#Porcentagem
tab4 <- prop.table (tab4, margin = 1)
tab4 * 100
#             FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO             30    41       29

#INAPTAS 2008
tab5 <- table(X2008INAPTOS$descricao_sexo, 
              X2008INAPTOS$descricao_grau_instrucao_agreg)
chisq.test(tab5)
#Pearson's Chi-squared test

#data:  tab5
#X-squared = 711, df = 2, p-value <0.0000000000000002

tab5
#           FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO         1517  1605     1011
#Porcentagem
tab5 <- prop.table (tab5, margin = 1)
tab5 * 100
#             FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO            37    39       24


#APTAS 2012

tab6 <- table(Banco2012Vereadores$descricao_sexo, 
              Banco2012Vereadores$descricao_grau_instrucao_agreg)
chisq.test(tab6)
#Pearson's Chi-squared test

#data:  tab6
#X-squared = 6113, df = 2, p-value <0.0000000000000002

tab6
#           FUNDAMENTAL  MÉDIO SUPERIOR
#FEMININO        29650 42029    25338
#Porcentagem
tab6 <- prop.table (tab6, margin = 1)
tab6 * 100
#            FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO            31    43       26

#INAPTAS 2012

tab7 <- table(X2012INAPTOS$descricao_sexo, 
              X2012INAPTOS$descricao_grau_instrucao_agreg)
chisq.test(tab7)
#Pearson's Chi-squared test

#data:  tab7
#X-squared = 515, df = 2, p-value <0.0000000000000002

tab7
#           FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO          3590  4195     2080
#Porcentagem
tab7 <- prop.table (tab6, margin = 1)
tab7 * 100
#            FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO           31    43       26


#APTAS 2016

tab8 <- table(Banco2016Vereadores$descricao_sexo, 
              Banco2016Vereadores$descricao_grau_instrucao_agreg)
chisq.test(tab8)
#Pearson's Chi-squared test

#data:  tab8
#X-squared = 4404, df = 2, p-value <0.0000000000000002

tab8
#          FUNDAMENTAL  MÉDIO SUPERIOR
#FEMININO         30236 46187    25742
#Porcentagem
tab8 <- prop.table (tab8, margin = 1)
tab8 * 100
#            FUNDAMENTAL    MÉDIO SUPERIOR
#FEMININO     30    45       25


#INAPTAS 2016

tab9 <- table(X2016INAPTOS$descricao_sexo, 
              X2016INAPTOS$descricao_grau_instrucao_agreg)
chisq.test(tab9)
#Pearson's Chi-squared test

#data:  tab9
#X-squared = 295, df = 2, p-value <0.0000000000000002

tab9
#            FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO          2627  3284     1447
#Porcentagem
tab9 <- prop.table (tab9, margin = 1)
tab9 * 100
#             FUNDAMENTAL MÉDIO SUPERIOR
#FEMININO           36    45       20

##5ªTabela####
freq(Banco2008Vereadores$descricao_ocupacao_agreg)
#Originalmente tem 4 categorias:
#                          Frequência Percentual
#ESPECIALIZADO FUNDAMENTAL     126728     51.703
#ESPECIALIZADO MÉDIO            58014     23.669
#ESPECIALIZADO SUPERIOR         43560     17.772
#NÃO ESPECIALIZADO              16805      6.856
#Total                          245107    100.00

#Na tab. da Adriana tem 2 apenas, 1- Não especializado + Espec. fundamemntal
# e 2- Espec. Médio + Superior. Então vamos juntar dessa forma
##2008
Banco2008Vereadores$Ocupação <- recode(Banco2008Vereadores$descricao_ocupacao_agreg,
                            1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                            2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))

freq(Banco2008Vereadores$Ocupação)
#       Frequência Percentual
#1         143533      58.56
#2         101574      41.44
#Total     245107      100.00

#Ok, arrumamos a V, agora vamos rodar a tab descritiva 
#entre sexo e a nova variável. 

tab9 <- table(Banco2008Vereadores$descricao_sexo, Banco2008Vereadores$Ocupação)
tab9
#            1      2
#FEMININO    24898  27561

options(scipen = 999, digits = 2)
#Porcentagem
tab9 <- prop.table (tab9, margin = 1)
tab9 * 100
#          1  2
#FEMININO  47 53


##2012
Banco2012Vereadores$Ocupação <- recode(Banco2012Vereadores$descricao_ocupacao_agreg,
                                       1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                                       2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))

freq(Banco2012Vereadores$Ocupação)
#        Frequência Percentual
#1         163884      54.86
#2         134826      45.14
#Total     298710     100.00


tab10 <- table(Banco2012Vereadores$descricao_sexo, Banco2012Vereadores$Ocupação)
tab10
#             1      2
#FEMININO   49441  47576

options(scipen = 999, digits = 2)
#Porcentagem
tab10 <- prop.table (tab10, margin = 1)
tab10 * 100
#          1  2
#FEMININO  51 49


##2016
Banco2016Vereadores$Ocupação <- recode(Banco2016Vereadores$descricao_ocupacao_agreg,
                                       1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                                       2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))
freq(Banco2016Vereadores$Ocupação)
#        Frequência Percentual
#1         167121      53.94
#2         142725      46.06
#Total     309846     100.00

tab11 <- table(Banco2016Vereadores$descricao_sexo, Banco2016Vereadores$Ocupação)
tab11
#             1      2
#FEMININO   54048  48117

#Porcentagem
tab11 <- prop.table (tab11, margin = 1)
tab11 * 100
#          1  2
#FEMININO  53 47


#########################INAPTAS
##2008
X2008INAPTOS$Ocupação <- recode(X2008INAPTOS$descricao_ocupacao_agreg,
                                       1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                                       2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))
freq(X2008INAPTOS$Ocupação)
#        Frequência Percentual
#1          12482      64.13
#2           6981      35.87
#Total      19463     100.00

tab12 <- table(X2008INAPTOS$descricao_sexo, X2008INAPTOS$Ocupação)
tab12
#             1     2
#FEMININO   2201  1932

#Porcentagem
tab12 <- prop.table (tab12, margin = 1)
tab12 * 100
#          1  2
#FEMININO  53 47

##2012
X2012INAPTOS$Ocupação <- recode(X2012INAPTOS$descricao_ocupacao_agreg,
                                1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                                2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))
freq(X2012INAPTOS$Ocupação)
#        Frequência Percentual
#1          13942      59.83
#2           9362      40.17
#Total      23304     100.00

tab13 <- table(X2012INAPTOS$descricao_sexo, X2012INAPTOS$Ocupação)
tab13
#             1     2
#FEMININO   5702 4163

#Porcentagem
tab13 <- prop.table (tab13, margin = 1)
tab13 * 100
#          1  2
#FEMININO  58 42

##2016
X2016INAPTOS$Ocupação <- recode(X2016INAPTOS$descricao_ocupacao_agreg,
                                1 <- c("NÃO ESPECIALIZADO","ESPECIALIZADO FUNDAMENTAL"),
                                2 <- c("ESPECIALIZADO MÉDIO", "ESPECIALIZADO SUPERIOR"))
freq(X2016INAPTOS$Ocupação)
#        Frequência Percentual
#1          9942      59.52
#2           6761      40.48
#Total      16703     100.00

tab14 <- table(X2016INAPTOS$descricao_sexo, X2016INAPTOS$Ocupação)
tab14
#             1    2
#FEMININO  4354 3004

#Porcentagem
tab14 <- prop.table (tab14, margin = 1)
tab14 * 100
#           1  2
#FEMININO  59 41

##6ªTabela####
#Agora sexo e receita, descritiva e qui-quadrado

#Linha
#sexofeminino <- banco[banco$variávelsexo == "nome da categoria",
#c ("varialvel receita", "outra variável")]

table(Banco2008Vereadores$descricao_sexo)
#FEMININO MASCULINO 
table(Banco2008Vereadores$receitas_total)
table(Banco2008Vereadores$nome_candidato)

sexofeminino <- Banco2008Vereadores[Banco2008Vereadores$descricao_sexo ==
                        "FEMININO", c("receitas_total", "nome_candidato")]


summary(sexofeminino$receitas_total)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0      85     450    1291    1474  186540 

#Qui-quadrado entre o sexo e as receitas 
tab15 <- table(Banco2008Vereadores$receitas_total, 
               Banco2008Vereadores$descricao_sexo)

chisq.test(tab15) 

#Pearson's Chi-squared test
#data:  tab15
#X-squared = 12339, df = 12341, p-value = 0.5
#Warning message:
#  In chisq.test(tab15) : Aproximação do qui-quadrado pode estar incorreta
#Aparece essa mensagem porque como aprendemos na aula, o qui-quadrado não é um teste  
#adequado para todo tipo de variável. Outros testes podem ser melhores, como
#O kendall, e o Rho.
library(Kendall)

Kendall(Banco2008Vereadores$receitas_total, 
        Banco2008Vereadores$codigo_sexo)
options(scipen = 999)
#tau = -0.0632, 2-sided pvalue =<0.0000000000000002

cor.test(~ receitas_total + codigo_sexo, data = Banco2008Vereadores,
         method = "pearson", continuity = F, conf.level = 0.95, log= "xy")

#Pearson's product-moment correlation

#data:  receitas_total and codigo_sexo
#t = -22, df = 245105, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.048 -0.040
#sample estimates:
#  cor 
#-0.044 




#2008 INAPTA

sexofemininoI8 <- X2008INAPTOS[X2008INAPTOS$descricao_sexo ==
                                      "FEMININO", c("receitas_total", "nome_candidato")]

# summary -inforacoes de receitas para as mulheres
summary(sexofemininoI8$receitas_total)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0       0       0           236      50   28505  


#APTA 2012
sexofemininoA12 <- Banco2012Vereadores[Banco2012Vereadores$descricao_sexo ==
                                 "FEMININO", c("receitas_total", "nome_candidato")]

# summary -inforacoes de receitas para as mulheres
summary(sexofemininoA12$receitas_total)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##0       0     332    1590    1690  151800 

#INAPTA 2012
sexofemininoI12 <- X2012INAPTOS[X2012INAPTOS$descricao_sexo ==
                                         "FEMININO", c("receitas_total", "nome_candidato")]

# summary -inforacoes de receitas para as mulheres
summary(sexofemininoI12$receitas_total)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
###0       0       0          161       0   35728 

#Rho - Como vimos que esse teste é melhor, vamos usar ele para os demais casos
cor.test(~ receitas_total + codigo_sexo, data = Banco2012Vereadores,
         method = "pearson", continuity = F, conf.level = 0.95, log= "xy")
#Pearson's product-moment correlation

#data:  receitas_total and codigo_sexo
#t = -70, df = 298708, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.13 -0.12
#sample estimates:
#  cor 
#-0.13 

#APTA 2016
sexofemininoA16 <- Banco2016Vereadores[Banco2016Vereadores$descricao_sexo ==
                                         "FEMININO", c("receitas_total", "nome_candidato")]

# summary -inforacoes de receitas para as mulheres
summary(sexofemininoA16$receitas_total)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###0     249     690    1661    2030  184684

#INAPTA 2016
sexofemininoI16 <- X2016INAPTOS[X2016INAPTOS$descricao_sexo ==
                                  "FEMININO", c("receitas_total", "nome_candidato")]

# summary -inforacoes de receitas para as mulheres
summary(sexofemininoI16$receitas_total)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0       0     148          435     462   20698  

#Rho - Como vimos que esse teste é melhor, vamos usar ele para os demais casos
cor.test(~ receitas_total + codigo_sexo, data = Banco2016Vereadores,
         method = "pearson", continuity = F, conf.level = 0.95, log= "xy")


#data:  receitas_total and codigo_sexo
#t = -72, df = 309844, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.13 -0.12
#sample estimates:
#  cor 
#-0.13 




##7ªTabela#####
options(scipen = 1000)
#Relação e qui-quadrado entre sexo e incumbente ou desafiante (INAPTAS)
tab16 <- table(X2008INAPTOS$descricao_sexo, X2008INAPTOS$incumb.xdesaf.)

tab16
#            DESAFIANTE INCUMBENTE
#FEMININO       4019        114

chisq.test(tab16)
#Pearson's Chi-squared test with Yates' continuity correction
#data:  tab16
#X-squared = 90, df = 1, p-value <0.0000000000000002

tab16 <- prop.table (tab16, margin = 1)
tab16 * 100

#              DESAFIANTE INCUMBENTE
#FEMININO       97.2        2.8


#2012
tab17 <- table(X2012INAPTOS$descricao_sexo, X2012INAPTOS$incumb.xdesaf.)
tab17
#             DESAFIANTE INCUMBENTE
#FEMININO      9726        139

chisq.test(tab17)
#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab17
#X-squared = 304, df = 1, p-value <0.0000000000000002

tab17 <- prop.table (tab17, margin = 1)
tab17 * 100

#                DESAFIANTE INCUMBENTE
#FEMININO          98.6        1.4


#2016
tab18 <- table(X2016INAPTOS$descricao_sexo, X2016INAPTOS$incumb.xdesaf.)
tab18
#             DESAFIANTE INCUMBENTE
#FEMININO     7287         71


chisq.test(tab18)
#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab18
#X-squared = 264, df = 1, p-value <0.0000000000000002

tab18 <- prop.table (tab18, margin = 1)
tab18 * 100

#             DESAFIANTE INCUMBENTE
#FEMININO         99.04       0.96


# TABELA 7 – CANDIDATAS 
# DESAFIANTES OU INCUMBENTES COM CANDIDATURAS APTAS 
# NAS CANDIDATURAS A VEREADOR NOS MUNICÍPIOS 
# COM MENOS DE 50,000 ELEITORES NAS ELEIÇÕES DE 2008, 2012 E 2016 (%)


library(memisc)

#NULO#   APTO 
#3899    241208 

Banco2008Vereadores$Aptos <- 
  recode(Banco2008Vereadores$desc_sit_cand_superior, 
     'Aptos'<- 'APTO')

#Candidaturas aptas####
#2008####
tab1 <- table(Banco2008Vereadores$descricao_sexo, 
              Banco2008Vereadores$incumb.xdesaf.)

tab1

# DESAFIANTE INCUMBENTE
# FEMININO       48434       4025
# MASCULINO     163537      29111

options(digits = 3, scipen = 999)


chisq.test(tab1)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab1
# X-squared = 1951, df = 1, p-value <0.0000000000000002
# 

Banco2008Vereadores$descricao_sexo <- 
  as.factor(Banco2008Vereadores$descricao_sexo)

Banco2008Vereadores$incumb.xdesaf. <- 
  as.factor(Banco2008Vereadores$incumb.xdesaf)


tab1 <- prop.table (tab1, margin = 1)
tab1 * 100

#             DESAFIANTE INCUMBENTE
# FEMININO       92.33       7.67
# MASCULINO      84.89      15.11

#2012####
table(Banco2012Vereadores$desc_sit_cand_superior)

Banco2012Vereadores$Aptos <- 
  recode(Banco2012Vereadores$desc_sit_cand_superior, 
         'Aptos'<- 'APTO')

Banco2012Vereadores$descricao_sexo <- 
  as.factor(Banco2012Vereadores$descricao_sexo)

Banco2012Vereadores$incumb.xdesaf. <- 
  as.factor(Banco2012Vereadores$incumb.xdesaf)

tab2 <- table(Banco2012Vereadores$descricao_sexo, 
              Banco2012Vereadores$incumb.xdesaf.)

tab2


# DESAFIANTE INCUMBENTE
# FEMININO       93117       3900
# MASCULINO     173649      28044

options(digits = 3, scipen = 999)


chisq.test(tab2)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab1
# X-squared = 1951, df = 1, p-value <0.0000000000000002
# 



tab2 <- prop.table (tab2, margin = 1)
tab2 * 100

#             DESAFIANTE INCUMBENTE
# FEMININO      95.98       4.02
# MASCULINO      86.10      13.90

#2016####
table(Banco2016Vereadores$desc_sit_cand_superior)

Banco2016Vereadores$Aptos <- 
  recode(Banco2016Vereadores$desc_sit_cand_superior, 
         'Aptos'<- 'APTO')

Banco2016Vereadores$descricao_sexo <- 
  as.factor(Banco2016Vereadores$descricao_sexo)

Banco2016Vereadores$incumb.xdesaf. <- 
  as.factor(Banco2016Vereadores$incumb.xdesaf)

tab3 <- table(Banco2016Vereadores$descricao_sexo, 
              Banco2016Vereadores$incumb.xdesaf.)

tab3


#           DESAFIANTE INCUMBENTE
# FEMININO       97662       4503
# MASCULINO     176902      30779

options(digits = 3, scipen = 999)


chisq.test(tab3)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab3
#X-squared = 7357, df = 1, p-value <0.0000000000000002


tab3 <- prop.table (tab3, margin = 1)
tab3 * 100

#             DESAFIANTE INCUMBENTE
# FEMININO        95.59       4.41
# MASCULINO      85.18      14.82


##8ªTabela#####
##Agr Vamos usar apenas os bancos de INAPTOS
#Para testar a carreira política em e o sexo 

#2008
tab19 <- table(X2008INAPTOS$descricao_sexo, X2008INAPTOS$descricao_ocupacao_agreg1)
tab19

#              NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                    4088                   45

chisq.test(tab19)
#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab19
#X-squared = 58, df = 1, p-value = 0.00000000000003

tab19 <- prop.table (tab19, margin = 1)
tab19 * 100
#                   NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                       98.9                  1.1


#2012
tab20 <- table(X2012INAPTOS$descricao_sexo, X2012INAPTOS$descricao_ocupacao_agreg1)
tab20
#              NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                   9786                   79

chisq.test(tab20)
#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab20
#X-squared = 130, df = 1, p-value <0.0000000000000002

tab20 <- prop.table (tab20, margin = 1)
tab20 * 100
#                    NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                         99.2                  0.8


#2016
tab21 <- table(X2016INAPTOS$descricao_sexo, X2016INAPTOS$descricao_ocupacao_agreg1)
tab21
#              NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                      7322                   36

chisq.test(tab21)
#Pearson's Chi-squared test with Yates' continuity correction

#data:  tab20
#X-squared = 125, df = 1, p-value <0.0000000000000002

tab21 <- prop.table (tab21, margin = 1)
tab21 * 100
#                    NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
#FEMININO                        99.51                 0.49


##8ªTabela#####
##Agr Vamos usar apenas os bancos de APTOS

# TABELA 8 – CANDIDATAS COM CARREIRA 
# POLÍTICA OU NÃO COM CANDIDATURAS APTAS NAS 
# CANDIDATURAS A VEREADOR NOS MUNICÍPIOS COM MENOS DE 
# 50.000 ELEITORES NAS ELEIÇÕES DE 2008, 2012 E 2016 (%)

library(memisc)

Banco2008Vereadores$Aptos <- 
  recode(Banco2008Vereadores$desc_sit_cand_superior, 
         'Aptos'<- 'APTO')

#CARREIRA POLÍTICA####
#Candidaturas aptas####
#2008####
tab1 <- table(Banco2008Vereadores$descricao_sexo, 
              Banco2008Vereadores$descricao_ocupacao_agreg1)

tab1


#            NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                     50657                 1802
# MASCULINO                   180185                12463

options(digits = 3, scipen = 999)


chisq.test(tab1)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab1
#X-squared = 692, df = 1, p-value <0.0000000000000002
# 



tab1 <- prop.table (tab1, margin = 1)
tab1 * 100

#             NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                     96.56                 3.44
# MASCULINO                    93.53                 6.47

#2012####
tab2 <- table(Banco2012Vereadores$descricao_sexo, 
              Banco2012Vereadores$descricao_ocupacao_agreg1)

tab2


#            NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                   95109                 1908
# MASCULINO                 188346                13347


chisq.test(tab2)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab2
#X-squared =  2923, df = 1, p-value <0.0000000000000002
# 



tab2 <- prop.table (tab2, margin = 1)
tab2 * 100

#             NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                     98.03                 1.97
# MASCULINO                   93.38                 6.62

#2016####
tab3 <- table(Banco2016Vereadores$descricao_sexo, 
              Banco2016Vereadores$descricao_ocupacao_agreg1)

tab3


#            NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                   99885                 2280
# MASCULINO                 192295                15386


chisq.test(tab3)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab3
#X-squared = 3412, df = 1, p-value <0.0000000000000002

# 

tab3 <- prop.table (tab3, margin = 1)
tab3 * 100

#             NÃO POLÍTICO DE CARREIRA POLÍTICO DE CARREIRA
# FEMININO                    97.77                 2.23
# MASCULINO                   92.59                 7.41


##Cálculos####
#Tab3
52459 + 4133
#= 56592x / 5245900
5245900/ 56592
x= 92.69685
100- 92.69685
y= 7.30315


################################
#Criação dos bancos para APÊNDICE Jacques

Nordeste <- Banco2008Vereadores[Banco2008Vereadores$sigla_regiao_agreg ==
                                      "NORDESTE", 
                                c("sigla_uf", "descricao_ue", 
                                  "porte_municipio", "QTD_ELEITORES")]

Norte <- Banco2008Vereadores[Banco2008Vereadores$sigla_regiao_agreg ==
                                  "NORTE", 
                                c("sigla_uf", "descricao_ue", 
                                  "porte_municipio", "QTD_ELEITORES")]

SUL <- Banco2008Vereadores[Banco2008Vereadores$sigla_regiao_agreg ==
                               "SUL", 
                             c("sigla_uf", "descricao_ue", 
                               "porte_municipio", "QTD_ELEITORES")]

Sudeste <- Banco2008Vereadores[Banco2008Vereadores$sigla_regiao_agreg ==
                             "SUDESTE", 
                           c("sigla_uf", "descricao_ue", 
                             "porte_municipio", "QTD_ELEITORES")]

CentroOeste <- Banco2008Vereadores[Banco2008Vereadores$sigla_regiao_agreg ==
                                 "CENTRO OESTE", 
                               c("sigla_uf", "descricao_ue", 
                                 "porte_municipio", "QTD_ELEITORES")]


save(CentroOeste, file = "CentroOeste.RData")
save(Nordeste, file = "Nordeste.RData")
save(Norte, file = "Norte.RData")
save(SUL, file = "SUL.RData")
save(Sudeste, file = "Sudeste.RData")

table(CentroOeste$porte_municipio)

write.csv(CentroOeste, "CentroOeste.txt")
write.csv(Nordeste, file = "Nordeste.txt")
write.csv(Norte, file = "Norte.txt")
write.csv(SUL, file = "SUL.txt")
write.csv(Sudeste, file = "Sudeste.txt")
