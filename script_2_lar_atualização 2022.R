##p.45 ####

#tabela 9
#Receita total por candidaturas aptas 

summary(Banco2008Vereadores$receitas_total)
#Min. 1st Qu.  Median Mean 3rd Qu.    Max. 
#0     130     620    1533    1837  279990 


summary(Banco2012Vereadores$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0     150     836    2379    2945  224738 

summary(Banco2016Vereadores$receitas_total)
#Min. 1st Qu.  Median  Mean 3rd Qu.   Max. 
#0     380    1130    2263    3022  196777

##P.46####
##library(ggplot2)
table(Banco2008Vereadores$total_votos_turno_1)

#Antes de pedir o plot, temos que logaritmizar os valores
#OU você pode pedir a transformação no próprio ggplot
Banco2008Vereadores$log.receitas <- 
  log(Banco2008Vereadores$receitas_total)

Banco2008Vereadores$log.votos <- 
  log(Banco2008Vereadores$total_votos_turno_1)


Obj1 <- ggplot(data = Banco2008Vereadores) + 
  geom_point(aes(x = receitas_total, 
                 y = total_votos_turno_1, color =receitas_total, 
                 size =total_votos_turno_1 ))+ 
  scale_x_log10()+ scale_y_log10() +
  labs(x = "Receitas", y = "Total Votos")

#title

##Outra possibilidade com a linha traçando os pontos entre os eixos
grafico <-ggplot(Banco2008Vereadores, 
                 aes(receitas_total, total_votos_turno_1, 
                     color =receitas_total, size =total_votos_turno_1))+ 
  geom_point()+  scale_x_log10()+ scale_y_log10()+ 
  labs(x = "Receitas", y = "Total Votos") 

grafico + geom_smooth(method = "lm", se = TRUE, color ="red") + 
  theme_classic() + ggtitle(2008) + theme(legend.position = "none")

## 2012
grafico2 <-ggplot(Banco2012Vereadores, 
                 aes(receitas_total, total_votos_turno_1, 
                     color =receitas_total, size =total_votos_turno_1))+ 
  geom_point()+  scale_x_log10()+ scale_y_log10()+ 
  labs(x = "Receitas", y = "Total Votos") 

grafico2 + geom_smooth(method = "lm", se = TRUE, color ="red") + 
  theme_classic() + ggtitle(2012) + theme(legend.position = "none")

##2016
grafico3 <-ggplot(Banco2016Vereadores, aes(receitas_total, total_votos_turno_1, 
                      color =receitas_total, size =total_votos_turno_1))+ 
  geom_point()+  scale_x_log10()+ scale_y_log10()+ 
  labs(x = "Receitas", y = "Total Votos") 

grafico3 + geom_smooth(method = "lm", se = TRUE, color ="red") + 
  theme_classic() + ggtitle(2016) + theme(legend.position = "none")

##p.47####
#VALORES ABSOLUTOS####
#2008
cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2008Vereadores, methodo= "pearson",
         conf.level = 0.95, log= "xy")

options(digits = 2)
#Pearson's product-moment correlation

#data:  total_votos_turno_1 and receitas_total
#t = 263, df = 245105, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.47 0.47
#sample estimates:
# cor 
#0.47 

cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2012Vereadores, methodo= "pearson",
         conf.level = 0.95, log= "xy")
#Pearson's product-moment correlation

#data:  total_votos_turno_1 and receitas_total
#t = 306, df = 298708, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.49 0.49
#sample estimates:
# cor 
#0.49 

cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2016Vereadores, methodo= "pearson",
         conf.level = 0.95, log= "xy")
#Pearson's product-moment correlation

#data:  total_votos_turno_1 and receitas_total
#t = 313, df = 309844, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.49 0.49
#sample estimates:
# cor 
#0.49 

##VALORES RELATIVOS####
#Faça um subst para cada ano para trabalhar com bancos menores


summary(Banco2008Vereadores$total_de_voto_no_municipio)
Banco2008Vereadores$total_de_receita_no_municipio
Banco2008Vereadores$receitas_total
Banco2008Vereadores$total_votos_turno_1
Banco2008Vereadores$nome_candidato

Banco2008Menor$total_de_receita_no_municipio

#2008
cor.test(~ total_de_voto_no_municipio + total_de_receita_no_municipio, 
         data = Banco2008Menor, methodo= "pearson",
         conf.level = 0.95, log= "xy")

#Pearson's product-moment correlation

#data:  total_de_voto_no_municipio and total_de_receita_no_municipio
#t = 366, df = 245105, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.59 0.60
#sample estimates:
# cor 
#0.59 

#2012
cor.test(~ total_de_voto_no_municipio + total_de_receita_no_municipio, 
         data = Banco2012Menor, methodo= "pearson",
         conf.level = 0.95, log= "xy")
#Pearson's product-moment correlation

#data:  total_de_voto_no_municipio and total_de_receita_no_municipio
#t = 401, df = 298708, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.59 0.59
#sample estimates:
# cor 
#0.59 

#2016
cor.test(~ total_de_voto_no_municipio + total_de_receita_no_municipio, 
         data = Banco2016Menor, methodo= "pearson",
         conf.level = 0.95, log= "xy")

#Pearson's product-moment correlation

#data:  total_de_voto_no_municipio and total_de_receita_no_municipio
#t = 469, df = 309844, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.64 0.65
#sample estimates:
# cor 
#0.64 

##P.48####
#No plot entre quantidade de votos e receitas vimos a existência de 
#vários pontos influentes (outliers) que podem fazer com que os resíduos
#se elevem bastante
# Para saber quem são esses casos construindo uma planilha com os valores
#preditos e os erros para cada caso a partir de um modelo
# Usamos o comando cbind que combina linhas e colunas em uma base de
#dados

##2008####
Banco2008Vereadores$receitas_total
summary(Banco2008Vereadores$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      36      91     158     202    3483 
#modelo <- lm(banco$variável ~ banco$independente)
#resid <- (cbind(banco$variável, predict(modelo), residuals(modelo)))

modelo1 <- lm(Banco2008Vereadores$total_votos_turno_1 ~ 
                Banco2008Vereadores$receitas_total)
summary(modelo1)

options(scipen = 1000, digits = 2)

resid1 <- (cbind(Banco2008Vereadores$total_votos_turno_1,
                predict(modelo1), residuals(modelo1)))
resid1


#Salve, e exclua o caso 
write_excel_csv2(Banco2008Menor, "Banco2008Menor1.xlsx")

modelo1.1 <- lm(Banco2008Menor1$total_votos_turno_1 ~ 
                Banco2008Menor1$receitas_total)
summary(modelo1.1)

#Correlação após a exclusão
cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2008Menor1, methodo= "pearson",
         conf.level = 0.95, log= "xy")

#	Pearson's product-moment correlation

#data:  total_votos_turno_1 and receitas_total
#t = 264, df = 245104, p-value <0.0000000000000002
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.47 0.47
#sample estimates:
 # cor 
#0.47

##2012####
Banco2012Vereadores$receitas_total
summary(Banco2012Vereadores$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      36      91     158     202    3483 
#modelo <- lm(banco$variável ~ banco$independente)
#resid <- (cbind(banco$variável, predict(modelo), residuals(modelo)))

modelo2 <- lm(Banco2012Vereadores$total_votos_turno_1 ~ 
                Banco2012Vereadores$receitas_total)
summary(modelo2)
resid2 <- (cbind(Banco2012Vereadores$total_votos_turno_1,
                 predict(modelo2), residuals(modelo2)))


#Salve, e exclua o caso 
write_excel_csv2(Banco2012Menor, "Banco2012Menor1.xls")

modelo2.1 <- lm(Banco2012Menor1$total_votos_turno_1 ~ 
                  Banco2012Menor1$receitas_total)
summary(modelo2.1)

#Correlação após a exclusão
cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2012Menor1, methodo= "pearson",
         conf.level = 0.95, log= "xy")
#	Pearson's product-moment correlation

# data:  total_votos_turno_1 and receitas_total
# t = 306, df = 298707, p-value <0.0000000000000002
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.49 0.49
# sample estimates:
#   cor 
# 0.49 

##2016####
Banco2016Vereadores$receitas_total
summary(Banco2016Vereadores$total_votos_turno_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      36      91     158     202    3483 
#modelo <- lm(banco$variável ~ banco$independente)
#resid <- (cbind(banco$variável, predict(modelo), residuals(modelo)))

modelo3 <- lm(Banco2016Vereadores$total_votos_turno_1 ~ 
                Banco2016Vereadores$receitas_total)
summary(modelo3)
resid3 <- (cbind(Banco2016Vereadores$total_votos_turno_1,
                 predict(modelo3), residuals(modelo3)))


#Salve, e exclua o caso 
write_excel_csv2(Banco2016Menor, "Banco2016Menor1.xls")

modelo3.1 <- lm(Banco2016Menor1$total_votos_turno_1 ~ 
                  Banco2016Menor1$receitas_total)
summary(modelo3.1)

#Correlação após a exclusão
cor.test(~ total_votos_turno_1 + receitas_total, 
         data = Banco2016Menor1, methodo= "pearson",
         conf.level = 0.95, log= "xy")
#	Pearson's product-moment correlation

# data:  total_votos_turno_1 and receitas_total
# t = 313, df = 309843, p-value <0.0000000000000002
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.49 0.49
# sample estimates:
#   cor 
# 0.49


##p.50####
#Regressão Linear Simples entre votos e sexo e renda (???)
#O correto nesse caso é regressão multipla pra testar 

##2008####
#Coeficiente padronizado de beta
#pct: lm.beta
library(lm.beta)
library(sjPlot)


Teste1 <- lm(total_de_voto_no_municipio ~ descricao_sexo + 
               total_de_receita_no_municipio, 
            data = Banco2008Vereadores)

Teste1beta <- lm.beta(Teste1)

tab_model(Teste1beta, show.ci = F, auto.label = F, 
          show.se = F, collapse.se =F,
          p.style = "numeric")



#Coefplot
obj1 <- coefplot(Teste1beta, title = "2008",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

##2012####
Teste2 <- lm(total_de_voto_no_municipio ~ descricao_sexo + 
               total_de_receita_no_municipio, 
             data = Banco2012Vereadores)

Teste2beta <- lm.beta(Teste2)

tab_model(Teste2beta, show.ci = F, auto.label = F, 
          show.se = F, collapse.se =F,
          p.style = "numeric")

obj2 <- coefplot(Teste2beta, title = "2012",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 0)

obj2 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 


##2016####
Teste3 <- lm(total_de_voto_no_municipio ~ descricao_sexo + 
               total_de_receita_no_municipio, 
             data = Banco2016Vereadores)

Teste3beta <- lm.beta(Teste3)

tab_model(Teste3beta, show.ci = F, auto.label = F, 
          show.se = F, collapse.se =F,
          p.style = "numeric")

obj3 <- coefplot(Teste3beta, title = "2016",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 0)

obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

##P.52####
library(tidyverse)
##VOTOS####
##2008####
table(Banco2008Vereadores$descricao_sexo)
# FEMININO MASCULINO 
# 52459    192648 

summary(Banco2008Vereadores$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0      36      91     158     202    3483 

Mulher <- Banco2008Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
summary(Mulher$total_votos_turno_1)
#Min. 1st Qu.  Median  Mean 3rd Qu.    Max. 
#0      21      55     113     134    2921

Homem1 <- Banco2008Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
summary(Homem1$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0      42     102     171     219    3483 

##2012#### 
summary(Banco2012Vereadores$total_votos_turno_1)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      22      72     140     182    4051 

Mulher2 <- Banco2012Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
summary(Mulher2$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       2      24      73      79    3152 

Homem <- Banco2012Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
summary(Homem$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      42     104     172     224    4051 

##2016####
summary(Banco2016Vereadores$total_votos_turno_1)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0      22      70     140     181    3498 

Mulher3 <- Banco2016Vereadores %>%
  filter(descricao_sexo == "FEMININO") 
summary(Mulher3$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       5      23      72      74    3236 

Homem2 <- Banco2016Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
summary(Homem2$total_votos_turno_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0       42     103     173     226    3498 

#GráficoExtra1- solicitado na correção dos professores####

Anos <- c(rep(c("2008", "2012", "2016")))  
Sexo <- c(rep(c("Homem", "Mulher")))
Médias <-c(170, 73, 173, 113, 172, 72)
Data <- data.frame(Anos, Sexo, Médias)

Gráfico <- ggplot(Data, aes(x = Anos, y = Médias,
                            fill = Sexo, label = Médias)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

Gráfico

Gráfico + theme_classic() + labs(x = "Anos", y=" Médias de votos") +
  theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Pastel2", type = "qual")

#GRÁFICOSExtras2022-  solicitado na correção dos professores####
#VOTOS####
SexoAno <- c("2008.Homem", "2008.Mulher", "2012.Homem", 
             "2012.Mulher", 
             "2016.Homem", "2016.Mulher")
Médias <- c(170, 113, 172, 73, 173, 72)
Data2 <- data.frame(SexoAno, Médias)
Gráfico2 <-  ggplot(Data2, aes(y=Médias, x=  as.factor(SexoAno), 
                               label = Médias)) +
  geom_bar(stat = "identity", width = .75,
           color="grey", fill=rgb(0.1,0.4,0.5,0.7))+
  geom_text(aes(label = Médias), vjust=1.6, color="white",size=3.5)+
  xlab("Sexo e ano") + ylab("Média de votos")

grafico <- ggplot(Data2, aes(y= SexoAno)) +
  geom_bar()

Objeto <-Gráfico2 + theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))

Objeto 

#Cores específicas####

df <- data.frame(Ano_Sexo=c("2008.Homem", "2008.Mulher", "2012.Homem",
                        "2012.Mulher", 
                        "2016.Homem", "2016.Mulher"),
                Média=c(170, 113, 172, 73, 173, 72))

p<-ggplot(df, aes(x=Ano_Sexo, y=Média, fill=Ano_Sexo)) +
  geom_bar(stat="identity", alpha=0.9)

p <- p + scale_fill_manual(values=c("#4170F0", "#ED9195", "#4170F0",
                               "#ED9195", "#4170F0", "#ED9195"))+
geom_text(aes(label = Médias), vjust=1.6, color="white",size=3.5)+
  xlab("Sexo e ano") + ylab("Média de votos")

p + theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  theme(legend.position="none")

#GráficoExtra2####

Anos <- c(rep(c("2008", "2012", "2016")))  
Sexo <- c(rep(c("Homem", "Mulher")))
Médias <-c(1599, 1590, 2559, 1291, 1198, 1661)
Data <- data.frame(Anos, Sexo, Médias)

Gráfico <- ggplot(Data, aes(x = Anos, y = Médias,
                            fill = Sexo, label = Médias)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

Gráfico

Gráfico + theme_classic() + labs(x = "Anos", y=" Médias das receitas") +
  theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Pastel2", type = "qual")



#GRÁFICOSExtras2022-  solicitado na correção dos professores####
#VOTOS####
SexoAno <- c("2008.Homem", "2008.Mulher", "2012.Homem", 
             "2012.Mulher", 
             "2016.Homem", "2016.Mulher")
Médias <- c(1599, 1291, 1198, 1590, 2559, 1661)
Data2 <- data.frame(SexoAno, Médias)

Gráfico2 <-  ggplot(Data2, aes(y=Médias, x=  as.factor(SexoAno), 
                               labe = Médias)) +
  geom_bar(stat = "identity", width = .75,
           color="grey", fill=rgb(0.1,0.4,0.5,0.7))+
  geom_text(aes(label = Médias), vjust=1.6, color="white",size=3.5)+
  labs(x = "Sexo Ano", y=" Médias das receitas")


Objeto <-Gráfico2 + theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))

Objeto + 
scale_y_continuous(labels = scales::number_format(prefix = "R$ "))
  
  
#Cores específicas####

df <- data.frame(Ano_Sexo=c("2008.Homem", "2008.Mulher", "2012.Homem",
                            "2012.Mulher", 
                            "2016.Homem", "2016.Mulher"),
                 Média=c(1599, 1291, 1198, 1590, 2559, 1661))

p<-ggplot(df, aes(x=Ano_Sexo, y=Média, fill=Ano_Sexo)) +
  geom_bar(stat="identity", alpha=0.9)

p <- p + scale_fill_manual(values=c("#4170F0", "#ED9195", "#4170F0",
                                    "#ED9195", "#4170F0", "#ED9195"))+
  geom_text(aes(label = Médias), vjust=1.6, color="white",size=3.5)+
  labs(x = "Sexo Ano", y=" Médias das receitas")

p + theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  theme(legend.position="none")+
  scale_y_continuous(labels = scales::number_format(prefix = "R$ "))


##Gráfico1####
#install.packages("ggplot2")
library(ggplot2)

Objc <- data.frame (Médias = c("Média.Geral","Média.Mulher",	"Média.Homem"), 
                    "2008"=	c(158, 55,	171),
                    "2012"=	c(140, 73, 172),
                    "2016"=	c(140,	72, 173	)) 

Objc.1 <- Objc %>% gather(Anos, Médias.Votos, -Médias)

Gra3 <- ggplot(data = Objc.1, aes(x= Médias, y= Médias.Votos, 
                                  group = Anos))+
  geom_line(aes(colour = Anos), size = 2) + 
  geom_point(aes(shape = Anos))

Gra3

Gra3 + theme_bw()+ theme(legend.position = "bottom")

#up 

##RECEITAS####
##2008####
table(Banco2008Vereadores$descricao_sexo)
# FEMININO MASCULINO 
# 52459    192648 

summary(Banco2008Vereadores$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0     130     620    1533    1837  279990 

summary(Mulher$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0      85     450    1291    1474  186540 

 
summary(Homem$receitas_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0     150     678    1599    1939  279990  

##2012####
summary(Banco2012Vereadores$receitas_total)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0     150     836     2379    2945     224738  


summary(Mulher2$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0       0     332    1590    1690  151800 

summary(Homem2$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     265    1198    2759    3472  224738 

##2016####
summary(Banco2016Vereadores$receitas_total)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     0     380    1130    2263    3022  196777 

summary(Mulher3$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0     249     690    1661    2030  184684 

Homem3 <- Banco2016Vereadores %>%
  filter(descricao_sexo == "MASCULINO") 
summary(Homem3$receitas_total)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     477    1450    2559    3450  1967774

##Gráfico2####
library(ggplot2)

Objc <- data.frame (Médias = c("Média.Geral","Média.Mulher",	"Média.Homem"), 
                    "2008"=	c(1533, 1291,	1599),
                    "2012"=	c(2379, 1590, 2759),
                    "2016"=	c(2263,	1661, 2559)) 

Objc.1 <- Objc %>% gather(Anos, Médias.Receitas, -Médias)

Gra3 <- ggplot(data = Objc.1, aes(x= Médias, y= Médias.Receitas, 
                                  group = Anos))+
  geom_line(aes(colour = Anos), size = 2) + 
  geom_point(aes(shape = Anos))

Gra3

Gra3 + theme_bw()+ theme(legend.position = "bottom")
