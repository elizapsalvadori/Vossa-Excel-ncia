### DISSERTa�AO MESTRADO ELIZA

## TEMAS BRANCOS
install.packages("sjPlot")
install.packages("ggplot")
theme_set(theme_sjplot())

## INSTALANDO PACOTES
install.packages("readxl")
install.packages("ggplot2")

## ABRINDO BANCO DE DADOS

library(readxl)
dados_oldmethodoly <- read_excel("~/MESTRADO CI�NCIA POLITICA/Disserta��o/dados_oldmethodoly.xlsx")
View(dados_oldmethodoly)

## CHAMANDO A BASE DE DADOS

attach(dados_oldmethodoly)

### CRIANDO VARI�VEL ELEVADA AO QUADRADO - INDEPENDENCIA JUDICIAL

dados_oldmethodoly$inde_jud <- dados_oldmethodoly$inde_jud^2


### CRIANDO VARI�VEL NORMAL - INDEPENDENCIA JUDICIAL
dados_oldmethodoly$jud<-sqrt(dados_oldmethodoly$inde_jud)

### TRANSFORMANDO VARI�VEIS EM NUMERICAS

idhnovo <- as.numeric(dados_oldmethodoly$idh)

gininovo <- as.numeric(dados_oldmethodoly$gini)

pibnovo <- as.numeric(dados_oldmethodoly$pib) 


###DEFASANDO A VARIAVEL DEPENDENTE 
dados_oldmethodoly$dep_lag <- Lag(dados_oldmethodoly$per_corrup�ao, -1)
summary(dados_oldmethodoly$dep_lag)


# ESTATISTICA DESCRITIVA VARIAVEL DEPENDENTE - PERCEPÇAO DE CORRUPÇAO

summary(dados_oldmethodoly$per_corrup�ao)

# GRAFICO DE BARRA DA VARIAVEL DEPENDENTE - PERCEP�AO DE CORRUP�AO
library(ggplot2)
library(sjPlot)
attach(dados_oldmethodoly)

ggplot(dados_oldmethodoly, aes(x=per_corrup�ao)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Percep��o da Corrup��o") +
  ylab("Frequ�ncia")
    
# HISTOGRAMA VD


attach(dados_oldmethodoly)
hist(dados_oldmethodoly$per_corrup�ao)


### ESTATISTICA DESCRITIVA VARIAVEL INDEPENDENTE - INDEPENDENCIA JUDCIAL

summary(dados_oldmethodoly$inde_jud)
pander(summary(dados_oldmethodoly$inde_jud))


## ESTATISTICA DESCRITIVA VARIAVEL INDEPENDENTE - SEM QUADRADO
pander(summary(dados_oldmethodoly$jud))

### HISTOGRAMA VARIAVEL INDEPENDENTE - INDEPENDENCIA JUDCIAL
# INSTALANDO PACOTEs
install.packages("ISLR")

## HISTOGRAMA DA VARIAVEL INDEPENDENTE - INDEPENDENCIA JUDICIAL
# INstalando pacote
install.packages('lvplot')

#CARREGANDO PACOTES
library(ggplot2)
library(sjPlot)
library(MASS)
library(dplyr)
library(cowplot)
library(lvplot)

# HISTOGRAMA
library(MASS)
attach(dados_oldmethodoly)

#HISTOGRAMA DA VARIAVEL INDEPENDENCIA JUDICIAL
ggplot(dados_oldmethodoly, aes(x=inde_jud)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Independência Judicial") +
  ylab("Frequência")

### CORRELAÇÃO VD + VI - INDEPENDENCIA 
attach(dados_oldmethodoly)
install.packages("pander")
library(pander)
library(stargazer)
cor(per_corrup�ao,inde_jud)
pander(cor.test(per_corrup�ao, inde_jud), caption= "Correla��o Independ�ncia Judicial")

### GRAFICO VI + VD - INDEPENDENCIA JUD

ggplot(dados_oldmethodoly, aes(x = inde_jud, y = per_corrup�ao, colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Independ�ncia Judicial", y= "Percep��o da Corrup��o")+
  facet_wrap(~year)

### GRAFICO VI + VD QUADR�TICO
ggplot(dados_oldmethodoly, aes(x= inde_jud, y=per_corrup�ao)) + geom_point() +
  theme_sjplot() +
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= "Independ�ncia Judicial", y= "Percep��o da Corrup��o")

### GRAFICO VI + VD QUADR�TICO FACTOR (YEAR)


ggplot(dados_oldmethodoly[dados_oldmethodoly$year != 2012,], aes(x = inde_jud,
                                                                 y = per_corrup�ao)) + 
  geom_point(aes(colour = factor(year))) +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= 'Independ�ncia Judicial', y= 'Percep��o da Corrup��o')

### ESTAT�STICA DESCRITIVA VI + VD - ACCOUNTABILITY 

pander(summary(dados_oldmethodoly$v2juaccnt_osp))


### CORRELA�AO VD  + VI - ACCONTABILITY

library(pander)
library(stargazer)
cor(per_corrup�ao, v2juaccnt_osp)
pander(cor.test(per_corrup�ao, v2juaccnt_osp), 
       caption = "Correla��o Accountability Judicial")

## CARREGANDO PACOTES
library(ggplot2)
library(ISLR)
attach(dados_oldmethodoly)



### ESTATISTICA DESCRITIVA VI - ACCOUNTABILITY JUDICIAL

summary(dados_oldmethodoly$v2juaccnt_osp)

### GRAFICO DE DISPERSÃO VI + VD - ACCOUNTABILITY JUDICIAL
library(ggplot2)
library(sjPlot)
attach(dados_oldmethodoly)


ggplot(dados_oldmethodoly, aes(x = v2juaccnt_osp, y = per_corrup�ao,
                               colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percep��o de Corrup��o")

ggplot(dados_oldmethodoly, aes(x = v2juaccnt_osp, y = per_corrup�ao, colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percep��o da Corrup��o")+
  facet_wrap(~year)


### HISTOGRAMA ACCOUNTABILITY JUDICIAL 
  attach(dados_oldmethodoly)
ggplot(dados_oldmethodoly, aes(x=v2juaccnt_osp)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Accountability Judicial") +
  ylab("Frequ�ncia")

## ESTATISTICA DESCRITIVA VC - NIVEL DE DEMOCRACIA
library(readxl)
dados_oldmethodoly <- read_excel("C:/Users/Eliza/Desktop/dados_oldmethodoly.xlsx")
View(dados_oldmethodoly)
attach(dados_oldmethodoly)
summary(dados_oldmethodoly$democracia)



install.packages("lme4")
library(lme4)
attach(dados_oldmethodoly)


### MODELO DE REGRESS�O LINEAR SEM CONTROLES
regressao1 <- lm(per_corrup�ao ~ inde_jud + v2juaccnt_osp, data= dados_oldmethodoly)
summary(regressao1)
par(mfrow=c(2,2))
plot(regressao1)
attach(dados_oldmethodoly)

library(coefplot)
library(sjPlot)
coefplot(regressao1) +
theme_sjplot()


## CHAMANDO PROGRAMAS

library(ggplot2)
library(sjPlot)
library(pander)

# GERANDO GRAFICO ENTRE VD E VI NA REGRESS�O

ggplot(dados_oldmethodoly, aes(x= inde_jud, y=per_corrup�ao)) + geom_point() +
theme_sjplot() +
stat_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
geom_smooth(method = 'lm') +
labs(x= "Independ�ncia Judicial", y= "Percep��o da Corrup��o")



### REGRESS�O LINEAR 2 - INCLUINDO CONTROLES

## TRANSFORMANDO VARIAVEIS EM NUMERICAS

idhnovo <- as.numeric(dados_oldmethodoly$idh)

gininovo <- as.numeric(dados_oldmethodoly$gini)

pibnovo <- as.numeric(dados_oldmethodoly$pib) 

## RODANDO REGRESSAO 2
library(coefplot)
attach(dados_oldmethodoly)

regressao2 <- lm(per_corrup�ao ~ inde_jud + v2juaccnt_osp + democracia + pib + idhnovo + gininovo, 
                 data=dados_oldmethodoly)

summary(regressao2)
par(mfrow=c(2,2))
plot(regressao2)
coefplot(regressao2) +
theme_sjplot()



### REGRESSAO 3 - ESTIMANDO REGRESSAO LINEAR COM EFEITOS FIXOS POR ANO
install.packages('lmer')
library(lme4)
attach(dados_oldmethodoly)


### RODANDO REGRESSAO

regressao3 <- lmer(per_corrup�ao ~ inde_jud + v2juaccnt_osp + democracia + pib + 
              idhnovo + gininovo + (1 | year), data = dados_oldmethodoly)
summary(regressao3)
par(mfrow=c(2,2))
plot(regressao3)
coefplot(regressao3) +
  theme_sjplot()



## GERANDO GRAFICO DE REGRESSAO

#ATIVANDO PACOTES 
library(sjPlot)
library(jtools)
plot_model(regressao3, type = 'slope') +
theme_sjplot()


### GERANDO GRAFICO COEFLPLOT REGRESSAO 3
library(coefplot)
summary(regressao3)
par(mfrow=c(2,2))
plot(regressao3)
coefplot(regressao3) +
  theme_sjplot()

### REGRESSAO 4 - ESTIMANDO REGRESSAO DE PAINEL (SEM TERMO QUADRATICO)
# Instalando pacote necessario

install.packages('plm')

## ATIVANDO PACOTES NECESSARIOS
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)


### CRIANDO VARI�VEL ELEVADA AO QUADRADO - INDEPENDENCIA JUDICIAL


#### RODANDO A REGRESSAO
attach(dados_oldmethodoly)
library(lme4)
library(plm)

regressao4 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ inde_jud + v2juaccnt_osp +
                    democracia + log(pib) + idhnovo + gininovo + dep_lag, cluster = 'year',
                  model = 'within', index = 'year')
summary(regressao4)
regressao4                    


### GERANDO GRAFICO COEFPLOT

par(mfrow=c(2,2))
plot(regressao4)
coefplot(regressao4) +
  theme_sjplot()

### ATIVANDO PACOTES 
library(sjPlot)
library(jtools)

#### GRAFICO REGRESSAO PAINEL VARIAVEL DEFASADA SEM TERMO QUADRATICO

library(coefplot)
summary(regressao4)
par(mfrow=c(2,2))
plot(regressao4)
coefplot(regressao4) +
  theme_sjplot()


### REGRESSAO 5 - ESTIMANDO REGRESSAO DE PAINEL COM TERMO QUADRATICO E VD DEFASADA


## ATIVANDO PACOTES NECESSARIOS
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)

###DEFASANDO A VARIAVEL
dados_oldmethodoly$dep_lag <- Lag(dados_oldmethodoly$per_corrup�ao, -1)
summary(dados_oldmethodoly$dep_lag)


#### RODANDO A REGRESSAO COM TERMO QUADRATICO
attach(dados_oldmethodoly)
library(lme4)
library(plm)
regressao5 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ inde_jud^2 + v2juaccnt_osp + 
                    democracia
                  + pib + idhnovo + gininovo + dep_lag, 
                  cluster = 'year', model = 'within', index = 'year')
summary(regressao5)
regressao5
plot_model(regressao5, type="slope")

par(mfrow=c(1, 1))
library(ggplot2)
library(plogr)
library(car)
library(ggplot2)
library(pander)
library(lmtest)
library(car)
dim(regressao5)





#### GERANDO GRAFICO REGRESSAO 5 - 
library(coefplot)
summary(regressao5)
par(mfrow=c(2,2))
plot(regressao5)
coefplot(regressao5) +
  theme_sjplot()

### ELABORANDO MAPA PARA PERCEP�AO DA CORRUP�AO

## INSTALANDO PACOTES 
install.packages('tidyverse')
install.packages('plm')
installed.packages('gdata')
install.packages('lmer')
install.packages('Hmisc')
install.packages('mapproj')
install.packages('rgdal')
install.packages('maptools')
install.packages('ggmap')
install.packages('mapproj')
install.packages('ggplot2')
install.packages('viridis')
install.packages('psych')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('rgeos')
install.packages('dplyr')
install.packages('tidyr')
install.packages('tmap')
install.packages('gdata')
install.packages('rgdal')
## ATIVAR PACOTES
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(mapproj)
library(rgdal)
library(viridis)
library(psych)
library(maptools)
library(ggmap)
library(mapproj)
library(ggplot2)
library(readxl)
library(ggpubr)
library(rgdal)
## CHAMANDO A BASE

attach(dados_oldmethodoly)


# importando shapefile
shapefile_w <- readOGR("C:/Users/Eliza/Desktop/World_Countries.shp")

# convertendo o shapefile para dataframe
shape_df <- fortify(shapefile_w)

shape_data <- fortify(shapefile_w@data)

# criando variavel id para fazer o merge
shape_data$id <- row.names(shape_data)

# merge com os paises
shape_df <- full_join(shape_df, shape_data, by = 'id')

names(shape_df)

# criando variavel para fazer o merge com os dados do QOG
shape_df$country <- shape_df$COUNTRY

class(bd2$per_corrup)

# modificando estrutura da variavel percep�ao de corrup�ao para numerica
bd2$per_corrup�ao <- as.numeric(bd2$per_corrup�ao)

# eliminando NAs da base de dados 
bd3 <- bd2 %>% drop_na(per_corrup�ao)

class(bd3$per_ccorrup�ao)

# agregando dados de percep�ao o de corrup�ao por pais para todos os anos
bd4 <- aggregate(bd3 %>% select(per_corrup�ao), by = list(bd3$country), FUN = mean)

bd4$country <- bd4$Group.1

bd4$country <- as.character(bd4$country)

# renomeando estados unidos
bd4$country[bd4$country == 'United States of America'] <- 'United States'

# mergindo com os dados
shape_df2 <- left_join(shape_df, bd4, by = 'country')

names(bd3)

describeBy(bd2$per_corrup�ao, bd2$country)

            
map1 <-  ggplot() + 
  geom_polygon(data = shape_df2[!shape_df2$country %in% c('Antarctica', 'Greenland (Denmark)'),],
                                  aes(x = long, y = lat, group = group, 
                                      fill = per_corrupcao),
                                  colour = "grey", size = .1) +
theme_void(base_size = 11) +
scale_fill_gradientn(colours = heat.colors(2)) +
coord_map() +
labs(fill = 'Corruption Perception Index') +
theme(legend.position = "bottom",
plot.title = element_text(size = 12, face = 'bold', hjust = .5),
plot.caption = element_text(hjust = .5))
map1



#### REGRESSAO 10 EFEITO FIXO + POLINOMIAL
attach(dados_oldmethodoly)
install.packages('plm')
library(lme4)
library(coefplot)
library(plm)
library(sjPlot)

regressao10 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ inde_jud + jud +
                     v2juaccnt_osp + democracia + log(pibnovo) + idhnovo +
                     gininovo + dep_lag, cluster = c("country", "year"),
                   model = "within", index = c("country", "year"))
summary(regressao10)
coefplot(regressao10) +
theme_sjplot()

  

### REGRESSAO 11 EFEITO FIXO, SEM POLINOMIO
# CHAMANDO PACOTES
library(lme4)
library(plm)
attach(dados_oldmethodoly)
regressao11 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ jud + v2juaccnt_osp + democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = c("country", "year"), model = 'within', index = c("country", "year"))
summary(regressao11)
coefplot(regressao11)+
theme_sjplot()

### REGRESSAO 12 EFEITO ALEATORIO COM POLINOMIO 
attach(dados_oldmethodoly)
library(lme4)
library(plm)
library(sjPlot)
regressao12 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ inde_jud + jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = c("country", "year"), 
                   model = 'random', index = c("country", "year"))
summary(regressao12)
regressao12
coefplot(regressao12) +
theme_sjplot()

### TESTE DE HAUSMMANN - QUAL EFEITO APLICAR
library(pander)
phtest(regressao10, regressao12)
pander(phtest(regressao10, regressao12))

### REGRESS�O 13 - EFEITO ALEATORIO SEM POLINOMIO

library(lme4)
library(plm)
library(sjPlot)
regressao13 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = c("country", "year"), 
                   model = 'random', index = c("country", "year"))
summary(regressao13)
regressao13
coefplot(regressao13) +
  theme_sjplot()


### REGRESSAO 14 - BALANCEADO "POOLING"
regressao20 <- plm(data = dados_oldmethodoly, per_corrup�ao ~ inde_jud + jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = c("country", "year"), 
                   model = 'pooling', index = c("country", "year"))
summary(regressao20)
regressao20
library(sjPlot)
coefplot(regressao20)+
theme_sjplot()


### TESTE DE CHOW - QUAL EFEITO APLICAR
library(pander)
library(plm)
pooltest(regressao20,regressao10)
pFtest(regressao10,regressao20)
pander(pFtest(regressao10,regressao20))


### TESTE BREUSCH-PAGAN - EFEITOS ALEATORIOS E POOLED
library(plm)
lmtest::bptest(regressao10)
pander(lmtest::bptest(regressao10))
lmtest::bptest(regressao20)
pander(lmtest::bptest(regressao20))

### TESTE DE AUTOCORRELA��O DURBIN WATSON 
library(plm)
lmtest::dwtest(regressao10)

### TESTE DE AUTOCORRELA��O BREUSCH-GODFREY
install.packages('carData')
library(car)
lmtest::bgtest(regressao10)

