### DISSERTaﾇAO MESTRADO ELIZA

## TEMAS BRANCOS
install.packages("sjPlot")
install.packages("ggplot")
theme_set(theme_sjplot())

## INSTALANDO PACOTES
install.packages("readxl")
install.packages("ggplot2")

## ABRINDO BANCO DE DADOS

library(readxl)
dados_oldmethodoly <- read_excel("MESTRADO CIÊNCIA POLITICA/Dissertação/dados_oldmethodoly.xlsx")
View(dados_oldmethodoly)


## CHAMANDO A BASE DE DADOS

attach(dados_oldmethodoly)

### CRIANDO VARIﾁVEL ELEVADA AO QUADRADO - INDEPENDENCIA JUDICIAL

dados_oldmethodoly$inde_jud <- dados_oldmethodoly$inde_jud^2


### CRIANDO VARIﾁVEL NORMAL - INDEPENDENCIA JUDICIAL
dados_oldmethodoly$jud<-sqrt(dados_oldmethodoly$inde_jud)

### TRANSFORMANDO VARIﾁVEIS EM NUMERICAS

idhnovo <- as.numeric(dados_oldmethodoly$idh)

gininovo <- as.numeric(dados_oldmethodoly$gini)

pibnovo <- as.numeric(dados_oldmethodoly$pib) 


###DEFASANDO A VARIAVEL DEPENDENTE 
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)

dados_oldmethodoly$dep_lag <- Lag(dados_oldmethodoly$per_corrupçao, -1)
summary(dados_oldmethodoly$dep_lag)


# ESTATISTICA DESCRITIVA VARIAVEL DEPENDENTE - PERCEPﾃ②O DE CORRUPﾃ②O

summary(dados_oldmethodoly$per_corrup軋o)

# GRAFICO DE BARRA DA VARIAVEL DEPENDENTE - PERCEPﾇAO DE CORRUPﾇAO
library(ggplot2)
library(sjPlot)
attach(dados_oldmethodoly)

ggplot(dados_oldmethodoly, aes(x=per_corrup軋o)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Percep鈬o da Corrup鈬o") +
  ylab("Frequ麩cia")

# HISTOGRAMA VD


attach(dados_oldmethodoly)
hist(dados_oldmethodoly$per_corrup軋o)


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
  xlab("Independﾃｪncia Judicial") +
  ylab("Frequﾃｪncia")

### CORRELAﾃ�グ VD + VI - INDEPENDENCIA 
attach(dados_oldmethodoly)
install.packages("pander")
library(pander)
library(stargazer)
cor(per_corrup軋o,inde_jud)
pander(cor.test(per_corrup軋o, inde_jud), caption= "Correla鈬o Independ麩cia Judicial")

### GRAFICO VI + VD - INDEPENDENCIA JUD

ggplot(dados_oldmethodoly, aes(x = inde_jud, y = per_corrup軋o, colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Independ麩cia Judicial", y= "Percep鈬o da Corrup鈬o")+
  facet_wrap(~year)

### GRAFICO VI + VD QUADRﾁTICO
ggplot(dados_oldmethodoly, aes(x= inde_jud, y=per_corrup軋o)) + geom_point() +
  theme_sjplot() +
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= "Independ麩cia Judicial", y= "Percep鈬o da Corrup鈬o")

### GRAFICO VI + VD QUADRﾁTICO FACTOR (YEAR)


ggplot(dados_oldmethodoly[dados_oldmethodoly$year != 2012,], aes(x = inde_jud,
                                                                 y = per_corrup軋o)) + 
  geom_point(aes(colour = factor(year))) +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= 'Independ麩cia Judicial', y= 'Percep鈬o da Corrup鈬o')

### ESTATﾍSTICA DESCRITIVA VI + VD - ACCOUNTABILITY 

pander(summary(dados_oldmethodoly$v2juaccnt_osp))


### CORRELAﾇAO VD  + VI - ACCONTABILITY

library(pander)
library(stargazer)
cor(per_corrup軋o, v2juaccnt_osp)
pander(cor.test(per_corrup軋o, v2juaccnt_osp), 
       caption = "Correla鈬o Accountability Judicial")

## CARREGANDO PACOTES
library(ggplot2)
library(ISLR)
attach(dados_oldmethodoly)

### CORRELAÇÃO VI'S = ACC + IND
library(pander)
pander(cor.test(v2juaccnt_osp, inde_jud), 
       caption = "Correlação entre Variáveis Indpendentes")

### ESTATISTICA DESCRITIVA VI - ACCOUNTABILITY JUDICIAL

summary(dados_oldmethodoly$v2juaccnt_osp)

### GRAFICO DE DISPERSﾃグ VI + VD - ACCOUNTABILITY JUDICIAL
library(ggplot2)
library(sjPlot)
attach(dados_oldmethodoly)


ggplot(dados_oldmethodoly, aes(x = v2juaccnt_osp, y = per_corrup軋o,
                               colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percep鈬o de Corrup鈬o")

ggplot(dados_oldmethodoly, aes(x = v2juaccnt_osp, y = per_corrup軋o, colour = factor(year))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percep鈬o da Corrup鈬o")+
  facet_wrap(~year)


### HISTOGRAMA ACCOUNTABILITY JUDICIAL 
attach(dados_oldmethodoly)
ggplot(dados_oldmethodoly, aes(x=v2juaccnt_osp)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Accountability Judicial") +
  ylab("Frequ麩cia")

## ESTATISTICA DESCRITIVA VC - NIVEL DE DEMOCRACIA
library(readxl)
dados_oldmethodoly <- read_excel("C:/Users/Eliza/Desktop/dados_oldmethodoly.xlsx")
View(dados_oldmethodoly)
attach(dados_oldmethodoly)
summary(dados_oldmethodoly$democracia)



install.packages("lme4")
library(lme4)
attach(dados_oldmethodoly)


### MODELO DE REGRESSﾃO LINEAR SEM CONTROLES
regressao1 <- lm(per_corrup軋o ~ inde_jud + v2juaccnt_osp, data= dados_oldmethodoly)
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

# GERANDO GRAFICO ENTRE VD E VI NA REGRESSﾃO

ggplot(dados_oldmethodoly, aes(x= inde_jud, y=per_corrup軋o)) + geom_point() +
  theme_sjplot() +
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= "Independ麩cia Judicial", y= "Percep鈬o da Corrup鈬o")



### REGRESSﾃO LINEAR 2 - INCLUINDO CONTROLES

## TRANSFORMANDO VARIAVEIS EM NUMERICAS

idhnovo <- as.numeric(dados_oldmethodoly$idh)

gininovo <- as.numeric(dados_oldmethodoly$gini)

pibnovo <- as.numeric(dados_oldmethodoly$pib) 

## RODANDO REGRESSAO 2
library(coefplot)
attach(dados_oldmethodoly)

regressao2 <- lm(per_corrup軋o ~ inde_jud + v2juaccnt_osp + democracia + pib + idhnovo + gininovo, 
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

regressao3 <- lmer(per_corrup軋o ~ inde_jud + v2juaccnt_osp + democracia + pib + 
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


### CRIANDO VARIﾁVEL ELEVADA AO QUADRADO - INDEPENDENCIA JUDICIAL


#### RODANDO A REGRESSAO
attach(dados_oldmethodoly)
library(lme4)
library(plm)

regressao4 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ inde_jud + v2juaccnt_osp +
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
dados_oldmethodoly$dep_lag <- Lag(dados_oldmethodoly$per_corrup軋o, -1)
summary(dados_oldmethodoly$dep_lag)


#### RODANDO A REGRESSAO COM TERMO QUADRATICO
attach(dados_oldmethodoly)
library(lme4)
library(plm)
regressao5 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ inde_jud^2 + v2juaccnt_osp + 
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

### ELABORANDO MAPA PARA PERCEPﾇAO DA CORRUPﾇAO

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

# modificando estrutura da variavel percep軋o de corrup軋o para numerica
bd2$per_corrup軋o <- as.numeric(bd2$per_corrup軋o)

# eliminando NAs da base de dados 
bd3 <- bd2 %>% drop_na(per_corrup軋o)

class(bd3$per_ccorrup軋o)

# agregando dados de percep軋o o de corrup軋o por pais para todos os anos
bd4 <- aggregate(bd3 %>% select(per_corrup軋o), by = list(bd3$country), FUN = mean)

bd4$country <- bd4$Group.1

bd4$country <- as.character(bd4$country)

# renomeando estados unidos
bd4$country[bd4$country == 'United States of America'] <- 'United States'

# mergindo com os dados
shape_df2 <- left_join(shape_df, bd4, by = 'country')

names(bd3)

describeBy(bd2$per_corrup軋o, bd2$country)


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



#### REGRESSAO 1 EFEITO FIXO 
attach(dados_oldmethodoly)
install.packages('plm')
library(lme4)
library(coefplot)
library(plm)
library(sjPlot)

regressao10 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ inde_jud + jud +
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
regressao11 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ jud + v2juaccnt_osp + democracia
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
regressao12 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ inde_jud + jud + v2juaccnt_osp + 
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


### REGRESSﾃO 13 - EFEITO ALEATORIO SEM POLINOMIO

library(lme4)
library(plm)
library(sjPlot)
regressao13 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = c("country", "year"), 
                   model = 'random', index = c("country", "year"))
summary(regressao13)
regressao13
coefplot(regressao13) +
  theme_sjplot()


### REGRESSAO 14 - BALANCEADO "POOLING"
regressao20 <- plm(data = dados_oldmethodoly, per_corrup軋o ~ inde_jud + jud + v2juaccnt_osp + 
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

### TESTE DE AUTOCORRELAﾇﾃO DURBIN WATSON 
library(plm)
lmtest::dwtest(regressao10)

### TESTE DE AUTOCORRELAﾇﾃO BREUSCH-GODFREY
install.packages('carData')
library(car)
lmtest::bgtest(regressao10)



#### REGRESSAO 1 EFEITOS FIXOS POR PAÍS + INDEPENDENCIA (normal) + ao quadrado
library(plm)
regressao1<- plm(data = dados_oldmethodoly, per_corrupçao ~ inde_jud
           + jud + democracia + log(pibnovo) + 
             idhnovo + gininovo + dep_lag, cluster = 'country', 
           model = 'within', index = c('country', 'year'))
summary(regressao1)
library(coefplot)
library(sjPlot)
coefplot(regressao1) +
theme_sjplot()

### REGRESSAO 2 EFEITOS FIXO POR PAIS + INDEPENDENCIA AO QUADRADO
library(plm)

regressao2 <- plm(data = dados_oldmethodoly, per_corrupçao ~ inde_jud +
                    democracia + log(pibnovo) + 
                 idhnovo + gininovo + dep_lag,
                 cluster = 'country', 
               model = 'within', index = c('country', 'year'))
summary(regressao2)
coefplot(regressao2)+
theme_sjplot()

### REGRESSAO 3 EFEITOS FIXOS POR PAIS + ACCOUNTABILITY 
library(plm)
regressao3 <- plm(data = dados_oldmethodoly, per_corrupçao ~ v2juaccnt_osp +
                    democracia + log(pibnovo) + 
                    idhnovo + gininovo + dep_lag,
                  cluster = 'country', 
                  model = 'within', index = c('country', 'year'))
summary(regressao3)
coefplot(regressao3) +
theme_sjplot()

### REGRESSAO 4 EFEITOS FIXOS POR PAIS + INDEPE E ACCOUNTABILITY

regressao4 <- plm(data = dados_oldmethodoly, per_corrupçao ~ inde_jud + jud
                  + v2juaccnt_osp +
                    democracia + log(pibnovo) + 
                    idhnovo + gininovo + dep_lag,
                  cluster = 'country', 
                  model = 'within', index = c('country', 'year'))
summary(regressao4)
coefplot(regressao4)+
theme_sjplot()


### REGRESSÃO DE PAINEL COM EFEITOS ALEATÓRIOS

library(lme4)
library(plm)
library(sjPlot)
regressao5 <- plm(data = dados_oldmethodoly, per_corrupçao ~ inde_jud + 
                     jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = 'country', 
                   model = 'random', index = c("country", "year"))
summary(regressao5)
coefplot(regressao5) +
  theme_sjplot()

### REGRESSÃO DE PAINEL COM EFEITOS ALEATORIOS SEM TERMO QUADRÁTICO

regressao6 <- plm(data = dados_oldmethodoly, per_corrupçao ~ jud + v2juaccnt_osp + 
                    democracia
                  + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                  cluster = 'country', 
                  model = 'random', index = c("country", "year"))
summary(regressao6)
coefplot(regressao5) +
  theme_sjplot()

#### TESTE DE HAUSMAN - EFEITO FIXO + ALEATORIO
library(pander)
phtest(regressao4, regressao5)
pander(phtest(regressao4, regressao5))


#### REGRESSAO DE PAINEL - MODELO POOLED

regressao7 <- plm(data = dados_oldmethodoly, per_corrupçao ~ inde_jud + jud +
                     v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = 'country', 
                   model = 'pooling', index = c("country", "year"))
summary(regressao7)
library(sjPlot)
coefplot(regressao7)+
  theme_sjplot()


### TESTE BREUSCH-PAGAN - EFEITOS ALEATORIOS E POOLED
library(plm)
lmtest::bptest(regressao4)
pander(lmtest::bptest(regressao4))
lmtest::bptest(regressao6)
pander(lmtest::bptest(regressao6))
pander(lmtest::bptest(regressao5))
pander(lmtest::bptest(regressao7))


### TESTE DE AUTOCORRELACAO BREUSCH-GODFREY
install.packages('carData')
library(car)
lmtest::bgtest(regressao4)
pander(lmtest::bgtest(regressao4))

### TESTE DE CHOW - QUAL EFEITO APLICAR
library(pander)
library(plm)
pooltest(regressao4,regressao7)
pFtest(regressao4,regressao7)
pander(pFtest(regressao4,regressao7))
