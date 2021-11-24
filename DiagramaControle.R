###############################
## DIAGRAMA DE CONTROLE NO R ##
##  NATALIA PAIVA IESC-UFRJ  ##
###############################

############
### Exemplo
###########

# MALÁRIA - Casos confirmados por Mês 1º Sintoma(s) e Ano 1º Sintoma(s)
# Sistema de Informação de Agravos de Notificação  (SINAN)
# Local: Brasil (região extra-amazônica)
# Período: 2012 - 2020

# Download em 23/11/21

# Mude o diretorio e diga ao R qual pasta do pc estamos trabalhando
# Session >> Set working directory >> choose directory

# Instalar o pacotes que usaremos, se necessario
# Lembre de instalar apenas 1 vez no pc!

#install.packages("readxl") # para importar arquivo extensao xlsx
#install.packages("dplyr") # manipular base de dados usando %>%
#install.packages("ggplot2") # grafico - diagrama
#install.packages("RColorBrewer") # paleta de cores

# Vamos chamar os pacotes que usaremos
library(readxl)  # para importar arquivo extensao xlsx
library(dplyr)   # manipular base de dados usando %>%
library(ggplot2) # grafico - diagrama
library(RColorBrewer) # paleta de cores

# veja as cores do pacote em
display.brewer.all()

# cores basicas do R em http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

####
## IMPORTANDO BASE DE DADOS
####


# Leitura do arquivo xlsx do pc  
dados <-  read_excel("malaria_extra-amaz_2012-2020.xlsx", range = "A4:M13")


dados

####
## DIAGRAMA DE CONTROLE - intervalo: media -+ 1.96 * DP
####


# para o conjunto de dados com meses nas colunas e anos nas linhas:

# criando media, desvio padrao e limites p/ cada mes 
# Vamos considerar: LI = media - 1.96 * DP ; LS = media + 1.96 * DP

# Vamos usar dados exceto a coluna 1 ou seja dados[,-1]
# nao vamos considerar o ultimo ano para media e desvio , nesse caso 2020 ou seja dados[-nrow(dados), ]
## MARGIN = 1 vai aplicar função nas linhas ; MARGIN = 2 vai aplicar função nas  colunas
## FUN eh a funcao: summary, mean, sd...



(media <- apply(dados[-nrow(dados),-1], MARGIN = 2, FUN =  mean))
(DP <-  apply(dados[-nrow(dados),-1],  MARGIN =  2, FUN = sd))


# CRIANDO O DATA.FRAME DO DIAGRAMA

diagrama <- data.frame(meses = rep(1:12, 2),
                       tipo = rep(c("Média", "2020"), each = 12), 
                       Intervalo = rep("Intervalo", 24),
                       valor = c(media, t(dados[nrow(dados),-1])),
                       DP = rep(DP, 2))
diagrama

diagrama$meses <- as.factor(diagrama$meses)


# Diagrama de Controle: intervalo Media +- 2 DP

ggplot(data= diagrama, aes(x= meses, y= valor, group= tipo, color= tipo)) +
  geom_line(size = 0.75) + geom_point()+
  scale_color_brewer(palette= "Dark2")+ # paleta de cores - pode ser Set1, Set2, Dark1...
  geom_ribbon(data = diagrama[1:12,], 
              aes(ymin= ifelse((valor - 1.96*DP) < 0, 0, valor - 1.96*DP),
                  ymax= valor + 1.96*DP, fill = "Intervalo"),
              alpha=.2, outline.type = "both", linetype = "dashed", col = "gray40")+
  scale_fill_manual(name='', values=c("Intervalo" = "grey40"),
                    labels = c(paste("Média ", " 2DP", sep = "\u00B1"))) +
  ylim(0, max(diagrama$valor + 2.0*diagrama$DP)) +
  labs(title = "Diagrama de Controle",
       subtitle = "Malária - Região extra-amazônica",
       y= "Número de casos notificados",
       x = "Meses",
       color = "")+
  theme_minimal()
  

### Diagrama de Controle: Media -+ 2 DP e Media +- 3 DP


ggplot(data= diagrama, aes(x= meses, y= valor, group= tipo, color= tipo)) +
  geom_line(size = 0.75) + geom_point()+
  scale_color_brewer(palette= "Dark2")+ # paleta de cores - pode ser Set1, Set2, Dark1...
  geom_ribbon(data = diagrama[1:12,], 
              aes(ymin= ifelse((valor - 3*DP) < 0, 0, valor - 3*DP), # se menor que 0, recebe 0
                  ymax= valor + 3*DP, fill = "Média -+ 3DP"),
              alpha=.2, outline.type = "both", linetype = "dashed", col = "rosybrown1")+
  geom_ribbon(data = diagrama[1:12,], 
              aes(ymin= ifelse((valor - 2*DP) < 0, 0, valor - 2*DP),
                  ymax= valor + 2*DP, fill = "Média -+ 2DP"),
              alpha=.2, outline.type = "both", linetype = "dashed", col = "lightcoral")+
  ylim(0, max(diagrama$valor + 3.0*diagrama$DP)) +
  scale_fill_manual(name='Intervalos', values=c("Média -+ 2DP" = "lightcoral", # rosa mais escuro
                                      "Média -+ 3DP" = "rosybrown1"), # rosa mais claro
                    labels = c(paste("Média ", " 2DP", sep = "\u00B1"), # vamos colocar +- de forma elegante
                               paste("Média ", " 3DP", sep = "\u00B1"))) +
  labs(title = "Diagrama de Controle",
       subtitle = "Malária - Região extra-amazônica",
       y= "Número de casos notificados",
       x = "Meses",
       color = "")+
  theme_minimal()


#### Existem formas mais elegantes ou simples de fazer diagrama de controle no R? 
#### Não tenha dúvidas, mas está ai uma maneira...

#### Inclusive tem outras formas para definir os limites inferior e superior,
#### excluir valores min e max dos anos, olhar medias moveis....


