###########

library(stringr)
library(readxl)
library(dplyr)
library(tidyr)

# Carrega tabela com todas as despesas consolidadas gerada da POF
despesas<-readRDS("todas-despesas-domicilios.rds")

# Carrega tabela que traduz itens POF --> SCN
tradutor <- read_excel(dir()[grep(pattern = "Tradutor_POF", x = dir())],
                       sheet = 1 , skip = 1)

tradutor$'Produto POF' <- str_sub(tradutor$'Produto POF' , 1 , 5) 
names(tradutor)[1] <- "codigo"
tradutor <- tradutor[!duplicated(tradutor$codigo),]

gastos_SCN <- left_join(despesas_mensais, tradutor)

