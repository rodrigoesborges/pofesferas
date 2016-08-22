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

# Carrega tabela que traduz itens SCN --> itens ISIC 4 - nível 20 setores - deprecado
#trad.agregado <- read_excel("Atividade -contas de divulgacao x Cnae 2.0 - Resumo.xls" ,
#           sheet = 1,
#           skip = 3)[68]

# Tabela de componentes hierarquizada cod68 x cod 20
componentes <- read.csv("cod68X20componentes-HIERARQ.csv", 
                          colClasses = c("item68x20" = "character","cod68" = "character"))


# algumas recodificações e enxugamento dos dicionários de tradução
tradutor$'Produto POF' <- str_sub(tradutor$'Produto POF' , 1 , 5) 
names(tradutor)[1] <- "codigo"
tradutor <- tradutor[!duplicated(tradutor$codigo),]

trad.agregado[componentes==""] <- NA
trad.agregado <- trad.agregado[complete.cases(trad.agregado),c(1,4)]

#merge dos gastos
gastos_SCN <- left_join(despesas_mensais, tradutor)

gastos_SCN <- gastos_SCN[complete.cases(gastos_SCN),]

gastos_SCN <- gastos_SCN %>% mutate(cod68 = substr(gastos_SCN$`Produto Contas Nacionais` , 1 , 4))

gastos_SCN <- left_join(gastos_SCN,trad.agregado)
