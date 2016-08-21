###########

library(stringr)
library(readxl)
library(dplyr)

# Carrega tabela que traduz itens POF --> SCN
tradutor <- readxl::read_excel("Tradutor_POF2009_ContasNacionais.xls", skip = 1)
names(tradutor)[c(1,3)] <- c("codigo7", "codigo_SCN")

tradutor <- tradutor %>%
  mutate(codigo = substr(codigo7,1,5))

tradutor2 <- tradutor[!duplicated(tradutor$codigo),2:5]

# Carrega tabela com todas as despesas consolidadas gerada da POF
todas_despesas<-readRDS("todas-despesas-domicilios.rds")


despesas_mensais <- todas_despesas[,c('cod.uc','codigo','despmes')]

gastos_SCN <- left_join(despesas_mensais, tradutor2)
