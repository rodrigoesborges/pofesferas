library(stringr)
library(readxl)
library(dplyr)
library(tidyr)

# Carrega tabela que traduz itens POF --> SCN
tradutor <- readxl::read_excel("2009/Tradutor_POF2009_ContasNacionais.xls", skip = 1)
names(tradutor)[c(1,3)] <- c("codigo", "codigo_SCN")
tradutor <- tradutor[ , c(1,3,4)]
tradutor$codigo <- str_sub(tradutor$codigo, 1, 5)
reg <- nrow(tradutor)
tradutor <- unique(tradutor)
nrow(tradutor) - reg


# Carrega tabela com todas despesas da POF e codigos dos produtos na POF
despesas <- readRDS("2009/todas-despesas-domicilios.rds")

linhas_antes <- nrow(despesas)

despesas <- inner_join(despesas, tradutor, by = "codigo")

linhas_antes - nrow(despesas) # perdemos 182 mil registro referentes aos 250 codigos sem tradução

saveRDS(despesas, "2009/despesas_SCN.rds")