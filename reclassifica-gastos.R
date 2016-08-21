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
# Carrega tabela que POF com caderneta de gastos e codigos dos produtos na POF

load(file = "2009/t_caderneta_despesa_s.rda")
t_caderneta_despesa_s <- t_caderneta_despesa_s %>% 
  mutate(codigo = substr(paste0(prod_num_quadro_grupo_pro,cod_item), 1, 5),
         despmes = (valor_anual_expandido2/fator_expansao2)/12 ,
         cod.uc = paste0(cod_uf, num_seq, num_dv, cod_domc, num_uc))


despesas_mensais <- t_caderneta_despesa_s[,c('cod.uc','codigo','despmes')]

gastos_SCN <- left_join(despesas_mensais, tradutor2)
