### Download do script que lê os dados no R  para POF 2018
## Download dos dados feito externamente


pastapof18 <- "2018"
linkpesquisa <- "ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/"
arqsleitura <- "Programas_de_Leitura_20200415.zip"
pastar <- "/Programas de Leitura/R"
arqlepof <- "scripts/leiturapof18.R"

f <- tempfile()
d <- tempdir()

download.file(paste0(linkpesquisa,arqsleitura),f)

unzip(f, exdir = d)

file.copy(from = list.files(paste0(d,pastar), full.names = T),to = arqlepof)

#Necessário pelo script
setwd("2018")

source(paste0("../",arqlepof))
