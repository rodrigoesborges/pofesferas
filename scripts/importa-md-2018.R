## Conversão para RDA dos microdados da POF 2017-2018

lugar <- "2018"
arqmd <- "Dados_20200403.zip"
### Descompacta os arquivos

unzip(paste0(lugar,"/",arqmd), exdir= lugar)
arqsconv <- list.files(lugar, pattern = "*.txt")

###Define função de carregar txt e salvar com mesmo nome
transfrda <- function(a) {
  b <- read.delim(paste0(lugar,"/",a), sep = "", header = F)
  save(b, file= paste0(lugar,"/",gsub(".txt","",a),".rda"))
}

lapply(arqsconv, transfrda)





