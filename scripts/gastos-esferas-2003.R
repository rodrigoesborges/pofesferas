#calculate expenses of workers and non workers -- 
#needs data exported from script "Define-criterio..."

if ( .Platform$OS.type != 'windows' ) options( encoding = "latin1" )

library(survey)		# load survey package (analyzes complex design surveys)
library(reshape2)	# load reshape2 package (transposes data frames quickly)
library(dplyr)
library(readxl)
library(stringr)

options( survey.lonely.psu = "adjust" )

domicilios_trabalhadores <- readRDS("RDS/t_dom_trab_control2003.rds")

load( "2003/t_caderneta_despesa.rda" )

load( "2003/t_despesa_90dias.rda")

load("2003/t_despesa_12meses.rda")

load("2003/t_despesa_veiculo.rda")

load("2003/t_despesa.rda") 

load("2003/t_despesa_esp.rda")

# Carrega tabela que traduz itens POF --> SCN
tradutor <- read_excel(dir(recursive = TRUE)[grep(pattern = "Trad_POF",
                                                  x = dir(recursive = TRUE))],
                       sheet = 1 , skip = 1, col_types = rep("text", 4))

#acerta codificação para o restante do script
# options( encoding = "utf8" )	
# Tabela de componentes hierarquizada cod68 x cod 20 - dicionário de tradução agregado
# componentes <- read.csv("tradutores/cod68X20componentes-HIERARQ.csv", 
#                         colClasses = c("item68x20" = "character","cod68" = "character"), fileEncoding = "utf-8")

# Carrega tabela com códigos POF que não entram inicialmente como Consumo Final das Famílias
# pofnaoconsumo <- read.csv("tradutores/codigos_semtradutor.csv", stringsAsFactors = FALSE, colClasses = c("x" = "character"))




# Definimos função para recodificar, recalcular e selecionar apenas dados necessários para as próximas fases
recod.despesas <- function (tabela) {
                            # n.cod.qd = "num_quadro",
                            # n.cod.it = "cod_item") {
  tabela <- tabela %>% mutate(codigo = str_sub(paste0(quadro, item), 1, 5),
                              despmes =  val_def_anual / deflator / 12 ,
                              cod.uc = paste0( uf , seq , dv , domcl , uc ))
  
  enxutades <- tabela[ , c('cod.uc' , 'codigo' , 'despmes')]
  resultado <- aggregate(despmes ~ cod.uc + codigo,
                         enxutades,
                         sum)
  resultado
}

# teste
teste <- recod.despesas(t_despesa)
sum(teste$codigo %in% str_sub(tradutor$`Cod Pof`, 1, 5))
length(teste$codigo)
length(tradutor$`Cod Pof`)
##

despesas_mensais_col <- recod.despesas(t_caderneta_despesa , n.cod.qd = "prod_num_quadro_grupo_pro")

despesas_mensais_ind <- recod.despesas(t_despesa_individual)

despesas_90 <- recod.despesas(t_despesa_90dias)

despesas_veic <- recod.despesas(t_despesa_veiculo)

despesas_12m <- recod.despesas(t_despesa_12meses)

rm(list = ls(pattern = "t_de"))
rm(t_caderneta_despesa)
gc()

totais_despesas <- do.call(rbind , mget(ls(pattern = "despesas_")))

rm(list = ls(pattern = "despesas_"))
gc()

# algumas recodificações e enxugamento dos dicionários de tradução
tradutor$'Produto POF' <- str_sub(tradutor$'Produto POF' , 1 , 5) 
names(tradutor)[1] <- "codigo"
tradutor <- tradutor[!duplicated(tradutor$codigo),]

#adiciona código fictício para códigos POF que não são traduzidos como itens de consumo final
# Código "98000" como Imposto
# Código "99000" como FBKF
imposto <- grepl(pattern = "IMPOSTO|TAXA|LICENÇA", x = pofnaoconsumo$desc)

pofnaoconsumo$cod685[imposto] <- "98000"
pofnaoconsumo$cod685[!imposto] <- "99000"
pofnaoconsumo$scn[imposto] <- "IMPOSTOS"
pofnaoconsumo$scn[!imposto] <- "FBKF"

names(pofnaoconsumo) <- c("codigo","Descrição POF",
                          "Produto Contas Nacionais", "Descrição Contas Nacionais")


#adiciona códigos ao tradutor geral
tradutor <- rbind(tradutor,pofnaoconsumo, stringsAsFactors = FALSE)

#junta códigos ficticios para FBKX e Imposto na tabela componentes
fbkf.tax <- data.frame(c("9800","9900"),c("U","V"),c("Impostos","FBKF"),c("4","5"), stringsAsFactors = FALSE)
names(fbkf.tax) <- names(componentes)
componentes <- rbind(componentes, fbkf.tax)

#tradutor para nível hierarquizado compatível com nível 68 SCN e nível 20 (ISIC v4)
trad.agregado <- componentes
trad.agregado[trad.agregado==""] <- NA
trad.agregado <- trad.agregado[complete.cases(trad.agregado),c(1,4)]

#merge dos gastos
gastos_SCN <- left_join(totais_despesas, tradutor)

gastos_SCN <- gastos_SCN[complete.cases(gastos_SCN),]

gastos_SCN <- gastos_SCN %>% mutate(cod68 = substr(gastos_SCN$`Produto Contas Nacionais` , 1 , 4))

gastos_SCN <- left_join(gastos_SCN,trad.agregado)

# Se quiser exportar o objeto com "microdados por família e tipo de despesa compatível com SCN"
# saveRDS(gastos_SCN,"RDS/microdados_despesas.rds")


###################### Função que gera as estimativas para cada item - semi adaptado ------------
cesta_esferas <-
  function(
    curCode ,
    family.level.income = domicilios_trabalhadores ,
    gastos_SCN = gastos_SCN ,
    componentes = componentes ,
    poststr = poststr
  ){
    
    curCode.plus.subcodes <-
      componentes[substring(componentes$item68x20,1,nchar(curCode)) == curCode,'item68x20'] 
    
    family.expenditures.by.code <- 
      gastos_SCN[ gastos_SCN$item68x20 %in% curCode.plus.subcodes , c( 'item68x20' , 'despmes' , 'cod.uc' ) ]
    
    family.level.spending <-
      aggregate( 
        despmes ~ cod.uc , 
        family.expenditures.by.code , 
        sum 
      )
    
    y <- merge( family.level.income , family.level.spending , all.x = TRUE )
    
    y[ is.na( y$despmes ) , 'despmes' ] <- 0
    z <- 
      merge( 
        y , 
        poststr[ , c( 'control' , 'estrato_unico' , 'fator_des' , 'pos_estrato' , 'tot_unidade_c' ) ] 
      )
    
    stopifnot( nrow( z ) == nrow( y ) )
    
    sample.pof <-
      svydesign(
        id = ~control , 
        strata = ~estrato_unico , 
        weights = ~fator_des ,
        data = z , 
        nest = TRUE
      )
    
    uc.totals <- 
      data.frame(
        pos_estrato = unique( z$pos_estrato ) , 
        Freq = unique( z$tot_unidade_c )
      )
    
    pof.design <- 
      postStratify(
        sample.pof , 
        ~pos_estrato , 
        uc.totals
      )
    
    st <- svymean( ~despmes , pof.design )
    
    sb <- 
      svyby(
        ~despmes , 
        ~trabalhador.cat , 
        pof.design , 
        svymean
      )
    
    ot <-
      data.frame( 
        trabalhador.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( SE( st ) ) , 
        cv = as.numeric( cv( st ) )
      )
    
    ob <-
      data.frame( 
        trabalhador.cat = sb$trabalhador.cat , 
        mean = coef( sb ) , 
        se = as.numeric( SE( sb ) ) , 
        cv = as.numeric( cv( sb ) )
      )
    
    w <- rbind( ot , ob )
    
    w$item68x20 <- curCode
    
    reshape( 
      w , 
      idvar = 'item68x20' ,
      timevar = 'trabalhador.cat' ,
      direction = 'wide'
    )
  }


### Exemplos da Função em execução -----------
# cesta_esferas( 
# 	"3.5" , 
# 	domicilios_trabalhadores ,
# 	gastos_SCN , 
# 	componentes , 
# 	poststr 
# )
# 
# cesta_esferas( 
# 	"1" , 
# 	domicilios_trabalhadores ,
# 	gastos_SCN , 
# 	componentes , 
# 	poststr 
# )

#### Gera a tabela final efetiva --------
tabela <- data.frame( num.despesa = NULL )
# Ver Itens SCN sem registro de Gasto
tabelar <- componentes[!(componentes$item68x20 %in% gastos_SCN$item68x20),]


for ( i in seq( nrow( componentes ) ) ){
  
  if ( !(componentes[i , 'item68x20'] %in% tabelar$item68x20)) {
    if ( !( componentes[ i , 'item68x20' ] %in% tabela$tipo.de.despesa) ){
      
      tabela[ nrow( tabela ) + 1 , 'num.despesa' ] <- 
        componentes[ i , 'item68x20' ]
      
      tabela[ nrow( tabela ) , 'setor' ] <- 
        componentes[ i , 'descrição' ]
      
    }
  }
}



for ( i in seq( nrow( tabela ) ) ){
  
  print( tabela[ i , 'num.despesa' ] )
  
  curRow <- 
    cesta_esferas( 
      tabela[ i , 'num.despesa' ] , 
      domicilios_trabalhadores ,
      gastos_SCN , 
      componentes , 
      poststr 
    )
  
  if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
  
}

res_cesta_esferas <- merge( componentes , allRows, all.x = TRUE )

#saveRDS(res_cesta_esferas[grepl(x=colnames(res_cesta_esferas), pattern="mean|cod|item|des")],"RDS/cesta_esferas.rds")
