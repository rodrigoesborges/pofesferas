#calculate expenses of workers and non workers -- 
#needs data exported from script "Define-criterio..."
options( encoding = "latin1" )		# # only macintosh and *nix users need this line

if ( .Platform$OS.type != 'windows' ) print( 'non-windows users: read this block' )

library(survey)		# load survey package (analyzes complex design surveys)
library(reshape2)	# load reshape2 package (transposes data frames quickly)
library(dplyr)
options( survey.lonely.psu = "adjust" )
load("t_dom_trab_control.rda")

load("2009/poststr.rda")

load( "2009/t_caderneta_despesa_s.rda" )

load("2009/t_despesa_individual_s.rda")

load( "2009/t_despesa_90dias_s.rda")

load("2009/t_despesa_12meses_s.rda")

load("2009/t_despesa_veiculo_s.rda")


load( "2009/codigos de alimentacao.rda" )

componentes <- componentes[ componentes$nivel.1 == 1 , ]

# Definimos função para recodificar, recalcular e selecionar apenas dados necessários para as próximas fases
recod.despesas <- function (tabela = t_despesa_individual_s,
                            n.cod.qd = "num_quadro",
                            n.cod.it = "cod_item") 
                            { tabela <- transform(tabela,
                                                  codigo = substr(paste0(eval(parse(text = n.cod.qd)),eval(parse(text = n.cod.it))) , 1 , 5) ,
                                                  despmes = ( valor_anual_expandido2 / fator_expansao2 / 12) ,
                                                  cod.uc = paste0(cod_uf , num_seq , num_dv , cod_domc , num_uc )
                                                  )
                           enxutades<-tabela[,c('cod.uc' , 'codigo' , 'despmes')]
                            aggregate(despmes ~ cod.uc + codigo,
                                      enxutades,
                                      sum)
  }
  
despesas_mensais_col <- recod.despesas(t_caderneta_despesa_s , n.cod.qd = "prod_num_quadro_grupo_pro")

despesas_mensais_ind <- recod.despesas(t_despesa_individual_s)

despesas_90 <- recod.despesas(t_despesa_90dias_s)

despesas_veic <- recod.despesas(t_despesa_veiculo_s)

despesas_12m <- recod.despesas(t_despesa_12meses_s)

rm(list = ls(pattern = "t_de"))
rm(t_caderneta_despesas_s)
gc()

totais_despesas <- do.call(rbind , mget(ls(pattern = "despesas_")))


rm(list = ls(pattern = "despesas_"))
gc()

###################### A ser adaptado ------------
cesta_esferas <-
	function(
		curCode ,
		family.level.income = domicilios_trabalhadores ,
		totais_despesas = totais_despesas ,
		componentes = componentes ,
		poststr = poststr
	){

		curCode.plus.subcodes <-
			componentes[ apply( componentes == curCode , 1 , any ) , 'codigo' ]

		family.expenditures.by.code <- 
			totais_despesas[ totais_despesas$codigo %in% curCode.plus.subcodes , c( 'codigo' , 'despmes' , 'cod.uc' ) ]

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
		
		w$top.codigo <- curCode
		
		reshape( 
			w , 
			idvar = 'top.codigo' ,
			timevar = 'trabalhador.cat' ,
			direction = 'wide'
		)
	}

	

cesta_esferas( 
	"1.5" , 
	domicilios_trabalhadores ,
	totais_despesas , 
	componentes , 
	poststr 
)
cesta_esferas( 
	"1.6" , 
	domicilios_trabalhadores ,
	totais_despesas , 
	componentes , 
	poststr 
)

tabela <- data.frame( tipo.de.despesa = NULL )

for ( i in seq( nrow( componentes ) ) ){

	for ( j in 1:3 ){
		
		if ( !( componentes[ i , paste0( 'desc.' , j ) ] %in% tabela$tipo.de.despesa ) ){
		
			tabela[ nrow( tabela ) + 1 , 'tipo.de.despesa' ] <- 
				componentes[ i , paste0( 'desc.' , j ) ]
				
			tabela[ nrow( tabela ) , 'top.codigo' ] <- 
				componentes[ i , paste0( 'nivel.' , j ) ]
		
		}
	}
}

tabela <- tabela[ tabela$tipo.de.despesa != "" , ]

head( tabela )

tail( tabela )
for ( i in seq( nrow( tabela ) ) ){
	
	print( tabela[ i , 'top.codigo' ] )
	
	curRow <- 
		cesta_esferas( 
			tabela[ i , 'top.codigo' ] , 
			domicilios_trabalhadores ,
			totais_despesas , 
			componentes , 
			poststr 
		)
		
	if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
	
}

res_cesta_esferas <- merge( tabela , allRows )

res_cesta_esferas

