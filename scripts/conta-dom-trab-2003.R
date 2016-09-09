# total de moradores por classe / divisão
library(survey)	

options( survey.lonely.psu = "adjust" )

load( "2003/t_morador.rda" )
#carrega dados domicilios trabalhadores
domicilios_trabalhadores <- readRDS("RDS/t_dom_trab_control2003.rds")

t_morador <-
  transform(
    t_morador ,
    
    estrato_unico = uf*100 + estrato,
    # unique family code
    cod.uc = paste0( uf , seq , dv , domcl , uc ) 
  )
#adiciona coluna 1
t_dom_fator <- unique(t_morador[,c('cod.uc', 'fator_set')])
t_dom_fator <- merge(t_dom_fator,unique(t_morador[,c('cod.uc','estrato_unico')]), all.x = TRUE)

x <- merge( domicilios_trabalhadores , 
            t_dom_fator, all.x = TRUE )
x <- x[is.na(x$fator) == FALSE, ]
x$one <- 1

# especifica o survey
sample.pof <- 
  svydesign(
    id = ~control , 
#    strata = ~estrato_unico ,
    weights = ~fator_set ,
    data = x ,
    nest = TRUE
  )

# pop.totals <- 
#   data.frame(
#     pos_estrato = unique( x$pos_estrato ) , 
#     Freq = unique( x$tot_unidade_c )
#   )
# 
# pof.design.pos <-
#   postStratify(
#     sample.pof , 
#     ~pos_estrato , 
#     pop.totals
#   )

#svytotal domicílios amostra
domiciliostrab_ntrab_amostra <-
  svyby( 
    ~ one, 
    ~trabalhador.cat , 
    sample.pof ,
    unwtd.count
  )

#svytotal pessoas
tot_dom_trab_ntrab <-
  svyby( 
    ~ one, 
    ~trabalhador.cat , 
    sample.pof ,
    svytotal
  )
