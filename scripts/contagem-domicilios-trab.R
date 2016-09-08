# total de moradores por classe / divisão
library(survey)	
load("2009/poststr.rda")
options( survey.lonely.psu = "adjust" )
#carrega dados domicilios trabalhadores
domicilios_trabalhadores <- readRDS("RDS/t_dom_trab_control.rds")
#carrega poststr precisa?
#adiciona coluna 1


x <- merge( domicilios_trabalhadores , poststr )
x$one <- 1
domicilios_trabalhadores$one <- 1
# especifica o survey
sample.pof <- 
  svydesign(
    id = ~control , 
    strata = ~estrato_unico ,
    weights = ~fator_des ,
    data = x ,
    nest = TRUE
  )

pop.totals <- 
  data.frame(
    pos_estrato = unique( x$pos_estrato ) , 
    Freq = unique( x$tot_pop )
  )

pof.design.pos <-
  postStratify(
    sample.pof , 
    ~pos_estrato , 
    pop.totals
  )

#svytotal domicílios amostra
domiciliostrab_ntrab_amostra <-
  svyby( 
    ~ one, 
    ~trabalhador.cat , 
    pof.design.pos ,
    unwtd.count
  )

#svytotal pessoas
pessoastrab_ntrab <-
  svyby( 
    ~ one, 
    ~trabalhador.cat , 
    pof.design.pos ,
    svytotal
  )
