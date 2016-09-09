# Script to Generate, from Brazil's POF 2008-2003, Average Income by Type 
# aggregated by class position, not income ranges
options( encoding = "latin1" )		# # only macintosh and *nix users need this line
options( encoding = "utf-8" )
# if ( .Platform$OS.type != 'windows' ) print( 'non-windows users: read this block' )


library(survey)		# load survey package (analyzes complex design surveys)
library(reshape2)	# load reshape2 package (transposes data frames quickly)
library(stringr)  # pad 0s to uniformize code padding
library(tidyr)
library(dplyr)
library(ggplot2)

options( survey.lonely.psu = "adjust" )

load("2003/t_morador.rda")

#load("2009/poststr.rda")

load( "2003/t_domicilio.rda" )

load( "2003/t_rendimentos.rda" )

load("2003/t_outros_reci.rda")

# use old translator for generating same table as 2008-09 script
tabelagregada <- read.csv2("tradutores/codigos-recodificacao-rendimentos.csv", encoding = "utf-8",
                          stringsAsFactors = FALSE)
names(tabelagregada) <- c("cod.novo","tipoderendimento","cod.rec")
# Do some recodes
t_domicilio <- t_domicilio %>% mutate(estrato_unico = uf*100 + estrato)

###############################
# trocado por tradutor de 2003 #
###############################

incomeRecodesX <- read.csv("tradutores/tradutor-detalhado2003-reag.csv",encoding = "UTF-8",
                           stringsAsFactors = FALSE)
# 
# incomeRecodes$cod.inc <-gsub(".*\\+.*","50000",incomeRecodes$cod.inc)
# incomeRecodesX <- data.frame()
# for (i in 1:nrow(incomeRecodes)) {
#   incomeRecodesX <- rbind(incomeRecodesX,
#                           cbind(incomeRecodes[i,1],
#                                 eval(parse(text = paste("c(" ,
#                                                         incomeRecodes[i,3],")")))),
#                                 stringsAsFactors = FALSE)}
# names(incomeRecodesX) <- c("cod.novo","cod.rec")

# incomeRecodesX$cod.rec <- as.numeric(incomeRecodesX$cod.rec)


t_rendimentos <- transform(t_rendimentos ,
    
    # monthly income - should be equal to rendimento corrigido
    recmes = rend_def_anual / deflator / 12 ,
    
    # unique family code
    cod.uc = paste0( uf , seq , dv , domcl , uc ),
    
    # unique income type code
    cod.rec = quadro * 1000 + pos_ocup
    
    # input recodes of income type according to incomeRecodes table -- done separately
  )


t_rendimentos_recoded <- merge (t_rendimentos, incomeRecodesX)
t_rendimentos_recoded <- t_rendimentos_recoded[ , c('cod.rec' , 'cod.uc', 'recmes' , 'fator_set' , 'fator','cod.novo' ) ]


t_outros_reci <-
  transform(
    t_outros_reci ,
    
    # monthly income
    recmes = rend_def_anual / deflator / 12 ,
    
    # unique family code
    cod.uc = paste0( uf , seq , dv , domcl , uc ),
    
    # unique income type code
    cod.rec = quadro*1000+ floor(item/100)
    # cod.rec = 99000
  )

t_outros_reci_recoded <- merge (t_outros_reci,incomeRecodesX)


t_outros_reci_recoded <- t_outros_reci_recoded[ , 
                                                c('cod.rec' ,
                                                  'cod.uc',
                                                  'recmes' ,
                                                  'fator_set' ,
                                                  'fator',
                                                  'cod.novo' ) ]

allincomes <- rbind(t_rendimentos_recoded,t_outros_reci_recoded)

t_morador <-
  transform(
    t_morador ,
    
    # unique family code
    cod.uc = paste0( uf , seq , dv , domcl , uc ) 
  )

domicilio.rendas <- t_morador[ , c( 'cod.uc' , 'renda' ) ]

nrow( domicilio.rendas )

domicilio.rendas <- unique( domicilio.rendas )

nrow( domicilio.rendas )

componentes <- incomeRecodesX
names(componentes) <- c("cod.novo","tipoderendimento","cod.rec")

# Now we generate the vector that will be used as base for our aggregation criteria
# First we take (in fixed form) all items / subcodes
todos.subcodigos.tiporenda <-
  componentes[substring(componentes$cod.novo,1,nchar(1)) == 1,'cod.novo']     

# next we aggregate by family/household the recoded income data
domicilios.porcodigo <- 
  allincomes[ allincomes$cod.novo %in% todos.subcodigos.tiporenda, c( 'cod.novo' , 'recmes' , 'cod.uc' ) ]

domicilios.porcodigo.agregados <- 
  aggregate( 
    recmes ~ cod.uc + cod.novo , 
    domicilios.porcodigo , 
    sum 
  )

# We've got all we need to actually generate the vector, proceed to it
renda_m_total <- domicilios.porcodigo.agregados %>% group_by(cod.uc) %>%
  summarise(cod.novo = "1",
            recmes = sum(recmes, na.rm = TRUE))

renda_m_total <- rbind(domicilios.porcodigo.agregados, renda_m_total)
renda_m_total <- spread(data = renda_m_total, key = cod.novo, value = recmes)

nomes <- names(renda_m_total)

subst_na <-  function(x) {
  x[is.na(x)] <- 0
  x
}

renda_m_total <- data.frame(lapply(renda_m_total, subst_na),
                            stringsAsFactors = FALSE)

names(renda_m_total) <- nomes
renda_m_total$cod.uc <- as.character(renda_m_total$cod.uc)

renda_m <- renda_m_total %>%
              mutate(renda_trabalho = ((`1.1.1` + `1.1.3` +
                                          `1.2.6` + `1.2.4` + 0.01 )/(`1`+ 0.01))*100)

domicilios_trabalhadores <- merge(domicilio.rendas, 
                                  renda_m[, c("cod.uc", "renda_trabalho")],
                                  all.x = TRUE)

# Aqui os NAs com baixa renda foram definidos como trabalhadores
domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total <= 2000, "renda_trabalho"] <- 100

domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total > 2000, "renda_trabalho"] <- 0


domicilios_trabalhadores <- 
  transform(
    domicilios_trabalhadores , 
    
    # create income categories
    trabalhador.cat = 
      cut(
        renda_trabalho , 
        c( 0 , 80, Inf ) ,
        include.lowest = TRUE , 
        labels = c( "Não Trabalhador" , "Trabalhador" )
      ) ,
    
    # create a control variable that matches 
    # the one in the `poststr` table
    control = substr( cod.uc , 1 , 6 )
  )

rm(t_morador)
rm(t_outros_reci)
rm(t_rendimentos)

#save household table with new income aggregation criteria
saveRDS(domicilios_trabalhadores, file = "RDS/t_dom_trab_control2003.rds")
# Big Function that does most of the work
gc()

tabela_2.1.1 <-
  function(
    # choose an income code
    incomeCode ,
    # specify the family-level data.frame with the income variable
    domicilios_trabalhadores = domicilios_trabalhadores ,
    # specify the income booklet data,
    # which must contain the variables
    # created above
    allincomes = allincomes ,
    # identify the components table to use
    componentes = componentes
    #,
    # identify the table to use for post-stratification
#    poststr = poststr
  ){
    
    # isolate all records containing the current code *anywhere*
    incomeCode.plus.subcodes <-
      componentes[substring(componentes$cod.novo,1,nchar(incomeCode)) == incomeCode,'cod.novo']      
    # old non-functioning  componentes[ apply( componentes == incomeCode , 1 , any ) , 'cod.novo' ]
    
    # isolate family-wide incomes to only matching codes
    family.incomes.by.code <- allincomes[allincomes$cod.novo %in% incomeCode.plus.subcodes ,
                                         c( 'cod.novo' , 'recmes' , 'cod.uc' , 'fator') ]
    if(nrow(family.incomes.by.code) == 0) {
      family.incomes.by.code <- data.frame(cod.novo = incomeCode ,
                                           recmes = 0,
                                           cod.uc = 0,
                                           fator = 0)
    }
    
    
    # aggregate incomes to the one-record-per-family-level
    family.level.income.aggregated <- aggregate(recmes ~ cod.uc , 
                                                family.incomes.by.code , sum )
    
    # merge the income and familiar income tables,
    # assuming that the income table has no missings
    y <- merge( domicilios_trabalhadores , family.level.income.aggregated , all.x = TRUE )
    
    # all missing values from the left-join above
    # should be converted to zeroes
    y[ is.na( y$recmes ) , 'recmes' ] <- 0
    
    
    # merge on necessary post-stratification variables..
    z <- left_join(y, unique(allincomes[ , c( 'cod.uc' , 'fator') ]), by = "cod.uc")
    
    z <- z[is.na(z$fator) == FALSE, ]
    
    # ..and confirm no record-loss
    # stopifnot( nrow( z ) == nrow( y ) )
    
    # construct the preliminary survey object
    # (not yet post-stratified)
    sample.pof <-
      svydesign(
        id = ~control , 
        # strata = ~estrato_unico , 
        weights = ~fator ,
        data = z , 
        nest = TRUE
      )
    
    # construct the target population table
    # uc.totals <- 
    #   data.frame(
    #     pos_estrato = unique( z$pos_estrato ) , 
    #     Freq = unique( z$tot_unidade_c )
    #   )
    
    # construct the final post-stratified survey object
    pof.design <- sample.pof
      # postStratify(
      #   sample.pof , 
      #   ~pos_estrato , 
      #   uc.totals
      # )
    
    # take the overall mean..
    st <- svymean( ~recmes , pof.design )
    
    # ..and the mean, broken down by income categories
    sb <- 
      svyby(
        ~recmes , 
        ~trabalhador.cat , 
        pof.design , 
        svymean
      )
    
    # make a single-row data.frame for the total..
    ot <-
      data.frame( 
        trabalhador.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( SE( st ) ) , 
        cv = as.numeric( cv( st ) )
      )
    
    # ..and a multi-row data.frame for the breakouts
    ob <-
      data.frame( 
        trabalhador.cat = sb$trabalhador.cat , 
        mean = coef( sb ) , 
        se = as.numeric( SE( sb ) ) , 
        cv = as.numeric( cv( sb ) )
      )
    
    # stack them
    w <- rbind( ot , ob )
    
    # throw on the current income type code
    w$cod.novo <- incomeCode
    
    # finish up with a single row of data,
    # stretched out into `wide` format
    reshape( 
      w , 
      idvar = 'cod.novo' ,
      timevar = 'trabalhador.cat' ,
      direction = 'wide'
    )
    # since the result of this `reshape` is the last line of this function
    # the function will return that result.
  }

# examples with new function ---------------------------------------------------------------------
# run a single line with our fancy new
# `tabela_2.1.1` for vegetables --

# Income as employee
tabela_2.1.1( 
  "1.1.1" , 
  domicilios_trabalhadores ,
  allincomes , 
  componentes
  #, 
  #poststr 
)

# hey why not run one more
# `tabela_2.1.1` for all labour derived income

# all labour derived income	
tabela_2.1.1( 
  "1.1" , 
  domicilios_trabalhadores ,
  allincomes , 
  componentes
  #, 
  #poststr 
)

# proper table replication ---------------------------------------------------------------------
# # # # # # # # # # # # # # # #
# create a table to populate  #

# make an empty, single-column table
tabela <- data.frame( tipo.de.rendimento = NULL )

# for every row in the `componentes` table...
for ( i in seq( nrow( tabelagregada ) ) ){
  # if the `tipoderendimento` does not yet exist in the `tabela`..
  if ( !( tabelagregada[ i , 'tipoderendimento' ] %in% tabela$tipo.de.rendimento ) ) {
    
    # add a new row, and add that `desc.#` to the `tabela` object..
    tabela[ nrow( tabela ) + 1 , 'tipo.de.rendimento' ] <- 
      tabelagregada[ i , 'tipoderendimento' ]
    
    # ..and also copy over the current code.
    tabela[ nrow( tabela ) , 'cod.novo' ] <- 
      tabelagregada[ i , 'cod.novo']
  }
}

# remove blank records from the final `tabela`
# tabela <- tabela[ tabela$tipo.de.rendimento != "" , ] -- not needed

# want to look at the `tabela` object?
# here are the first six..
#head( tabela )

# ..and the last six records.
#tail( tabela )


# alright.  now scan through each record in the `tabela` data.frame
for ( i in seq( nrow( tabela ) ) ){
  
  # run the `tabela_2.1.1` function on the current code
  print( tabela[ i , 'cod.novo' ] )
  
  # save the result into a new object `curRow`
  curRow <- 
    tabela_2.1.1( 
      tabela[ i , 'cod.novo' ] , 
      domicilios_trabalhadores,
      allincomes , 
      componentes
      # ,
      # poststr
    )
  
  # if it's the first run, make a new `allRows` object.  otherwise, stack it.
  if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
  
}

# merge on the descriptions
result_2.1.1 <- merge( tabela , allRows )

# take a look at the final table..
#result_2.1.1
# ..or export them using one of the techniques discussed on http://twotorials.com

