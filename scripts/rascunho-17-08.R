saveRDS(family.level.income.aggregated, "todos_rendimentos_familias.rds")
library(tidyr)
library(dplyr)
library(ggplot2)
load("2009/t_morador_s.rda")

family.level.income.aggregated <- readRDS("todos_rendimentos_familias.rds")

renda_m_total <- family.level.income.aggregated %>% group_by(cod.uc) %>%
  summarise(cod.novo = "1",
            recmes = sum(recmes, na.rm = TRUE))

renda_m_total <- rbind(family.level.income.aggregated, renda_m_total)
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
str(renda_m_total)

renda_m <- renda_m_total %>% mutate(renda_trabalho = ((`1.1.1` + `1.1.3` +
                                                `1.2.1` + `1.2.4` + 0.01 ) / (`1`+ 0.01))*100)
str(renda_m)

sapply(seq(60, 95, 5), function (x) nrow(renda_m[renda_m$renda_trabalho < x,])/55993) %>% 
  plot()

domicilios_trabalhadores <- merge(family.level.income, 
                                renda_m[, c("cod.uc", "renda_trabalho")],
                                all.x = TRUE)

# Aqui os NAs com baixa renda foram definidor como trabalhadores
domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total <= 2000, "renda_trabalho"] <- 100

domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total > 2000, "renda_trabalho"] <- 0


domicilios_trabalhadores <- 
  transform(
    domicilios_trabalhadores , 
    
    # create income categories
    renda.cat = 
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

family.level.income <- domicilios_trabalhadores

tabela_2.1.1 <-
  function(
    # choose an income code
    incomeCode ,
    # specify the family-level data.frame with the income variable
    family.level.income = domicilios_trabalhadores ,
    # specify the income booklet data,
    # which must contain the variables
    # created above
    allincomes = allincomes ,
    # identify the components table to use
    componentes = componentes ,
    # identify the table to use for post-stratification
    poststr = poststr
  ){
    
    # isolate all records containing the current code *anywhere*
    incomeCode.plus.subcodes <-
      componentes[substring(componentes$cod.novo,1,nchar(incomeCode)) == incomeCode,'cod.novo']      
    
    # isolate family-wide incomes to only matching codes
    family.incomes.by.code <- 
      allincomes[ allincomes$cod.novo %in% incomeCode.plus.subcodes , c( 'cod.novo' , 'recmes' , 'cod.uc' ) ]
    
    # aggregate incomes to the one-record-per-family-level
    family.level.income.aggregated <-
      aggregate( 
        recmes ~ cod.uc + cod.novo , 
        family.incomes.by.code , 
        sum 
      )
    
    # merge the income and familiar income tables,
    # assuming that the income table has no missings
    y <- merge( domicilios_trabalhadores , family.level.income.aggregated , all.x = TRUE )
    
    # all missing values from the left-join above
    # should be converted to zeroes
    y[ is.na( y$recmes ) , 'recmes' ] <- 0
    
    
    # merge on necessary post-stratification variables..
    z <- 
      merge( 
        y , 
        poststr[ , c( 'control' , 'estrato_unico' , 'fator_des' , 'pos_estrato' , 'tot_unidade_c' ) ] 
      )
    
    # ..and confirm no record-loss
    stopifnot( nrow( z ) == nrow( y ) )
    
    # construct the preliminary survey object
    # (not yet post-stratified)
    sample.pof <-
      svydesign(
        id = ~control , 
        strata = ~estrato_unico , 
        weights = ~fator_des ,
        data = z , 
        nest = TRUE
      )
    
    # construct the target population table
    uc.totals <- 
      data.frame(
        pos_estrato = unique( z$pos_estrato ) , 
        Freq = unique( z$tot_unidade_c )
      )
    
    # construct the final post-stratified survey object
    pof.design <- 
      postStratify(
        sample.pof , 
        ~pos_estrato , 
        uc.totals
      )
    
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

tabela <- data.frame( tipo.de.rendimento = NULL )

for ( i in seq( nrow( componentes ) ) ){
  if ( !( componentes[ i , 'tipoderendimento' ] %in% tabela$tipo.de.rendimento ) ) {
    
    # add a new row, and add that `desc.#` to the `tabela` object..
    tabela[ nrow( tabela ) + 1 , 'tipo.de.rendimento' ] <- 
      componentes[ i , 'tipoderendimento' ]
    
    # ..and also copy over the current code.
    tabela[ nrow( tabela ) , 'cod.novo' ] <- 
      componentes[ i , 'cod.novo']
  }
}


for ( i in seq( nrow( tabela ) ) ){
  
  # run the `tabela_2.1.1` function on the current code
  print( tabela[ i , 'cod.novo' ] )
  
  # save the result into a new object `curRow`
  curRow <- 
    tabela_2.1.1(incomeCode = tabela[ i , 'cod.novo' ])
  
  # if it's the first run, make a new `allRows` object.  otherwise, stack it.
  if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
  
}


result_2.1.1 <- merge( tabela , allRows )

result_2.1.1

saveRDS(object = result_2.1.1, file = "tabela_renda_m_trabalhadores.rds")
save(list= ls(), file = "ambiente_tabela_renda_trab.rda")

svyhist(  ~recmes , 
  ~trabalhador.cat , 
  pof.design)
