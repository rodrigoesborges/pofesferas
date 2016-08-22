
options( encoding = "latin1" )		# # only macintosh and *nix users need this line





if ( .Platform$OS.type != 'windows' ) print( 'non-windows users: read this block' )


library(survey)		# load survey package (analyzes complex design surveys)
library(reshape2)	# load reshape2 package (transposes data frames quickly)
library(stringr)  # pad 0s to uniformize code padding

options( survey.lonely.psu = "adjust" )

load("2009/t_morador_s.rda")

load("2009/poststr.rda")

load( "2009/t_domicilio_s.rda" )

load( "2009/t_rendimentos_s.rda" )

load("2009cadastro-produtos.rda")

load("2009/t_outros_reci_s.rda")

incomeRecodes <- read.csv("codigos-recodificacao-rendimentos.csv",sep = ";" , as.is = 1:3)

incomeRecodes$cod.inc <-gsub(".*\\+.*","50000",incomeRecodes$cod.inc)
incomeRecodesX <- data.frame()
for (i in 1:nrow(incomeRecodes)) {incomeRecodesX <- rbind(incomeRecodesX,cbind(incomeRecodes[i,1],eval(parse(text = paste("c(" , incomeRecodes[i,3],")")))))}
names(incomeRecodesX) <- c("cod.novo","cod.rec")


t_rendimentos_s <-
  transform(
    t_rendimentos_s ,
 
    # monthly income - should be equal to rendimento corrigido
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12 ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ),
    
    # unique income type code
    cod.rec = paste0( num_quadro, substr(cod_item,1,3))
    
    # input recodes of income type according to incomeRecodes table -- done separately
  )


t_rendimentos_recoded <- merge (t_rendimentos_s, incomeRecodesX)
t_rendimentos_recoded <- t_rendimentos_recoded[ , c('cod.rec' , 'cod.uc', 'recmes' , 'fator_expansao1' , 'fator_expansao2','cod.novo' ) ]


t_outros_reci_s <-
  transform(
    t_outros_reci_s ,
    
    # monthly income
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12 ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ),
    
    # unique income type code
    cod.rec = paste0( num_quadro, substr(cod_item,1,3))
  )

t_outros_reci_recoded <- merge (t_outros_reci_s,incomeRecodesX)


t_outros_reci_recoded <- t_outros_reci_recoded[ , c('cod.rec' , 'cod.uc', 'recmes' , 'fator_expansao1' , 'fator_expansao2','cod.novo' ) ]

allincomes <- rbind(t_rendimentos_recoded,t_outros_reci_recoded)

t_morador_s <-
  transform(
    t_morador_s ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ) 
  )

family.level.income <- t_morador_s[ , c( 'cod.uc' , 'renda_total' ) ]

nrow( family.level.income )

family.level.income <- unique( family.level.income )

nrow( family.level.income )


family.level.income <- 
  transform(
    family.level.income , 
    
    # create income categories
    renda.cat = 
      cut(
        renda_total , 
        c( 0 , 830 , 1245 , 2490 , 4150 , 6225 , 10375 , Inf ) ,
        include.lowest = TRUE , 
        labels = c( "[0,830]" , "(830,1245]" , "(1245,2490]" , "(2490,4150]" , "(4150,6225]" , "(6225,10375]" , ">10375" )
      ) ,
    
    # create a control variable that matches 
    # the one in the `poststr` table
    control = substr( cod.uc , 1 , 6 )
  )


rm( t_morador_s )

gc()

componentes <- incomeRecodes
names(componentes) <- c("cod.novo","tipoderendimento","cod.rec")



tabela_2.1.1 <-
  function(
    # choose an income code
    incomeCode ,
    # specify the family-level data.frame with the income variable
    family.level.income = family.level.income ,
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
      componentes[substring(componentes$cod.novo,1,nchar(1)) == 1,'cod.novo']      
    
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
    y <- merge( family.level.income , family.level.income.aggregated , all.x = TRUE )
    
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
        ~renda.cat , 
        pof.design , 
        svymean
      )
    
    # make a single-row data.frame for the total..
    ot <-
      data.frame( 
        renda.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( SE( st ) ) , 
        cv = as.numeric( cv( st ) )
      )
    
    # ..and a multi-row data.frame for the breakouts
    ob <-
      data.frame( 
        renda.cat = sb$renda.cat , 
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
      timevar = 'renda.cat' ,
      direction = 'wide'
    )
    # since the result of this `reshape` is the last line of this function
    # the function will return that result.
  }


tabela_2.1.1( 
  "1.1.1" , 
  family.level.income ,
  allincomes , 
  componentes , 
  poststr 
)


tabela_2.1.1( 
  "1.1" , 
  family.level.income ,
  allincomes , 
  componentes , 
  poststr 
)


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
    tabela_2.1.1( 
      tabela[ i , 'cod.novo' ] , 
      family.level.income ,
      allincomes , 
      componentes , 
      poststr 
    )
  
  # if it's the first run, make a new `allRows` object.  otherwise, stack it.
  if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
  
}

result_2.1.1 <- merge( tabela , allRows )

result_2.1.1

