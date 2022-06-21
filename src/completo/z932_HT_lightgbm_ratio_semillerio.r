#Optimización bayesiana  LightGBM  con  ratio_num_leaves y semillerio


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("primes")
require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------

parametrizar  <- function( lparam )
{
  param_fijos  <- copy( lparam )
  hs  <- list()

  for( param  in  names( lparam ) )
  {

    if( length( lparam[[ param ]] ) > 1 )
    {
      desde  <- as.numeric( lparam[[ param ]][[1]]  )
      hasta  <- as.numeric( lparam[[ param ]][[2]]  )

      if( length( lparam[[ param ]] ) == 2 )
      {
         hs  <- append( hs,  
                        list( makeNumericParam( param, lower= desde, upper= hasta)  ) )
      } else {
         hs  <- append( hs, 
                        list( makeIntegerParam( param, lower= desde, upper= hasta) ) )
      }

      param_fijos[[ param ]] <- NULL  #lo quito 
    }
  }

  return( list( "param_fijos" =  param_fijos,
                "paramSet"    =  hs ) )
}

#------------------------------------------------------------------------------
# Particiona un dataset en forma estratificada

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------

vpos_optima  <- c()

fganancia_lgbm_meseta  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")

  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 , PARAM$const$POS_ganancia, PARAM$const$NEG_ganancia  ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  
  setorder( tbl, -gan_acum )  

  gan  <- tbl[ 1:200 , mean(gan_acum) ]
  pos  <- tbl[ 1:200 , as.integer( mean(posicion) ) ]

  vpos_optima  <<- c( vpos_optima, pos )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
x  <- list( learning_rate= 0.1,
            feature_fraction= 0.5,
            leaves_coverage= 0.6,
            logistic_leaf= -3 )

EstimarGanancia_lightgbm  <- function( x )
{
  gc()
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  param_completo  <- copy( c( param_fijos,  x ) )

  #trafo de ratio_num_leaves
  hojas_maximo  <- nrow( dtrain ) / x$min_data_in_leaf
  param_completo$num_leaves  <-   as.integer( x$ratio_num_leaves * hojas_maximo )
  param_completo$ratio_num_leaves  <- NULL

  param_completo$num_iterations         <- ifelse( param_fijos$boosting== "dart", 999, 99999 )  #un numero muy grande
  param_completo$early_stopping_rounds  <- as.integer(200 + 4/param_completo$learning_rate )

  tb_prediccion_semillerio  <- as.data.table( list( "pred_acumulada" = rep( 0, nrow(dataset_test) )) ) 

  vnum_iterations   <- c()
  vposicion_optima  <- c()
  vganancia_test    <- c()

  #Para cada una de las  PARAM$semillerio  semillas
  for( semilla  in  ksemillas )
  {
    param_completo$seed  <- semilla 

    vpos_optima  <<- c()
    set.seed( param_completo$seed )
    modelo_train  <- lgb.train( data= dtrain,
                                valids= list( valid= dvalidate ),
                                eval=   fganancia_lgbm_meseta,
                                param=  param_completo,
                                verbose= -100 )

    vnum_iterations  <- c( vnum_iterations, modelo_train$best_iter )
    posicion_optima  <- vpos_optima[ modelo_train$best_iter ]
    vposicion_optima  <- c( vposicion_optima , posicion_optima )

    #aplico el modelo a testing y calculo la ganancia
    prediccion  <- predict( modelo_train, 
                            data.matrix( dataset_test[ , campos_buenos, with=FALSE]) )

    tbl  <- copy( dataset_test[ , list(clase01) ] )
    tbl[ , prob := prediccion ]
    setorder( tbl, -prob )
    ganancia_test  <- tbl[ 1:posicion_optima, 
                           sum( ifelse( clase01, PARAM$const$POS_ganancia, PARAM$const$NEG_ganancia ) )]

    vganancia_test  <- c( vganancia_test, ganancia_test )

    tb_prediccion_semillerio[  , pred_acumulada := pred_acumulada +  as.numeric( frank(prediccion, ties.method= "random") ) ]

  }

  tbl  <- copy( dataset_test[ , list(clase01) ] )
  tbl[ , prob := tb_prediccion_semillerio$pred_acumulada ]

  setorder( tbl, -prob )
  tbl[ , pos := .I ]

  cantidad_test_normalizada  <- as.integer( nrow(dataset_test) * (mean(vposicion_optima) / nrow( dvalidate )) )

  cat( "posicion_optima:",  mean(vposicion_optima), "  "  )
  for( i in 1:length(vposicion_optima) ) cat( vposicion_optima[i], "  " ) 
  cat( "\n" )
  cat( "cantidad_test_normalizada: ", cantidad_test_normalizada, "\n" )

  ganancia_test  <- tbl[ pos <= cantidad_test_normalizada, 
                         sum( ifelse( clase01, PARAM$const$POS_ganancia, PARAM$const$NEG_ganancia ) )]

  rm( tbl )
  gc()

  ganancia_test_normalizada  <- test_multiplicador * ganancia_test

  cat( "gan_individual:", test_multiplicador*min( vganancia_test ), test_multiplicador*mean( vganancia_test ), test_multiplicador*max( vganancia_test ), "\n",
       "gan_ensemble:",  ganancia_test_normalizada, "\n" )

  #voy grabando las mejores column importance
  if( ganancia_test_normalizada >  GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_test_normalizada
    tb_importancia    <- as.data.table( lgb.importance( modelo_train ) )

    fwrite( tb_importancia,
            file= paste0( PARAM$files$output$importancia, GLOBAL_iteracion, ".txt" ),
            sep= "\t" )
  }


  #logueo final
  xx  <- copy( c( param_fijos,  x ) )
  xx$early_stopping_rounds  <- NULL
  xx$num_iterations  <- as.integer( mean( vnum_iterations ) )
  xx$estimulos   <-  cantidad_test_normalizada
  xx$ganancia  <- ganancia_test_normalizada
  xx$iteracion_bayesiana  <- GLOBAL_iteracion

  exp_log( xx,  arch= PARAM$files$output$BOlog )

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------
#esta es la funcion mas mistica de toda la asignatura
# sera explicada en  Laboratorio de Implementacion III

vprob_optima  <- c()

fganancia_lgbm_mesetaCV  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 & vpesos>1,
                                               PARAM$const$POS_ganancia,
                                               PARAM$const$NEG_ganancia  ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]

  gan  <-  tbl[ , max(gan_acum) ]

  pos  <- which.max(  tbl[ , gan_acum ] ) 
  vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbmCV  <- function( x )
{
  gc()
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #Hago el trafo de los parametros
  vfilas_efectivas  <- nrow( dtrain ) * ( ( PARAM$crossvalidation_folds-1) / PARAM$crossvalidation_folds)
  vcoverage  <- pmax(  1,  as.integer( vfilas_efectivas * x$leaves_coverage )  )
  vratio  <- 1 / ( 1 + exp( - x$logistic_leaf ) )
  vmin_data_in_leaf  <- pmax( 10, as.integer(vratio*vcoverage) )
  vnum_leaves  <- pmax( 1,  as.integer( vcoverage/ vmin_data_in_leaf) )

  xprima  <-  copy( x )
  xprima$min_data_in_leaf  <- vmin_data_in_leaf
  xprima$num_leaves        <- vnum_leaves
  xprima$leaves_coverage  <- NULL
  xprima$logistic_leaf    <- NULL
  cat( "min_data_in_leaf: ", xprima$min_data_in_leaf ,  
        "num_leaves:", xprima$num_leaves, 
        "leaves_coverage:", x$leaves_coverage,
        "logistic_leaf:", x$logistic_leaf, 
        "nrow", nrow( dtrain ),
        "\n" )

  param_completo  <- c( param_fijos,  xprima )


  param_completo$num_iterations         <- ifelse( param_fijos$boosting== "dart", 999, 99999 )
  param_completo$early_stopping_rounds  <- as.integer(200 + 4/param_completo$learning_rate )

  vprob_optima  <<- c()

  set.seed( param_completo$seed )
  modelocv  <- lgb.cv( data= dtrain,
                       eval=   fganancia_lgbm_mesetaCV,
                       param=  param_completo,
                       stratified= TRUE,                   #sobre el cross validation
                       nfold= PARAM$crossvalidation_folds,
                       verbose= -100 )

  desde  <- (modelocv$best_iter-1)*PARAM$crossvalidation_folds + 1
  hasta  <- desde + PARAM$crossvalidation_folds -1

  prob_corte            <-  mean( vprob_optima[ desde:hasta ] )
  cantidad_normalizada  <-  -1

  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  ganancia_normalizada  <- ganancia * PARAM$crossvalidation_folds


  #voy grabando las mejores column importance
  if( ganancia_normalizada >  GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_normalizada

    param_impo <-  copy( param_completo )
    param_impo$early_stopping_rounds  <- 0
    param_impo$num_iterations  <- modelocv$best_iter

    modelo  <- lgb.train( data= dtrain,
                       param=  param_impo,
                       verbose= -100 )

    tb_importancia    <- as.data.table( lgb.importance( modelo ) )

    fwrite( tb_importancia,
            file= paste0( PARAM$files$output$importancia, GLOBAL_iteracion, ".txt" ),
            sep= "\t" )

  }


  #logueo final
  xx  <- copy( c( param_fijos,  x ) )
  xx$early_stopping_rounds  <- NULL
  xx$num_iterations  <- modelocv$best_iter
  xx$prob_corte  <-  prob_corte
  xx$estimulos   <-  cantidad_normalizada
  xx$ganancia  <- ganancia_normalizada
  xx$iteracion_bayesiana  <- GLOBAL_iteracion

  exp_log( xx,  arch= PARAM$files$output$BOlog )

  return( ganancia_normalizada )
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:PARAM$semillerio ]   #me quedo con PARAM$semillerio primos al azar

#cargo el dataset que tiene la Training Strategy
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dentrada )
dataset   <- fread( nom_arch )


#creo la clase_binaria {0.1}  con la que debo trabajar  -----------------------
dataset[ part_train == 1L, 
         clase01:= ifelse( get( PARAM$const$campo_clase ) %in%  PARAM$clase_train_POS, 1L, 0L ) ]

dataset[ part_validate == 1L, 
         clase01:= ifelse( get( PARAM$const$campo_clase ) %in%  PARAM$clase_validate_POS, 1L, 0L ) ]

dataset[ part_test == 1L, 
         clase01:= ifelse( get( PARAM$const$campo_clase ) %in%  PARAM$clase_test_POS, 1L, 0L ) ]


#los campos que se pueden utilizar para la prediccion
campos_buenos  <- setdiff( copy(colnames( dataset )),
                           c( PARAM$const$campo_clase, "clase01",
                              "part_train","part_validate","part_test" ) )

#la particion de train siempre va
dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ part_train==1, campos_buenos, with=FALSE] ),
                        label=   dataset[ part_train==1, clase01],
                        weight=  dataset[ part_train==1, ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_test_POS, 1.0000001, 1.0)],
                        free_raw_data= FALSE
                      )


#calculo  validation y testing, segun corresponda
if( PARAM$crossvalidation == FALSE )
{
  if( PARAM$validate == TRUE )
  {
    dvalidate  <- lgb.Dataset( data=  data.matrix( dataset[ part_validate==1, campos_buenos, with=FALSE] ),
                               label= dataset[ part_validate==1, clase01],
                               free_raw_data= FALSE
                             )

    dataset_test  <- dataset[ part_test== 1 ]
    test_multiplicador  <- 1

  } else {

    #divido en mitades los datos de testing
    particionar( dataset, 
                 division= c(1,1),
                 agrupa= c("part_test", "foto_mes","clase_ternaria" ), 
                 seed= PARAM$semilla,
                 campo= "fold_test"
                )

    # fold_test==1  lo tomo para validation
    dvalidate  <- lgb.Dataset( data=  data.matrix( dataset[ part_test==1 & fold_test==1, campos_buenos, with=FALSE] ),
                               label= dataset[ part_test==1 & fold_test==1, clase01],
                               free_raw_data= FALSE
                             )

    dataset_test  <- dataset[ part_test==1 & fold_test==2, ]
    test_multiplicador  <- 2
  }

}


rm( dataset )
gc()


#Prepara todo la la Bayesian Optimization -------------------------------------
hiperparametros <- PARAM[[ PARAM$algoritmo ]]
apertura  <- parametrizar( hiperparametros )
param_fijos  <-  apertura$param_fijos


#si ya existe el archivo log, traigo hasta donde procese
if( file.exists( PARAM$files$output$BOlog ) )
{
  tabla_log  <- fread( PARAM$files$output$BOlog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_ganancia   <- tabla_log[ , max(ganancia) ]
  rm(tabla_log)
} else  {
  GLOBAL_iteracion  <- 0
  GLOBAL_ganancia   <- -Inf
}


#Aqui comienza la configuracion de mlrMBO
if( PARAM$crossvalidation ) {
  funcion_optimizar  <- EstimarGanancia_lightgbmCV
} else {
  funcion_optimizar  <- EstimarGanancia_lightgbm
}


configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= PARAM$BO$minimize,   #estoy Maximizando la ganancia
              noisy=    PARAM$BO$noisy,
              par.set=  makeParamSet( params= apertura$paramSet ),     #definido al comienzo del programa
              has.simple.signature = PARAM$BO$has.simple.signature   #paso los parametros en una lista
             )

#archivo donde se graba y cada cuantos segundos
ctrl  <- makeMBOControl( save.on.disk.at.time= PARAM$BO$save.on.disk.at.time,  
                         save.file.path=       PARAM$files$output$BObin )  
                         
ctrl  <- setMBOControlTermination( ctrl, 
                                   iters= PARAM$BO$iterations )   #cantidad de iteraciones
                                   
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km",
                        predict.type= "se",
                        covtype= "matern3_2",
                        control= list(trace= TRUE) )



# grabo catalogo   ------------------------------------------------------------
# no todos los archivos generados pasan al catalogo
# el catalogo no se graba al final, para permitir que se pueden correr experimentos aguas abajo
#   con las iteraciones de la optimziacion bayesiana que se tienen hasta el momento

exp_catalog_add( action= "HT",
                 type=   "file",
                 key=    "BOlog",
                 value = PARAM$files$output$BOlog )

#--------------------------------------


#Aqui inicio la optimizacion bayesiana
if( !file.exists( PARAM$files$output$BObin ) ) {

  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)

} else {
  #si ya existe el archivo RDATA, debo continuar desde el punto hasta donde llegue
  #  usado para cuando se corta la virtual machine
  run  <- mboContinue( PARAM$files$output$BObin )   #retomo en caso que ya exista
}


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
