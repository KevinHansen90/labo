# LightGBM  Motivacional
# para motivar a los alumnos a hacer la  "Tarea Hogar TRES"

# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# el resultado queda en  el bucket en  ./exp/KA6520/ 
# son varios archivos, subirlos inteligentemente a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)


#--------------------------------------

#ordeno el dataset por  < numero_de_cliente, foto_mes > ´para calcular los lags
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
columnas_lag  <- setdiff( colnames(dataset), c("numero_de_cliente","foto_mes","clase_ternaria") )
nlag  <- 1      #orden del lag
sufijo  <- "_lag1"
dataset[ , paste0( columnas_lag, sufijo) := shift( .SD, nlag, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= columnas_lag ]

#creo los delta lags
sufijodelta  <- paste0( "_delta", nlag )

#uso un espantoso for para crear los delta lags
for( vcol in columnas_lag )
{
  dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
}

#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#establezco donde entreno, desde enero a noviembre,  SIN  junio-2020
#entreno en la UNION de 11 meses
dataset[ , train  := 0L ]
dataset[ foto_mes >= 202001 & foto_mes<=202011 & foto_mes != 202006, 
         train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA6520/", showWarnings = FALSE )
setwd( "./exp/KA6520/" )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=         0.0100111367304997,
                                   num_iterations=      260,
                                   num_leaves=         1023,
                                   min_data_in_leaf=   1380,
                                   feature_fraction=      0.479139191686628,
                                   seed=             111599
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "652_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== 202101 ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente ) ]
tb_entrega[  , prob := prediccion ]

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
for( envios  in  c( 10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500 ) )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0( "KA_652_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------
