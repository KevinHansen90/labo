# XGBoost  sabor original ,  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/HP/Desktop/ECD/MDD")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( objective=       "binary:logistic",
                                   max_depth=           2,
                                   min_child_weight=    9,
                                   eta=                 0.0904038910518703,
                                   colsample_bytree=    0.373750040154235,
                                   max_bin=256,
                                   max_leaves=0,
                                   scale_pos_weight=1
                                   ),
                      nrounds= 184
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer( prediccion > 0.0158754661672741 ) )  ) #genero la salida

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5610/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/KA5610/KA_561_003.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
