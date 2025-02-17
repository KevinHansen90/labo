#esqueleto de grid search
#se espera que los alumnos completen lo que falta para recorrer TODOS cuatro los hiperparametros 

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(111599, 111611, 111623, 111637, 111641) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(75,25), agrupa="clase_ternaria", seed= 111599 )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 75% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 25% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.25

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( semillas, param_basicos )
{
  #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio  <- mean( unlist(ganancias) )

  return( ganancia_promedio )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/HP/Desktop/ECD/MDD")   #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/paquete_premium_202011.csv")
dataset <- dataset_mod3
#Reemplazo NA por knn
install.packages("performanceEstimation")
require(performanceEstimation)
dataset_mod <- knnImp(dataset[,3:158])

#Análisis ad hoc
PCA_dataset <- prcomp(dataset_mod, center = TRUE, scale = TRUE)

dataset_mod2 <- cbind(dataset[,1:2],dataset_mod,dataset[,159:159])
t(apply(dataset_mod2,2,sd))

write.csv(t(summary(dataset_mod2[clase_ternaria=="CONTINUA"])),"C:/Users/HP/Desktop/ECD/MDD/S_CONTINUA.csv")
write.csv(t(summary(dataset_mod2[clase_ternaria=="BAJA+1"])),"C:/Users/HP/Desktop/ECD/MDD/S_BAJA1.csv")
write.csv(t(summary(dataset_mod2[clase_ternaria=="BAJA+2"])),"C:/Users/HP/Desktop/ECD/MDD/S_BAJA2.csv")

write.csv(apply(dataset_mod2[clase_ternaria=="CONTINUA"],2,sd),"C:/Users/HP/Desktop/ECD/MDD/dcontinua.csv")
write.csv(apply(dataset_mod2[clase_ternaria=="BAJA+1"],2,sd),"C:/Users/HP/Desktop/ECD/MDD/dbaja1.csv")
write.csv(apply(dataset_mod2[clase_ternaria=="BAJA+2"],2,sd),"C:/Users/HP/Desktop/ECD/MDD/dbaja2.csv")

dataset_mod3 <- dataset_mod2[,':='(var_rent=mrentabilidad/mrentabilidad_annual
                  ,margen_mult=mactivos_margen*mpasivos_margen
                  ,mov_trj=ctarjeta_debito_trx+ctarjeta_visa_trx+ctarjeta_master_trx
                  ,pf_pr=(mplazo_fijo_dolares+mplazo_fijo_pesos)/mprestamos_personales
                  ,deb_aut=ccuenta_debitos_automaticos+ctarjeta_visa_debitos_automaticos+ctarjeta_master_debitos_automaticos
                  ,trx_serv=cpagomiscuentas+ctransferencias_recibidas+ctransferencias_emitidas+cextraccion_autoservicio+chomebanking_trx+catm_trx
                  ,mov_trj2=(Master_cconsumos+Visa_cconsumos)*(Master_status+Visa_status)
                  ,trx_vol=(ctarjeta_debito_trx+ctarjeta_visa_trx+ctarjeta_master_trx)/(ctrx_quarter/3))]


summary(PCA_dataset)
write.csv(PCA_dataset$rotation,"C:/Users/HP/Desktop/ECD/MDD/PCA1.csv")


#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/HT2020/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/HT2020/gridsearch.txt"

#Escribo los titulos al archivo donde van a quedar los resultados
#atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
#la forma que no suceda lo anterior es con append=TRUE
cat( file=archivo_salida,
     sep= "",
     "cp", "\t",
     "max_depth", "\t",
     "min_bucket", "\t",
     "min_split", "\t",
     "ganancia_promedio", "\n")



#itero por los loops anidados para cada hiperparametro

i=1


for( vcp  in  c(-0.1 )  )
{
for( vmax_depth  in  c(6,8,10,12))
{
for( vmin_bucket  in  c(10,20,50,100,500))
{
for( vmin_split  in  c(50,100,200,300,400,500))
{ if(vmin_bucket*2>vmin_split)
  next

  #notar como se agrega
  param_basicos  <- list( "cp"=         vcp,       #complejidad minima
                          "minsplit"=  vmin_split,  #minima cantidad de registros en un nodo para hacer el split
                          "minbucket"=  vmin_bucket,          #minima cantidad de registros en una hoja
                          "maxdepth"=  vmax_depth ) #profundidad máxima del arbol

  #Un solo llamado, con la semilla 17
  ganancia_promedio  <- ArbolesMontecarlo( ksemillas,  param_basicos )

  #escribo los resultados al archivo de salida
  cat(  file=archivo_salida,
        append= TRUE,
        sep= "",
        vcp, "\t",
        vmax_depth, "\t",
        vmin_bucket, "\t",
        vmin_split, "\t",
        ganancia_promedio, "\n"  )
  print(i)
  print(Sys.time())
  i <- i+1
  

}
}
}
}
