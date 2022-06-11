#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset_grande  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- copy( dataset_grande[  clase_ternaria =="BAJA+2"  & foto_mes>=202001  & foto_mes<=202011, ] )

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset_grande[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "active_quarter",
                     "cliente_vip",
                     "internet",
                     "cliente_edad",
                     "cliente_antiguedad",
                     "mrentabilidad",
                     "mrentabilidad_annual",
                     "mcomisiones",
                     "mactivos_margen",
                     "mpasivos_margen",
                     "cproductos",
                     "mcuenta_corriente",
                     "mcaja_ahorro",
                     "mcaja_ahorro_dolares",
                     "ctarjeta_debito_trx",
                     "mautoservicio",
                     "ctarjeta_visa_trx",
                     "mtarjeta_visa_consumo",
                     "ctarjeta_master_trx",
                     "mtarjeta_master_consumo",
                     "mprestamos_personales",
                     "mprestamos_prendarios",
                     "mprestamos_hipotecarios",
                     "mplazo_fijo_dolares",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos",
                     "minversion1_dolares",
                     "cpayroll_trx",
                     "mpayroll",
                     "mcuenta_debitos_automaticos",
                     "mpagomiscuentas",
                     "ctransferencias_recibidas",
                     "mtransferencias_recibidas",
                     "ctransferencias_emitidas",
                     "mtransferencias_emitidas",
                     "cextraccion_autoservicio",
                     "ccallcenter_trx",
                     "chomebanking_trx",
                     "ccajas_trx",
                     "ctrx_quarter",
                     "cmobile_app_trx",
                     "Master_status",
                     "Master_mconsumospesos",
                     "Master_mconsumosdolares",
                     "Master_mlimitecompra",
                     "Visa_status",
                     "Visa_mconsumospesos",
                     "Visa_mconsumosdolares",
                     "Visa_mlimitecompra")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#primero, creo la carpeta donde van los resultados
dir.create( "./exp/", showWarnings= FALSE )
dir.create( "./exp/ST7620", showWarnings= FALSE )
setwd( "~/buckets/b1/exp/ST7620" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=4 & distintos <=5 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_trx),  cluster2 ]


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )
