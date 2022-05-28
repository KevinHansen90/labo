#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
#------------------------------------------------------------------------------
#Función para verificar si acierto en la decisión
decision  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------
#Inicializo variables
GLOBAL_iteracion <- 0
GLOBAL_jugadores  <-  sample( c( (501:599 )/1000 , 0.7 ) )
GLOBAL_tiros_total  <- 0
GLOBAL_tiros_max  <- 15000
ids_juegan <- 1:100


n_rondas <- 0
target_aciertos_consec <- 6
target_errados_consec <-  5
tiros_porcentaje <- 0.8
tiros <- round((GLOBAL_tiros_max-GLOBAL_tiros_total)/length(ids_juegan)*tiros_porcentaje)

#------------------------------------------------------------------------------
#Función para jugar
jugar <- function()
{
  while(GLOBAL_tiros_total<GLOBAL_tiros_max & (GLOBAL_tiros_max-GLOBAL_tiros_total)/length(ids_juegan)*tiros_porcentaje>50 & length(ids_juegan)>=2)
{
  planilla <- data.table( "id"= 1:100 )
  planilla  <-planilla[ ids_juegan,  tiros:= 0]
  planilla  <-planilla[ ids_juegan,  aciertos:= 0]
  planilla  <-planilla[ ids_juegan,  aciertos_consec:= 0]
  planilla  <-planilla[ ids_juegan,  errados_consec:= 0]
  planilla  <-planilla[ ids_juegan,  pasa_ronda:= 0]
for (tiros in 1:tiros)
{
for(j in 1:length(ids_juegan))
{
  if(planilla[ids_juegan[j],aciertos_consec]<target_aciertos_consec)
  {
  if(planilla[ ids_juegan[j],errados_consec]<target_errados_consec)
  {
  res <- 0
  res <- sum(runif(1) <GLOBAL_jugadores[j])
  planilla <- planilla[ ids_juegan[j],  tiros:= tiros+1] #anoto tiros
  planilla <- planilla[ ids_juegan[j],  aciertos:= aciertos+res] #anoto aciertos
  planilla <- planilla[ ids_juegan[j],  aciertos_consec:= if(aciertos_consec>=0 && res>0) {aciertos_consec+res} else {0}] #anoto aciertos consec
  planilla <- planilla[ ids_juegan[j],  errados_consec:= if(errados_consec>=0 && res==0) {errados_consec+1} else {0}] #anoto aciertos consec
  GLOBAL_tiros_total  <- GLOBAL_tiros_total+1
  }
  else
  next
  }
  else
  planilla <- planilla[ ids_juegan[j],  pasa_ronda:= 1]
  next
}
}
mediana <- planilla[pasa_ronda==0 & errados_consec<target_errados_consec,median(aciertos)] 
planilla <- planilla[pasa_ronda==0 & errados_consec<target_errados_consec & mediana<aciertos,pasa_ronda:=1] 
ids_juegan <- planilla[pasa_ronda==1,id]
n_rondas <- n_rondas+1
#print(n_rondas)
#print(length(ids_juegan))
}
return(decision(ids_juegan))
}

#------------------------------------------------------------------------------
#Función para repetir experimento
ganador  <- function()
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )
  
  for( experimento  in  1:10000 )
  {
    if( experimento %% 1 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
    
    veredicto  <- jugar()
    
    tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
  }
  
  cat("\n")
  
  tiros_total  <-  tabla_veredictos[  , max(tiros_total) ]
  tasa_eleccion_correcta  <-  tabla_veredictos[  , mean(acierto) ]
  return(tasa_eleccion_correcta)
} 
#------------------------------------------------------------------------------
#Experimento
set.seed(111599)
jugar()