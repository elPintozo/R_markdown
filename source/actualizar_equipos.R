#===================================================================================================
#actualizar_equipos.R: Esta función remueve los dos peores equipos y adiciona 2 nuevos. Además actualiza
#aleatoriamente el valor de los parámetros de los equipos
#Entrada: nombre de alumno 2
#Salida: tabla con equipos aleatorios y atributos
#===================================================================================================

actualizar_equipos=function(entrada)
{

  #Codificar nombres y definir semilla
  entrada=strsplit(entrada,"")[[1]]
  diccionario <- letters[1:26]
  conversion=match(entrada,diccionario)
  conversion[which(is.na(conversion))]=0
  conversion=sum(conversion)
  set.seed(conversion)
  
  
  #Lectura de archivos y creación de tablas
  tabla=read.csv("source/BD/equipos.csv",header=T,sep=";") 
  tabla2=read.csv("source/BD/BD.csv",header=T,sep=";") 
  sel1=sample(1:nrow(tabla),2)
  sel2=sample(1:nrow(tabla),2)
  
  #Creación de nombres
  nombre1=tabla$Primero[sel1]
  nombre2=tabla$Segundo[sel2]
  nombres=paste(nombre1,nombre2)
  
  #Creación de parámetros
  ind_def=round(runif(20, min(c(tabla2$DF1,tabla2$DF2)), max(tabla2$DF1,tabla2$DF2)),2)
  ind_off=round(runif(20, min(c(tabla2$OFT1,tabla2$OFT2)), max(tabla2$OFT1,tabla2$OFT2)),2)
  ind_spi=round(runif(20, min(c(tabla2$RT1,tabla2$RT2)), max(tabla2$RT1,tabla2$RT2)),2)
  COND=rep("ANT",20)
  
  
  equipos2=as.character(equipos$EQUIPO)
  equipos2[which(as.character(tabla_resumen$EQ[19])==equipos$EQUIPO)]=nombres[1]
  equipos2[match(as.character(tabla_resumen$EQ[20]),equipos$EQUIPO)]=nombres[2]
  COND[match(as.character(tabla_resumen$EQ[19]),equipos$EQUIPO)]="NUE"
  COND[match(as.character(tabla_resumen$EQ[20]),equipos$EQUIPO)]="NUE"
  
  
  #Creación de salida
  salida=data.frame(EQUIPO=equipos2,OFF=ind_off,DEF=ind_def,SPI=ind_spi,COND)
  return(salida) 
}