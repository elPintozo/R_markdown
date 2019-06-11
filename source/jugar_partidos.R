#===================================================================================================
#jugar_partidos.R: Esta función efectúa los partidos de todos los equipos que ingresen como tabla
#y emplea una función basada el regresión lineal multivariada para predecir los resultados.
#Entrada: tabla con equipos
#Salida: tabla con partidos por equipo, atributos y resultados
#===================================================================================================
jugar_partidos=function(equipos)
{ # n=20
  # k=2
  # P=factorial(n)/factorial(n-k)
  tab_per=permutations(n=length(equipos$EQUIPO),r=2,v=as.character(equipos$EQUIPO))
  
  OFF1=equipos$OFF[match(tab_per[,1],equipos$EQUIPO)]
  DEF1=equipos$DEF[match(tab_per[,1],equipos$EQUIPO)]
  SPI1=equipos$SPI[match(tab_per[,1],equipos$EQUIPO)]
  OFF2=equipos$OFF[match(tab_per[,2],equipos$EQUIPO)]
  DEF2=equipos$DEF[match(tab_per[,2],equipos$EQUIPO)]
  SPI2=equipos$SPI[match(tab_per[,2],equipos$EQUIPO)]
  
  salida=data.frame(EQ1=tab_per[,1],EQ2=tab_per[,2],OFT1=OFF1, DF1=DEF1, RT1=SPI1, OFT2=OFF2, DF2=DEF2 ,RT2=SPI2 )
  jugarM1=readRDS("source/regresion/jugarM1.rds")
  jugarM2=readRDS("source/regresion/jugarM2.rds")
  
  resultado1=round(predict(jugarM1,newdata = salida))
  resultado2=round(predict(jugarM2,newdata = salida))
  
  resultado1[which(resultado1<0)]=0
  resultado2[which(resultado2<0)]=0
  
  salida$REQ1=resultado1
  salida$REQ2=resultado2
  
  names(salida)=c("EQ1","EQ2","OFF1","DEF1","SPI1","OFF2","DEF2","SPI2","GEQ1","GEQ2")
  return(salida)
}