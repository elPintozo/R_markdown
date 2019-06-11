#===================================================================================================
#tabla_resumen.R: Esta función construye la tabla de resumen final del torneo definiendo las posiciones
#Entrada: tabla con resultado de los partidos
#Salida: tabla con posiciones del torneo
#===================================================================================================

resumen_torneo=function(partidos)
{
  nombre_equipos=as.character(unique(partidos$EQ1))
  ind_OFF=NULL;ind_DEF=NULL;ind_DEF=NULL; ind_SPI=NULL;
  GFAV=NULL;  GCON=NULL;  PJUG=NULL;  PGAN=NULL;  PEMP=NULL;  PPER=NULL;  PTJE=NULL
  COND=NULL
  #Cálculo de indicadores
  for (a in 1:length(nombre_equipos))
  {
    p1=partidos[which(as.character(partidos$EQ1)==nombre_equipos[a]),]
    p2=partidos[which(as.character(partidos$EQ2)==nombre_equipos[a]),]
    ind_OFF[a]=p1$OFF1[1]
    ind_DEF[a]=p1$DEF1[1]
    ind_SPI[a]=p1$SPI1[1]
    GFAV[a]=sum(p1$GEQ1)+sum(p2$GEQ2)
    GCON[a]=sum(p1$GEQ2)+sum(p2$GEQ1)
    PJUG[a]=nrow(p1)+nrow(p2)
    PGAN[a]=length(which(p1$GEQ1>p1$GEQ2))+length(which(p2$GEQ1<p2$GEQ2))
    PEMP[a]=length(which(p1$GEQ1==p1$GEQ2))+length(which(p2$GEQ1==p2$GEQ2))
    PPER[a]=length(which(p1$GEQ1<p1$GEQ2))+length(which(p2$GEQ1>p2$GEQ2))
    PTJE[a]=PGAN[a]*3+PEMP[a]
  }
  
  salida=data.frame(EQ=nombre_equipos,OFF=ind_OFF,DEF=ind_DEF,SPI=ind_SPI,PPER,PEMP,PGAN,PJUG,GCON,GFAV,PTJE)
  salida=salida[order(-PTJE,-PGAN,-GFAV),]
  salida$POS=c(1:20)
  COND=rep("---",20)
  COND[1]="GAN"
  COND[19]="DES";COND[20]="DES";
  salida$COND=COND
  return(salida)
}


