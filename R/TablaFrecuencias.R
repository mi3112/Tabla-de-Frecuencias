#' Tabla de Frecuencias

#' @author Michelle Paredes Escobar
#' @description El siguiente codigo permite realizar una tabla de frecuencias basado en los metodos
#' estudiados en estadistica basica y disenio experimental
#'
#' The next code allows create a frecuency table baase in the metodology studied at
#' DoE and stadistics
#'
#' @param data base de datos
#' @return tabla de frecuencias
#'

#' Numero de datos totales "n"
#' Total number of data "n"
num <- function(data){
  return(dim(data)[1] * dim(data)[2])
}

#' Valor minimo
#' Min value
minVal <- function(data){
  return(min (data))
}

#' Valor maximo
#' Max value
maxVal <- function(data){
  return(max(data))
}

#' Rango
#' Rate
rate <- function(maxVal, minVal){
  return(maxVal - minVal)
}

#' Numero de categorias por el metodo de Storges
#' Number of categories. Storges
numStorge <- function(num){
  return(ceiling(1 + log2(n)))
}

#' Numero de decimales
decimalnumcount<-function(data){
  b=0
  row <- nrow(data)
  col <- ncol(data)
  for(i in 1:row){
    for(j in 1:col){
      data <- as.character(data)
      stopifnot(class(data)=="character")
      data <-gsub("(.*)(\\.)|([0]*$)","","x")
      a <- nchar(data)
      if(a>b){
        b=a
      }else{
        b=b
      }
    }
  }
  print(b)
}

#' Ancho del intervalo
anchoIntervalo <- function(rate, numStorge, decimalnumcount){
  a1 <- rate/numStorge
  a2 <- as.integer(a1)
  if(a2 - a1 != 0){
    c <- (decimalnumcount+1)/decimalnumcount^(d+1)
    a3 <- round(a1/c)*c
    a3
  }else{
    a3 <- a1
    a3
  }
  print(a3)
}

#' Rango de la tabla de frecuencias RTfD
TdF <- function(numStorge, anchoIntervalo){
  numStorge * anchoIntervalo
}

#' Exceso
exceso <- function(TdF, rate){
  TdF - rate
}


#' Intervalo menor
lowInt <- function(minVal, exceso){
  minVal - (exceso/2)
}

#' Intervalo mayor
upperInt <- function(maxVal, exceso){
  maxVal + (exceso/2)
}

#' Numero de intervalos
intNumber <- function(numStorge){
  1:numStorge
}

#' Limite inferior del intervalo
infLimit <- function(lowInt, upperInt, anchoIntervalo){
  seq(from = lowInt, to = (upperInt - anchoIntervalo), by = anchoIntervalo)
}

#' Limite superior del intervalo
supLimit <- function(lowInt, upperInt, anchoIntervalo){
  seq((from = lowInt + anchoIntervalo), to = upperInt, by = anchoIntervalo)
}

#' Frecuencia Absoluta
frecAbsoluta <- function(data){
  row <- nrow(data)
  col <- ncol(data)
  vacio <- numeric(numStorge)
  for(h in 1:numStorge){
    cont = 0
    for(i in 1:row){
      for(j in 1:col){
        if(data[i,j] >= lowInt + (h-1)*anchoIntervalo && x[i,j] < lowInt + (h*anchoIntervalo)){
          cont = cont + 1
          vacio[h] <- cont
        }
      }
    }
  }
  print(vacio)
}

#' Frecuencia Absoluta Acumulada
frecAbsolutaAcum <- function(frecAbsoluta){
  vacio1 <- numeric(numStorge)
  cont1 <- frecAbsoluta[1]
  for(i in 2:length(frecAbsoluta)){
    cont1 <- cont1 + frecAbsoluta[i]
    vacio1[1] <- frecAbsoluta[1]
    vacio1[i] <- cont1
  }
  print(vacio1)
}

#' Frecuencia Relativa
frecRelativa <- function(frecAbsoluta, num){
  frecAbsoluta/num
}

#' Frecuencia Relativa Acumulada
frecRelativaAcum <- function(frecRelativa, num){
  frecRelativa/num
}

#' Construccion final de la tabla

TdFA <- function(){
  data.frame(infLimit, supLimit, frecAbsoluta, frecAbsolutaAcum, frecRelativa,
             frecRelativaAcum)
}
