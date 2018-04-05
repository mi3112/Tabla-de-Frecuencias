# Tabla-de-Frecuencias

This repository contains an experimental idea to elaborate a package which allows the creation of a frequency table and all the parameters that are needed.

The functions were created based on statics and Doe books

# Using the package
to install the package

>install.package(TablaFrecuencias)

# What can you do with the package?

You can group data in mutually exclusive categories that indicate the number of observations in each category, which provides an added value to the data grouping. The frequency distribution presents the classified observations so that the existing number in each class can be seen.

# decimalnumcount

With this function you can quantify the number of decimals of each data in a database. That will be saved the number of decimal places, since the theory so indicates. Note that the data class must be changed to character so that the code works correctly

> decimalnumcount<-function(data){
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


# Ancho de intervalo

Based on the Storge method, this function returns the length of each interval of a cumulative frequency table

> anchoIntervalo <- function(rate, numStorge, decimalnumcount){
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
