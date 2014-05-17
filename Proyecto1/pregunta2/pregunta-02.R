#pregunta 01                      

#para correrlo : (solo ale)
#source ("C:/Users/Alessandro/Documents/USB/Estadistica/Proyectos/Proyecto1/pregunta2/pregunta-02.R")

#caragamos la libreria datasets
library(help="datasets")
data(chickwts)
help(chickwts)


#Leer datos de Texas
print ("Seleccione el archivo .csv que corresponde a la data de Texas")
dataT     = read.csv (file.choose(), header = TRUE) #archivo csv de texas
texas7    = dataT[,2]                               #datos del 2007
texas2    = dataT[,3]                               #datos del 2002

#Para datos apareados usamos D = Xi - Yi

mediaVT <- vector()
for ( i in 1:length(texas7) ){
	d       = texas7[i] - texas2[i]
      mediaVT <- c(mediaT, d)
}

mediaT   = mean(mediaVT) #media de los datos apareados de texas
varianza <- var(mediaVT)  #varianza de los datos apareados de texas

#nivel de confianza 95%
intervalo = function(media, varianza, n, I) {
	z     = abs (qnorm( (1 - I)/2 ))
      a     = varianza / sqrt(n)
      error = varianza / sqrt(n)      
      LS    = media + z*error
      LI    = media - z*error
      return (c(LI, LS))
}

print("el intervalo de confianza al 95% es ")
print(intervalo(mediaT, varianza, length(texas7), 0.95))