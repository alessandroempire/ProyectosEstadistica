#pregunta 01                      

#para correrlo : (solo ale)
#source ("C:/Users/Alessandro/Documents/USB/Estadistica/Proyecto1/pregunta1/pregunta-01.R")

#Parte a : lleve una descripcion estadistica completa para cada muestra.

#Definimos
coVar     <- function(x) ( sd(x)/mean(x) )          #funcion de coeficiente de varianza
par(mfrow = c(2,2))                                 #interfaz grafica
pdf( "pregunta01.pdf" , 7 , 5)                         
                                                    

#Leer datos de Nebraska
print ("Seleccione el archivo .csv que corresponde a la data de Nebraska")
dataN     = read.csv (file.choose(), header = TRUE) #archivos csv de nebraska
nebraska  = dataN[,2]                               #datos del 2007
analysisN = summary(nebraska)                       #analisis descriptivo de cada var
print  ("analisis descriptivo de nebraska")
print  (analysisN)
desStdN   = sqrt(var(nebraska))                     #desviacion estandar de nebraska
print  ("desviacion estandar de nebraska")
print  (desStdN)
coeffVarN = coVar(nebraska)                         #coeficiente de varianza
print  ("coeficiente de varianza de nebraska")
print  (coeffVarN)

boxplot( nebraska                                   #boxplot de los datos de nebraska
        ,main = "Boxplot de Nebraska" )

hist   ( nebraska                                   #histograma de los datos de nebraska
        ,xlab = "Hijos"
        ,ylab = "Frecuencia" )


#Leer datos de Texas
print ("Seleccione el archivo .csv que corresponde a la data de Texas")
dataT     = read.csv (file.choose(), header = TRUE) #archivo csv de texas
texas     = dataT[,2]                               #datos del 2007
analysisT = summary(texas)                          #analisis descriptivo de cada var
print ("analisis descriptivo de texas")
print (analysisT)
desStdT   = sqrt(var(texas))                        #desviacion estandard de texas
print ("desviacion estandar de texas")
print (desStdT)
coeffVarT = coVar(texas)                            #coeficiente de varianza
print ("coeficiente de varianza de texas")
print (coeffVarT)
boxplot(texas, main = "Boxplot de Texas")           #boxplot de los datos de texas 
hist   ( texas                                      #histograma de los datos de texas
        ,xlab = "Hijos"
        ,ylab = "Frecuencia" )
 
dev.off()                                           #cerramos el PDF