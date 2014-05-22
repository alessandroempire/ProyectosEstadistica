require(XLConnect)

# Se leen los datos de la tabla
wb = loadWorkbook("operarios.xlsx")
datos = readWorksheet(wb, sheet = 1)

mujeres_contados <- datos[,"Female"]
hombres_contados <- datos[,"Male"]

# Se calculan los totales de mujeres y hombres
total_mujeres <- sum(mujeres_contados)
total_hombres <- sum(hombres_contados)
total <- total_mujeres + total_hombres

# Se saca la proporcion verdadera
proporcion_verdadera <- total_mujeres/total
 
# Se obtiene una proporcion al azar del numero de mujeres, se calcula el intervalo de confianza de esa proporcion y 
intervalos <- array(0,dim=c(100,2))
contador <- 0
for (i in 1:100) {
	muestra <- sample(1:30, 1)
	intervalo <- prop.test(muestra, 30, conf.level=.88)$conf.int
	if (intervalo[1] <= proporcion_verdadera && proporcion_verdadera <= intervalo[2]) {
		contador <- contador + 1
	}
}

cat("El porcentaje de intervalos que contienen la proporcion verdadera es: ", contador,"%\n")
