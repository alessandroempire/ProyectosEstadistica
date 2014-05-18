require(XLConnect)

# Se leen los datos de la tabla
wb = loadWorkbook("operarios.xlsx")
datos = readWorksheet(wb, sheet = 1)

# 
mujeres_contados <- datos[,"Female"]
hombres_contados <- datos[,"Male"]
total_mujeres <- sum(mujeres_contados)
total <- total_mujeres + sum(hombres_contados)


intervalos <- array(0,dim=c(100))
for (i in 1:100) {
	muestra <- sample(mujeres_contados, 30)
	intervalo <- t.test(muestra, conf.level=.88)$conf.int
	intervalos[i] <- intervalo
}

print(intervalos)
