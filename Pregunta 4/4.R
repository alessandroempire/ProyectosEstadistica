#pregunta 2.a y 2.b

# Muestras de Entidades federales de:

# Aragua

a = read.table("~/Dropbox/Estadistica/Proyecto1 Ene-Mar14/aramus_5.txt")
aragua = a[,1]

# Distrito Capital
d = read.table("~/Dropbox/Estadistica/Proyecto1 Ene-Mar14/dcmus_5.txt")
dc = d[,1]

# Miranda
m = read.table("~/Dropbox/Estadistica/Proyecto1 Ene-Mar14/mirmus_5.txt")
miranda = m[,1]

# Zulia
z = read.table("~/Dropbox/Estadistica/Proyecto1 Ene-Mar14/zulmus_5.txt")
zulia = z[,1]


#Funcion de coeficiente de varianza
co.var <- function(x) (sd(x)/mean(x) )

var1 = co.var(aragua)
print (var1)
var2 = co.var(dc)
print (var2)
var3 = co.var(miranda)
print (var3)
var4 = co.var(zulia)
print (var4)



#Parte 2B

#Creamos las muestras necesarias
aragua1<- numeric(0)
dc1<-numeric(0)
miranda1<-numeric(0)
zulia1<-numeric(0)

for(i in 1:length(aragua)) {
	if (aragua[i] > 3) {
		aragua1<- c(aragua1, aragua[i])
	}
}

for(i in 1:length(dc)) {
	if (dc[i] > 3) {
		dc1<-c(dc1, dc[i])
	}
}

for(i in 1:length(miranda)) {
	if (miranda[i] > 3) {
		miranda1<-c(miranda1, miranda[i])
	}
}

for(i in 1:length(zulia)) {
	if (zulia[i] > 3) {
		zulia1<-c(zulia1, zulia[i])
	}
}

# Intervalo de confianza a nivel de confianza del 88%
t.test(aragua1,conf.level = 0.88)
t.test(dc1,conf.level = 0.88)
t.test(miranda1,conf.level = 0.88)
t.test(zulia1,conf.level = 0.88)


#Funcion de coeficiente de varianza
co.var <- function(x) (sd(x)/mean(x) )

var1 = co.var(aragua)
print (var1)
var2 = co.var(dc)
print (var2)
var3 = co.var(miranda)
print (var3)
var4 = co.var(zulia)
print (var4)

#Histogramas 
png(filename ="Aragua.png")
hist(aragua, main = "Aragua",xlab = "Hijos", ylab = "Frecuencia")
dev.off()

png(filename ="DistCap.png")
hist(dc, main = "Distrito Capital", xlab = "Hijos",  ylab = "Frecuencia")
dev.off()

png(filename ="Miranda.png")
hist(miranda, main = "Miranda",xlab = "Hijos",  ylab = "Frecuencia")
dev.off()

png(filename ="Zulia.png")
hist(zulia, main = "Zulia", xlab = "Hijos", ylab = "Frecuencia")
dev.off()

