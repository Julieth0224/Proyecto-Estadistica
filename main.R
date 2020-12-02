rm(list=ls())


setwd('E:/Programming/Statistics/Proyecto')
table = read.csv("diabetes.csv",header = TRUE) # Se lee un comma separated value file. con la información



#########################
#  Análisis Descriptivo #
#########################


summary(table$Edad) #En su mayoría, se esta trabajando con personas mayores
boxplot(table$Edad) #Se visualiza con el boxplot
hist(table$Edad, main = "Histograma de Edad", 
     xlab = "Edad", ylab = "Frecuencia", col = "mediumpurple1")

summary(table$Peso)
boxplot(table$Peso)
hist(table$Peso, main = "Histograma de Peso", 
     xlab = "Peso", ylab = "Frecuencia", col = "seagreen1")

summary(table$IMC) #Da la impresión de que La mayoria de los datos presentan sobrepeso
boxplot(table$IMC)
hist(table$IMC, main = "Histograma de IMC", 
     xlab = "IMC", ylab = "Frecuencia", col = "indianred1")

summary(table$Tension.SISTOLICA) 
boxplot(table$Tension.SISTOLICA)
hist(table$Tension.SISTOLICA, main = "Histograma de Tension Sistolica", 
     xlab = "Tension Sistolica", ylab = "Frecuencia", col = "chocolate")

summary(table$Colesterol.Total) 
boxplot(table$Colesterol.Total)
hist(table$Colesterol.Total, main = "Histograma de Colesterol", 
     xlab = "Colesterol", ylab = "Frecuencia", col = "lavender")

summary(table$Glicemia.de.ayuno)
boxplot(table$Glicemia.de.ayuno)
hist(table$Glicemia.de.ayuno, main = "Histograma de Glicemia de Ayuno", 
     xlab = "Glicemia", ylab = "Frecuencia", col = "lightseagreen")

summary(table$Hemoglobina.A1C)
boxplot(table$Hemoglobina.A1C)
hist(table$Hemoglobina.A1C, main = "Histograma de Hemoglobina", 
     xlab = "Hemoglobina", ylab = "Frecuencia", col = "paleturquoise")

summary(table$Creatinina)
boxplot(table$Creatinina)
hist(table$Creatinina, main = "Histograma de Creatinina", 
     xlab = "Creatinina", ylab = "Frecuencia", col = "lawngreen")

summary(table$Perimetro.Abdominal)
boxplot(table$Perimetro.Abdominal)
hist(table$Perimetro.Abdominal, main = "Histograma de Perimetro Abdominal", 
     xlab = "Perimetro Abdominal", ylab = "Frecuencia", col = "khaki1")


#############################
# Estimación de Párametros  #
#############################

# Diferencia entre promedio poblacional de peso entre diabeticos tipo 1 y 2
indexes_tipo1 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 1)
indexes_tipo2 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 2)


peso_tipo1 = mean(table$Peso[indexes_tipo1])
peso_tipo2 = mean(table$Peso[indexes_tipo2])

diff_pesos_por_tipo = peso_tipo1 - peso_tipo2 # La diferencia de pesos, es minima segun la evidencia



#Intervalos de confianza de la hemoglobina glicosilada por edad
#Entre 40 y 50 años
alpha = 0.05
hemogoblina = subset(table, !is.na(Hemoglobina.A1C) & Edad >= 40 & Edad < 50 ,select=c(Hemoglobina.A1C))
hemogoblina = hemogoblina$Hemoglobina.A1C

media = mean(hemogoblina)
S = sd(hemogoblina)
n = length(hemogoblina)
longitud = qt(1-alpha/2,df=n-1)*S/sqrt(n)
media - longitud #Inferior
media + longitud #Superior

# Entre 50 y 60 años
hemogoblina = subset(table, !is.na(Hemoglobina.A1C) & Edad >= 50 & Edad < 60 ,select=c(Hemoglobina.A1C))
hemogoblina = hemogoblina$Hemoglobina.A1C

media = mean(hemogoblina)
S = sd(hemogoblina)
n = length(hemogoblina)
longitud = qt(1-alpha/2,df=n-1)*S/sqrt(n)
media - longitud #Inferior
media + longitud #Superior

#Mayor a 60 años
hemogoblina = subset(table, !is.na(Hemoglobina.A1C) & Edad >= 60 ,select=c(Hemoglobina.A1C))
hemogoblina = hemogoblina$Hemoglobina.A1C

media = mean(hemogoblina)
S = sd(hemogoblina)
n = length(hemogoblina)
longitud = qt(1-alpha/2,df=n-1)*S/sqrt(n)
media - longitud #Inferior
media + longitud #Superior
media
S
n


########################
# Pruebas de Hipótesis #
########################

# La mayoria de los diabeticos tipo 2 son obesos?
indexes_tipo2 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 2)
n = length(indexes_tipo2)
#Queremos ver si hay la mayoria de diabeticos tipo2  son obesos, luego consideramos el caso extremo
#Consideramos con la hipotesis nula en el que se espera igual cantidad de obesos, como no obesos, p = 0.5 (probabilidad de ser obeso, con diabetes tipo 2)

#Se considera obeso cuando IMC supera los 30
obesos = length(which(table$IMC[indexes_tipo2] >= 30))
no_obesos = length(which(table$IMC[indexes_tipo2] < 30))

p0 = 0.5
p_barra = obesos/n
Z = (p_barra - p0)/sqrt(p0*(1-p0)/n)


#Se obtiene Z de -10.96, mostrando con total certeza que NO la mayoria de los diabeticos tipo 2 son obesos.


#Si se repite, considerando el sobrepeso, y no la obesidad
sobrepeso = length(which(table$IMC[indexes_tipo2] >= 25))

p0 = 0.5
p_barra = sobrepeso/n
Z = (p_barra - p0)/sqrt(p0*(1-p0)/n)

pnorm(Z,lower.tail =FALSE)

#Se obtiene valor p < 0.001 con alto nivel de significancia
#Mostrando así que en efecto, la mayoría de los diabeticos tipo 2 tienen sobrepeso








# Repitiendo la prueba con tipo 1

indexes_tipo1 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 1)
n = length(indexes_tipo1)
#Se considera obeso cuando IMC supera los 30
obesos = length(which(table$IMC[indexes_tipo1] >= 30))
no_obesos = length(which(table$IMC[indexes_tipo1] < 30))

p0 = 0.5
p_barra = obesos/n
Z = (p_barra - p0)/sqrt(p0*(1-p0)/n)

#Se obtiene Z de -5.96, mostrando con total certeza que NO la mayoria de los diabeticos tipo 2 son obesos.
# Si algo, son menores


#Si se repite, considerando el sobrepeso, y no la obesidad
sobrepeso = length(which(table$IMC[indexes_tipo1] >= 25))

p0 = 0.5
p_barra = sobrepeso/n
Z = (p_barra - p0)/sqrt(p0*(1-p0)/n)

pnorm(Z,lower.tail =FALSE)

# Se obtiene un valor p < 0.001, indicando que la mayoría de diabeticos tipo 1 tienen sobrepeso

#De esta manera, independiente del tipo de de diabetes, la mayoría tienen sobrepeso y contrario a 
# como se puede creer, muy pocos presentan obesidad


# Entonces surge la pregunta si existe alguna diferencia entre pesos, y se realiza una prueba de hipótesis comparando
n1 = length(which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 1))
n2 = length(which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 2))

indexes_tipo1 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 1)
indexes_tipo2 = which(table$Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia == 2)

u1 = mean(table$Peso[indexes_tipo1])
u2 = mean(table$Peso[indexes_tipo2])
#N grande, entonces se pueden aproximar sigma^2 con S^2
S1_cuadrado = sd(table$Peso[indexes_tipo1])^2
S2_cuadrado = sd(table$Peso[indexes_tipo2])^2

Z = (u1 - u2) / sqrt(S1_cuadrado/n1 + S2_cuadrado/n2)

pnorm(Z)
# Se obtiene p de 0.51 al considerar H0 = u1 -u2 = 0
# Es decir, la evidencia indica, que no hay diferencia en el peso según la diabetes tipo 1 y 2


#############################
# Posibles Modelos Lineales #
#############################

# Existe alguna relación entre peso y la glicemia en ayunos(La evidencia indica que No!)

model = lm(table$Glicemia.de.ayuno~table$Peso)
plot(table$Peso,table$Glicemia.de.ayuno,main="Scatterplot", xlab="Peso", ylab="Glicemia en Ayunas", ylim=c(0,400))
summary(model)
abline(model, col = "darkmagenta", lwd = 2)

# Generalmente hay una relación entre la hemogoblina glicosilada y la microalbuminuria
model = lm(table$Microalbuminuria~table$Hemoglobina.A1C)
plot(table$Hemoglobina.A1C,table$Microalbuminuria,main="Scatterplot", xlab="Hemoglobina", ylab="Microalbuminuria", ylim=c(0,100))
summary(model)
abline(model, col = "darkblue", lwd = 2)

#Hay una relación con nivel de significancia alta entre Glicemia de ayuno y el perimetro abdominal
plot(table$Glicemia.de.ayuno,table$Perimetro.Abdominal, main="Scatterplot", xlab="Glicemia en Ayunas", ylab="Perimetro Abdominal")
model = lm(table$Perimetro.Abdominal~table$Glicemia.de.ayuno)
summary(model)
abline(model, col = "seagreen", lwd = 2)

#Hay una relación con nivel de significancia alta entre Glicemia de ayuno y creatinina
plot(table$Glicemia.de.ayuno,table$Creatinina,main="Scatterplot", xlab="Glicemia en Ayunas", ylab="Creatinina")
model = lm(table$Creatinina~table$Glicemia.de.ayuno)
summary(model)
abline(model, col = "darkred", lwd = 2)

#Estos posibles modelos lineales, la evidencia indica ser falsos, y no se pueden establecer relaciones lineales directas entre ellos.



