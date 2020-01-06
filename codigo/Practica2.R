# Leer los datos
winequality_red <- read.csv('/home/joni/Garran_Dell_Linux/Data_Science/Tipologia/Tipol_Prac_2/wine.csv')
head(winequality_red)
str(winequality_red)

# Numeros de valores desconocidos por campo
sapply(winequality_red, function(x) sum(is.na(x)))
sapply(winequality_red, function(x) sum(0))

#OUTLIERS
boxplot.stats(winequality_red$fixed.acidity)$out
boxplot.stats(winequality_red$volatile.acidity)$out
boxplot.stats(winequality_red$citric.acid)$out
boxplot.stats(winequality_red$residual.sugar)$out
boxplot.stats(winequality_red$chlorides)$out
boxplot.stats(winequality_red$free.sulfur.dioxide)$out
boxplot.stats(winequality_red$total.sulfur.dioxide)$out
boxplot.stats(winequality_red$density)$out
boxplot.stats(winequality_red$pH)$out
boxplot.stats(winequality_red$sulphates)$out
boxplot.stats(winequality_red$alcohol)$out

#Vector con los nombres de variables independientes
var_indep<-c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol")

#Graficas de outliers
for (i in var_indep){
  boxplot(winequality_red[,i] ~ winequality_red$quality, col='chartreuse3', xlab='Quality', ylab=i, varwidth=T)
}

#Eliminar algunos outliers
outliers <- boxplot.stats(winequality_red$citric.acid)$out
winequality_red <- winequality_red[-which(winequality_red$citric.acid %in% outliers),]

outliers2 <- winequality_red$total.sulfur.dioxide[winequality_red$total.sulfur.dioxide >250]
winequality_red <- winequality_red[-which(winequality_red$total.sulfur.dioxide %in%outliers2),]

#Exportar los datos
write.csv(winequality_red, "winequality_red_limpio.csv")

winequality_red_DataFrame<- data.frame(winequality_red)

#Histogramas
for (i in var_indep){
  hist(winequality_red_DataFrame[,i],breaks = 50,main=i)
}

#Graficas Q-Q
for (i in var_indep){
  qqnorm(winequality_red_DataFrame[,i], main=i)
  qqline(winequality_red_DataFrame[,i],col=2)
}

#Test de normalitat Shapiro-Wilk:
for (i in 1:11) {
  print(shapiro.test(as.numeric( unlist(winequality_red_DataFrame[,i]))))
}

#Correlacion Spearman
for (i in 1:11) {
  print(cor.test(winequality_red_DataFrame[,i],winequality_red_DataFrame$quality,method = 'spearman'))
}

# Regresores cuantitativos con mayor coeficiente
acidity = winequality_red$fixed.acidity
volAcidity = winequality_red$volatile.acidity
citricAcid = winequality_red$citric.acid
resSugar = winequality_red$residual.sugar
chlorides = winequality_red$chlorides
sulfDioxide = winequality_red$free.sulfur.dioxide
totSulfDioxide = winequality_red$total.sulfur.dioxide
density = winequality_red$density
ph = winequality_red$pH
sulphates = winequality_red$sulphates
alcohol = winequality_red$alcohol

# Variable a predecir
calidad = winequality_red$quality

# Generación de varios modelos
modelo1 <- lm(calidad ~ volAcidity + sulphates + alcohol, data = winequality_red) #Vars que influeixen mes sobre quality nomes
modelo2 <- lm(calidad ~ volAcidity + citricAcid + chlorides + totSulfDioxide + density + sulphates + alcohol, data = winequality_red)
modelo3 <- lm(calidad ~ acidity + volAcidity + citricAcid + resSugar + chlorides, data = winequality_red)
modelo4 <- lm(calidad ~ acidity + volAcidity + citricAcid + resSugar + chlorides + sulfDioxide + totSulfDioxide + density + ph, data = winequality_red)
modelo5 <- lm(calidad ~ acidity + volAcidity + citricAcid + resSugar + chlorides + sulfDioxide + totSulfDioxide + density + ph + sulphates + alcohol, data = winequality_red)

# Tabla con los coeficientes de determinación de cada modelo
tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared,
                               2, summary(modelo2)$r.squared,
                               3, summary(modelo3)$r.squared,
                               4, summary(modelo4)$r.squared,
                               5, summary(modelo5)$r.squared),
                             ncol = 2, byrow = TRUE)
colnames(tabla.coeficientes) <- c("Modelo", "R2")
tabla.coeficientes

#Calcular totes les correlacions entre variables
cor(winequality_red,method="spearman")

#Factoritzar la variable 'quality' (necessari per a poder incloure a les gràfiques)
ff <- as.factor(winequality_red$quality)

#Seleccionem les parelles de variables amb correlacions mes grans (>0.60)
#1) total_sulfur vs free_sulfur
fit1 <- lm (totSulfDioxide ~ sulfDioxide, data = winequality_red)
summary(fit1)
plot(sulfDioxide,totSulfDioxide,
     col = winequality_red$quality,
     pch = 16,
     cex = 2,
     ylab = "Total Sulfur",
     xlab = "Free Sulfur",
     main = "Total Sulfur vs Free Sulfur (cor=0.7897)")
abline(fit1)
legend ("topright",legend = levels(ff),col=c(1:6),pch=16)

#2) pH vs acidity
fit2 <- lm (acidity ~ ph, data = winequality_red)
summary(fit2)
plot(ph,acidity,
     col = winequality_red$quality,
     pch = 16,
     cex = 2,
     xlab = "pH",
     ylab = "acidity",
     main = "Acidity vs pH (cor=-0.7067)")
abline(fit2)
legend ("topright",legend = levels(ff),col=c(1:6),pch=16)

#3) Citric.Acid vs acidity
fit3 <- lm (acidity ~ citricAcid, data = winequality_red)
summary(fit3)
plot(citricAcid,acidity,
     col = winequality_red$quality,
     pch = 16,
     cex = 2,
     xlab = "Citric Acid",
     ylab = "acidity",
     main = "Citric Acid vs acidity (cor=0.6617)")
abline(fit3)
legend ("topright",legend = levels(ff),col=c(1:6),pch=16)

#4) Density vs acidity
fit4 <- lm (acidity ~ density, data = winequality_red)
summary(fit4)
plot(density,acidity,
     col = winequality_red$quality,
     pch = 16,
     cex = 2,
     xlab = "Density",
     ylab = "Acidity",
     main = "Acidity vs Density (cor=0.6231)")
abline(fit4)
legend ("topright",legend = levels(ff),col=c(1:6),pch=16)

#5) Citric Acid vs Volatile.acidity
fit5 <- lm (citricAcid ~ volAcidity, data = winequality_red)
summary(fit5)
plot(volAcidity,citricAcid,
     col = winequality_red$quality,
     pch = 16,
     cex = 2,
     ylab = "Citric Acid",
     xlab = "Volatile acidity",
     main = "Citric Acid vs Volatile acidity (cor=-0.6103)")
abline(fit5)
legend ("topright",legend = levels(ff),col=c(1:6),pch=16)



       