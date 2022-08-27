#========# Ejemplo propelente #========#
#======== Montgomery et al. 
#======== Reading Data
if(1) {
  #======
  rm(list=ls())

  prob <- "Combustible (Propelente)"
  cat(paste("\n",prob,":\n"))
  
  datos <- read.table('../../datos/propellant.dat',header=TRUE)
  print(datos)
  
  x <- datos$edad
  y <- datos$fuerza
  
  plot(x,y, type="n", xlab="edad", ylab="fuerza", main="Ejemplo propelente")
  points(x,y, col="red", pch=19)
  
  mod1 <- lm(y ~ x)
  print(mod1)
  plot(mod1)
  
  plot(x,y)
  abline(mod1)
  abline(h=0, lty=2)
  
  
  print(mod2 <- lm(fuerza ~ edad, data=datos))
  print(summary(mod2))
  print(anova(mod2))
  
  plot(fitted(mod2), resid(mod2))
  qqnorm(resid(mod2))
#======
}
#======================================

