### --------------------------------- ###
### EJEMPLO DE SELECCIÓN DE VARIABLES ###
### --------------------------------- ###

### Se tiene un modelo que compara el consumo de gasolina con 10 variables
### específicas del diseño de automoviles. Se realizó una muestra de 34 modelos
### entre 1973 y 1974

### Las Variales usadas son:
###	mpg	Miles/(US) gallon
###	cyl	Number of cylinders
###	disp	Displacement (cu.in.)
###	hp	Gross horsepower
###	drat	Rear axle ratio
###	wt	Weight (1000 lbs)
###	qsec	1/4 mile time
###	vs	Engine (0 = V-shaped, 1 = straight)
###	am	Transmission (0 = automatic, 1 = manual)
###	gear	Number of forward gears
###	carb	Number of carburetors


### Se pide ajustar el modelo completo y encontrar si existe el modelo que
### estime de mejor manera la variable respuesta y estimarlos

### CARGAR PAQUETE OLSRR
library(olsrr)

## 1) SE CARGAN LOS DATOS Y SE CORRE EL MODELO
model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
summary(model)
anova(model)

## 2) CRITERIOS PARA EVALUAR MODELOS CON SUBCONJUNTOS DE VARIABLES
# Permite conocer para todos los modelos especificados con las variables inciales
# las medidas de los criterios de evaluación de modelos, para encontrar los posibles
# mejores modelos.

ap<-ols_step_all_possible(model)
ap
plot(ap)

# Permite conocer el mejor modelo para cada uno de los subconjuntos de m variables
bs <- ols_step_best_subset(model, details = TRUE)
bs
plot(bs)

## 3) CRITERIOS COMPUTACIONALES. REGRESIÓN POR PASOS CON TODOS CRITERIOS

## 3.1) Regresión forward con todos los criterios

sfp<-ols_step_forward_p(model)
plot(sfp)
sfpd<-ols_step_forward_p(model, details=TRUE)
sfp

## 3.2) Regresión backward con todos los criterios

sbp<-ols_step_backward_p(model)
plot(sbp)
sbpd<-ols_step_backward_p(model, details=TRUE)
sbp

## 3.3) Regresión por pasos con todos los crierios

sbb<-ols_step_both_p(model)
plot(sbb)
sbbd<-ols_step_both_p(model, details=TRUE)
sbb

## 4) CRITERIOS COMPUTACIONALES. REGRESIÓN POR PASOS CON CRITERIO AIC

## 4.1) Regresión forward con AIC

sfpaic<-ols_step_forward_p(model)
plot(sfpaic)
sfpaicd<-ols_step_forward_p(model, details=TRUE)
sfpaic

## 4.2) Regresión backward con AIC

sbpaic<-ols_step_backward_p(model)
plot(sbpaic)
sbpdaic<-ols_step_backward_p(model, details=TRUE)
sbpdaic

## 4.3) Regresión por pasos AIC

sbbaic<-ols_step_both_p(model)
plot(sbbaic)
sbbdaic<-ols_step_both_p(model, details=TRUE)
sbbdaic


## 5) PARA CADA CRITERIO AJUSTAR EL MEJOR MODELO

## 5.1) El mejor modelo 
## Primer conjunto R2 R2A AIC		1 11 56 176 386 638 848 968 1013 1022
## Segundo conjuntoCP MALLOWS   	  13 69 216 466 719 896 982 
## POR SUBCONJUNTO DE VARIABLES 	1 11 56 176 386 638 848 968 1013 1022
## FORWARD TODOS  11  			cyl wt hp
## BACKWARD TODOS 12  			cyl vs carb gear draft
## STEPWISE TODOS 13  			cyl wt
## FORWARD AIC    14  			cyl wt hp
## BACKWARD AIC   15  			cyl vs carb gear draft
## STEPWISE AIC   16  			cyl wt


##          1         wt                                       
##     	2         cyl wt                                   
##     	3         wt qsec am                               
##     	4         hp wt qsec am                            
##     	5         disp hp wt qsec am                       
##     	6         disp hp drat wt qsec am                  
##     	7         disp hp drat wt qsec am gear             
##     	8         disp hp drat wt qsec am gear carb        
##     	9         disp hp drat wt qsec vs am gear carb     
##	     10         cyl disp hp drat wt qsec vs am gear carb 
##	     11  	    cyl wt hp
## 	     12  	    cyl vs carb gear draft 

model01 <- lm(mpg~wt, data=mtcars)
model02 <- lm(mpg~cyl+wt, data=mtcars)
model03 <- lm(mpg~wt+qsec+am, data=mtcars)
model04 <- lm(mpg~hp+wt+qsec+am,, data=mtcars)
model05 <- lm(mpg~disp+hp+wt+qsec+am, data=mtcars)
model06 <- lm(mpg~disp+hp+drat+wt+qsec+am, data=mtcars)
model07<- lm(mpg~disp+hp+drat+wt+qsec+am+gear, data=mtcars)
model08<- lm(mpg~disp+hp+drat+wt+qsec+am+gear+carb, data=mtcars)
model09<- lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
model10<- lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
model11<- lm(mpg~cyl+hp+wt, data=mtcars)
model12<- lm(mpg~cyl+drat+vs+gear+carb, data=mtcars)

summary(model01)
summary(model02)
summary(model03)
summary(model04)
summary(model05)
summary(model06)
summary(model07)
summary(model08)
summary(model09)
summary(model10)
summary(model11)
summary(model12)

model <- lm(y ~ ., data = surgical)
