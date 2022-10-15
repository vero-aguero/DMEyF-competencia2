##
## Sobre El Futuro
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## The future is not something to predict. The future is something to build.
## --- Franco Ongaro

# Profundizaremos en los puntos de corte.
# IMPORTANTE: En esta competencia se puede entrenar usando Marzo. Sin embargo,
# vamos aprovechar (y recomendar que usted también lo haga) Enero para
# experimentar contra Marzo.
# Data drift -> todas las variables que están expresadas en $ tienen data drift
# Veo la estructura de una variable, si veo que muta  puedo tener presencia de data drift
# Concept drift -> Cambian las estructuras, cambia la relación entre las variables objetivo y las variables
# Cambia el peso de la variable.

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("/Users/vaguero/maestria/DMEyF2022/")
# Poner sus semillas
semillas <- c(318601,878131,955649,644141,751057)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#INICIO de la seccion donde se deben hacer cambios con variables nuevas

#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

#variable extraida de una tesis de maestria de Irlanda
dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
dataset[ , mv_status02       := Master_status +  Visa_status ]
dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 10, Master_status), 
                                        Visa_status)  ]

dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 10, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

#Aqui debe usted agregar sus propias nuevas variables

#--------------------------Inicio mis variables-------------------------------------------------
#Aqui debe usted agregar sus propias nuevas variables

dataset[ , tasa_rentabilidad := mrentabilidad / mrentabilidad_annual ]

#Activos vs pasivos. (idea en clase)
#Se definen como activos, saldo de la cuenta, plazos fijos, inversiones.
#Se definen como pasivos a todos los gastos realizados con tarjetas, prestamos adquiridos, comisiones.
dataset[ , activos := rowSums( cbind( mcuentas_saldo, mplazo_fijo_pesos, mplazo_fijo_dolares, minversion1_pesos, minversion1_dolares, minversion2 ), na.rm = T ) ]
dataset[ , pasivos := rowSums( cbind( Visa_msaldototal, Master_msaldototal, mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios, mcomisiones_mantenimiento, mcomisiones_otras), na.rm = T ) ]
#Balance entre activos y pasivos
dataset[ , balance := activos - pasivos ]
# Activos vs pasivos. Si activos es 0 va 1
dataset[ , activos_vs_pasivos := ifelse ( activos == 0, pasivos / 1, pasivos / activos ) ]

#Analizamos si tiene alguna tarjeta de crédito
dataset[ , tiene_visa := ifelse( ctarjeta_visa > 0, 1 ,0 ) ]
dataset[ , tiene_master := ifelse( ctarjeta_master > 0, 1 ,0 ) ]
#Cantidad de tarjetas
dataset[ , num_tarjetas := rowSums( cbind ( ctarjeta_visa, ctarjeta_master ), na.rm = T ) ]
#Operaciones con las tarjetas de crédito, entendiendo o suponiendo que si usa tarjeta es un elemento más para asegurar su continuidad en el banco
dataset[ , usa_visa := ifelse ( ctarjeta_visa_transacciones > 0, 1, 0) ]
dataset[ , usa_master := ifelse ( ctarjeta_master_transacciones > 0, 1, 0) ]
#Revisamos si es un cliente activo con las tarjetas de credito
dataset[ , cliente_activo_en_tarjetas := ifelse ( ( ctarjeta_visa_transacciones + ctarjeta_master_transacciones ) > 0, 1, 0) ]

#Analizamos pagos en las tarjetas
dataset[ , total_saldo := rowSums( cbind( Visa_msaldototal, Master_msaldototal ) , na.rm=T ) ]
dataset[ , total_pagado := rowSums ( cbind ( Visa_mpagado, Master_mpagado ), na.rm=T )]
dataset[ , pagado_vs_saldo := ifelse ( total_saldo == 0, 0, total_pagado / total_saldo)]

#Limites de las tarjetas
dataset[ , total_limitecompra := rowSums( cbind( Visa_mlimitecompra, Master_mlimitecompra ) , na.rm=T ) ]
#Limite de compra vs háberes recibidos. num_tarjetas puede ser 0!!
dataset[ , consumo_tarjeta_promedio := ifelse( num_tarjetas == 0, 0, total_limitecompra / num_tarjetas ) ]

#total de ingresos
dataset[ , total_payroll := rowSums( cbind  (mpayroll, mpayroll2 ), na.rm=T )]

#Limite de compras de tarjetas vs ingresos (sueldo) (idea en clase). total_payroll puede ser 0!
dataset[ , limitecompra_vs_ingresos := ifelse( num_tarjetas == 0, 0, total_limitecompra / total_payroll ) ]

#--------------------------Fin mis variables-------------------------------------------------


enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "CONTINUA", 0, 1)
enero$clase_ternaria <- NULL

## ---------------------------
## Step 2: Un modelo simple de LGBM
## ---------------------------

# Armamos el dataset de train para LGBM
dtrain  <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

model_lgm <- lightgbm(data = dtrain,
            nrounds = 100,
            params = list(objective = "binary",
                          max_bin = 31,
                          min_data_in_leaf = 1721,
                          learning_rate = 0.00500244938678657,
                          num_leaves = 234,
                          feature_fraction = 0.242688769836711,
                          num_iterations = 926),
             verbose = -1)

## ---------------------------
## Step 3: Veamos como funcionó en Marzo
## ---------------------------
#cuantos casos tengo en marzo
marzo$pred <- predict(model_lgm, data.matrix(marzo[, 1:217]))
sum((marzo$pred > 0.025) * ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))

## ---------------------------
## Step 4: Veamos cuán distintos los scores entregados
## ---------------------------
#cuantas predicciones distintas me dio lightgbm
length(marzo$pred)
length(unique(marzo$pred))
#a partir de esto se desprende -> punto de corte

## Preguntas
## - ¿Qué diferencia observa con respecto a ?

## ---------------------------
## Step 4: En el leaderboard público.
## ---------------------------

# Simulamos un Leaderboard público:
set.seed(semillas)
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)

# Vemos la cantidad de casos que estaríamos mandando:clase_ternaria
sum(marzo$pred > 0.025) # En mi caso dice que estaría mandando 7744

# Y obtendríamos una ganancia de
# Privado
sum((marzo$pred[split] > 0.025) * ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.5

# Público
sum((marzo$pred[-split] > 0.025) * ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.5

# Pero... que pasa si mandamos otra cantidad de casos?
# Vamos a mandar los N mejores casos, de a separaciones de M

## ---------------------------
## Step 4: Buscando el mejor punto de corte en el leaderboard público.
## ---------------------------

# Ordenamos el dataset segun su probabilidad de forma ascendente
setorder(marzo, cols = -pred)

# PROBAR MULTIPLES VALORES
set.seed(semillas[5])
m <- 500
f <- 2000
t <- 12000

leaderboad <- data.table()
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)
marzo$board[split] <- "privado"
marzo$board[-split] <- "publico"
for (s in seq(f, t, m)) {
    privado <- marzo[1:s, sum(ifelse(board == "privado",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    publico <- marzo[1:s, sum(ifelse(board == "publico",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    leaderboad <- rbindlist(list(leaderboad,
                        data.table(envio = s, board = "privado", valor = privado),
                        data.table(envio = s, board = "publico", valor = publico)
                        ))
}
# Graficamos
ggplot(leaderboad, aes(x = envio, y = valor, color = board)) + geom_line()

## ACTIVE LEARNING: Juegue con los parámetros y busque si hay alguna información
## en el leaderboard público que le de una estrategia para elegir la cantidad
## adecuada para ganar maximizar la ganancia del privado.
#Máximo del mínimo local

