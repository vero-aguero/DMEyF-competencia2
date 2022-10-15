#Necesita para correr en Google Cloud
# 32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread( "./datasets/competencia2_2022.csv.gz")

#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/FE7110/", showWarnings = FALSE )
setwd("./exp/FE7110/")   #Establezco el Working Directory DEL EXPERIMENTO

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

#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
#infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
#infinitos_qty  <- sum( unlist( infinitos) )
#if( infinitos_qty > 0 )
#{
#  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
#  dataset[mapply(is.infinite, dataset)] <- NA
#}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
#nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
#nans_qty  <- sum( unlist( nans) )
#if( nans_qty > 0 )
#{
#  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
#  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
#  dataset[mapply(is.nan, dataset)] <- 0
#}




#--------------------------------------
#grabo el dataset
fwrite( dataset,
        "dataset_myFE_v2.csv.gz",
        logical01= TRUE,
        sep= "," )
