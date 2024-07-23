library(stats) #Instalar i carregar paquet.
library(tibble) #Instalar i carregar paquet.
library(forecast) #Instalar i carregar paquet.

ventana <- 12
#ventana <- 24
###################################################
##Funcio per escollir el model ARIMA més adequat.## 
###################################################
model_arima <- function(datos_train, ventana, datos_test) {
  resultats <- list()
  
  frecuencia <- 7 #Definir la freqëncia de la serie temporal (7 dies).
    
  for (d in 0:2) {
    for (p in 0:2) {
      for (q in 0:2) {
        for (D in 0:1) {  
          for (P in 0:1) {  
            modelo <- tryCatch(
              arima(datos_train, order = c(p, d, q), seasonal = list(order = c(P, D, 0), period = frecuencia)),
              error = function(e) NULL
              )
              
            if (!is.null(modelo)) {
              prediccion <- tryCatch(
                predict(modelo, n.ahead = ventana),
                error = function(e) NULL
              )
                
              if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
                mse <- mean((datos_test[1:ventana] - prediccion$pred)^2)  # Calcular MSE con datos_test
                sd_errores <- sd(datos_test[1:ventana] - prediccion$pred) 
                resultats[[length(resultats) + 1]] <- list(
                  ARIMA = paste("ARIMA(", p, ",", d, ",", q, ")", sep = ""),
                  Seasonal = paste("SARIMA(", P, ",", D, ",0)", sep = ""),
                  MSE = mse,
                  SD_Errores = sd_errores,
                  AIC = AIC(modelo),
                  BIC = BIC(modelo)
                )
              }
            }
          }
        }
      }
    }
  }
    
  return(resultats)
}


#################################
##Importem dades Ibex-35 diàries i apliquem la funcio.
#################################
datos<-c(8792.5,8906.1,8944.3,9070.7,9207.1,9232.5,9157.4,9183.2,9254.6,9312.3,9237.7,9278.7,9310,9362.9,9378.5,9421.9,9494.8,9450.9,9415.6,9406.4,9290.3,9293.7,9314.6,9241,9082,9076.7,9043.6,9147.3,9211.3,9183.2,9167.7,9182.8,9234.10,9201.5,9191.4,9211.6,9213.1,9251.5,9305,9267,9163.5,9116.1,9191.1,9180.1,9167.5,9050.2,9167.5,9317.3,9289.1,9310.8,9359.8,9338.3,9309.7,9344.4,9333.7,9432.8,9430.8,9495,9431.9,9439.8,9436.4,9364.7,9265.8,9274,9393,9481.3,9510.6,9593,9644.8,9588.4,9486.3,9285.00,9248.8,9252.9,9331.1,9453.7,9478.7,9438.3,9438,9455.7,9451.8,9519.6,9571.5,9543.5,9519.2,9600.5,9694.7,9685.1,9641.5,9502.9,9328.7,9307.1,9368.4,9358.6,9301.8,9354.4,9502.2,9434.3,9429.6,9347.5,9350.5,9278,9267.7,9262.8,9314,9315.6,9324.7,9338.9,9490.1,9581.2,9551.1,9505.9,9449.6,9416.3,9392,9314.4,9310,9364.6,9435.2,9455.4,9424.1,9549,9549.7,9482.1,9527.2,9645.8,9548.9,9502,9386,9366.9,9331.9,9426.8,9428,9319,9165.5,9102.9,9157.7,9235.8,9151.3,9352.1,9360.4,9336,9232.9,9287.1,9298.5,9212.7,9146.8,9029.1,8995.5,8975.8,8984.8,8962.8,8918.3,9013.9,9017.3,9075,9260.4,9293.9,9241.5,9235.9,9283.8,9405.2,9371.7,9461.7,9623.3,9640.7,9667.4,9761.4,9839,9827.5,9887.4,9905.5,9939,9936.1,10003.4,10062.6,10058.2,10140.8,10178.3,10238.4,10258.1,10146,10223.4,10198,10118.7,10096.1,10171.7,10095.6,10054.9,10106.7,10101,10104.3,10111.9,10121.8,10086.2,10102.1,10182.1,10053.4,10182.4,10164.5,10209,10060.3,10067.1,10004.9,10094.8,10076.9,9994.1,9867.8,9880.3,9858.3,9968.1,9859.2,9974,9916.6,9936.6,9890.3,10039.3,10077.7,10014,10062.5,9941.3,10003,9888.2,9905.4,9896.6,9984.7,9925.4,9916.6,9927.3,9886.4,9944.8,10038.2,10107.2,10138.9,10130.6,10138.4,10113.8,10068.6,10001.3,10064.7,10069.8,10117.1,10197.2,10319.6,10305.7,10325.7,10388.9,10560.5,10490.5)
plot(datos)
datos_train<-c(8792.5, 8906.1, 8944.3, 9070.7, 9207.1, 9232.5, 9157.4, 9183.2, 9254.6, 9312.3, 9237.7, 9278.7, 9310, 9362.9, 9378.5, 9421.9, 9494.8, 9450.9, 9415.6, 9406.4, 9290.3, 9293.7, 9314.6, 9241, 9082, 9076.7, 9043.6, 9147.3, 9211.3, 9183.2, 9167.7, 9182.8, 9234.1, 9201.5, 9191.4, 9211.6, 9213.1, 9251.5, 9305, 9267, 9163.5, 9116.1, 9191.1, 9180.1, 9167.5, 9050.2, 9167.5, 9317.3, 9289.1, 9310.8, 9359.8, 9338.3, 9309.7, 9344.4, 9333.7, 9432.8, 9430.8, 9495, 9431.9, 9439.8, 9436.4, 9364.7, 9265.8, 9274, 9393, 9481.3, 9510.6, 9593, 9644.8, 9588.4, 9486.3, 9285, 9248.8, 9252.9, 9331.1, 9453.7, 9478.7, 9438.3, 9438, 9455.7, 9451.8, 9519.6, 9571.5, 9543.5, 9519.2, 9600.5, 9694.7, 9685.1, 9641.5, 9502.9, 9328.7, 9307.1, 9368.4, 9358.6, 9301.8, 9354.4, 9502.2, 9434.3, 9429.6, 9347.5, 9350.5, 9278, 9267.7, 9262.8, 9314, 9315.6, 9324.7, 9338.9, 9490.1, 9581.2, 9551.1, 9505.9, 9449.6, 9416.3, 9392, 9314.4, 9310, 9364.6, 9435.2, 9455.4, 9424.1, 9549, 9549.7, 9482.1, 9527.2, 9645.8, 9548.9, 9502, 9386, 9366.9, 9331.9, 9426.8, 9428, 9319, 9165.5, 9102.9, 9157.7, 9235.8, 9151.3, 9352.1, 9360.4, 9336, 9232.9, 9287.1, 9298.5, 9212.7, 9146.8, 9029.1, 8995.5, 8975.8, 8984.8, 8962.8, 8918.3, 9013.9, 9017.3, 9075, 9260.4, 9293.9, 9241.5, 9235.9, 9283.8, 9405.2, 9371.7, 9461.7, 9623.3, 9640.7, 9667.4, 9761.4, 9839, 9827.5, 9887.4, 9905.5, 9939, 9936.1, 10003.4, 10062.6, 10058.2, 10140.8, 10178.3, 10238.4, 10258.1, 10146, 10223.4, 10198, 10118.7, 10096.1, 10171.7, 10095.6, 10054.9, 10106.7, 10101, 10104.3, 10111.9, 10121.8, 10086.2, 10102.1, 10182.1, 10053.4, 10182.4)
datos_test<-c(10164.5, 10209, 10060.3, 10067.1, 10004.9, 10094.8, 10076.9, 9994.1, 9867.8, 9880.3, 9858.3, 9968.1, 9859.2, 9974, 9916.6, 9936.6, 9890.3, 10039.3, 10077.7, 10014, 10062.5, 9941.3, 10003, 9888.2, 9905.4, 9896.6, 9984.7, 9925.4, 9916.6, 9927.3, 9886.4, 9944.8, 10038.2, 10107.2, 10138.9, 10130.6, 10138.4, 10113.8, 10068.6, 10001.3, 10064.7, 10069.8, 10117.1, 10197.2, 10319.6, 10305.7, 10325.7, 10388.9, 10560.5, 10490.5)
datos_suavizados<-c(8976.65, 9009.03, 9034.44, 9055.59, 9073.98, 9120.75, 9164.95, 9203.17, 9231.97, 9258.51, 9287.33, 9316.64, 9344.66, 9369.58, 9390.36, 9405.11, 9411.96, 9409.03, 9394.15, 9367.77, 9330.35, 9282.35, 9236.64, 9205.37, 9183.15, 9164.6, 9150.82, 9143.19, 9143.1, 9151.93, 9171.69, 9191.95, 9202.32, 9209.88, 9218.44, 9225.29, 9227.74, 9223.09, 9216.31, 9211.78, 9204.02, 9187.6, 9172.75, 9173.03, 9182.62, 9195.71, 9215.47, 9237.39, 9256.96, 9279.48, 9310.53, 9340, 9357.8, 9373.19, 9390.88, 9406.61, 9416.11, 9415.1, 9408.5, 9400.93, 9397.02, 9401.38, 9409.34, 9420.78, 9435.58, 9453.6, 9470.23, 9471.64, 9463.95, 9453.3, 9438.33, 9427, 9414.43, 9395.75, 9377.75, 9371.77, 9389.13, 9421.22, 9451, 9468.9, 9476.89, 9489.79, 9515.06, 9548.46, 9575.39, 9581.21, 9568.1, 9546.68, 9527.56, 9509.79, 9486.48, 9458.09, 9425.07, 9398.29, 9387.67, 9386.38, 9387.55, 9384.33, 9373.58, 9358.13, 9340.79, 9324.39, 9311.94, 9307.36, 9314.58, 9337.52, 9367.59, 9392.51, 9414.26, 9434.83, 9448.34, 9448.7, 9440.9, 9429.9, 9413.3, 9401.19, 9403.68, 9416.56, 9429.98, 9441.96, 9464.44, 9498.29, 9524.92, 9533.11, 9525.75, 9505.69, 9481.55, 9467.55, 9457.39, 9430.88, 9384.4, 9333.85, 9295.08, 9269.9, 9259.59, 9259.71, 9252.19, 9239.5, 9238.34, 9248.8, 9261.68, 9267.8, 9263.35, 9245.05, 9209.61, 9167.38, 9130.14, 9095.43, 9060.8, 9033.05, 9017.89, 9015.62, 9026.54, 9050.96, 9082.86, 9114.82, 9149.92, 9191.24, 9236.79, 9285.67, 9337.02, 9389.94, 9442.02, 9497.11, 9559.11, 9621.38, 9679, 9732.15, 9780.99, 9825.7, 9866.47, 9905.04, 9943.18, 9982.62, 10022.8, 10061.6, 10096.6, 10125.7, 10148.6, 10164.7, 10173.9, 10175.8, 10170.3, 10159.9, 10146.8, 10133.3, 10121.8, 10112.6, 10106.2, 10102.8, 10102.4, 10104.2, 10107.7, 10112.2, 10117, 10120.3, 10123.9, 10127.5, 10130.4)


resultats_arima <- model_arima(datos_train, ventana, datos_test)
print(resultats_arima)


#################################
#Passem els resultats a una taula. 
#################################
df_resultats <- do.call(rbind, lapply(resultats_arima, function(x) as.data.frame(t(unlist(x)))))

df_resultats$Modelo <- paste0("Modelo", seq_len(nrow(df_resultats)))

df_resultats <- df_resultats[, c(ncol(df_resultats), 1:(ncol(df_resultats)-1))]

print(df_resultats)

#Busquem els models que tinguen millor error, AIC, BIC i desviació.
mejor_modelo_mse <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$MSE))]]
mejor_modelo_aic <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$AIC))]]
mejor_modelo_bic <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$BIC))]]
mejor_modelo_sd_errores <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$SD_Errores))]]
print(mejor_modelo_mse)
print(mejor_modelo_aic)
print(mejor_modelo_bic)
print(mejor_modelo_sd_errores)

#################################
#Grafiquem els resultats. 
#################################
plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")


frecuencia <- 7 #Definir la freqëncia de la serie temporal (7 dies).

for (d in 0:2) {
  for (p in 0:2) {
    for (q in 0:2) {
      for (D in 0:1) {  
        for (P in 0:1) {  
          modelo <- tryCatch(
            arima(datos_train, order = c(p, d, q), seasonal = list(order = c(P, D, 0), period = frecuencia)),
            error = function(e) NULL
          )
          
          if (!is.null(modelo)) {
            prediccion <- tryCatch(
              predict(modelo, n.ahead = ventana),
              error = function(e) NULL
            )
            
            if (!is.null(prediccion)) {
              tiempo_prediccion <- seq(length(datos_train) + 1, length(datos_train) + ventana)
              lines(tiempo_prediccion, prediccion$pred, col = "cyan")
            }
          }
        }
      }
    }
  }
}


lines(datos,col="black")
lines(datos_train,col="gray")
lines(datos_suavizados, col = "blue")

legend("topleft", legend = c("Dades observades", "Prediccions ARIMA"),
       col = c("black",  "cyan"), lty = c(1, 1),cex=0.7)
title(main = "IBEX35 amb Prediccions ARIMA", xlab = "Temps", cex.main=2)


#################################
#Grafiquem els 5 millos resultats, incloent el millor resultat que proporciona la funcio auto.arima. 
#################################
plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")


modelo1 <- arima(datos_train, order = c(0, 0, 1), seasonal = list(order = c(1, 0, 0), period = 7))
modelo2<- arima(datos_train, order = c(2, 0, 0), seasonal = list(order = c(1, 0, 0), period = 7))
modelo3 <- arima(datos_train, order = c(2, 1, 2), seasonal = list(order = c(1, 1, 0), period = 7))
modelo4 <- arima(datos_train, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0), period = 7))
modelo5 <- arima(datos_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 7))

tmp.pred1 <- predict(modelo1, n.ahead = ventana)
lines(c(1:length(datos_train), length(datos_suavizados) + 1:length(tmp.pred1$pred)), c(datos_train, tmp.pred1$pred), col = "cyan")
tmp.pred2 <- predict(modelo2, n.ahead = ventana)   
lines(c(1:length(datos_train), length(datos_suavizados) + 1:length(tmp.pred2$pred)), c(datos_train, tmp.pred2$pred), col = "orange")
tmp.pred3 <- predict(modelo3, n.ahead = ventana)        
lines(c(1:length(datos_train), length(datos_suavizados) + 1:length(tmp.pred3$pred)), c(datos_train, tmp.pred3$pred), col = "#9ACD32")
tmp.pred4 <- predict(modelo4, n.ahead = ventana)     
lines(c(1:length(datos_train), length(datos_suavizados) + 1:length(tmp.pred4$pred)), c(datos_train, tmp.pred4$pred), col = "purple")
tmp.pred5 <- predict(modelo5, n.ahead = ventana)      
lines(c(1:length(datos_train), length(datos_suavizados) + 1:length(tmp.pred5$pred)), c(datos_train, tmp.pred5$pred), col = "red")

lines(datos,col="black")
lines(datos_train,col="gray")
lines(datos_suavizados, col = "blue")

legend("topleft", legend = c("Dades observades"),
       col = c("black"), lty = c(1, 1, 1),cex=0.7)
title(main = "IBEX35 amb Prediccions ARIMA", xlab = "Temps",cex.main=2)



##############
##REMOSTREIG##
##############
library(data.table)#Instalar i carregar paquet.

#Funció remostreig.
mi.remuestra <- function(datos_ob, datos_ob_suav) {
  size.tmp <- length(datos_ob)
  residus.tmp <- datos_ob - datos_ob_suav
  residus.adims.tmp <- residus.tmp / datos_ob_suav
  remuestra.adims.tmp <- sample(residus.adims.tmp, size.tmp, replace = TRUE)
  reconstruidos.tmp <- datos_ob_suav + remuestra.adims.tmp * datos_ob_suav
  return(reconstruidos.tmp)
}

#Paràmetres dels models ARIMA a avaluar, triat anteriorment.
modelos_sarima <- list(
  list(order = c(0, 0, 1), seasonal = list(order = c(1, 0, 0))),
  list(order = c(2, 0, 0), seasonal = list(order = c(1, 0, 0))),
  list(order = c(2, 1, 2), seasonal = list(order = c(1, 1, 0))),
  list(order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0))),
  list(order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0)))
)



plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

#Realitzar el remostreig i calcular mètriques per a cada model ARIMA.
resultats_rem1 <- data.frame(
  Realizacion = integer(),
  Modelo = character(),
  MSE = numeric(),
  Desviacio = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)


for (i in 1:4) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
  lines(rem1.nvl, col = "cyan")
  
  for (modelo_sarima in modelos_sarima) {
    modelo <- tryCatch(
      Arima(rem1.nvl, order = modelo_sarima$order, seasonal = modelo_sarima$seasonal),
      error = function(e) NULL
    )
    
    if (!is.null(modelo)) {
      prediccion <- tryCatch(
        predict(modelo, n.ahead = ventana),
        error = function(e) NULL
      )
      
      if (!is.null(prediccion)) {
        mse <- mean((datos_test[1:ventana] - prediccion$pred)^2)
        aic <- AIC(modelo)
        bic <- BIC(modelo)
        desv <- sd(datos_test[1:ventana] - prediccion$pred)
        
        
        resultats_rem1 <- rbind(resultats_rem1, data.frame(
          Realizacion = i,
          Modelo = paste("SARIMA(", paste(modelo_sarima$order, collapse = ","), ")(", paste(modelo_sarima$seasonal$order, collapse = ","), ")", sep = ""),
          MSE = mse,
          Desviacio = desv,
          AIC = aic,
          BIC = bic,
          stringsAsFactors = FALSE
        ))
        
        tiempo_prediccion <- seq(length(datos_train) + 1, length(datos_train) + length(datos_test[1:12]))
        lines(tiempo_prediccion, prediccion$pred, col = "pink")
      }
    }
  }
}

print(resultats_rem1)

lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")

legend("topleft", legend = c("Dades de Calibració", "Sèrie Suavitzada", "Remostreig","Prediccions"), col = c("black", "blue", "cyan","pink"), lty = c(1, 1, 1,1), cex = 0.7)
title(main = "Conjunt de prediccions de 4 sèries de l'IBEX-35 remostrejades", xlab = "Temps",cex.main=2)


#Busquem el model amb millor error quadràtic mitjà en cada realització, i després observem quin model té més presència en les 100 realitzacions.
mejores_modelos <- do.call(rbind, lapply(split(resultats_rem1, resultats_rem1$Realizacion), function(x) x[which.min(x$MSE), ]))
frecuencias_modelos <- table(mejores_modelos$Modelo)
modelo_mas_repetido <- names(which.max(frecuencias_modelos))
veces_repetido <- max(frecuencias_modelos)
print(mejores_modelos)
cat("El model que més se repeteix amb el MSE més petit és:", modelo_mas_repetido, "amb", veces_repetido, "repeticions.\n")

#Busquem el model amb el AIC més baix en cada realització, i després observem quin model té més presència en les 100 realitzacions.
mejores_modelos1 <- do.call(rbind, lapply(split(resultats_rem1, resultats_rem1$Realizacion), function(x) x[which.min(x$AIC), ]))
frecuencias_modelos1 <- table(mejores_modelos1$Modelo)
modelo_mas_repetido1 <- names(which.max(frecuencias_modelos1))
veces_repetido1 <- max(frecuencias_modelos1)
print(mejores_modelos1)
cat("El model que més se repeteix amb el AIC més petit és:", modelo_mas_repetido1, "amb", veces_repetido1, "repeticions.\n")

#Busquem el model amb el BIC més baix en cada realització, i després observem quin model té més presència en les 100 realitzacions.
mejores_modelos2 <- do.call(rbind, lapply(split(resultats_rem1, resultats_rem1$Realizacion), function(x) x[which.min(x$BIC), ]))
frecuencias_modelos2 <- table(mejores_modelos2$Modelo)
modelo_mas_repetido2 <- names(which.max(frecuencias_modelos2))
veces_repetido2 <- max(frecuencias_modelos2)
print(mejores_modelos2)
cat("El model que més se repeteix amb el BIC més petit és:", modelo_mas_repetido2, "amb", veces_repetido2, "repeticions.\n")

#Busquem el model amb millor desviació en cada realització, i després observem quin model té més presència en les 100 realitzacions.
mejores_modelos3 <- do.call(rbind, lapply(split(resultats_rem1, resultats_rem1$Realizacion), function(x) x[which.min(x$Desviacio), ]))
frecuencias_modelos3 <- table(mejores_modelos3$Modelo)
modelo_mas_repetido3 <- names(which.max(frecuencias_modelos3))
veces_repetido3 <- max(frecuencias_modelos3)
print(mejores_modelos3)
cat("El model que més se repeteix amb millor desviació:", modelo_mas_repetido3, "amb", veces_repetido3, "repeticions.\n")

modelos_arima <- list(
  list(order = c(0,1,1), seasonal = list(order = c(1, 1, 0), period = 12))  
)

#Gràfica amb el model ARIMA(2,0,0)(1,0,0), millor model segons l'error quadràtic mitjà i la desviació.
resultats_rem1 <- data.frame(
  Realizacion = integer(),
  Modelo = character(),
  MSE = numeric(),
  Desviacio = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
  lines(rem1.nvl, col = "cyan")
  
  for (modelo_arima in modelos_arima) {
    modelo <- tryCatch(
      Arima(rem1.nvl, order = modelo_arima$order, seasonal = modelo_arima$seasonal),
      error = function(e) NULL
    )
    
    if (!is.null(modelo)) {
      prediccion <- tryCatch(
        predict(modelo, n.ahead = ventana),
        error = function(e) NULL
      )
      
      if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
        mse <- mean((datos_test[1:ventana] - prediccion$pred)^2)
        aic <- AIC(modelo)
        bic <- BIC(modelo)
        desv <- sd(datos_test[1:ventana] - prediccion$pred)
       
        resultats_rem1 <- rbind(resultats_rem1, data.frame(
          Realizacion = i,
          Modelo = paste("SARIMA(", paste(modelo_arima$order, collapse = ","), ")(", paste(modelo_arima$seasonal$order, collapse = ","), ")", sep = ""),
          MSE = mse,
          Desviacio = desv,
          AIC = aic,
          BIC = bic,
          stringsAsFactors = FALSE
        ))
        
        tiempo_prediccion <- seq(length(datos_train) + 1, length(datos_train) + ventana)
        lines(tiempo_prediccion, prediccion$pred, col = "pink")
      }
    }
  }
}


print(resultats_rem1)

tmp.pred4 <- predict(modelo4, n.ahead = ventana)   
lines(c(1:length(datos_suavizados), length(datos_suavizados) + 1:length(tmp.pred2$pred)), c(datos_suavizados, tmp.pred2$pred), col = "#9ACD32")
lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")
legend("topleft", legend = c("Dades de Calibracio", "Serie Suavitzada", "Remostreig"), col = c("black", "blue", "cyan"), lty = c(1, 1, 1), cex = 0.7)
title(main = "Conjunt de prediccions amb model ARIMA(0,1,1)(1,1,0)", xlab = "Temps",cex.main=2)



###############################
#Crear intervals de predicció.#
###############################
modelos_arima <- list(
  list(order = c(2,0,0), seasonal = list(order = c(1, 0, 0), period = 12))  
)


plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
  lines(rem1.nvl, col = "cyan")
  
  for (modelo_arima in modelos_arima) {
    modelo <- tryCatch(
      Arima(rem1.nvl, order = modelo_arima$order, seasonal = modelo_arima$seasonal),
      error = function(e) NULL
    )
    
    
    if (!is.null(modelo)) {
      forecasted <- tryCatch(
        forecast(modelo, h = ventana, level = c(20,30,40,50,60,70,80,85,90,95,99)),
        error = function(e) NULL
      )
      
    }
  }
}


print(forecasted)
plot(forecasted)

#Dibuixar intervals de confiança amb la sèrie de dades observades per visualitzar els intervals de confiança.
plot(datos_train, type = "l", col = "black", 
     xlim = c(1, length(datos_train) + ventana), 
     ylim = range(c(datos_train, forecasted$mean, forecasted$lower, forecasted$upper)), 
     xlab = "Temps", ylab = "IBEX35",
     main = "Predicció ARIMA a 12 dies amb intervals de confiança", cex.main=2.2)

lines((length(datos_train) + 1):(length(datos_train) + ventana), forecasted$mean, col = "#9ACD32", lwd = 2)


colors <- colorRampPalette(c("purple", "grey"))(length(forecasted$level))
alpha <- seq(0.8, 0.1, length.out = length(forecasted$level))  # Degradado de opacidad


for (i in seq_along(forecasted$level)) {
  lower_bound <- forecasted$lower[,i]
  upper_bound <- forecasted$upper[,i]
  
  
  polygon(c((length(datos_train) + 1):(length(datos_train) + ventana), rev((length(datos_train) + 1):(length(datos_train) + ventana))),
          c(lower_bound, rev(upper_bound)),
          col = rgb(col2rgb(colors[i])[1]/255, col2rgb(colors[i])[2]/255, col2rgb(colors[i])[3]/255, alpha[i]), 
          border = NA) 
  
  lines((length(datos_train) + 1):(length(datos_train) + ventana), lower_bound, col = rgb(0, 0, 1, alpha[i] * 0.5), lty = 2)
  lines((length(datos_train) + 1):(length(datos_train) + ventana), upper_bound, col = rgb(0, 0, 1, alpha[i] * 0.5), lty = 2)
}

lines((length(datos_train) + 1):(length(datos_train) + ventana), forecasted$mean, col =  "#9ACD32", lwd = 2)
lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")
legend("topleft", legend = c("Dades observades", "Serie Suavitzada", "Prediccions mitjanes"), col = c("black", "blue", "#9ACD32"), lty = c(1, 1, 1), cex = 0.7)

