library(stats) #Instalar i carregar paquet.
library(tibble) #Instalar i carregar paquet.
install.packages("forecast")
library(forecast)

ventana <- 5

#################################
##Importem dades Ibex-35 mensuals i apliquem la funcio.
#################################
datos<-c(8792.5,8906.1,8944.3,9070.7,9207.1,9232.5,9157.4,9183.2,9254.6,9312.3,9237.7,9278.7,9310,9362.9,9378.5,9421.9,9494.8,9450.9,9415.6,9406.4,9290.3,9293.7,9314.6,9241,9082,9076.7,9043.6,9147.3,9211.3,9183.2,9167.7,9182.8,9234.10,9201.5,9191.4,9211.6,9213.1,9251.5,9305,9267,9163.5,9116.1,9191.1,9180.1,9167.5,9050.2,9167.5,9317.3,9289.1,9310.8,9359.8,9338.3,9309.7,9344.4,9333.7,9432.8,9430.8,9495,9431.9,9439.8,9436.4,9364.7,9265.8,9274,9393,9481.3,9510.6,9593,9644.8,9588.4,9486.3,9285.00,9248.8,9252.9,9331.1,9453.7,9478.7,9438.3,9438,9455.7,9451.8,9519.6,9571.5,9543.5,9519.2,9600.5,9694.7,9685.1,9641.5,9502.9,9328.7,9307.1,9368.4,9358.6,9301.8,9354.4,9502.2,9434.3,9429.6,9347.5,9350.5,9278,9267.7,9262.8,9314,9315.6,9324.7,9338.9,9490.1,9581.2,9551.1,9505.9,9449.6,9416.3,9392,9314.4,9310,9364.6,9435.2,9455.4,9424.1,9549,9549.7,9482.1,9527.2,9645.8,9548.9,9502,9386,9366.9,9331.9,9426.8,9428,9319,9165.5,9102.9,9157.7,9235.8,9151.3,9352.1,9360.4,9336,9232.9,9287.1,9298.5,9212.7,9146.8,9029.1,8995.5,8975.8,8984.8,8962.8,8918.3,9013.9,9017.3,9075,9260.4,9293.9,9241.5,9235.9,9283.8,9405.2,9371.7,9461.7,9623.3,9640.7,9667.4,9761.4,9839,9827.5,9887.4,9905.5,9939,9936.1,10003.4,10062.6,10058.2,10140.8,10178.3,10238.4,10258.1,10146,10223.4,10198,10118.7,10096.1,10171.7,10095.6,10054.9,10106.7,10101,10104.3,10111.9,10121.8,10086.2,10102.1,10182.1,10053.4,10182.4,10164.5,10209,10060.3,10067.1,10004.9,10094.8,10076.9,9994.1,9867.8,9880.3,9858.3,9968.1,9859.2,9974,9916.6,9936.6,9890.3,10039.3,10077.7,10014,10062.5,9941.3,10003,9888.2,9905.4,9896.6,9984.7,9925.4,9916.6,9927.3,9886.4,9944.8,10038.2,10107.2,10138.9,10130.6,10138.4,10113.8,10068.6,10001.3,10064.7,10069.8,10117.1,10197.2,10319.6,10305.7,10325.7,10388.9,10560.5,10490.5)
datos_suavizados<-c(8924.19, 8960.77, 8993.65, 9059.43, 9114.48, 9159.13, 9193.75, 9218.66, 9237.99, 9255.55, 9275.12, 9300.49, 9331.1, 9362.62, 9390.7, 9410.99, 9419.86, 9418, 9406.1, 9384.88, 9354.24, 9313.4, 9261.57, 9197.96, 9143.18, 9118.63, 9116.45, 9128.79, 9158.32, 9186.3, 9194.02, 9192, 9191.86, 9194.69, 9201.59, 9213.64, 9226.86, 9236.19, 9236.56, 9222.9, 9202.74, 9188.68, 9175.47, 9157.84, 9158.44, 9187.3, 9219.43, 9247.73, 9283.88, 9311.69, 9314.94, 9312.44, 9321.23, 9339.55, 9365.64, 9397.74, 9427.18, 9447.05, 9450.44, 9430.43, 9395.59, 9361.38, 9343.25, 9356.68, 9401.32, 9461.36, 9521.02, 9564.49, 9567.1, 9519.97, 9449.1, 9380.5, 9329.12, 9318.8, 9349.03, 9384.47, 9411.84, 9428.91, 9433.45, 9447.56, 9476.83, 9504.79, 9536.25, 9576.02, 9606.88, 9628.09, 9638.89, 9617.25, 9560.29, 9487.22, 9417.21, 9369.45, 9353.04, 9357.84, 9373.76, 9390.67, 9398.59, 9397.62, 9387.87, 9369.45, 9346.44, 9322.81, 9302.54, 9289.6, 9290.35, 9307.2, 9342.56, 9398.83, 9455.98, 9491.57, 9506.96, 9503.51, 9470.55, 9419.85, 9381.67, 9362.49, 9356.15, 9368.49, 9405.35, 9454.09, 9491.27, 9506.09, 9515.03, 9534.59, 9544.06, 9533.55, 9518.43, 9486.77, 9436.87, 9404.2, 9388.39, 9363.82, 9334.02, 9292.35, 9232.14, 9182.58, 9172.98, 9203.46, 9248.33, 9281.87, 9304.77, 9317.6, 9302.4, 9267.03, 9231.29, 9188.54, 9132.19, 9074.13, 9027.83, 8994.84, 8976.72, 8975.02, 8989.54, 9018.52, 9060.19, 9112.79, 9163.8, 9202.48, 9237.26, 9276.59, 9317.42, 9367.47, 9434.46, 9506.9, 9573.76, 9635.5, 9692.55, 9745.38, 9793.98, 9837.88, 9876.63, 9909.77, 9940.14, 9971.04, 10005.8, 10047.7, 10093.7, 10137.7, 10173.3, 10194.2, 10198.9, 10191.8, 10177.6, 10160.9, 10146.2, 10133.5, 10122.6, 10113.6, 10106.9, 10103.2, 10102.9, 10106.8, 10112.3, 10116.3, 10116, 10108.1, 10103, 10113.9, 10127.9, 10132.5, 10133.9, 10125.3, 10099.9, 10076.8, 10056.5, 10020, 9978.96, 9944.55, 9910.78, 9890.37, 9896.06, 9910.44, 9920.24, 9929.58, 9942.58, 9963.36, 9988.62, 10011, 10023, 10017.2, 9995.34, 9966.27, 9939, 9922.51, 9919.37, 9923.14, 9927.41, 9925.75, 9927.03, 9946.58, 9979.8, 10022.1, 10071.8, 10111.8, 10125.1, 10114.7, 10093.8, 10073.1, 10063, 10074.1, 10107.7, 10154.3, 10204.7, 10249.5, 10292.6, 10347.3, 10405, 10432.6, 10473.8)


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


#Gràfica amb el model ARIMA(2,0,0)(1,0,0), millor model segons l'error quadràtic mitjà i la desviació.
modelos_arima <- list(
  list(order = c(2,0,0), seasonal = list(order = c(1, 0, 0), period = 12))  
)


plot(datos, type = "l", col = "black", xlim = c(1, length(datos) + ventana), ylim = range(c(datos, datos_suavizados, datos)), 
     ylab = "IBEX35", xlab = "Temps")


#Realitzar el remostreig i calcular mètriques per a cada model ARIMA.
resultats_rem1<- data.frame(Realizacion=integer(), Modelo=character(), MSE=numeric(), Desviacio=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos, datos_suavizados)
  lines(rem1.nvl, col="cyan")
  
  
  for (modelo_arima in modelos_arima) {
    modelo <- tryCatch(
      arima(rem1.nvl, order = modelo_arima),
      error = function(e) NULL
    )
    
    if (!is.null(modelo)) {
      prediccion <- tryCatch(
        predict(modelo, n.ahead = ventana),
        error = function(e) NULL
      )
      
            tiempo_prediccion <- seq(length(datos) + 1, length(datos) + ventana)
        lines(tiempo_prediccion, prediccion$pred, col = "pink")
      }
    }
  }
 
# Añadir la visualización final de los datos
tmp.pred2 <- forecast(modelo2, h = ventana)   
lines(c(1:length(datos_suavizados), length(datos_suavizados) + 1:length(tmp.pred2$mean)), c(datos_suavizados, tmp.pred2$mean), col = "#9ACD32")
lines(datos, col = "black")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")



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
  rem1.nvl <- mi.remuestra(datos, datos_suavizados)
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
plot(datos, type = "l", col = "black", 
     xlim = c(1, length(datos) + ventana), 
     ylim = range(c(datos, forecasted$mean, forecasted$lower, forecasted$upper)), 
     xlab = "Temps", ylab = "IBEX35",
     main = "Predicció ARIMA a 3 dies amb intervals de confiança",cex.main=2.2)

lines((length(datos) + 1):(length(datos) + ventana), forecasted$mean, col = "#9ACD32", lwd = 2)


colors <- colorRampPalette(c("purple", "grey"))(length(forecasted$level))
alpha <- seq(0.8, 0.1, length.out = length(forecasted$level))  # Degradado de opacidad


for (i in seq_along(forecasted$level)) {
  lower_bound <- forecasted$lower[,i]
  upper_bound <- forecasted$upper[,i]
  

  polygon(c((length(datos) + 1):(length(datos) + ventana), rev((length(datos) + 1):(length(datos) + ventana))),
          c(lower_bound, rev(upper_bound)),
          col = rgb(col2rgb(colors[i])[1]/255, col2rgb(colors[i])[2]/255, col2rgb(colors[i])[3]/255, alpha[i]), 
          border = NA) 

  lines((length(datos) + 1):(length(datos) + ventana), lower_bound, col = rgb(0, 0, 1, alpha[i] * 0.5), lty = 2)
  lines((length(datos) + 1):(length(datos) + ventana), upper_bound, col = rgb(0, 0, 1, alpha[i] * 0.5), lty = 2)
}

lines((length(datos) + 1):(length(datos) + ventana), forecasted$mean, col = "#9ACD32", lwd = 2)
lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")
legend("bottomright", legend = c("Dades observades", "Serie Suavitzada", "Prediccions mitjanes"), col = c("black", "blue", "#9ACD32"), lty = c(1, 1, 1), cex = 0.7)










