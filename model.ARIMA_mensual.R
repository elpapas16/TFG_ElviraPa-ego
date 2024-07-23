library(stats) #Instalar i carregar paquet.
library(tibble) #Instalar i carregar paquet.
install.packages("forecast")
library(forecast)

ventana <- 12
#ventana <- 24
###################################################
##Funcio per escollir el model ARIMA més adequat.## 
###################################################
model_arima <- function(datos_train, ventana, datos_test) {
  resultats <- list()
  
  for (d in 0:2) {
    for (p in 0:2) {
      for (q in 0:2) {
        modelo <- tryCatch(
          arima(datos_train, order = c(p, d, q)),
          error = function(e) NULL
        )
        
        if (!is.null(modelo)) {
          prediccion <- tryCatch(
            predict(modelo, n.ahead = ventana),
            error = function(e) NULL
          )
          
          if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
            mse <- mean((datos_test[1:ventana] - prediccion$pred)^2)  
            sd_errores <- sd(datos_test[1:ventana] - prediccion$pred) 
            resultats[[length(resultats) + 1]] <- list(
              ARIMA = paste("ARIMA(", p, ",", d, ",", q, ")", sep = ""),
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
  
  return(resultats)

}

#################################
##Importem dades Ibex-35 mensuals i apliquem la funcio.
#################################
datos<-c(3000, 2841.6, 2762.4, 2484.5, 2643.7, 2765, 2867.8, 2834.5, 2511.3, 2060.7, 2303.2, 2289.2, 2248.8, 2333.7, 2622.8, 2789, 2755.3, 2862.3,2752.8,2708.4,2766.18,2830.47,2765.08,2610.56,2603.3,2711.79,2857.18,2750.42,2610.56,2813.89,2553.82,2289.85,2160.96,2032.13,2110.77, 2342.73, 2344.57, 2529.3,2576.04,2658.49,2686,2834.36,2854.6,2981.16,3360.2,3177.33,3449.63,3280.1,3615.22,3980.53,3739.68,3489.08,3520.73,3512.59,3200.71,3364.16,3337.01,3176.63,3194.52,3287.09,3087.68,3040.06,3024.54,2931.7,3091.86,3226.49,3219.05,3373.39,3432.75,3376.77,3277.33,3510.12,3630.76,3734.49,3915.52,3857.12,4082.9,4111.66,4264.1,3979.35,4025.47,4215.57,4306.74,4667.8,5154.77,5327.42,5305.43,5520.68,5909.33,6300.82,6884.56,6810.89,6532.33,7269.72,6380.39,6932.1,7255.4,7958.99,8900.09,10209.16,10025.68,10005.72,10146.4,10493.7,8264.7,7676.5,8800,9645.5,9836.6,9878.8,9997.3,9740.7,9975.4,10072.3,10218.6,9391.9,9806.1,9525.4,9741.5,10958.1,11641.4,10835.1,12585.8,11935,11467.9,10688.5,10581.3,10531.6,10884.7,10950,10363.1,9214.5,9109.8,10116,9551.4,9308.3,9761,9500.7,8878.4,8480,8321.1,7314,7774.3,8364.7,8397.6,8050.4,8135.5,8249.7,8154.4,7949.9,6913,6249.3,6435.7,5431.7,6139.4,6685.6,6036.9,5947.7,5999.4,5870.5,6489.5,6508.5,6862,7061.7,7111.3,6703.6,7129.5,7252.5,7737.2,7929.9,8249.4,8018.1,8109.5,7959.3,8078.3,7919.3,7869.5,8029.2,8418.3,8693,9080.8,9223.9,9391,9258.8,9001.6,9427.1,9783.2,10115.6,10008.9,10813.9,10493.8,10557.8,10733.9,11104.3,11740.7,11854.3,11892.5,11340.5,11548.1,11818,12144.7,12934.7,13753,13849.3,14146.5,14553.2,14248.4,14641.7,14374.6,15329.4,14892,14802.4,14479.8,14576.5,15890.5,15759.9,15182.3,13229,13170.4,13269,13798.3,13600.9,12046.2,11881.3,11707.3,10987.5,9116,8910.6,9195.8,8450.4,7620.9,7815,9038,9424.3,9787.8,10855.1,11365.1,11756.1,11414.8,11644.7,11940,10947.7,10333.6,10871.3,10492.2,9359.4,9263.4,10499.8,10187,10514.5,10812.9,9267.2,9859.1,10806,10850.8,10576.5,10878.9,10476,10359.9,9630.7,8718.6,8546.6,8954.9,8449.5,8566.3,8509.2,8465.9,8008,7011,6089.8,7102.2,6738.1,7420.5,7708.5,7842.9,7934.6,8167.5,8362.3,8230.3,7920,8419,8320.6,7762.7,8433.4,8290.5,9186.1,9907.9,9837.6,9916.7,9920.2,10114.2,10340.5,10459,10798.7,10923.5,10707.2,10728.8,10825.5,10477.8,10770.7,10279.5,10403.3,11178.3,11521.1,11385,11217.6,10769.5,11180.7,10259,9559.9,10360.7,10386.9,9544.2,8815.8,8461.4,8723.1,9025.7,9034,8163.3,8587.2,8716.8,8779.4,9143.3,8688.2,9532.1,9315.2,9555.5,10462.9,10715.8,10880,10444.5,10502.2,10299.5,10381.5,10523.5,10211,10043.9,10451.5,9840.3,9600.4,9980.6,9465.5,9622.7,9870.7,9399.1,9389.2,8893.5,9077.2,8539.9,9056.7,9277.7,9240.3,9570.6,9004.2,9198.8,8971,8812.9,9244.6,9257.5,9352,9549.2,9367.9,8741.5,6785.4,6922.3,7096.5,7231.4,6877.4,6969.5,6716.6,6452.2,8076.9,8073.7,7757.5,8225,8580,8815,9148.9,8821.2,8675.7,8846.6,8796.3,9057.7,8305.1,8713.8,8612.8,8479.2,8445.1,8584.2,8851.5,8098.7,8156.2,7886.1,7366.8,7956.5,8363.2,8229.1,9034,9394.6,9232.5,9241,9050.2,9593,9641.5,9505.9,9428,9017.3,10058.2,10102.1,10077.7,10001.3)
datos_train<-c(3000, 2841.6, 2762.4, 2484.5, 2643.7, 2765, 2867.8, 2834.5, 2511.3, 2060.7, 2303.2, 2289.2, 2248.8, 2333.7, 2622.8, 2789, 2755.3, 2862.3, 2752.8, 2708.4, 2766.18, 2830.47, 2765.08, 2610.56, 2603.3, 2711.79, 2857.18, 2750.42, 2610.56, 2813.89, 2553.82, 2289.85, 2160.96, 2032.13, 2110.77, 2342.73, 2344.57, 2529.3, 2576.04, 2658.49, 2686, 2834.36, 2854.6, 2981.16, 3360.2, 3177.33, 3449.63, 3280.1, 3615.22, 3980.53, 3739.68, 3489.08, 3520.73, 3512.59, 3200.71, 3364.16, 3337.01, 3176.63, 3194.52, 3287.09, 3087.68, 3040.06, 3024.54, 2931.7, 3091.86, 3226.49, 3219.05, 3373.39, 3432.75, 3376.77, 3277.33, 3510.12, 3630.76, 3734.49, 3915.52, 3857.12, 4082.9, 4111.66, 4264.1, 3979.35, 4025.47, 4215.57, 4306.74, 4667.8, 5154.77, 5327.42, 5305.43, 5520.68, 5909.33, 6300.82, 6884.56, 6810.89, 6532.33, 7269.72, 6380.39, 6932.1, 7255.4, 7958.99, 8900.09, 10209.2, 10025.7, 10005.7, 10146.4, 10493.7, 8264.7, 7676.5, 8800, 9645.5, 9836.6, 9878.8, 9997.3, 9740.7, 9975.4, 10072.3, 10218.6, 9391.9, 9806.1, 9525.4, 9741.5, 10958.1, 11641.4, 10835.1, 12585.8, 11935, 11467.9, 10688.5, 10581.3, 10531.6, 10884.7, 10950, 10363.1, 9214.5, 9109.8, 10116, 9551.4, 9308.3, 9761, 9500.7, 8878.4, 8480, 8321.1, 7314, 7774.3, 8364.7, 8397.6, 8050.4, 8135.5, 8249.7, 8154.4, 7949.9, 6913, 6249.3, 6435.7, 5431.7, 6139.4, 6685.6, 6036.9, 5947.7, 5999.4, 5870.5, 6489.5, 6508.5, 6862, 7061.7, 7111.3, 6703.6, 7129.5, 7252.5, 7737.2, 7929.9, 8249.4, 8018.1, 8109.5, 7959.3, 8078.3, 7919.3, 7869.5, 8029.2, 8418.3, 8693, 9080.8, 9223.9, 9391, 9258.8, 9001.6, 9427.1, 9783.2, 10115.6, 10008.9, 10813.9, 10493.8, 10557.8, 10733.9, 11104.3, 11740.7, 11854.3, 11892.5, 11340.5, 11548.1, 11818, 12144.7, 12934.7, 13753, 13849.3, 14146.5, 14553.2, 14248.4, 14641.7, 14374.6, 15329.4, 14892, 14802.4, 14479.8, 14576.5, 15890.5, 15759.9, 15182.3, 13229, 13170.4, 13269, 13798.3, 13600.9, 12046.2, 11881.3, 11707.3, 10987.5, 9116, 8910.6, 9195.8, 8450.4, 7620.9, 7815, 9038, 9424.3, 9787.8, 10855.1, 11365.1, 11756.1, 11414.8, 11644.7, 11940, 10947.7, 10333.6, 10871.3, 10492.2, 9359.4, 9263.4, 10499.8, 10187, 10514.5, 10812.9, 9267.2, 9859.1, 10806, 10850.8, 10576.5, 10878.9, 10476, 10359.9, 9630.7, 8718.6, 8546.6, 8954.9, 8449.5, 8566.3, 8509.2, 8465.9, 8008, 7011, 6089.8, 7102.2, 6738.1, 7420.5, 7708.5, 7842.9, 7934.6, 8167.5, 8362.3, 8230.3, 7920, 8419, 8320.6, 7762.7, 8433.4, 8290.5, 9186.1, 9907.9, 9837.6, 9916.7, 9920.2, 10114.2, 10340.5, 10459, 10798.7, 10923.5, 10707.2, 10728.8, 10825.5, 10477.8, 10770.7, 10279.5, 10403.3, 11178.3, 11521.1, 11385, 11217.6, 10769.5, 11180.7, 10259, 9559.9, 10360.7, 10386.9, 9544.2, 8815.8, 8461.4, 8723.1, 9025.7, 9034, 8163.3, 8587.2, 8716.8, 8779.4, 9143.3, 8688.2, 9532.1, 9315.2, 9555.5, 10462.9, 10715.8, 10880, 10444.5, 10502.2, 10299.5, 10381.5, 10523.5, 10211, 10043.9, 10451.5, 9840.3)
datos_test<-c(9600.4, 9980.6, 9465.5, 9622.7, 9870.7, 9399.1, 9389.2, 8893.5, 9077.2, 8539.9, 9056.7, 9277.7, 9240.3, 9570.6, 9004.2, 9198.8, 8971, 8812.9, 9244.6, 9257.5, 9352, 9549.2, 9367.9, 8741.5, 6785.4, 6922.3, 7096.5, 7231.4, 6877.4, 6969.5, 6716.6, 6452.2, 8076.9, 8073.7, 7757.5, 8225, 8580, 8815, 9148.9, 8821.2, 8675.7, 8846.6, 8796.3, 9057.7, 8305.1, 8713.8, 8612.8, 8479.2, 8445.1, 8584.2, 8851.5, 8098.7, 8156.2, 7886.1, 7366.8, 7956.5, 8363.2, 8229.1, 9034, 9394.6, 9232.5, 9241, 9050.2, 9593, 9641.5, 9505.9, 9428, 9017.3, 10058.2, 10102.1, 10077.7, 10001.3)
datos_suavizados<-c(2762.27, 2743.28, 2700.9, 2651.69, 2606.69, 2573.55, 2557.72, 2554.12, 2536.17, 2535.15, 2544.95, 2559.46, 2572.57, 2578.17, 2581.15, 2587.41, 2602.86, 2633.38, 2670.92, 2707.38, 2734.69, 2744.77, 2738.63, 2719.32, 2689.89, 2653.38, 2612.83, 2571.31, 2531.85, 2497.5, 2470.31, 2451.34, 2441.67, 2442.37, 2454.51, 2479.16, 2517.39, 2570.28, 2636.86, 2713.9, 2798.18, 2886.46, 2975.53, 3062.15, 3143.1, 3215.16, 3276.07, 3325.89, 3364.7, 3392.57, 3409.58, 3415.79, 3411.28, 3396.11, 3372.63, 3343.46, 3311.27, 3278.7, 3248.39, 3222.99, 3205.15, 3197.52, 3200.42, 3213.03, 3234.56, 3264.21, 3301.16, 3344.62, 3393.77, 3447.83, 3505.65, 3567.71, 3634.47, 3706.37, 3783.88, 3867.47, 3957.58, 4054.68, 4160.35, 4278.17, 4411.72, 4564.61, 4740.4, 4929.92, 5123.99, 5313.41, 5487.44, 5658.22, 5837.89, 6038.6, 6272.51, 6550.7, 6884.29, 7231.35, 7548, 7844.72, 8132.01, 8252.19, 8341.51, 8451.93, 8635.42, 8862.25, 9079.76, 9276.95, 9442.82, 9567.06, 9640.41, 9644.83, 9615.27, 9585.9, 9536.45, 9503.49, 9691.75, 9917.73, 10082.2, 10295, 10417, 10506.8, 10581.6, 10643.4, 10693.8, 10742.3, 10806.9, 10846.3, 10820.2, 10788.5, 10754.3, 10656.7, 10541.7, 10455.5, 10235.1, 10051.6, 9878.78, 9689.98, 9473.57, 9269.3, 9108.99, 8945.4, 8790.79, 8711.26, 8672.46, 8596.08, 8468.01, 8299.69, 8102.56, 7888.03, 7663.95, 7467.62, 7336.4, 7252.64, 7147.31, 6987.7, 6820.19, 6691.14, 6593.06, 6498.49, 6423.93, 6385.85, 6375.21, 6399.01, 6464.3, 6578.09, 6717.91, 6845.25, 6976.58, 7128.36, 7280.72, 7424.81, 7551.79, 7652.8, 7738.93, 7821.28, 7910.92, 8018.96, 8140.39, 8266.5, 8388.57, 8497.87, 8601.72, 8707.39, 8822.18, 8953.39, 9097.28, 9255.32, 9428.95, 9619.61, 9828.76, 10043.4, 10250.4, 10436.9, 10593.4, 10737.5, 10886.5, 11057.9, 11269.1, 11508.7, 11765.1, 12027, 12277.8, 12518.6, 12750.9, 12975.8, 13209.3, 13431.4, 13622.4, 13826.4, 14060.8, 14306.3, 14543.8, 14686.1, 14675, 14602.6, 14560.6, 14523.8, 14448.9, 14324.6, 14139.9, 13883.7, 13580.4, 13265.3, 12909.7, 12485, 11999.1, 11476.7, 11010.5, 10693, 10466.5, 10241.4, 10045.3, 9905.8, 9818.44, 9792.05, 9835.49, 9913.93, 9981.75, 10073.9, 10225.3, 10384.6, 10483.7, 10561.5, 10657.2, 10728.3, 10764.8, 10741.8, 10634.6, 10516.8, 10448.8, 10384.6, 10322, 10310.5, 10319.1, 10278.8, 10207.1, 10173.1, 10138.9, 10044.3, 9910.45, 9758.38, 9623.81, 9534.72, 9420.64, 9211.12, 8941.83, 8672.16, 8409.8, 8162.42, 7975.55, 7870.07, 7815.14, 7779.87, 7755.66, 7738.45, 7724.19, 7708.83, 7696.02, 7704.84, 7754.34, 7863.58, 8027.9, 8221.06, 8416.8, 8588.89, 8735.7, 8869.23, 9001.49, 9144.49, 9305.65, 9481, 9666.52, 9858.24, 10036.8, 10192.7, 10316.8, 10399.7, 10453.6, 10507.5, 10590.7, 10688.1, 10770.9, 10830.2, 10856.9, 10841.7, 10787.1, 10724.8, 10686.6, 10632.9, 10514.2, 10377, 10267.5, 10161.7, 10018.5, 9827.2, 9621.35, 9434.36, 9280.15, 9160.13, 9075.75, 9028.43, 8990.2, 8957.21, 8996.91, 9128.81, 9276.44, 9391.87, 9497.72, 9616.62, 9740.83, 9861.74, 9970.72, 10059.2, 10130.9, 10191, 10251.2, 10315.6, 10359.6, 10349, 10308.6, 10273.7, 10243.9)


resultats_arima <- model_arima(datos_train, ventana, datos_test)
print(resultats_arima)

#################################
#Passem els resultats a una taula. 
#################################
df_resultats <- do.call(rbind, lapply(resultats_arima, function(x) as.data.frame(t(unlist(x)))))

df_resultats$Modelo <- paste0("Modelo", seq_len(nrow(df_resultats)))#Agregar una columna d'indexs per identificar cada model.

df_resultats <- df_resultats[, c(ncol(df_resultats), 1:(ncol(df_resultats)-1))] #Reordenar les columnes per a que el model estiga al inici.

print(df_resultats)

#Busquem els models que tinguen millor error, AIC, BIC i desviació.
mejor_modelo_mse <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$MSE))]]
mejor_modelo_sd_errores <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$SD_Errores))]]
mejor_modelo_aic <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$AIC))]]
mejor_modelo_bic <- resultats_arima [[which.min(sapply(resultats_arima , function(x) x$BIC))]]
print(mejor_modelo_mse$ARIMA)
print(mejor_modelo_sd_errores$ARIMA)
print(mejor_modelo_aic$ARIMA)
print(mejor_modelo_bic$ARIMA)



#################################
#Grafiquem els resultats. 
#################################
plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")


for (d in 0:2) {
  for (p in 0:2) {
    for (q in 0:2) {
      modelo <- tryCatch(
        arima(datos_train, order = c(p, d, q)),
        error = function(e) NULL
      )
      
      if (!is.null(modelo)) {
        prediccion <- tryCatch(
          predict(modelo, n.ahead = ventana)$pred,
          error = function(e) NULL
        )
        
        if (!is.null(prediccion)) {
          tiempo_prediccion <- seq(length(datos_train) + 1, length(datos_train) + ventana)
          
          
          lines(c(seq_along(datos_train), tiempo_prediccion), c(datos_train, prediccion), col = "cyan")
        }
      }
    }
  }
}



lines(datos,col="black")
lines(datos_train,col="gray")
lines(datos_suavizados, col = "blue")
legend("topleft", legend = c("Dades observades", "Prediccions ARIMA"),
       col = c("black", "cyan"), lty = c(1, 1), cex = 0.8)
title(main = "IBEX-35 amb Prediccions ARIMA", xlab = "Temps",cex.main=2)


#Gràficar només prediccions sobre valors observats complets.
plot(datos_test, type = "l", col = "black", xlim = c(1, length(datos_test)), 
     ylab = "IBEX35", xlab = "Temps")

# Iterar sobre los posibles valores de p, d, y q
for (d in 0:2) {
  for (p in 0:2) {
    for (q in 0:2) {
      # Intentar ajustar el modelo ARIMA
      modelo <- tryCatch(
        arima(datos_train, order = c(p, d, q)),
        error = function(e) NULL
      )
      
      # Si el modelo se ajustó correctamente
      if (!is.null(modelo)) {
        # Intentar predecir con el modelo ajustado
        prediccion <- tryCatch(
          predict(modelo, n.ahead = ventana)$pred,
          error = function(e) NULL
        )
        
        # Si la predicción se realizó correctamente
        if (!is.null(prediccion)) {
          lines(seq(1,length(datos_test[1:12])), prediccion, col = "cyan")
        }
      }
    }
  }
}

#Gràficar només prediccions sobre els 12 valors observats.
plot(datos_test[1:ventana], type = "l", col = "black", xlim = c(1, ventana), 
     ylab = "IBEX35", xlab = "Temps")

# Iterar sobre los posibles valores de p, d, y q
for (d in 0:2) {
  for (p in 0:2) {
    for (q in 0:2) {
      # Intentar ajustar el modelo ARIMA
      modelo <- tryCatch(
        arima(datos_train, order = c(p, d, q)),
        error = function(e) NULL
      )
      
      # Si el modelo se ajustó correctamente
      if (!is.null(modelo)) {
        # Intentar predecir con el modelo ajustado
        prediccion <- tryCatch(
          predict(modelo, n.ahead = ventana)$pred,
          error = function(e) NULL
        )
        
        # Si la predicción se realizó correctamente
        if (!is.null(prediccion)) {
          # Asegurarse de que la predicción tenga la misma longitud que los datos de prueba para la ventana deseada
          prediccion <- prediccion[1:ventana]
          
          # Graficar las predicciones en el gráfico
          lines(seq(1, ventana), prediccion, col = "cyan")
        }
      }
    }
  }
}



#################################
#Grafiquem els 5 millos resultats, incloent el millor resultat que proporciona la funcio auto.arima. 
#################################
plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")


modelo1 <- arima(datos_train, order = c(1, 0, 1))
modelo2<- arima(datos_train, order = c(2, 0, 0))
modelo3 <- arima(datos_train, order = c(2, 2, 1))
modelo4 <- arima(datos_train, order = c(0, 2, 1))
modelo5 <- arima(datos_train, order = c(0, 1, 0))

tmp.pred1 <- predict(modelo1, n.ahead = ventana)
lines(c(1:length(datos_train), length(datos_train) + 1:length(tmp.pred1$pred)), c(datos_train, tmp.pred1$pred), col = "cyan")
tmp.pred2 <- predict(modelo2, n.ahead = ventana)   
lines(c(1:length(datos_train), length(datos_train) + 1:length(tmp.pred2$pred)), c(datos_train, tmp.pred2$pred), col = "orange")
tmp.pred3 <- predict(modelo3, n.ahead = ventana)        
lines(c(1:length(datos_train), length(datos_train) + 1:length(tmp.pred3$pred)), c(datos_train, tmp.pred3$pred), col = "#9ACD32")
tmp.pred4 <- predict(modelo4, n.ahead = ventana)     
lines(c(1:length(datos_train), length(datos_train) + 1:length(tmp.pred4$pred)), c(datos_train, tmp.pred4$pred), col = "purple")
tmp.pred5 <- predict(modelo5, n.ahead = ventana)      
lines(c(1:length(datos_train), length(datos_train) + 1:length(tmp.pred5$pred)), c(datos_train, tmp.pred5$pred), col = "pink")
  
lines(datos,col="black")
lines(datos_train,col="gray")
lines(datos_suavizados, col = "blue")

legend("topright", legend = c("Dades observades"),
       col = c("black"), lty = c(1, 1, 1), cex = 0.7)
title(main = "IBEX35 amb Prediccions ARIMA", xlab = "Temps",cex.main=2)


#Gràficar només prediccions sobre valors observats complets.
plot(datos_test, type = "l", col = "black", xlim = c(1, length(datos_test)), 
     ylab = "IBEX35", xlab = "Temps")

# Ajustar los modelos ARIMA
modelo1 <- arima(datos_train, order = c(1, 0, 1))
modelo2 <- arima(datos_train, order = c(2, 0, 0))
modelo3 <- arima(datos_train, order = c(2, 2, 1))
modelo4 <- arima(datos_train, order = c(0, 2, 1))
modelo5 <- arima(datos_train, order = c(0, 1, 0))

# Realizar predicciones y graficarlas
tmp.pred1 <- predict(modelo1, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred1, col = "cyan")

tmp.pred2 <- predict(modelo2, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred2, col = "orange")

tmp.pred3 <- predict(modelo3, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred3, col = "#9ACD32")

tmp.pred4 <- predict(modelo4, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred4, col = "purple")

tmp.pred5 <- predict(modelo5, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred5, col = "pink")


#Gràficar només prediccions sobre els 12 valors observats.
plot(datos_test[1:ventana], type = "l", col = "black", xlim = c(1, ventana), 
     ylab = "IBEX35", xlab = "Temps")

# Ajustar los modelos ARIMA
modelo1 <- arima(datos_train, order = c(1, 0, 1))
modelo2 <- arima(datos_train, order = c(2, 0, 0))
modelo3 <- arima(datos_train, order = c(2, 2, 1))
modelo4 <- arima(datos_train, order = c(0, 2, 1))
modelo5 <- arima(datos_train, order = c(0, 1, 0))

# Realizar predicciones y graficarlas
tmp.pred1 <- predict(modelo1, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred1, col = "cyan")

tmp.pred2 <- predict(modelo2, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred2, col = "orange")

tmp.pred3 <- predict(modelo3, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred3, col = "#9ACD32")

tmp.pred4 <- predict(modelo4, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred4, col = "purple")

tmp.pred5 <- predict(modelo5, n.ahead = ventana)$pred
lines(seq(1, ventana), tmp.pred5, col = "pink")




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

#Paràmetres dels models ARIMA a avaluar, triat anteriorment mitjançant la sèrie inicial.
modelos_arima <- list(
  c(1, 0, 1),
  c(2, 0, 0),
  c(2, 2, 1),
  c(0,2, 1),
  c(0, 1, 0)
)



plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

#Realitzar el remostreig i calcular mètriques per a cada model ARIMA.
resultats_rem1<- data.frame(Realizacion=integer(), Modelo=character(), MSE=numeric(), Desviacio=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)

for (i in 1:4) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
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
      
      if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
        mse <- mean((datos_test[1:ventana] - prediccion$pred)^2)
        aic <- AIC(modelo)
        bic <- BIC(modelo)
        desv <- sd(datos_test[1:ventana] - prediccion$pred)
        
        # Agregamos los resultados al data.frame temporario de la realización actual
        resultats_rem1 <- rbind(resultats_rem1, data.frame(
          Realizacion = i,
          Modelo = paste("ARIMA(", paste(modelo_arima, collapse = ","), ")", sep = ""),
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

lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")

legend("bottomright", legend = c("Dades de Calibració", "Serie Suavitzada", "Remostreig", "Prediccions amb ARIMA"), col = c("black", "blue", "cyan","pink"), lty = c(1, 1, 1,1), cex = 0.7)
title(main = "Conjunt de prediccions de 4 sèries de l'IBEX-35 remostrejades", xlab = "Temps",cex.main=2)

#Gràficar només prediccions sobre valors observats complets.
pred_min <- Inf
pred_max <- -Inf
predicciones <- list()

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
  
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
      
      if (!is.null(prediccion)) {
        predicciones <- c(predicciones, list(prediccion$pred))
        pred_min <- min(pred_min, min(prediccion$pred, na.rm = TRUE))
        pred_max <- max(pred_max, max(prediccion$pred, na.rm = TRUE))
      }
    }
  }
}

plot(datos_test, type = "l", col = "black", xlim = c(1, length(datos_test)), 
     ylim = c(min(datos_test), pred_max), ylab = "IBEX35", xlab = "Temps")
for (pred in predicciones) {
  lines(seq(1, ventana), pred, col = "pink")
}
  

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

#######
#Gràfica amb el model ARIMA(2,0,0), millor model segons l'error quadràtic mitjà i la desviació.
modelos_arima <- list(
  c(2, 0, 0))


plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

#Realitzar el remostreig i calcular mètriques per a cada model ARIMA.
resultats_rem1<- data.frame(Realizacion=integer(), Modelo=character(), MSE=numeric(), Desviacio=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
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
      
      if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
        mse <- mean((datos_test[1:12] - prediccion$pred)^2)
        aic <- AIC(modelo)
        bic <- BIC(modelo)
        desv <- sd(datos_test[1:12] - prediccion$pred)
        
        resultats_rem1 <- rbind(resultats_rem1, data.frame(
          Realizacion = i,
          Modelo = paste("ARIMA(", paste(modelo_arima, collapse = ","), ")", sep = ""),
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

# Añadir la visualización final de los datos
tmp.pred2 <- forecast(modelo2, h = ventana)   
lines(c(1:length(datos_suavizados), length(datos_suavizados) + 1:length(tmp.pred2$mean)), c(datos_suavizados, tmp.pred2$mean), col = "#9ACD32")
lines(datos, col = "black")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")
title(main = "Conjunt de prediccions amb model ARIMA(2,0,0)", xlab = "Temps",cex.main=2)

######
#Gràfica amb el model ARIMA(2,2,1), millor model segons AIC i BIC.
modelos_arima <- list(
  c(2, 2, 1))


plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

#Realitzar el remostreig i calcular mètriques per a cada model ARIMA.
resultats_rem1<- data.frame(Realizacion=integer(), Modelo=character(), MSE=numeric(), Desviacio=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
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
      
      if (!is.null(prediccion) && length(prediccion$pred) == ventana) {
        mse <- mean((datos_test[1:12] - prediccion$pred)^2)
        aic <- AIC(modelo)
        bic <- BIC(modelo)
        desv <- sd(datos_test[1:12] - prediccion$pred)

        resultats_rem1 <- rbind(resultats_rem1, data.frame(
          Realizacion = i,
          Modelo = paste("ARIMA(", paste(modelo_arima, collapse = ","), ")", sep = ""),
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

tmp.pred3 <- predict(modelo3, n.ahead = ventana) 
lines(c(1:length(datos_suavizados), length(datos_suavizados) + 1:length(tmp.pred2$pred)), c(datos_suavizados, tmp.pred2$pred), col = "#9ACD32")
lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")
title(main = "Conjunt de prediccions amb model ARIMA(2,1,1)", xlab = "Temps",cex.main=2)

###############################
#Crear intervals de predicció.#
###############################
modelos_arima <- list(
  c(2, 0, 0))


plot(datos, type = "l", col = "black", xlim = c(1, length(datos_train) + ventana), ylim = range(c(datos, datos_suavizados, datos, datos_train, datos_suavizados)), 
     ylab = "IBEX35", xlab = "Temps")
lines(datos_train, col = "gray")
lines(datos_suavizados, col = "blue")

for (i in 1:100) {
  rem1.nvl <- mi.remuestra(datos_train, datos_suavizados)
  lines(rem1.nvl, col="cyan")
  
  
  for (modelo_arima in modelos_arima) {
    modelo <- tryCatch(
      arima(rem1.nvl, order = modelo_arima),
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
     main = "Predicció ARIMA amb intervals de confiança",cex.main=2.5)

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

lines((length(datos_train) + 1):(length(datos_train) + ventana), forecasted$mean, col = "#9ACD32", lwd = 2)
lines(datos, col="black")
lines(datos_train, col="gray")
lines(datos_suavizados, col="blue")
legend("bottomright", legend = c("Dades observades", "Serie Suavitzada", "Prediccions mitjanes"), col = c("black", "blue", "#9ACD32"), lty = c(1, 1, 1), cex = 0.7)










