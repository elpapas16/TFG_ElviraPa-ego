%Carreguem les dades de l'IBEX35 obtingudes de manera mensual.
ibex_mensual=[3000, 2841.6, 2762.4, 2484.5, 2643.7, 2765, 2867.8, 2834.5, 2511.3, 2060.7, 2303.2, 2289.2, 2248.8, 2333.7, 2622.8, 2789, 2755.3, 2862.3,2752.8,2708.4,2766.18,2830.47,2765.08,2610.56,2603.3,2711.79,2857.18,2750.42,2610.56,2813.89,2553.82,2289.85,2160.96,2032.13,2110.77, 2342.73, 2344.57, 2529.3,2576.04,2658.49,2686,2834.36,2854.6,2981.16,3360.2,3177.33,3449.63,3280.1,3615.22,3980.53,3739.68,3489.08,3520.73,3512.59,3200.71,3364.16,3337.01,3176.63,3194.52,3287.09,3087.68,3040.06,3024.54,2931.7,3091.86,3226.49,3219.05,3373.39,3432.75,3376.77,3277.33,3510.12,3630.76,3734.49,3915.52,3857.12,4082.9,4111.66,4264.1,3979.35,4025.47,4215.57,4306.74,4667.8,5154.77,5327.42,5305.43,5520.68,5909.33,6300.82,6884.56,6810.89,6532.33,7269.72,6380.39,6932.1,7255.4,7958.99,8900.09,10209.16,10025.68,10005.72,10146.4,10493.7,8264.7,7676.5,8800,9645.5,9836.6,9878.8,9997.3,9740.7,9975.4,10072.3,10218.6,9391.9,9806.1,9525.4,9741.5,10958.1,11641.4,10835.1,12585.8,11935,11467.9,10688.5,10581.3,10531.6,10884.7,10950,10363.1,9214.5,9109.8,10116,9551.4,9308.3,9761,9500.7,8878.4,8480,8321.1,7314,7774.3,8364.7,8397.6,8050.4,8135.5,8249.7,8154.4,7949.9,6913,6249.3,6435.7,5431.7,6139.4,6685.6,6036.9,5947.7,5999.4,5870.5,6489.5,6508.5,6862,7061.7,7111.3,6703.6,7129.5,7252.5,7737.2,7929.9,8249.4,8018.1,8109.5,7959.3,8078.3,7919.3,7869.5,8029.2,8418.3,8693,9080.8,9223.9,9391,9258.8,9001.6,9427.1,9783.2,10115.6,10008.9,10813.9,10493.8,10557.8,10733.9,11104.3,11740.7,11854.3,11892.5,11340.5,11548.1,11818,12144.7,12934.7,13753,13849.3,14146.5,14553.2,14248.4,14641.7,14374.6,15329.4,14892,14802.4,14479.8,14576.5,15890.5,15759.9,15182.3,13229,13170.4,13269,13798.3,13600.9,12046.2,11881.3,11707.3,10987.5,9116,8910.6,9195.8,8450.4,7620.9,7815,9038,9424.3,9787.8,10855.1,11365.1,11756.1,11414.8,11644.7,11940,10947.7,10333.6,10871.3,10492.2,9359.4,9263.4,10499.8,10187,10514.5,10812.9,9267.2,9859.1,10806,10850.8,10576.5,10878.9,10476,10359.9,9630.7,8718.6,8546.6,8954.9,8449.5,8566.3,8509.2,8465.9,8008,7011,6089.8,7102.2,6738.1,7420.5,7708.5,7842.9,7934.6,8167.5,8362.3,8230.3,7920,8419,8320.6,7762.7,8433.4,8290.5,9186.1,9907.9,9837.6,9916.7,9920.2,10114.2,10340.5,10459,10798.7,10923.5,10707.2,10728.8,10825.5,10477.8,10770.7,10279.5,10403.3,11178.3,11521.1,11385,11217.6,10769.5,11180.7,10259,9559.9,10360.7,10386.9,9544.2,8815.8,8461.4,8723.1,9025.7,9034,8163.3,8587.2,8716.8,8779.4,9143.3,8688.2,9532.1,9315.2,9555.5,10462.9,10715.8,10880,10444.5,10502.2,10299.5,10381.5,10523.5,10211,10043.9,10451.5,9840.3,9600.4,9980.6,9465.5,9622.7,9870.7,9399.1,9389.2,8893.5,9077.2,8539.9,9056.7,9277.7,9240.3,9570.6,9004.2,9198.8,8971,8812.9,9244.6,9257.5,9352,9549.2,9367.9,8741.5,6785.4,6922.3,7096.5,7231.4,6877.4,6969.5,6716.6,6452.2,8076.9,8073.7,7757.5,8225,8580,8815,9148.9,8821.2,8675.7,8846.6,8796.3,9057.7,8305.1,8713.8,8612.8,8479.2,8445.1,8584.2,8851.5,8098.7,8156.2,7886.1,7366.8,7956.5,8363.2,8229.1,9034,9394.6,9232.5,9241,9050.2,9593,9641.5,9505.9,9428,9017.3,10058.2,10102.1,10077.7,10001.3]
%Ho dibuixem graficament.
plot(ibex_mensual,"blue")
axis([0 length(ibex_mensual) min(ibex_mensual) max(ibex_mensual)]);
title('IBEX-35 mensual');
hold off
%% 
%CORBA SUAVITZADA AMB LA FAMILIA DE WAVELETS TRIADA.
[C,L]=wavedec(dataTrain_mensual,3,'bior3.5');
%Després de realitzar les descomposicions truncarem a partir d'un valor i recompondrem el senyal, obtenint així les dades ja suavitzades.
coef=sqrt(2*log(100));%Calculem la part que va dins de l'arrel de la formula mitjançant la qual calculem l'umbral.
cD1=detcoef(C,L,1);
des_tip=std(cD1);%Calculem la desviació tipíca de la part de detalls.
thr=coef*des_tip;%Calculem el valor de l'umral multiplicant els coeficients calculats anteriorment.
Cthr=wthresh(C,'h',thr);%Trunquem amb la funció forta.
ibex_mensual_reconstruida1=waverec(Cthr,L,'bior3.5');

%Millorem la suvaització posant un % de suvització.
smoothing_percentage = 4; % Percentaje de suavització (ajusta segons siga necesssrai)
% Calcula el tamany de la finestra de suavització.
window_size = round(length(ibex_mensual_reconstruida1) * (smoothing_percentage / 100));

if mod(window_size, 2) == 0
    window_size = window_size + 1;
end

% Aplicar suavització amb mitjana mòbil.
smooth_data_final = movmean(ibex_mensual_reconstruida1, window_size);

% Visualizar els resultats.
figure;
plot(dataTrain_mensual,'r-' , 'DisplayName', 'Dades originals');
hold on;
plot(ibex_mensual_reconstruida1, 'b-', 'DisplayName', 'Suavització Wavelet');
legend('Location', 'northwest');
xlabel('Mesos');
ylabel('Valor IBEX35');
title('Suavització mitjançant wavelets');
hold off;
%% FUNCIÓ GENERAL.
smoothing_percentages = [4,10, 15, 20, 25, 30, 35, 40, 45, 50];
% Visualitzar els resultats
figure;
plot(dataTrain_mensual, 'r-', 'DisplayName', 'Dades originals');
hold on;
plot(ibex_mensual_reconstruida1, 'b-', 'DisplayName', 'Suavització Wavelet');
% Aplicar la suavització amb diferents percentatges
colors = lines(length(smoothing_percentages)); % Generar colors per a cada percentatge

for i = 1:length(smoothing_percentages)
    smoothing_percentage = smoothing_percentages(i);

    % Calcula el tamany de la finestra de suavització
    window_size = round(length(ibex_mensual_reconstruida1) * (smoothing_percentage / 100));
    
    if mod(window_size, 2) == 0
        window_size = window_size + 1;
    end

    % Aplicar suavització amb mitjana mòbil
    smooth_data_final = movmean(ibex_mensual_reconstruida1, window_size);
   
    % Visualitzar els resultats
    plot(smooth_data_final, 'Color', colors(i, :), 'DisplayName', sprintf('Suavització %d%%', smoothing_percentage));
end
legend;
xlabel('Mesos');
ylabel('Valor IBEX35');
title('Suavitzacions amb diferents percentatges');
hold off;
%% 
% Mesura de suavització.
% f=ibex_mensual_reconstruida1;
f=smooth_data_final;
N = length(f);
second_diffs = zeros(1, N-2);

% Calcular les segones derivades.
for i = 2:N-1
    second_diffs(i-1) = (f(i+1) - 2*f(i) + f(i-1))^2;
end

% Calcular la mitjana de les segones derivades.
S = mean(second_diffs);

% Resultat per pantalla.
disp(['La medida de suavidad es: ', num2str(S)]);


%El RMS també es calcula.
rms=sqrt(norm(smooth_data_final-dataTrain_mensual)^2/length(smooth_data_final));
%%
%Passar a format vector.
str = sprintf('%g, ', smooth_data_final);
str = [str(1:end-2) ')'];  % Elimina la última coma y añade paréntesis de cierre
disp(str);
