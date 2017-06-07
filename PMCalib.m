function PMCalib()
load('aqCalib.mat');
Diam=[0.265 0.290 0.325 0.375 0.425 0.475 0.540 0.615 0.675 0.750 0.900 1.150 1.450 1.800 2.250 2.750 3.250 3.750 4.500 5.750 7.000 8.000 9.250]';
Dens=[2750 2750 2750 2750 2750 2750 2750 2750 2750 2750 2750 2850 2850 2850 2850 2180 2180 2180 2180 2180 2180 2180 2180]'; 
Mass=((4*pi/3)*(Diam./2).^3).*Dens/1000000;

PM1mod=aqCalibNUM(:,1:11)*Mass(1:11);
PM25mod=aqCalibNUM(:,1:15)*Mass(1:15);
PM10mod=aqCalibNUM(:,1:23)*Mass(1:23);

plot(PM10mod,aqCalibPM(:,1),'o')
save('aqCalibMod.mat','PM1mod','PM25mod','PM10mod');

end
