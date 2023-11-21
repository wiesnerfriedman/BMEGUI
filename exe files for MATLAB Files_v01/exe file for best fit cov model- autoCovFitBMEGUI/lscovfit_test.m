function lscovfit_test()

lag = 0:15;
weight  = ones(size(lag)) + randn(size(lag)) * 0.01;

covval1 = nuggetC(lag,0.3) + exponentialC(lag,[0.7,4]);
covval2 = nuggetC(lag,0.3) + gaussianC(lag,[0.7,4]);
covval3 = nuggetC(lag,0.3) + sphericalC(lag,[0.7,4]);

covparam1 = lscovfit(lag,covval1,weight,'exponential');
covparam2 = lscovfit(lag,covval2,weight,'gaussian');
covparam3 = lscovfit(lag,covval3,weight,'spherical');

disp(['exponential (Nugget)      : ',num2str(covparam1)]);
disp(['gaussian (Nugget)         : ',num2str(covparam2)]);
disp(['spherical (Nugget)        : ',num2str(covparam3)]);

covval4 = nuggetC(lag,0.3) + exppowC(lag,[0.7,4,1.5]);
covparam4 = lscovfit(lag,covval4,weight,'exppow');
disp(['exppow (Nugget)        : ',num2str(covparam4)]);

covval5 = exppowC(lag,[1,4,1.5]);
covparam5 = lscovfit(lag,covval5,weight,'exppownn');
disp(['exppow (No Nugget)        : ',num2str(covparam5)]);
