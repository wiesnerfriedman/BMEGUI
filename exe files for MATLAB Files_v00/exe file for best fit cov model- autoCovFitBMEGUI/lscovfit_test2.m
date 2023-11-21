function lscovfit_test2()

load('testdata_2306_ALL.mat');
lonlat = [TestDataLon,TestDataLat];

distmat = calcdistmatdeg2km(lonlat);

lagparam = 15;
[lagbound,dummy] = calclagbound(distmat,lagparam);

[lag,covval,dummy,weight] = ...
    calccovmom(distmat,TestDataVal,lagbound);

[estparam1,aic1] = lscovfit(lag,covval,weight,'exponential');
[estparam2,aic2] = lscovfit(lag,covval,weight,'gaussian');
[estparam3,aic3] = lscovfit(lag,covval,weight,'spherical');
[estparam4,aic4] = lscovfit(lag,covval,weight,'exppow');
[estparam5,aic5] = lscovfit(lag,covval,weight,'exppownn');

[aic1,aic2,aic3,aic4,aic5]
disp(['exponential (Nugget)      : ',num2str(estparam1)]);
disp(['gaussian (Nugget)         : ',num2str(estparam2)]);
disp(['spherical (Nugget)        : ',num2str(estparam3)]);
disp(['exppow (Nugget)           : ',num2str(estparam4)]);
disp(['exppow (No Nugget)        : ',num2str(estparam5)]);

figure;
hold on;
plot(lag,covval,'ro');
mdllag = 0:lag(end)/100:lag(end);
mdlval1 = nuggetC(mdllag,estparam1(1)) + ...
          exponentialC(mdllag,[estparam1(2),estparam1(3)]);
mdlval2 = nuggetC(mdllag,estparam2(1)) + ...
          gaussianC(mdllag,[estparam2(2),estparam2(3)]);
mdlval3 = nuggetC(mdllag,estparam3(1)) + ...
          sphericalC(mdllag,[estparam3(2),estparam3(3)]);
mdlval4 = nuggetC(mdllag,estparam4(1)) + ...
          exppowC(mdllag,[estparam4(2),estparam4(3),estparam4(4)]);
mdlval5 = exppowC(mdllag,estparam5);

plot(mdllag,mdlval1,'-b');
plot(mdllag,mdlval2,'-g');
plot(mdllag,mdlval3,'-c');
plot(mdllag,mdlval4,'-m');
plot(mdllag,mdlval5,'-y');

nugget1 = estparam1(1);
nugget2 = estparam2(1);
nugget3 = estparam3(1);

sillval = covval(1)/100:covval(1)/100:covval(1);
rangeval = lag(2):max(lag)/100:max(lag);
[mesh1,mesh2] = meshgrid(sillval,rangeval);
covparam = [mesh1(:),mesh2(:)];

fval1 = zeros(size(covparam,1),1) * NaN;
fval2 = zeros(size(covparam,1),1) * NaN;
fval3 = zeros(size(covparam,1),1) * NaN;

for i = 1:size(covparam,1)
    fval1(i) = objfunexponential([nugget1,covparam(i,:)],...
                                 lag,covval,weight);
    fval2(i) = objfungaussian([nugget2,covparam(i,:)],...
                              lag,covval,weight);
    fval3(i) = objfunspherical([nugget3,covparam(i,:)],...
                              lag,covval,weight);
end

fval1mesh = reshape(fval1,size(rangeval,2),size(sillval,2));
fval2mesh = reshape(fval2,size(rangeval,2),size(sillval,2));
fval3mesh = reshape(fval3,size(rangeval,2),size(sillval,2));

figure;
hold on;
contour(mesh1,mesh2,fval1mesh,80);
plot(covparam(fval1==min(fval1),1),covparam(fval1==min(fval1),2),'rx');
plot(estparam1(2),estparam1(3),'bx');

figure;
hold on;
contour(mesh1,mesh2,fval2mesh,80);
plot(covparam(fval2==min(fval2),1),covparam(fval2==min(fval2),2),'rx');
plot(estparam2(2),estparam2(3),'bx');

figure;
hold on;
contour(mesh1,mesh2,fval3mesh,80);
plot(covparam(fval3==min(fval3),1),covparam(fval3==min(fval3),2),'rx');
plot(estparam3(2),estparam3(3),'bx');
