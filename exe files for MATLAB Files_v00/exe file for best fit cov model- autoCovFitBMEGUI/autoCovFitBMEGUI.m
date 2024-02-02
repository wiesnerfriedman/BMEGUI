function autoCovFitBMEGUI
%function autoCovFitBMEGUI(lagSpt,covValSpt,lagTmp,covValTmp)

%
% This function is written for automatic covariance fitting
% in BMEGUI. Funtions calls another function and find optimal
% covariance parameters (Sill and Range) and covariance model
%
% INPUTS:  lagSpt       Spatial cov model Lag
%          covValSpt    Spatial Cavariance values
%          lagTmp       Temporal cov model Lag
%          covValTmp    Temporal Cavariance values
%
% Prahlad Jat
% BMEGUI3@bmelab
% July 2011
%
%
txt = sprintf('Obtaining Best fit Covariance Model\n\n Please Wait .......');
disp(txt);
h = waitbar(.01,txt, 'Name','BMEGUI');  



lagSpt    = dlmread('lagSpt.txt');
covValSpt = dlmread('covValSpt.txt');

lagTmp    = dlmread('lagTmp.txt');
covValTmp = dlmread('covValTmp.txt');

lagSpt    = lagSpt';
covValSpt = covValSpt';
lagTmp    = lagTmp';
covValTmp = covValTmp';

waitbar(0.1,h) ; %************************  P Jat  
%%% weightSpt  = 1./(ones(size(lagSpt)).*exp(lagSpt));
%%% weightTmp  = 1./(ones(size(lagTmp)).*exp(lagTmp));
weightSpt  = ones(size(lagSpt));
weightTmp  = ones(size(lagTmp));


[covparamSpt1,aicvalSpt1] = lscovfit(lagSpt,covValSpt,weightSpt,'exponential');
waitbar(0.2,h) ; %************************  P Jat  

[covparamSpt2,aicvalSpt2] = lscovfit(lagSpt,covValSpt,weightSpt,'gaussian');
waitbar(0.3,h) ; %************************  P Jat  

[covparamSpt3,aicvalSpt3] = lscovfit(lagSpt,covValSpt,weightSpt,'spherical');
waitbar(0.4,h) ; %************************  P Jat 

[covparamTmp1,aicvalTmp1] = lscovfit(lagTmp,covValTmp,weightTmp,'exponential');
waitbar(0.5,h) ; %************************  P Jat  

[covparamTmp2,aicvalTmp2] = lscovfit(lagTmp,covValTmp,weightTmp,'gaussian');
waitbar(0.6,h) ; %************************  P Jat  

[covparamTmp3,aicvalTmp3] = lscovfit(lagTmp,covValTmp,weightTmp,'spherical');
waitbar(0.75,h) ; %************************  P Jat  

% now find best fit model and associated paramets based on AIC values
sptAIC = [aicvalSpt1,aicvalSpt2,aicvalSpt3];
tmpAIC = [aicvalTmp1,aicvalTmp2,aicvalTmp3];
minSptAIC = find(sptAIC==min(sptAIC));
minTmpAIC = find(sptAIC==min(sptAIC));
waitbar(0.80,h) ; %************************  P Jat  
if minSptAIC==1
    modelSpt   = 'exponentialC';
    modelSptN  = 1;
    sillSpt    = covparamSpt1(1);
    rangeSpt   = covparamSpt1(2);
elseif minSptAIC==2
    modelSpt   = 'gaussian';
    modelSptN  = 2;
    sillSpt    = covparamSpt2(1);
    rangeSpt   = covparamSpt2(2);
else
    modelSpt   = 'spherical';
    modelSptN  = 3;
    sillSpt    = covparamSpt3(1);
    rangeSpt   = covparamSpt3(2);
end



if minTmpAIC==1
    modelTmp   = 'exponentialC';
    modelTmpN  = 1;
    rangeTmp   = covparamTmp1(2);
elseif minTmpAIC==2
    modelTmp   = 'gaussian';
    modelTmpN  = 2;
    rangeTmp   = covparamTmp2(2);
else
    modelTmp   = 'spherical';
    modelTmpN  = 3;
    rangeTmp   = covparamTmp3(2);
end
fittedCovModelInfo = [modelSptN, sillSpt,rangeSpt,modelTmpN, rangeTmp];
if exist('fittedCovModel.txt', 'file')
    delete 'fittedCovModel.txt'
end  
waitbar(0.85,h) ; %************************  P Jat  

dlmwrite('fittedCovModel.txt',fittedCovModelInfo);

close(h) %------------ close Progress Bar WINDOW -------------------------- 

    







