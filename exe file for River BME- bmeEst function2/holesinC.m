function [C]=holesinC(D,param);% holesinC                  - sinusoidal hole effect covariance model (Jan 1,2001)%% Compute the sinusoidal hole effect covariance matrix% from a distance matrix.%% SYNTAX :%% [C]=holesinC(D,param);%% INPUT :%% D       n by m   distance matrix, having real values >=0.% param   1 by 2   vector of parameters such that param(1) is%                  the sill and param(2) is the periodicity of%                  the hole effect.%% OUTPUT :%% C       n by m   covariance matrix with same size as D.%% NOTE :%% This model is only valid in a d-dimensional space with d<=3.isnull=find(D==0);D(isnull)=NaN;C=param(1)*sin(pi*D/param(2))./((pi*D)/param(2));C(isnull)=param(1);