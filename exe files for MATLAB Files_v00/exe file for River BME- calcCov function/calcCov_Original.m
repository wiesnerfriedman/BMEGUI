function calcCov(dataFile1,dataFile2,dataFile3,outFile);
% calcCov.exe (Ver. 0.1) 
%
% Taking the average value at the duplicated points
%
% Input: data file containing X, Y, t, and val
% Output: data file containing averaged X, Y, t, and val

try
    strfdata = dlmread(dataFile1);
    strfX = strfdata(:,1);
    strfY = strfdata(:,2);
    strfT = strfdata(:,3);
    strfVal = strfdata(:,4);

    rlagdata = dlmread(dataFile2);
    rLag = rlagdata(:,1);
    rLagTol = rlagdata(:,2);

    tlagdata = dlmread(dataFile3);
    tLag = tlagdata(:,1);
    tLagTol = tlagdata(:,2);
    
    % Dummy %
    [dummy] = coord2dist([1,2],[4,5]);
    
    [Z,cMS,tME,nanratio]=valstv2stg([strfX,strfY,strfT],strfVal);
    [C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol);
    sizeC = size(C);
    if sizeC(1,1) == 1
        C = C';
    end
    dlmwrite(outFile,C,'delimiter',',','precision','%12.10g');
catch
    error('calcCov function failed')
end