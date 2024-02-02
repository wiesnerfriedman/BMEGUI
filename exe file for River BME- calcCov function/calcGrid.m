function calcGrid(dataFile,outFile1,outFile2);
% removeDupli.exe (Ver. 0.1) 
%
% Taking the average value at the duplicated points
%
% Input: data file containing X, Y, t, and val
% Output: data file containing averaged X, Y, t, and val

try
    rawdata = dlmread(dataFile);
    rawX = rawdata(:,1);
    rawY = rawdata(:,2);
    rawT = rawdata(:,3);
    rawVal = ones(size(rawX));
    [Z,cMS,tME,nanratio]=valstv2stg([rawX,rawY,rawT],rawVal);    

    dlmwrite(outFile1,cMS,'delimiter',',','precision','%12.10g');
    dlmwrite(outFile2,tME','delimiter',',','precision','%12.10g');
catch
    error('calcGrid function failed')
end