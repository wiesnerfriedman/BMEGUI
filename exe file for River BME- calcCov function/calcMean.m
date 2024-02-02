function calcMean(dataFile,outFile1,outFile2,p);
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
    rawVal = rawdata(:,4);
    
    if (isdeployed)
        p = str2num(p);
    end

    [Z,cMS,tME,nanratio]=valstv2stg([rawX,rawY,rawT],rawVal);
    [ms,mss,mt,mts]=stmean2(Z,cMS,tME,p);
        
    dlmwrite(outFile1,[cMS,ms,mss],'delimiter',',','precision','%12.10g');
    dlmwrite(outFile2,[tME',mt',mts'],'delimiter',',','precision','%12.10g');
catch
    error('calcMean function failed')
end