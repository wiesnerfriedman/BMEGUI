function removeMean(dataFile1,dataFile2,dataFile3,outFile);
% removeMean.exe (Ver. 0.1) 
%
% Taking the average value at the duplicated points
%
% Input: data file containing X, Y, t, and val
% Output: data file containing averaged X, Y, t, and val

try
    rawdata = dlmread(dataFile1);
    vectX = rawdata(:,1);
    vectY = rawdata(:,2);
    vectT = rawdata(:,3);
    rawdata = dlmread(dataFile2);
    sptlX = rawdata(:,1);
    sptlY = rawdata(:,2);
    sptlM = rawdata(:,3);
    rawdata = dlmread(dataFile3);
    tempG = rawdata(:,1);
    tempM = rawdata(:,2);

    mstI=stmeaninterpstv([sptlX,sptlY],tempG',sptlM,tempM',[vectX,vectY,vectT]);
        
    dlmwrite(outFile,mstI,'delimiter',',','precision','%12.10g');
catch
    error('removeMean function failed')
end