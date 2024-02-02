function removeDupli(dataFile,outFile);
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
    rawId = rawdata(:,4);
    rawType = rawdata(:,5);
    rawVal = rawdata(:,6);
    [pu1,za1,i1] = avedupli([rawX,rawY,rawT],rawId);
    [pu2,za2,i2] = avedupli([rawX,rawY,rawT],rawType);
    [pu3,za3,i3] = avedupli([rawX,rawY,rawT],rawVal);
    if ~isequal(pu1,pu2,pu3)
        error('');
    end
    dlmwrite(outFile,[pu1,za1,za2,za3],'delimiter',',','precision','%12.10g');
catch exception
    error('removeDupli function failed');
end