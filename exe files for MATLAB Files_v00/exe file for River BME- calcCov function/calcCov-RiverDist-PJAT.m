function calcCov(dataFile1,dataFile2,dataFile3,outFile);
% calcCov.exe (Ver. 0.1) 
%
% Taking the average value at the duplicated points
%
% Input: data file containing X, Y, t, and val
% Output: data file containing averaged X, Y, t, and val


%%%% P JAT: This file call following functions:
%                                    function getRiverTopology_4BMEGUI()
%                                    function coord2distRiver()
%
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
    %[C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol); % P Jat Blocked
    
    
    
    
    %---------- P Jat: Modified for River Metrics -------------------------
    %Use River Topology if River Dist has to be analyzed (P Jat May,2011)
    existID = exist('stdPathRT.txt', 'file');
    if existID==2
        RTfile = textread('stdPathRT.txt', '%s', 'delimiter', '\n','whitespace', '');
        RiverNetwork = csvread(RTfile{1},1);
        [riverTopology]=getRiverTopology_4BMEGUI(RiverNetwork);        
        [C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol,...
            {'coord2distRiver'    [riverTopology]});
    else
        [C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol);           
    end
    %----------------------------------------------------------------------
    
    
    
      
    
    sizeC = size(C);
    if sizeC(1,1) == 1
        C = C';
    end
    dlmwrite(outFile,C,'delimiter',',','precision','%12.10g');
catch
    error('calcCov function failed')
end    

