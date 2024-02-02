function calcCov(dataFile1,dataFile2,dataFile3,outFile);
% calcCov.exe (Ver. 0.1) 
%
% Taking the average value at the duplicated points
%
% Input: data file containing X, Y, t, and val
% Output: data file containing averaged X, Y, t, and val


%%%% P JAT: This file call following functions:
%                                    function getRiverTopology()
%                                    function coord2distRiver()
%
%---- Add Progress Bar : P Jat--------------------------------------------
txt = sprintf('Calculating Covariance Model\n\n Please Wait .......');
disp(txt)
h = waitbar(.10,txt, 'Name','BMEGUI');     % Handle for Progress Bar: P Jat

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
    
 
    %[Z,cMS,tME,nanratio]=valstv2stg([strfX,strfY,strfT],strfVal);
    %[C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol); % P Jat Blocked
    
    
    
    waitbar(.15,h) ; %************************
    
    %---------- P Jat: Modified for River Metrics -------------------------
    %Use River Topology if River Dist has to be analyzed (P Jat May,2011)
    
    existID = exist('stdPathRT.txt', 'file');
    if (existID==2 & tLag ==0)
        if exist('riverTopology')
            delete 'riverTopology';
        end  
        if exist('riverReaches')
            delete 'riverReaches';
        end 
        RTfile = textread('stdPathRT.txt', '%s', 'delimiter', '\n','whitespace', '');
        XnetCord=dlmread('XnetCord.txt');
        YnetCord=dlmread('YnetCord.txt');
        RiverNetwork = [XnetCord,YnetCord]; % Because of precision (we lose precision in csv file)
        %%%%RiverNetwork = csvread(RTfile{1},1); 

        % Obtain River Topology using 'getRiverTopology()' function        
        sRiverOutlet = RiverNetwork(end,:); % Get RiverOutlet (last line of RiverNetwork)
        RiverNetwork = RiverNetwork(1:end-1,:);% Remove Last Lat/Lon (RiverOutlet)
        nanID = find(isnan(RiverNetwork(:,1)));
        riverReachesRaw =[];
        kk=1;
            for ij=1:length(nanID)
                riverReach ={RiverNetwork(kk:nanID(ij)-1,:)};
                riverReachesRaw =[riverReachesRaw,riverReach];
                kk = nanID(ij)+1;
            end

        [riverReaches,riverTopology]=getRiverTopology(riverReachesRaw, sRiverOutlet);  
        %save riverTopology.mat riverTopology    
        save riverTopology riverTopology;
        save riverReaches riverReaches;     % Save river reaches (array 1xn cells) use in bmeEst(), lin124
    
    
        waitbar(.45,h) ; %************************
        %phMR=[cMS tME];
        phMR = [strfX,strfY,strfT];
        pTolerance=0.01;
        %[c1R,c2R]=cartesian2riverProj(riverReachesRaw,phMR,pTolerance);
        [c1R,c2R]=cartesian2riverProj(riverReachesRaw,phMR,pTolerance);
        waitbar(.85,h) ; %************************
        disp('Yes:      cartesian2riverProj works well')
        [Z,idcMS,tME,nanratio]=valstv2stg(c1R,strfVal);  %aggregated data
        cMSR=idcMS(:,1:4); 
        save cMSR cMSR   % save for BME estimation
        disp('Yes:      valstv2stg works well')
        
        [C np]=stcov(Z,cMSR,tME,Z,cMSR,tME,rLag,rLagTol,tLag,tLagTol,{'coord2distRiver' [riverTopology]});
        disp('Yes:      stcov works well')
        disp('____________________________________________')
        disp('using River Distances function: coord2distRiver');
        waitbar(.95,h) ; %************************
        
    else
        [Z,cMS,tME,nanratio]=valstv2stg([strfX,strfY,strfT],strfVal);
        [C np]=stcov(Z,cMS,tME,Z,cMS,tME,rLag,rLagTol,tLag,tLagTol);           
    end
    %----------------------------------------------------------------------
    
    
    
      
    
    sizeC = size(C);
    if sizeC(1,1) == 1
        C = C';
    end
    dlmwrite(outFile,C,'delimiter',',','precision','%12.10g');
    close(h) %------------ close Progress Bar WINDOW --------------------------
catch
    error('calcCov function failed')
end    

