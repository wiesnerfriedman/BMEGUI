function bmeEstStat(dataFile,paramFile,smeanFile,tmeanFile,outFile1,outFile2);
% bmeEst.exe (Ver. 0.1)
%
% Calculate BME estimation
%
% Input: Data file
%        Parameter file
%        Spatial mean trend file
%        Temporal mean trend file
%        Output file1
% Output: Outputfile

try
    % Get data points and values
    rawdata = dlmread(dataFile);
    rawX = rawdata(:,1);
    rawY = rawdata(:,2);
    rawT = rawdata(:,3);
    rawId = rawdata(:,4);
    rawType = rawdata(:,5);
    rawVal1 = rawdata(:,6);
    rawVal2 = rawdata(:,7);

    % Get spatial mean trend
    smeandata = dlmread(smeanFile);
    sptlX = smeandata(:,1);
    sptlY = smeandata(:,2);
    sptlM = smeandata(:,3);

    % Get temporal mean trend
    tmeandata = dlmread(tmeanFile);
    tempG = tmeandata(:,1);
    tempM = tmeandata(:,2);

    % Read parameter file
    [estId,estFrom,estTo,sptl,temp,metr,nhmax,nsmax,numEstT,inclDataPts,...
     covm1,covp11,covp12,covp13,...
     covm2,covp21,covp22,covp23,covm3,covp31,covp32,covp33,...
     covm4,covp41,covp42,covp43,minVal,flgLog,flgRemvMean,...
     op1,op3,op4,op8,orderVal] = ...
                textread(paramFile,...
    '%d %f %f %f %f %f %d %d %d %d %s %f %f %f %s %f %f %f %s %f %f %f %s %f %f %f %n %d %d %n %n %n %n %n');

	% Set BME estimation parameters
    dmax = [sptl temp metr];
	if covm2{1} == '0'
	    covmodel = covm1{1};
	    covparam = [covp11 covp12 covp13];
	elseif covm3{1} == '0'
	    covmodel = {covm1{1},covm2{1}};
	    covparam = {[covp11 covp12 covp13],[covp21 covp22 covp23]};
	elseif covm4{1} == '0'
	    covmodel = {covm1{1},covm2{1},covm3{1}};
	    covparam = {[covp11 covp12 covp13],[covp21 covp22 covp23],[covp31 covp32 covp33]};
	else
	    covmodel = {covm1{1},covm2{1},covm3{1},covm4{1}};
	    covparam = {[covp11 covp12 covp13],[covp21 covp22 covp23],[covp31 covp32 covp33],[covp41 covp42 covp43]};
	end
    options=BMEoptions;
    if op1 ~= -1
        options(1) = op1;
    end
    if op3 ~= -1
        options(3) = op3;
    end
    if op4 ~= -1
        options(4) = op4;
    end
    if op8 ~= -1
        options(8) = op8;
    end
    if orderVal == 99
	    order = NaN;
    else
        order = orderVal;
    end
    
    % Create estimation points
    tg = estFrom:(estTo-estFrom)/(numEstT-1):estTo;
    coordX = rawX(rawId==estId);
    coordY = rawY(rawId==estId);
    sdata = unique([coordX,coordY],'rows');
    if size(sdata,1) ~= 1
        error('')
    end
    sk = [(ones(size(tg)) * sdata(1))',(ones(size(tg)) * sdata(2))',tg'];

    if inclDataPts
        dataPtsT = rawT(rawId==estId);
        sk = [sk;(ones(size(dataPtsT)) * sdata(1)),(ones(size(dataPtsT)) * sdata(2)),dataPtsT];
        sk = sortrows(sk);
        sk = unique(sk,'rows');
    end

    % Create coordinate matrix
    rawCoord = [rawX,rawY,rawT];

    % Set coordinates for hard data and soft data
    ch = rawCoord(rawType == 0,:);
    cs1 = rawCoord(rawType == 1,:);
    cs2 = rawCoord(rawType == 2,:);

    % Calculate mean interpolation
    if flgRemvMean
        mstI=stmeaninterpstv([sptlX,sptlY],tempG',sptlM,tempM',rawCoord);
    end

    % Set hard data values
    if flgLog
        val = rawVal1(rawType == 0,:);
        val(val<=0) = minVal;
        zh = log(val);
    else
        zh = rawVal1(rawType == 0,:);
    end
    % Create mean trend removed value
    if flgRemvMean
        zh = zh -mstI(rawType == 0,:);
    end

    % Set soft data values (Uniform)
    if flgLog
        lowLimi = rawVal1(rawType == 1,:);
        upLimi = rawVal2(rawType == 1,:);
        nZeroLow = lowLimi(lowLimi<=0,:);
        nZeroUp = upLimi(lowLimi<=0,:);
        if length(upLimi(lowLimi<=0,:)) > 0
            upLimi(lowLimi<=0,:) = minVal + (nZeroUp - nZeroLow);
            lowLimi(lowLimi<=0,:) = minVal;
        end
        zlow = log(lowLimi);
        zup = log(upLimi);
    else
        zlow = rawVal1(rawType == 1,:);
        zup = rawVal2(rawType == 1,:);
    end
    % Create mean trend removed value
    if flgRemvMean
        zlow = zlow - mstI(rawType == 1,:);
        zup = zup - mstI(rawType == 1,:);
    end
    % Create soft data (Uniform)
    [stype1,nl1,limi1,probdens1]=probaUniform(zlow,zup);

    % Set soft data values (Gaussian)
    if flgLog
        rmean = rawVal1(rawType == 2,:);
        rstd = rawVal2(rawType == 2,:);
        rvar = rstd.^2;
        zvar = log(1.+(rvar./rmean.^2));
        zmean = log(rmean)-(zvar.^2)/2;
    else
        zmean = rawVal1(rawType == 2,:);
        zstd = rawVal2(rawType == 2,:);
        zvar = zstd.^2;
    end
    % Create mean trend removed value
    if flgRemvMean
        zmean = zmean - mstI(rawType == 2,:);
    end
    % Create soft data (Gaussian)
    [stype2,nl2,limi2,probdens2]=probaGaussian(zmean,zvar);

    %Concatenate soft data
    if (size(cs1,1) == 0) & (size(cs2,1) == 0)
        cs = [];
        stype = 2;
        nl=[];
        limi=[];
        probdens=[];
    elseif (size(cs1,1) == 0) & (size(cs2,1) ~= 0)
        cs = cs2;
        stype = stype2;
        nl = nl2;
        limi = limi2;
        probdens = probdens2;
    elseif (size(cs1,1) ~= 0) & (size(cs2,1) == 0)
        cs = cs1;
        stype = stype1;
        nl = nl1;
        limi = limi1;
        probdens = probdens1;
    else
        cs = [cs1;cs2];
        [stype,nl,limi,probdens]=probacat(stype1,nl1,limi1,probdens1,...
                                      stype2,nl2,limi2,probdens2);
    end

	% Dummy
	exponentialC([1 2],[1 2]);
	gaussianC([1 2],[1 2]);
	sphericalC([1 2],[1 2]);
	holesinC([1 2],[1 2]);
	holecosC([1 2],[1 2]);
 
    % Combine duplicated data
    [chC,zhC,csC,stypeC,nlC,limiC,probdensC] = combinedupli(ch,zh,'ave',cs,stype,nl,limi,probdens);
	
	% Calculate BME momemnts
	[moments,info]=BMEprobaMoments(sk,chC,csC,zhC,...
                                   stypeC,nlC,limiC,probdensC,...
                                   covmodel,covparam,nhmax,nsmax,dmax,order,options);

    % Output the result
    estMean = moments(:,1);
    estVar = moments(:,2);
    if flgRemvMean
        mk = stmeaninterpstv([sptlX,sptlY],tempG',sptlM,tempM',sk);
        estMean = estMean + mk;
    end
    dlmwrite(outFile1, [sk(:,3),estMean,estVar],'delimiter',',','precision','%12.10g');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if length(csC) ~= 0
        idxLoc = find(csC(:,1)==sdata(1) & csC(:,2)==sdata(2));
        dataLimi = [];
        dataProb = [];
        dataTime = [];
        if flgRemvMean
            dataMst = [];    
        end
        for i = 1:length(idxLoc)
            dataLimi = [dataLimi;limiC(idxLoc(i),:)];
            dataProb = [dataProb;probdensC(idxLoc(i),:)];
            dataTime = [dataTime;csC(idxLoc(i),3)];
            if flgRemvMean
                dataMst = [dataMst;mstI(rawCoord(:,1)==sdata(1) & rawCoord(:,2)==sdata(2) & ...
                                        rawCoord(:,3)==csC(idxLoc(i),3))];
            end
        end
        if flgRemvMean
            for i = 1:size(dataLimi,2)
                dataLimi(:,i) = dataLimi(:,i) + dataMst;
            end
        end
        if length(dataLimi) ~= 0
            dlmwrite(outFile2, [dataTime,dataLimi,dataProb],'delimiter',',','precision','%12.10g');
        end
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
catch
    error('bmeEstStat function failed')
    
end
