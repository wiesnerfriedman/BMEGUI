function flagRT = checkRiverNetworkContinuity(workingDirectory)
% Before analysing River Network make sure all RIVER REACHES are
% connencted with main REACH and main REACH coonected with DOWN STREAM
% OUTLET point
%
% INPUT:   fileName (string)                Name of River Network File 
%                                           where last row is considered as
%                                           Down Stream (ds) Outlet point
%
%          distanceTolerance (scalar)       distance tolerance to determine
%                                           when two points are co-located
%                                           default is 0.02
%
%
%
%distanceTolerance=0; 
existID = exist('stdPathRT.txt', 'file');   % Analyze River Network ?
if (existID==2)
    RTfile = textread('stdPathRT.txt', '%s', 'delimiter', '\n','whitespace', '');
    
    [pathstr, name, ext] = fileparts(RTfile{1}) 
    if ext =='.shp'             
            c=shaperead(RTfile{1}); 
            cordX =[];
            cordY =[];
            for ii=1:length(c)
                cordX = [cordX; (c(ii,1).X)';NaN];
                cordY = [cordY; (c(ii,1).Y)';NaN];
            end
            
            % Invoke GUI asking user for Outlet cordinates
            prompt = {'Enter network outlet Latitude (X):               ','Enter network outlet Longitude (Y):         '};
            dlg_title = 'BMEGUI Enter network Outlet cordinates (X, Y)';
            num_lines = 1;
            def = {'10','20'};
            answer = inputdlg(prompt,dlg_title,num_lines,def,'on');        
            
            sRiverOutlet = [str2num(answer{1}),str2num(answer{2})];
            %RiverNetwork = [cordX, cordY]
            RiverNetwork = [cordX(1:end-1), cordY(1:end-1)];
    
    
    elseif ext =='.csv'
        RiverNetwork = csvread(RTfile{1},1); 
        % check that riverReaches are river reaches, etc.
        if size(RiverNetwork,2)~=2, 
          error('River Network must have two columns (Longitude, Latitude)'); 
        end
        sRiverOutlet = RiverNetwork(end,:);     % Get RiverOutlet (last row)
        RiverNetwork = RiverNetwork(1:end-1,:); % remove down stream point 
    else
        errordlg('Only *.csv and *.shp files are acceptable')
    end
    
    
    
    
    nanID = find(isnan(RiverNetwork(:,1)));
    riverReachesRaw =[];
    kk=1;
   for ij=1:length(nanID)
        riverReach ={RiverNetwork(kk:nanID(ij)-1,:)};
        riverReachesRaw =[riverReachesRaw,riverReach];
        kk = nanID(ij)+1;
   end 
distanceTolerance =0; 
% check that riverReaches are river reaches, etc.
if size(sRiverOutlet,1)~=1 | size(sRiverOutlet,2)~=2, 
  error('sRiverOutlet must be of size 1 x 2'); 
end
xDS=sRiverOutlet(1,1);
yDS=sRiverOutlet(1,2);

% Get the endpoints of the raw river reaches
nr=length(riverReachesRaw);
for ir=1:nr
  riverReachesRawFirstNode(ir,1:2)=riverReachesRaw{ir}(1,:);
  riverReachesRawLastNode(ir,1:2)=riverReachesRaw{ir}(end,:);
end

infoval=0;
infoMsg=[];
   % Check if RiverOutlet is on the River Reach
   idxReachOutlet=[];
   for ir=1:nr        
      idx=find( xDS ==riverReachesRaw{ir}(:,1) & yDS ==riverReachesRaw{ir}(:,2) );
      if length(idx)>1
        infoval=1;
        infoMsg=sprintf('Found the river network downstream point more than once in reach %d',ir);
      elseif length(idx)==1
            if ~isempty(idxReachOutlet)
                infoval=2;
                infoMsg=sprintf('Found the river network downstream point in reach %d and reach %d',ir,idxReachOutlet(1));
            else
               idxReachOutlet=[ir,idx];
            end
     % elseif sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(1,:)& sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(end,:)
     %     infoval=3;
     %     infoMsg=sprintf('The River Downstream point,sRiverOutlet, is not at one end of the reach. The point is currently located in element %d of reach %d',idx,ir);
      else
          infoval=4;
          infoMsg='River-outlet is a location not found in the river network'; 
      end
   end
  
     
irRawDS=idxReachOutlet(1,1); 
ir=1;
riverOrder=1;
if idxReachOutlet(1,2)>1
  riverReaches{1}=riverReachesRaw{irRawDS}(end:-1:1,:);
else
  riverReaches{1}=riverReachesRaw{irRawDS};
end;
riverReachesRaw(irRawDS)=[];
riverReachesRawFirstNode(irRawDS,:)=[];
riverReachesRawLastNode(irRawDS,:)=[];
riverTopology(1,1:3)=[ir riverOrder 0];

networkInterupted=0;
finishedWithPreviousOrder=1;
i=1;
% Go through all the river reach network
while length(riverReachesRaw)>0 & networkInterupted==0    
  if finishedWithPreviousOrder==1
    riverOrder=riverOrder+1;
  end
  idxPreviousOrder=find(riverTopology(:,2)==riverOrder-1); % index to reaches of previous order
  if length(idxPreviousOrder)==0     
    riverTopology;
    networkInterupted=1;
    infoval=5;
    infoMsg='Interuption in the river network, check last reach for break';    
  end
  finishedWithPreviousOrder=1;
  for j=1:length(idxPreviousOrder)                         % For each reach of previous order
    irPO=idxPreviousOrder(j);                                  
    DistToFirstNodes=coord2dist(riverReaches{irPO}(end,:),riverReachesRawFirstNode);
    DistToLastNodes=coord2dist(riverReaches{irPO}(end,:),riverReachesRawLastNode);
    idxFirstNodesUend=find(DistToFirstNodes<=distanceTolerance);  
    idxLastNodesUend=find(DistToLastNodes<=distanceTolerance);
    idxLastNodesUend=setdiff(idxLastNodesUend,idxFirstNodesUend);
    %
    %   If there is one upstream reach only then combine the up and downstream
    %   reach together.  If there is more than one upstream reach then put 
    %   them in the new network
    if length([idxFirstNodesUend idxLastNodesUend])==1  
      finishedWithPreviousOrder=0;
      if length(idxFirstNodesUend)==1
        riverReaches{irPO}=[riverReaches{irPO};riverReachesRaw{idxFirstNodesUend}];
      else
        riverReaches{irPO}=[riverReaches{irPO};riverReachesRaw{idxLastNodesUend}(end:-1:1,:)];
      end
    elseif length([idxFirstNodesUend idxLastNodesUend])>1
      for k=1:length(idxFirstNodesUend)
        ir=ir+1;
        riverReaches{ir}=riverReachesRaw{idxFirstNodesUend(k)};
        riverTopology(ir,1:3)=[ir riverOrder irPO];
      end
      for k=1:length(idxLastNodesUend)
        ir=ir+1;
        riverReaches{ir}=riverReachesRaw{idxLastNodesUend(k)}(end:-1:1,:);
        riverTopology(ir,1:3)=[ir riverOrder irPO];
      end      
    end
    if length([idxFirstNodesUend idxLastNodesUend])>0
      riverReachesRaw([idxFirstNodesUend idxLastNodesUend])=[];
      riverReachesRawFirstNode([idxFirstNodesUend idxLastNodesUend],:)=[];
      riverReachesRawLastNode([idxFirstNodesUend idxLastNodesUend],:)=[];  
    end
  end
end



    if ~isempty(riverReachesRaw)        
        hh = figure;
        for i=1:length(riverReaches)
            plot(riverReaches{i}(:,1),riverReaches{i}(:,2),'b');
            hold on;
        end
        for i=1:length(riverReachesRaw)
            plot(riverReachesRaw{i}(:,1),riverReachesRaw{i}(:,2),'r'); 
            hold on;
        end
        xlim([min(RiverNetwork(:,1)), max(RiverNetwork(:,1))]);
        title(sprintf('River Reaches (in red) are not connected \n Please make sure all Reaches are connected\n Use ArcGIS to connect them'));
        xlabel('X');
        ylabel('Y');
        box on;
        scatter(xDS, yDS,25,'o');
        saveStr = [char(workingDirectory),'\River_Network_Error.png'];
        saveas(hh,saveStr) ;        
        close gcf;
        flagRT =  1;
    else              
        flagRT =0;
    end
    dlmwrite('ErrorRiverNet.txt',flagRT,'delimiter','\t','precision','%.6f')
    
end
  
