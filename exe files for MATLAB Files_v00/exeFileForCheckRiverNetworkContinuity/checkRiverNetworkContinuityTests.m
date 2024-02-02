function checkRiverNetworkContinuityTests(workingDirectory)
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
%------------------ DELETE OLD FILES IF EXIST -----------------------------
if exist('riverNetX.txt', 'file')
 delete riverNetX.txt; end
if exist('riverNetY.txt', 'file')     
 delete riverNetY.txt; end
if exist('connectedNetX.txt', 'file')
 delete connectedNetX.txt; end
if exist('connectedNetY.txt', 'file')                      
 delete connectedNetY.txt; end
if exist('disconnectedNetX.txt', 'file')
 delete disconnectedNetX.txt; end
if exist('disconnectedNetY.txt', 'file')
 delete disconnectedNetY.txt; end 
if exist('RiverNetwork11.csv', 'file')
 delete RiverNetwork11.csv; end   

 if exist('XnetCord.txt','file')
     delete XnetCord.txt; end
  if exist('YnetCord.txt','file')
     delete YnetCord.txt; end    
%----------------------------End File deletion-----------------------------


try  
   existID = exist('stdPathRT.txt', 'file');   % Analyze River Network ?
   if (existID==2)
    RTfile = textread('stdPathRT.txt', '%s', 'delimiter', '\n','whitespace', '');
    
    [pathstr, name, ext] = fileparts(RTfile{1}); 
    if ext =='.shp'            
            c=shaperead(RTfile{1}); 
            cordX =[];
            cordY =[];
            for ii=1:length(c)
                cordX = [cordX; (c(ii,1).X)'];
                cordY = [cordY; (c(ii,1).Y)'];
            end
            
            % Invoke GUI asking user for Outlet cordinates
            prompt = {'Enter network outlet Latitude (X):               ','Enter network outlet Longitude (Y):         '};
            dlg_title = 'BMEGUI Enter network Outlet cordinates (X, Y)';
            num_lines = 1;
            def = {'-74.99454443','40.63935082'};           
            answer = inputdlg(prompt,dlg_title,num_lines,def,'on'); 
            
            h = waitbar(0,'Checking river network. Please wait...');

            RiverNetwork = [cordX, cordY];           
            sRiverOutlet = [str2num(answer{1}),str2num(answer{2})]; 
 
            nanID = find(isnan(RiverNetwork(:,1)));
            riverReachesRaw =[];
            kk=1;
            for ij=1:length(nanID)-1
                riverReach ={RiverNetwork(kk:nanID(ij)-1,:)};
                riverReachesRaw =[riverReachesRaw,riverReach];
                kk = nanID(ij)+1;
            end  
           
    elseif ext =='.csv'
        h = waitbar(0,'Checking river network. Please wait...');
        RiverNetwork = csvread(RTfile{1},0); 
        % check that riverReaches are river reaches, etc.
        if size(RiverNetwork,2)~=2, 
          error('River Network must have two columns (Longitude, Latitude)'); 
        end
        sRiverOutlet = RiverNetwork(end,:);     % Get RiverOutlet (last row)
        RiverNetwork = RiverNetwork(1:end-1,:); % remove down stream point 
        
        nanID = find(isnan(RiverNetwork(:,1)));
        riverReachesRaw =[];
        kk=1;
       for ij=1:length(nanID)
            riverReach ={RiverNetwork(kk:nanID(ij)-1,:)};
            riverReachesRaw =[riverReachesRaw,riverReach];
            kk = nanID(ij)+1;
       end 
       
    else
        h = waitbar(0,'Checking river network. Please wait...');
        errordlg('Only *.csv and *.shp files are acceptable')
    end


%---------Save riverNetwork for covariance and BME est %-------------------------
waitbar(.05,h) ; %************************
RiverNetworkSave = [RiverNetwork;sRiverOutlet];
dlmwrite('XnetCord.txt',RiverNetworkSave(:,1),'delimiter','\t','precision','%.12f');
dlmwrite('YnetCord.txt',RiverNetworkSave(:,2),'delimiter','\t','precision','%.12f');
%csvwrite('RiverNetwork11.csv', RiverNetworkSave);% end-1 because last line is NaN
waitbar(.1,h) ; %************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%          
% Change RiverNetwork File Path
% fid = fopen('stdPathRT.txt', 'w');
% fwrite(fid, [cd,'/RiverNetwork11.csv']);
% fclose(fid);
   
    distanceTolerance =0; 
    % check that riverReaches are river reaches, etc.
    if size(sRiverOutlet,1)~=1 | size(sRiverOutlet,2)~=2, 
      error('sRiverOutlet must be of size 1 x 2'); 
    end
    xDS=sRiverOutlet(1,1);
    yDS=sRiverOutlet(1,2);
    
    
    

%     %-------------- Write X, Y coordinates if there no network error---
     data=[RiverNetwork;[xDS, yDS]];     % River Coordinates from original file
     data(isnan(data))=-9999;
     dlmwrite('riverNetX.txt', data(:,1));
     dlmwrite('riverNetY.txt', data(:,2));
%     %------------------------------------------------------------------
    
    
    
    

    % Get the endpoints of the raw river reaches
    nr=length(riverReachesRaw);    
    for ir=1:nr
      riverReachesRawFirstNode(ir,1:2)=riverReachesRaw{ir}(1,:);
      riverReachesRawLastNode(ir,1:2)=riverReachesRaw{ir}(end,:);
    end
    
    %-------------  Check if RiverOutlet is on the River Reach ----------------
    infoval=0;
    infoMsg=[];
    waitbar(.17,h) ; %************************
      
       % Check if RiverOutlet is on the River Reach
       idxReachOutlet=[];
       idxReachOutletST=[];
       for ir=1:nr 
          t=0.45/(nr-ir);
          waitbar(.15+t,h) ; %************************         
          idx=find( xDS ==riverReachesRaw{ir}(:,1) & yDS ==riverReachesRaw{ir}(:,2) );          
          if length(idx)>1
            infoval=1;
            infoMsg=sprintf('Found the river network downstream point more than once in reach %d',ir);
          elseif length(idx)==1              
                if ~isempty(idxReachOutlet)
                    infoval=2;
                    idxReachOutlet=[ir,idx];
                    idxReachOutletST=[idxReachOutletST; idxReachOutlet]
                    infoMsg=sprintf('Found the river network downstream point in reach %d and reach %d',ir,idxReachOutlet(1));
                else
                   idxReachOutlet=[ir,idx];
                   idxReachOutletST=[idxReachOutletST; idxReachOutlet];
                end
         % elseif sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(1,:)& sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(end,:)
         %     infoval=3;
         %     infoMsg=sprintf('The River Downstream point,sRiverOutlet, is not at one end of the reach. The point is currently located in element %d of reach %d',idx,ir);
          else
              infoval=4;
              infoMsg='River-outlet is a location not found in the river network'; 
          end
       end
      idxReachOutlet=idxReachOutletST;
              
%        figure;
%        plot(xDS, yDS, 'sr');
%        hold on;
%        plot(RiverNetwork(:,1), RiverNetwork(:,2),'-b');
%        legend('Network Outlet', 'Stream/river Network');
%        xlabel('X');
%        ylabel('Y');
%        set(gcf,'NumberTitle','off','Name','BMEGUI: Stream/River network')

       %---------- Outlet error check-----------------------------------------
       if size(idxReachOutlet,1)==1
           NetworkErrorNo=0; % NO Error          
       elseif isempty(idxReachOutlet)
           NetworkErrorNo=1; % % Outlet location outside network 
           %title('Error: Outlet location outside network', 'FontSize',15, 'color','red')
       elseif size(idxReachOutlet,1)>1
           NetworkErrorNo=2; % Multiple outlet locations
           %title('Error: Multiple outlet locations detected', 'FontSize',15, 'color','red')
       else
           NetworkErrorNo=3; % unknown outlet error
           %title('Error: Unknown outlet error detected', 'FontSize',15, 'color','red')
       end
       %---------- end: Outlet error check------------------------------------
       
       waitbar(.65,h) ; %************************
       
        if NetworkErrorNo==0             
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
           
            waitbar(.9,h) ; %************************        
                if ~isempty(riverReachesRaw) 
                    close(gcf);
                    NetworkErrorNo=4; % Broken network error
                    conNet=[];
                    disConNet=[];
                    
                    connectedNetX=[];
                    connectedNetY=[];
                    disconnectedNetX=[];
                    disconnectedNetY=[];                   
                    
%                                     hh = figure;
%                                     scatter(xDS, yDS,25,'sr');
%                                     hold on;

                    for i=1:length(riverReaches)
                        conNet=[conNet;[riverReaches{i};NaN,NaN]];
                        connectedNetX=[connectedNetX; [riverReaches{i}(:,1);-9999]]; % -9999 as NaN (discountinuation of stream)
                        connectedNetY=[connectedNetY; [riverReaches{i}(:,2);-9999]];
                    end
                    for i=1:length(riverReachesRaw)                        
                        disConNet=[disConNet;[riverReachesRaw{i};NaN,NaN]];
                        disconnectedNetX=[disconnectedNetX; [riverReachesRaw{i}(:,1);-9999]];
                        disconnectedNetY=[disconnectedNetY; [riverReachesRaw{i}(:,2);-9999]];                                
                    end
                     
                     %-------------- Write X, Y coordinates if there is network error---             
                     dlmwrite('connectedNetX.txt',connectedNetX);
                     dlmwrite('connectedNetY.txt',connectedNetY);
                     dlmwrite('disconnectedNetX.txt',[disconnectedNetX;xDS]);
                     dlmwrite('disconnectedNetY.txt',[disconnectedNetY;yDS]);
                     %----------------------------------------------------------------
                     
                     waitbar(.95,h) ; %************************
%                             plot(conNet(:,1), conNet(:,2),'b'); 
%                             plot(disConNet(:,1), disConNet(:,2),'om','size',10); 
%                             %xlim([min(RiverNetwork(:,1)), max(RiverNetwork(:,1))]);
%                             %title(sprintf('River Reaches (in magenta) are not connected \n Please make sure all Reaches are connected\n Use ArcGIS to connect them'));
%                             xlabel('X');
%                             ylabel('Y');
%                             box on;
%                             legend('Network Outlet','Connected Network','Unconnected Network');
%                             set(gcf,'NumberTitle','off','Name','BMEGUI: Stream/River network')
%                             title(sprintf('Error: River Reaches (in magenta) are not connected \n Please make sure all Reaches are connected\n Use ArcGIS to connect them'), 'FontSize',15, 'color','red')
                   
                    %saveStr = [char(workingDirectory),'\River_Network_Error.png'];
                    %saveas(hh,saveStr) ;        
                    %close gcf;
                    %flagRT =  1;
                else              
                    %flagRT =0;
                end
                dlmwrite('ErrorRiverNet.txt',NetworkErrorNo,'delimiter','\t','precision','%.6f')
                %NetworkErrorNo=0; % NO Error   
                %NetworkErrorNo=1; % Outlet location outside network    
                %NetworkErrorNo=2; % Multiple outlet locations  
                %NetworkErrorNo=3; % unknown outlet error   
                %NetworkErrorNo=4; % Broken network error
                %NetworkErrorNo=5; % Unknown error identified in river network 
        else
                dlmwrite('ErrorRiverNet.txt',NetworkErrorNo,'delimiter','\t','precision','%.6f');    
        end
    end
                %saveStr = [char(workingDirectory),'\River_Network_Figure.png'];
                %saveas(gcf,saveStr) ;  
                %close(gcf);
                waitbar(1,h) ; %************************
                close(h);
                %if NetworkErrorNo==4
                %    system('riverNetworkPlotError.py');
                %elseif NetworkErrorNo==3 
                %    % do nothing because ERROR 3 is unknown error
                %else
                %    system('riverNetworkPlot.py');
                %end
                                    
catch
    waitbar(1,h) ; %************************
    close(h);
    NetworkErrorNo=5;    % Unknown error in river network 
    dlmwrite('ErrorRiverNet.txt',NetworkErrorNo,'delimiter','\t','precision','%.6f')    
end


