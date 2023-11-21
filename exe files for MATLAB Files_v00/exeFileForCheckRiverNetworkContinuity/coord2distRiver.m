function [rD]=coord2distRiver(c1,c2,riverTopology);

% coord2distRiver                 - computes the distance between two points along a river network.  
%
%SYNTAX:
%
%[rD]=coord2distRiver(c1,riverTopology);
%
%
%INPUT:
%
%c1         n1 x 5      First set of coordinates along the river network.  First column is a unique reach ID
%                       the Second column is the longitudinal distance along that reach starting from 
%                       downstream.
%                       column 1: X position (in original units, i.e. lat/long)
%                       column 2: Y position
%                       column 3: Reach ID
%                       column 4: Distance to DS End
%                       column 5: Time (only if using space/time)
%
%c2         n1 x 5      Second set of coordinates along the river network.  First column is a unique reach ID
%                       the Second column is the longitudinal distance along that reach starting from 
%                       downstream.
%                       column 1: X position (in original units, i.e. lat/long)
%                       column 2: Y position
%                       column 3: Reach ID
%                       column 4: Distance to DS End
%                       column 5: Time (only if using space/time)
%
%
%riverTopology     nr x 4      nr = # of reaches.  Matrix where each line identifies a reach.
%                              First Column     = Unique Reach ID
%                              Second Column    = Order of Reach
%                              Third Column     = Downstream Reach ID
%                              Fourth Column    = Longitudinal Length of Reach
%
%
%OUTPUT:
%
%rD          n1 x n1     Matrix of Distances whose dimensions equal the total # of reaches ID in n1,n2
%
%
%


%Check for errors in c1,c2 and riverTopology
sizec1=size(c1);
sizec2=size(c2);
sizerp=size(riverTopology);
clear i j;
if sizec1(2)<4
    error('c1 must be of size n1 x 4 (space) or n1 x 5 (space/time)');
end;

if sizec2(2)<4
    error('c2 must be of size n1 x 4 (space) or n1 x 5 (space/time)');
end;

if sizerp(2)~=4
    error('riverTopology must be of size nr x 4');
end;

%for i=1:length(c1(:,3))
 %   if c1(i,4) > riverTopology(c1(i,3),4)
  %      error(sprintf('error in row %d of c1, Total Reach Length must be greater than distance in c1',i));
   %   end;
    %end;

%for i=1:length(c2(:,3))
 %   if c2(i,4) > riverTopology(c2(i,3),4)
  %      error(sprintf('error in row %d of c1, Total Reach Length must be greater than distance in c2',i));
   % end;
%end;


clear i j;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Calculate the distance matrix

for i=1:size(c1,1)
  for j=1:size(c2,1)
    clear d;
    k=1;
    firstreachOrder=riverTopology(c1(i,3),2);
    secondreachOrder=riverTopology(c2(j,3),2);
    if c1(i,3) == c2(j,3)
      d(1)=abs(c1(i,4)-c2(j,4));%Check if points are on the same reach
    else
      if firstreachOrder>secondreachOrder %Determine if one point is directly downstream of another
        reachDS=riverTopology(c1(i,3),3);
        secondreach=c2(j,3);
      else
        reachDS=riverTopology(c2(j,3),3);
        secondreach=c1(i,3);
      end;
      ID=1;
      reachIDs=[];
      idx=[];
      while reachDS ~= 0 %find the DS reaches to determine if the points are DS of each other
        reachIDs(ID)=reachDS;
        reachDS=riverTopology(reachDS,3);
        ID=ID+1;
      end;
      idx=find(secondreach==reachIDs);
      if isempty(idx) %If not directly downstream, then calculate distance the following way
        firstreachDS=riverTopology(c1(i,3),3);
        secondreachDS=riverTopology(c2(j,3),3);
        d(1)=c1(i,4)+c2(j,4);
        while firstreachOrder ~= secondreachOrder
          if firstreachOrder > secondreachOrder
            d(k+1)=riverTopology(firstreachDS,4);
            firstreachOrder=riverTopology(firstreachDS,2);
            firstreachDS=riverTopology(firstreachDS,3);
            k=k+1;
          else
            d(k+1)=riverTopology(secondreachDS,4);
            secondreachOrder=riverTopology(secondreachDS,2);
            secondreachDS=riverTopology(secondreachDS,3);
            k=k+1;
          end;
        end;
        while firstreachDS ~= secondreachDS
          d(k+1)=riverTopology(firstreachDS,4) + riverTopology(secondreachDS,4);
          firstreachDS=riverTopology(firstreachDS,3);
          secondreachDS=riverTopology(secondreachDS,3);
          k=k+1;
        end;        
      else  %If points are directly downstream, calculate distance this way
        if c1(i,3) > c2(j,3)
          d(1)=c1(i,4); 
          reachDS=riverTopology(c1(i,3),3);
          while reachDS ~= c2(j,3)
            d(k+1)=riverTopology(reachDS,4);
            k=k+1;
            reachDS=riverTopology(reachDS,3);
          end;
          d(k+1)=riverTopology(c2(j,3),4)-c2(j,4);          
        elseif c1(i,3) < c2(j,3)
          reachDS=riverTopology(c2(j,3),3);
          d(1)=c2(j,4); 
          while reachDS ~= c1(i,3) 
            d(k+1)=riverTopology(reachDS,4);
            reachDS=riverTopology(reachDS,3);
            k=k+1;
          end;
          d(k+1)=riverTopology(c1(i,3),4)-c1(i,4);             
        else
          clear d;
          d(1)=abs(c1(i,4)-c2(j,4));
        end; 
      end;
    end;
      rD(i,j)=sum(d); %The final distance matrix
  end;
end;
      

        
        