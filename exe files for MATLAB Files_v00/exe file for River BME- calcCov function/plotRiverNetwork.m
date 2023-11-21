function plotRiverNetwork(riverReaches,riverTopology,plotscheme)
% plotRiverNetwork                - plots River Network and Topology
%
% SYNTAX
%  plotRiverNetwork(riverReaches,riverTopology,plotscheme)
%
% INPUT
%  riverReaches     numreaches x 2  cell array, each cell = 1 reach with x,y coordinates       
%
%  riverTopology    numreaches x 4  matrix of Topology variables
%                                   Column 1: Reach Number
%                                   Column 2: Order Number
%                                   Column 3: D/S Reach Number
%                                   Column 4: Total Reach Length
%
%  plotscheme       scalar          1 label the river reach number
%                                   2 label the river reach number + length
%                                   3 label the river reach number + D/S reach + length
%                                   4 label the river order numbers
%                                   5 plot river network only (as blue lines)
%                                   11,12 and 13 same as 1,2 and 3 with legend for order color
%                                   default is 1
%  
%
%  
%
%  
% NOTE :
% Color scheme is a different color for each reach if river 
% topology is not provided, and it is different color for each
% order if river topology is provided
%

colorst=['bgrcmyk'];
hold on;
legendHandle=[];
legendText={};
if nargin<3, plotscheme=1; end;

for ir=1:length(riverReaches)
  if plotscheme<4
    plot(riverReaches{ir}(:,1),riverReaches{ir}(:,2),colorst(mod(ir-1,7)+1));
    plot(riverReaches{ir}(1,1),riverReaches{ir}(1,2),['*' colorst(mod(ir-1,7)+1)]);
    switch plotscheme
        case 1
            h=text(mean(riverReaches{ir}(:,1)),mean(riverReaches{ir}(:,2)),num2str(ir));
            set(h,'FontSize',6);
        case 2
            h=text(mean(riverReaches{ir}(:,1)),mean(riverReaches{ir}(:,2)),num2str(riverTopology(ir,4)));
            set(h,'FontSize',6);
            h=text(mean(riverReaches{ir}(:,1))-0.2,mean(riverReaches{ir}(:,2)),num2str(ir));
            set(h,'FontSize',6,'Color','r');
        case 3
            h=text(mean(riverReaches{ir}(:,1)),mean(riverReaches{ir}(:,2)),num2str(riverTopology(ir,4),2));   
            set(h,'FontSize',5);
            h=text(mean(riverReaches{ir}(:,1))-0.2,mean(riverReaches{ir}(:,2)),num2str(riverTopology(ir,3),2));   
            set(h,'FontSize',5);
            h=text(mean(riverReaches{ir}(:,1))-0.4,mean(riverReaches{ir}(:,2)),num2str(ir));
            set(h,'FontSize',5,'Color','r');
        case 4
            h=text(mean(riverReaches{ir}(:,1)),mean(riverReaches{ir}(:,2)),num2str(riverTopology(ir,2),2));
            set(h,'FontSize',6);
     end;
 elseif plotscheme==5
     plot(riverReaches{ir}(:,1),riverReaches{ir}(:,2));
 end;
end;

if plotscheme>10
  norder=max(riverTopology(:,2));
  for j=1:norder
    idx=find(riverTopology(:,2)==j);
    for i=1:length(idx)
      h=plot(riverReaches{idx(i)}(:,1),riverReaches{idx(i)}(:,2),colorst(j));
      h=plot(riverReaches{idx(i)}(1,1),riverReaches{idx(i)}(1,2),['*' colorst(j)]);
      switch plotscheme
        case 11
          text(mean(riverReaches{idx(i)}(:,1)),mean(riverReaches{idx(i)}(:,2)),num2str(idx(i)));
        case 12
          g=text(mean(riverReaches{idx(i)}(:,1)),mean(riverReaches{idx(i)}(:,2)),num2str(riverTopology(idx(i),4)));   
          set(g,'FontSize',6);
          g=text(mean(riverReaches{idx(i)}(:,1))-0.2,mean(riverReaches{idx(i)}(:,2)),num2str(idx(i)));
          set(g,'FontSize',6,'Color','r'); 
        case 13
          g=text(mean(riverReaches{idx(i)}(:,1)),mean(riverReaches{idx(i)}(:,2)),num2str(riverTopology(idx(i),4)));   
          set(g,'FontSize',6);
          g=text(mean(riverReaches{idx(i)}(:,1))-0.2,mean(riverReaches{idx(i)}(:,2)),num2str(riverTopology(idx(i),3),2));   
          set(g,'FontSize',5);
          g=text(mean(riverReaches{idx(i)}(:,1))-0.4,mean(riverReaches{idx(i)}(:,2)),num2str(idx(i)));
          set(g,'FontSize',6,'Color','r');
        otherwise 
          error('Invalid Value for Plotscheme, see help plotRiverNetwork');
      end;
    end;
    legendHandle=[legendHandle h];
    legendText{length(legendHandle)}=['River order ' num2str(j)];
  end;
  legend(legendHandle,legendText);
end;