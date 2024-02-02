function [xypoints]=cell2matrix(pVal)
% cell2matrix                     - Dissolves a cell array into a single matrix 
%
%SYNTAX:
%[xypoints]=cell2matrix(pVal)
%
%INPUT
%
%pVal             cell array        cell array where each cell corresponds to a n x 2 matrix of
%                                   x,y coordinates
%
%OUTPUT
% [xypoints]      np x nc           matrix of x,y points arranged in matrix format, where np = 
%                                   total number of points in original cell array
%                                   NOTE: This matrix is useful for masking


xypoints=[];
for i=1:length(pVal)
  xypoints1=pVal{i}(:,1:end);
  xypoints=[xypoints;xypoints1];
end;