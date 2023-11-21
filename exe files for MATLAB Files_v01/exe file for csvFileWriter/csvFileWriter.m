function csvFileWriter(fileName1)
%csvFileWriter- Write X, Y, BMEmean, BMEvar vector data in csv file
%
%
%   INPUTS
%       fileName:  output filename (File name to be saved as)
%
%
%
% Read files
X  = dlmread('dataX_csv.txt');
Y  = dlmread('dataY_csv.txt');
BMEmean = dlmread('dataMean_csv.txt');
BMEvar = dlmread('dataVar_csv.txt');


if exist('dataX_csv.txt')~=2
    f = errordlg('There is an error: Please re-start BMEGUI and try again.', 'BMEGUI file writing error');
end

    
saveName =  [fileName1,'.csv'];
fid = fopen(saveName,'w');
headers = {'X','Y', 'BME mean', 'BME variance'};
cellfun(@(x) fprintf(fid,'%s,',x),headers);
fprintf(fid,'\n');
%fclose(fid);
dlmwrite(saveName,[X(1:end-1)',Y(1:end-1)',BMEmean(1:end-1)', BMEvar(1:end-1)'],'-append','delimiter',',');
fclose(fid);
% (1:end-1)'   : Because files from BMEGUI Python script have ',' at last
% line because scipt [myfile.write(str(item)+',')] put coma after writing
% number




% ext = '.csv';
% %wait bar
% h = waitbar(0,['Writing file : ',fname,ext,', Please wait...']);



end

