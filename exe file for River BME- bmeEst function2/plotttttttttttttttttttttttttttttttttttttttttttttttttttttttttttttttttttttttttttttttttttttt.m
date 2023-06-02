%------------ Display plot


[xydispGrid]=getXYGridData(x2,y2,sk,moments(:,1),moments(:,1));

pkRgrid=[sk(:,1:2);xydispGrid(:,1:2)];
pkRgriddataR=[moments(:,1);xydispGrid(:,4)];


figure;hold on;
[xg yg]=meshgrid([-75.2:0.01:-73.8],[40.1:0.01:41.0]);
[zg]=griddata(pkRgrid(:,1),pkRgrid(:,2),pkRgriddataR,xg,yg);
pcolor(xg,yg,zg);



%------ Place mask on top of the colorfigure
% add path***
load raritanbuffer_2.mat;
bax=[-75.2 -73.8 40.1 41.0];
maskcontour=bufferbound05km;
raritanmask(bax,maskcontour);
pVal2=readARCe00('raritanwma.e00');

