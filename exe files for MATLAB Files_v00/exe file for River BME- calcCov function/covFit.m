function range = covFit(Lag, covVal)
%
%
%
%
% INPUT:
%     Lag           : lag distance of the experimental variogram
%     covVal        : experimental covariance values
%
%
%
% Output arguments:
%
%     range         : range of best fit single covariance model
%
%
%
if nargin == 0
    help covFit
    return
elseif nargin>0 && nargin < 2;
    error('wrong number of input arguments');
end
 rLag =Lag;
 Cr =covVal;
 
 
% compsite model
% a    = rLag(2):(rLag(3)-rLag(2))/3:rLag(end)*5;
% b    = rLag(2):(rLag(3)-rLag(2))/3:rLag(end)*5;
% %w = (ones(length(rLag),1).*exp(rLag(end:-1:1))')'; 
% w   = (ones(length(rLag),1)'); 
% st  = [];
% for i=1:length(a)
%     for j=1:length(a)
%         covdc  = Cr(1)*(2/3)*exp(-3*rLag/a(i))+Cr(1)*(1/3)*exp(-3*rLag/b(j));
%         [bw,sew_b,msew] = lscov(Cr,covdc',w) ;
%         st     = [st; msew];
%     end
% end
% 
% idx = find(st==min(st))/i;
% a0  = floor(idx);
% b0  = ((idx -a0)*i);
% 
% covPlot  = Cr(1)*(2/3)*exp(-3*rLag/b0)+Cr(1)*(1/3)*exp(-3*rLag/a0);
% plot(rLag, covPlot,'-.b'); 
%
%
% simple Model Fit
w = 1./rLag(2:end);
w =[w,0];

a =rLag(2):(rLag(3)-rLag(2))/3:rLag(end)*15;
st = [];
stG =[];
for i=1:length(a)
  covd = Cr(1)*exp(-3*rLag/a(i));
  [bw,sew,msew] = lscov(Cr,covd',w) ;
  st= [st; msew];
  
  covdG = Cr(1)*exp(-3*(rLag/a(i)).^2);  
  [bwG,sewG,msewG] = lscov(Cr,covdG',w) ;  
  stG= [stG; msewG];
end

if min(st)<min(stG)
    idx = find(st==min(st));
    covPlot  = Cr(1)*exp(-3*rLag/a(idx));
    %plot(rLag, covPlot,'-.b'); 
    range = a(idx);
else
    idx         = find(stG==min(stG));
    covPlot     = Cr(1)*exp(-3*(rLag/a(idx)).^2); 
    %plot(rLag, covPlot,'-.g'); 
    range       = a(idx);
end

return 