import numpy as np
##import matplotlib.pyplot as plt
import matplotlib.numerix as nx
import pylab as p
import os
import sys

################ For MATLAB program ################
##XX(find(isnan(XX)))=-9999;     % replace NaN to -9999
##dlmwrite('X.txt',XX')
##
##YY(find(isnan(YY)))=-9999;     % replace NaN to -9999
##dlmwrite('Y.txt',YY')
####################################################
from pylab import *
##N = 15
##y = rand(N)
##t = arange(N)
##label = ['L%d' %i for i in range(N)]
##for i in range(N):
##    plot([t[i]],[y[i]], marker='o', markersize=10, label=label[i])
##    text(t[i],y[i],label[i], fontsize=16)
##grid()
###legend()
##show()


ax = p.subplot(111)
x =[]
y =[]
file = open("connectedNetX.txt")
for line in file.xreadlines():
    dataX =float(line.rstrip('\n')); 
    if dataX==-9999:        
        dataXX =None
        x.append(dataXX)
    else:
        dataXX=dataX
        x.append(dataX)
file.close()


file = open("connectedNetY.txt")
for line in file.xreadlines():
    dataY =float(line.rstrip('\n'));  
    if dataY==-9999:        
        dataYY =None
        y.append(dataYY)
    else:
        dataYY=dataY
        y.append(dataY)
file.close()


xx=[]
yy=[]
file = open("disconnectedNetX.txt")
for line in file.xreadlines():
    dataX =float(line.rstrip('\n')); 
    if dataX==-9999:        
        dataXX =None
        xx.append(dataXX)        
    else:
        dataXX=dataX
        xx.append(dataX)
file.close()


file = open("disconnectedNetY.txt")
for line in file.xreadlines():
    dataY =float(line.rstrip('\n'));  
    if dataY==-9999:        
        dataYY =None        
        yy.append(dataYY)
    else:
        dataYY=dataY
        yy.append(dataY)    
file.close()

#----- For broken river network-----------------------------
p.plot([xx[len(xx)-1]],[yy[len(yy)-1]], marker='s', markersize=10,color='r')
line1, = p.plot(x,y, '-g',lw=2)
line2, = p.plot(xx,yy, '-m',lw=1)
p.legend(('Network Outlet', 'Connected Network', 'Un-connected Network'),'upper right' )
#----- For broken river network-----------------------------



p.title("Error: Broken network ",color='r' )
p.xlabel('x')
p.ylabel('y')
p.grid(True)         
#p.ylim(-2,2)
#p.savefig('RiverNetwork.png', dpi = 500)
p.show()


