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
file = open("riverNetX.txt")
for line in file.xreadlines():
    dataX =float(line.rstrip('\n')); 
    if dataX==-9999:        
        dataXX =None
        x.append(dataXX)
    else:
        dataXX=dataX
        x.append(dataX)
file.close()


file = open("riverNetY.txt")
for line in file.xreadlines():
    dataY =float(line.rstrip('\n'));  
    if dataY==-9999:        
        dataYY =None
        y.append(dataYY)
    else:
        dataYY=dataY
        y.append(dataY)
file.close()


p.plot([x[len(x)-1]],[y[len(y)-1]], marker='s', markersize=10,color='r')
line, = p.plot(x, y, 'g',lw=2)
p.legend(('Network Outlet', 'Stream Network'),'upper right' )

#----- For broken river network-----------------------------
# if broekn network call different function
#line1, = p.plot(x[1:30000],y[1:30000], '-g',lw=2)
#line2, = p.plot(x[30000:len(x)-1],y[30000:len(y)-1], '-b',lw=1)
#p.legend(('Network Outlet', 'Connected Network', 'Un-connected Network'),'upper right' )
#----- For broken river network-----------------------------


p.xlabel('x')
p.ylabel('y')
p.grid(True)         
#p.ylim(-2,2)
p.show()





