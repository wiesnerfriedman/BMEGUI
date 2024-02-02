"""TGIS tool: TGIS graphic functions
Name: tgisgraph.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

# Import matplotlib libraries
import matplotlib
matplotlib.use('GTK')
from matplotlib.figure import Figure
from matplotlib.axes import Subplot
from matplotlib.backends.backend_gtk import FigureCanvasGTK
from matplotlib.backends.backend_gtk \
    import NavigationToolbar2GTK as NavigationToolbar
from pylab import *

def createFigObj():
    figObj = Figure(figsize=(5,5),dpi=60)
    plotObj = figObj.add_subplot(111)
    plotObj.clear()
    return (figObj,plotObj)

def createCavObj(figObj,boxObj):
    cavObj = FigureCanvasGTK(figObj)
    cavObj.show()
    tbObj = NavigationToolbar(cavObj,boxObj)
    return (cavObj,tbObj)

def plotGraph(figObj,plotObj,cavObj,tbObj,areaObj,boxObj):
    # Define new canvas objects
    while True:
        try:
            tbObj.destroy()
            cavObj.destroy()
            break
        except:
            break
    cavObj = FigureCanvasGTK(figObj)
    cavObj.show()
    tbObj = NavigationToolbar(cavObj,boxObj)
    # Plot raw histogram
    areaObj.pack_start(cavObj)
    areaObj.pack_start(tbObj,False,True)
    return (cavObj,tbObj)

def setPlotLabels(plotObj,strXlabel,strYlabel,strTitle,flgGrid):
    # Plot raw histogram
    plotObj.set_xlabel(strXlabel)
    plotObj.set_ylabel(strYlabel)
    plotObj.set_title(strTitle)
    plotObj.grid(flgGrid)
    

def setColorMap(strColorMap):
    eval(strColorMap)
