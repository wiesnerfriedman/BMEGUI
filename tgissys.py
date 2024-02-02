"""TGIS tool: TGIS system parameters
Name: tgissys.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

import time

###  Constant setting  ###
# Glade file name
gladeFile = 'tgisgui.glade'

# Name of file extension
extParamFile = '.ysp'

# Name of error file
strErrFile = 'err' + time.strftime('%y%m%d') + '.txt'

# Number of decimals for spatial coordinate
numSptlDeci = 4

# Number of decimals for parameters
numParamDeci = 4

# Graphics properties
# Minimum length
epsVal = 1.0e-7

# Denominator of buffer size: abs(maxVal-minVal)/buffDeno
buffDeno = 10

# Title and axis of Histogram
strRawHistTitle = 'Histogram of Raw '
strRawHistY = 'Frequency'

strLogHistTitle = 'Histogram of Log- '
strLogHistY = 'Frequency'

# Title of explanatory data analsys polots
strExpIdTitle = 'Station:'
strExpIdX = 'Time '

strStatLocTitle = 'Location of Stations'

strExpTimeTitle = 'Spatial Distribution at '

# Title and axis of Mean trend plots
strTempMeanTitle = "Temporal Mean Trend"
strTempMeanX = "Time "
strSptlRawTitle = "Spatial Mean Trend (Raw) "
strSptlSmTitle = "Spatial mean trend (Smoothed) "

# BME estimation
numFileNumDeci = 4

#
extResStatFile = '.yse'
extResMapFile = '.yme'
