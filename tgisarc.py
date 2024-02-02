"""TGIS tool: ArcGIS file select
Name: tgisarc.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

from tgisgui import *
import os
import shutil

# Set flag
flgStdAlone = 0

# Get working space and data file
progroot = sys.argv[0]
workSpace = sys.argv[1]
dataFile = sys.argv[2]
dataFileRT = sys.argv[3]   # P jat (May 11, 2011)
# Set program root directory
progroot = progroot[0:string.rfind(progroot,'\\')]
# Set parameter file name
numSlaChar =  string.rfind(dataFile,'\\')
numDotChar = string.rfind(dataFile,'.')
numSlaCharRT =  string.rfind(dataFileRT,'\\') # P jat (May 11, 2011)
numDotCharRT = string.rfind(dataFileRT,'.')   # P jat (May 11, 2011)
if (numSlaChar == -1) and (numDotChar == -1):
    paramFileHeader = dataFile
elif (numSlaChar == -1) and (not numDotChar == -1):
    paramFileHeader = dataFile[0:numDotChar]
elif (not numSlaChar == -1) and (numDotChar == -1):
    paramFileHeader = dataFile[numSlaChar+1:]
else:
    paramFileHeader = dataFile[numSlaChar+1:numDotChar]


#------------------- P Jat- May 11, 2011----------------------------------------
if (numSlaCharRT == -1) and (numDotCharRT == -1):
    self.paramFileHeaderRT = dataFileNameRT
elif (numSlaCharRT == -1) and (not numDotCharRT == -1):
    self.paramFileHeaderRT = dataFileNameRT[0:numDotCharRT]
elif (not numSlaCharRT == -1) and (numDotCharRT == -1):
    self.paramFileHeaderRT = dataFileNameRT[numSlaCharRT+1:]
else:
    paramFileHeaderRT = dataFileRT[numSlaCharRT+1:numDotCharRT]   
#--------------------end P Jat- May 11, 2011------------------------------------



# Copy user initial parameter file
if not os.path.exists(workSpace + '\\' + \
                      paramFileHeader + '.py'):
    shutil.copyfile(progroot + '\\tgisini.py',workSpace + '\\' + \
                    paramFileHeader + '.py')
sys.path.append(workSpace)
strImport = 'from ' + paramFileHeader + ' import *'
exec(strImport)
os.chdir(progroot)

#--------------------P Jat- May 11, 2011------------------------------------
# Copy user initial parameter file
if not os.path.exists(workSpace + '\\' + \
                      paramFileHeaderRT + '.py'):
    shutil.copyfile(progroot + '\\tgisini.py',workSpace + '\\' + \
                    paramFileHeaderRT + '.py')
sys.path.append(workSpace)
strImport = 'from ' + paramFileHeaderRT + ' import *'
exec(strImport)
os.chdir(progroot)
#--------------------end P Jat- May 11, 2011------------------------------------


# Execute application
app = TgisGui(flgStdAlone,userConst,workSpace,dataFile)
gtk.main()

