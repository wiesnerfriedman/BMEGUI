"""TGIS tool: Stand alone mode starter
Name: tgisstd.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
Modified by P Jat
"""

from tgisgui import *
import string
import shutil

execfile('stdalone.py')
if not os.path.exists('stdPath.txt'):
    print "Operation is canceled"
else:
    dataFile = open('stdPath.txt','r')    
    stdAloneWorkSpace = string.strip(dataFile.readline())
    stdAloneDataFile = string.strip(dataFile.readline())
    dataFile.close()
    # Set flag
    flgStdAlone = 1
    # Set parameter file name
    numSlaChar =  string.rfind(stdAloneDataFile,'\\')
    numDotChar = string.rfind(stdAloneDataFile,'.')
    if (numSlaChar == -1) and (numDotChar == -1):
        paramFileHeader = stdAloneDataFile
    elif (numSlaChar == -1) and (not numDotChar == -1):
        paramFileHeader = stdAloneDataFile[0:numDotChar]
    elif (not numSlaChar == -1) and (numDotChar == -1):
        paramFileHeader = stdAloneDataFile[numSlaChar+1:]
    else:
        paramFileHeader = stdAloneDataFile[numSlaChar+1:numDotChar]
    # Copy user initial parameter file

    #---- replace white space in File name if it exist
    paramFileHeader = paramFileHeader.replace(' ','')
    
    #--- test if file name starts with numeric and issue error if so 
    try:
        string.atoi(paramFileHeader[0])
        #execfile('000AABB000.py')
        print 'yes number is there'
        execfile('DataFileNameError.py')
        raise IOError('First letter is numeric')
    except:        
        #pass    
        if not os.path.exists(stdAloneWorkSpace + '\\' + \
                              paramFileHeader + '.py'):
            shutil.copyfile('tgisini.py',stdAloneWorkSpace + '\\' + \
                            paramFileHeader + '.py')
        sys.path.append(stdAloneWorkSpace)
        strImport = 'from ' + paramFileHeader + ' import *'    
        exec(strImport)
     
       


        # Execute application
        app = TgisGui(flgStdAlone,userConst,stdAloneWorkSpace,stdAloneDataFile, stdAloneDataFile)
        gtk.main()


