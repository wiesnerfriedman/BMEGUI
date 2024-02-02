"""TGIS tool: TGIS general functions
Name: tgisgen.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

from numpy import *
from pylab import find
import os
import copy
from tgisiocls import *
from tgisdatacls import *
from tgiserrcls import *
from tgissys import *
from tgiserrmsg import *
import scipy.stats




#---P Jat: Add libraries for GAUSSIAN FUNCTION ---------
import random
from scipy.stats import norm

#--- Add FUNCTION for GAUSSIAN DIST --------------------
def normal(mu,sigma):
    def f(x):
        z = 1.0*(x-mu)/sigma
        e = math.e**(-0.5*z**2)
        C = math.sqrt(2*math.pi)*sigma
        return 1.0*e/C
    return f
#-------------------------------------------------------




def createDataFileReader(fileName):
    if not isinstance(fileName,str):
        raise TgisError('')
    fileType = fileName[string.rfind(fileName,'.')+1:]
    if fileType=='txt':
        objDataFile = GeoEASReader(fileName)
    elif fileType=='csv':
        objDataFile = CsvReader(fileName)
    else:
        raise TgisError(errTgis02)
    return objDataFile

def hardenData(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    dataType = objData.getVal1()
    val1 = objData.getVal2()
    val2 = objData.getVal3()
    val3 = objData.getVal4()    # P Jat : Read 4th data Col
    val4 = objData.getVal5()    # P Jat : Read 5th data Col
    if (dataType==None) or (val1==None) or (val2==None):
        raise TgisError('')
    if (len(dataType)!=len(val1)) or (len(dataType)!=len(val2)):
        raise TgisError('')
    hardVal = []
    for idx,item in enumerate(dataType):
        if int(item) == 0:
            hardVal.append(val1[idx])
        elif int(item) == 1:
            hardVal.append((val1[idx]+val2[idx])/2)
        elif int(item) == 2:
            hardVal.append(val1[idx])
        elif int(item) == 3:            # P Jat; Add Type 3 data (Triangular Dist Data)
            hardVal.append((val1[idx]+val2[idx]+val3[idx])/3)
        elif int(item) == 4:            # P Jat; Add Type 4 data (Truncated Gaussian Dist Data)
            f = normal(val1[idx],val2[idx])
            hardVal.append(val1[idx]+(float(f(val3[idx])-f(val4[idx]))/float(norm.cdf(val4[idx], val1[idx],val2[idx])-norm.cdf(val3[idx],val1[idx],val2[idx])))*val2[idx])
        else:  
            raise TgisError(errTgis04)
    return hardVal

def uniquerow(aryData):
    if not isinstance(aryData,ndarray):
        raise TgisError('')
    numCol = aryData.shape[1]
    if numCol > 3:
        raise TgisError('')
    unqX = unique(aryData[:,0])
    for idx,item in enumerate(unqX):
        unqY = unique(aryData[where(aryData[:,0]==item),1])[:,newaxis]
        if idx == 0:
            if numCol == 3:
                for idx2,item2 in enumerate(unqY):
                    unqT = aryData[where(logical_and((aryData[:,0]==item),\
                                                     (aryData[:,1]==item2))),2]
                    unqT = unique(unqT)[:,newaxis]
                    if idx2 == 0:
                        unqYT = concatenate((ones((unqT.shape[0],1))*item2,\
                                             unqT),1)
                    else:
                        unqTemp = concatenate((ones((unqT.shape[0],1))*item2,\
                                               unqT),1)
                        unqYT = concatenate((unqYT,unqTemp))
                unqPt = concatenate((ones((unqYT.shape[0],1))*item,unqYT),1)
            else:
                unqPt = concatenate((ones((unqY.shape[0],1))*item,unqY),1)
        else:
            if numCol == 3:
                for idx2,item2 in enumerate(unqY):
                    unqT = aryData[where(logical_and((aryData[:,0]==item),\
                                                     (aryData[:,1]==item2))),2]
                    unqT = unique(unqT)[:,newaxis]
                    if idx2 == 0:
                        unqYT = concatenate((ones((unqT.shape[0],1))*item2,\
                                             unqT),1)
                    else:
                        unqTemp = concatenate((ones((unqT.shape[0],1))*item2,\
                                               unqT),1)
                        unqYT = concatenate((unqYT,unqTemp))
                unqTemp = concatenate((ones((unqYT.shape[0],1))*item,unqYT),1)
                unqPt = concatenate((unqPt,unqTemp))
            else:
                unqTemp = concatenate((ones((unqY.shape[0],1))*item,unqY),1)
                unqPt = concatenate((unqPt,unqTemp))
    return unqPt

def findDupliId(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getId() == None:
        raise TgisError('')
    if objData.getSptlCoord() == None:
        raise TgisError('')
    arySptl = objData.getSptlAsAry()
    unqPt = uniquerow(arySptl)
    dupPt = []
    for item in unqPt:
        idxDupli = find(logical_and((arySptl[:,0]==item[0]),\
                                    (arySptl[:,1]==item[1])))
        dupId = []
        for idx in idxDupli:
            dupId.append(objData.getId()[idx])
        if not (dupId.count(dupId[0]) == len(dupId)):
            dupPt.append([item.tolist(),unique(dupId).tolist()])
    return dupPt

def findDupliLoc(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getId() == None:
        raise TgisError('')
    if objData.getSptlCoord() == None:
        raise TgisError('')
    dupPt = []
    aryId = array(objData.getId())
    unqId = unique(aryId)
    if len(aryId) == len(unqId.tolist()):
        return dupPt
    else:
        for item in unqId:
            idxDupli = find(aryId==item)
            dupLoc = []
            for idx in idxDupli:
                dupLoc.append(objData.getSptlAsAry()[idx].tolist())
            if not (dupLoc.count(dupLoc[0]) == len(dupLoc)):
                dupPt.append([item,uniquerow(array(dupLoc)).tolist()])
        return dupPt

def removeDupliId(objData):
    # Param #
    flgDupliId = 2
    # Param #
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getId() == None:
        raise TgisError('')
    if objData.getSptlCoord() == None:
        raise TgisError('')
    arySptl = objData.getSptlAsAry()
    unqPt = uniquerow(arySptl)
    listId = copy.copy(objData.getId())
    unqId = []
    for item in unqPt:
        idxDupli = find(logical_and((arySptl[:,0]==item[0]),\
                                    (arySptl[:,1]==item[1])))
        dupId = []
        for idx in idxDupli:
            dupId.append(objData.getId()[idx])
        if not (dupId.count(dupId[0]) == len(dupId)):
            if flgDupliId == 1:
                finalId = max(dupId)
            elif flgDupliId == 2:
                finalId = min(dupId)
            else:
                raise TgisError('')
            for idx in idxDupli:
                listId[idx] = finalId
            unqId.append(finalId)
        else:
            unqId.append(dupId[0])
    if not (len(unique(unqId).tolist()) == len(unqId)):
        raise TgisError('Same ID is assigned to different location')
    else:
        return listId

def removeDupliLoc(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getId() == None:
        raise TgisError('')
    if objData.getSptlCoord() == None:
        raise TgisError('')
    arySptl = objData.getSptlAsAry()
    aryId = copy.copy(array(objData.getId()))
    unqId = unique(aryId)
    if len(aryId) == len(unqId.tolist()):
        return arySptl
    else:
        for item in unqId:
            idxDupli = find(aryId==item)
            dupLoc = []
            for idx in idxDupli:
                dupLoc.append(objData.getSptlAsAry()[idx].tolist())
            if not (dupLoc.count(dupLoc[0]) == len(dupLoc)):
                #dupX = scipy.stats.mean(uniquerow(array(dupLoc))[:,0])
                #dupY = scipy.stats.mean(uniquerow(array(dupLoc))[:,1])
                dupX = sum(uniquerow(array(dupLoc))[:,0]) / \
                       len(uniquerow(array(dupLoc))[:,0])
                dupY = sum(uniquerow(array(dupLoc))[:,1]) / \
                       len(uniquerow(array(dupLoc))[:,0])
                for idx in idxDupli:
                    arySptl[idx,:] = array([dupX,dupY])
        return arySptl

def createId(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getId() != None:
        raise TgisError('')
    if objData.getSptlCoord() == None:
        raise TgisError('')
    arySptl = objData.getSptlAsAry()
    unqPt = uniquerow(arySptl)
    unqPt = unqPt[argsort(unqPt[:,0])]
    listUnq = arange(1,len(unqPt)+1).tolist()
    formatStr = "%." + str(int(log10(max(listUnq)))+1) + "d"
    unqId = []
    for i in xrange(len(listUnq)):
        unqId.append(eval('formatStr %listUnq[i]'))
    #unqId = map(lambda x: eval('formatStr %x'),listUnq)
    listId = map(lambda x: str(int(x)),zeros(arySptl.shape[0]).tolist())
    for idx,item in enumerate(unqPt):
        idxDupli = find(logical_and((arySptl[:,0]==item[0]),\
                                    (arySptl[:,1]==item[1])))
        for idxPt in idxDupli:
            listId[idxPt] = unqId[idx]
    if '0' in listId:
        TgisError('')
    return listId

def createSystemId(objData):
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getSysId() != None:
        raise TgisError('')
    if objData.getId() == None:
        raise TgisError('')
    listId = objData.getId()
    unqId = unique(listId)
    unqSysId = arange(1,len(unqId)+1)
    listSysId = zeros(len(listId)).tolist()
    for idx,item in enumerate(unqId):
        idxDupli = find(array(listId) == item)
        for idxPt in idxDupli:
            listSysId[idxPt] = unqSysId[idx]
    if 0 in listSysId:
        TgisError('')
    return listSysId

def aveDupli(objData):
    # Param #
    dataFileName = 'aveData.ysd'
    outFileName = 'aveOut.ysd'
    # Param #
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if objData.getSTAsAry()==None:
        raise TgisError('')
    if objData.getId()==None:
        raise TgisError('')
    if objData.getSysId()==None:
        raise TgisError('')
    if objData.getVal1AsAry()==None:
        raise TgisError('')
    if objData.getVal2AsAry()==None:
        raise TgisError('')
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Get station ID and system ID
    aryId = objData.getSysIdAsAry()
    # Get data type
    aryType = objData.getVal1AsAry()
    # Get hardened data
    aryVal = objData.getVal2AsAry()
    # Create data file
    aryConc = concatenate((aryST,aryId,aryType,aryVal),1)
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName):
        os.remove(outFileName)
    # Create command string
    cmdBME = 'removeDupli.exe ' + dataFileName + ' ' + outFileName
    
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')
    # Read outputfile
    objDataReader = MatDataReader(outFileName)
    objAveData = GeoData()
    objAveData.setX(objDataReader.getNthCol(0,1))
    objAveData.setY(objDataReader.getNthCol(1,1))
    objAveData.setT(objDataReader.getNthCol(2,1))
    objAveData.setSysId(objDataReader.getNthCol(3,1))
    objAveData.setVal1(objDataReader.getNthCol(4,1))
    objAveData.setVal2(objDataReader.getNthCol(5,1))
    return objAveData

def getLogZero(listVal,logZeroType,logZeroVal):
    if not isinstance(listVal,list):
        raise TgisError('')
    if not isinstance(logZeroType,int):
        raise TgisError('')
    if (not isinstance(logZeroVal,int)) and (not isinstance(logZeroVal,float)):
        raise TgisError('')
    if logZeroType == 0:
        aryVal = array(listVal)
        detecLimit = min(take(aryVal,find(aryVal>0)))
        logZero = detecLimit/logZeroVal
    elif logZeroType == 1:
        logZero = logZeroVal
    else:
        raise TgisError('')
    return logZero

def logTran(listVal,logZeroType,logZeroVal):
    if not isinstance(listVal,list):
        raise TgisError('')
    if not isinstance(logZeroType,int):
        raise TgisError('')
    if (not isinstance(logZeroVal,int)) and (not isinstance(logZeroVal,float)):
        raise TgisError('')
    if logZeroType == 0:
        aryVal = array(listVal)
        detecLimit = min(take(aryVal,find(aryVal>0.0)))
        logZero = log(detecLimit/logZeroVal)
    elif logZeroType == 1:
        logZero = log(logZeroVal)
    else:
        raise TgisError('')
    logVal=[]
    for item in listVal:
        if item <= 0.0:
            logVal.append(logZero)
        elif item > 0.0:
            logVal.append(log(item))
    return logVal

def calcStat(listVal):
    if not isinstance(listVal,list):
        raise TgisError('')
    meanVal = mean(listVal)
    #stdVal = scipy.stats.std(listVal)     # Original Line
    stdVal  = std(listVal)      # Replaced (P Jat)
    skewVal = scipy.stats.skew(listVal)
    kurtVal = scipy.stats.kurtosis(listVal)
    maxVal  = max(listVal)
    minVal  = min(listVal)
    return meanVal,stdVal,skewVal,kurtVal,maxVal,minVal

def aggregateData(objData,aggPeriod):
    # Param #
    dataFileName = 'aggData.ysd'
    outFileName1 = 'aggOut1.ysd'
    outFileName2 = 'aggOut2.ysd'
    # Param #
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if not isinstance(aggPeriod,float):
        raise TgisError('')
    if aggPeriod <= 0.0:
        raise TgisError('')
    # Get time event
    roundTime = map(lambda x: floor(x/aggPeriod)*aggPeriod,objData.getT())
    aryTime = array(roundTime)[:,newaxis]
    # Get ST coordinate
    arySptl = objData.getSptlAsAry()
    # Get station ID and system ID
    aryId = objData.getSysIdAsAry()
    # Get data type
    aryType = objData.getVal1AsAry()
    # Get data
    aryValRaw = objData.getVal2AsAry()
    aryValLog = objData.getVal3AsAry()
    # Create data file
    aryConc = concatenate((arySptl,aryTime,aryId,aryType,aryValRaw),1)
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName1):
        os.remove(outFileName1)
    # Create command string
    cmdBME = 'removeDupli.exe ' + dataFileName + ' ' + outFileName1
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName1):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')

    # Create data file
    aryConc = concatenate((arySptl,aryTime,aryId,aryType,aryValLog),1)
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName2):
        os.remove(outFileName2)
    # Create command string
    cmdBME = 'removeDupli.exe ' + dataFileName + ' ' + outFileName2
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName2):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')

    # Read outputfile
    objDataReader1 = MatDataReader(outFileName1)
    rawX = objDataReader1.getNthCol(0,1)
    rawY = objDataReader1.getNthCol(1,1)
    rawT = objDataReader1.getNthCol(2,1)
    rawId = objDataReader1.getNthCol(3,1)
    rawType = objDataReader1.getNthCol(4,1)
    rawVal = objDataReader1.getNthCol(5,1)
    objDataReader2 = MatDataReader(outFileName2)
    logX = objDataReader2.getNthCol(0,1)
    logVal = objDataReader2.getNthCol(5,1)
    if rawX != logX:
        raise TgisError('')
    objAggData = GeoData()
    objAggData.setX(rawX)
    objAggData.setY(rawY)
    objAggData.setT(rawT)
    objAggData.setSysId(rawId)
    objAggData.setVal1(rawType)
    objAggData.setVal2(rawVal)
    objAggData.setVal3(logVal)
    return objAggData

def calcMean(objData,meanParam,flgLogTran):
    # Param #
    dataFileName = 'meanData.ysd'
    outFileName1 = 'meanOut1.ysd'
    outFileName2 = 'meanOut2.ysd'
    # Param #
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if not isinstance(meanParam,list):
        raise TgisError('')
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Get data
    if not flgLogTran:
        aryVal = objData.getVal2AsAry()
    else:
        aryVal = objData.getVal3AsAry()
    # Create data file
    aryConc = concatenate((aryST,aryVal),1)
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName1):
        os.remove(outFileName1)
    if os.path.exists(outFileName2):
        os.remove(outFileName2)
    # Create command string
    cmdBME = 'calcMean.exe ' + dataFileName + ' ' + outFileName1 + ' ' + \
             outFileName2 + ' "[' + str(meanParam[0]) + ',' + \
             str(meanParam[1]) + ',' + \
             str(meanParam[2]) + ',' + str(meanParam[3]) + ']"'
    # Execute command string
    retCode = os.system(cmdBME)
    print retCode                      # P jat------------------
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName1):
            raise TgisError('')
        if not os.path.exists(outFileName2):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')             ################ P Jat-------------------

    # Read outputfile
    objDataReader = MatDataReader(outFileName1)
    sptlGridX = objDataReader.getNthCol(0,1)
    sptlGridY = objDataReader.getNthCol(1,1)
    rawSptlMean = objDataReader.getNthCol(2,1)
    smSptlMean = objDataReader.getNthCol(3,1)
    objDataReader = MatDataReader(outFileName2)
    tempGrid = objDataReader.getNthCol(0,1)
    rawTempMean = objDataReader.getNthCol(1,1)
    smTempMean = objDataReader.getNthCol(2,1)
    return [sptlGridX,sptlGridY,rawSptlMean,smSptlMean,\
            tempGrid,rawTempMean,smTempMean]

def createGrid(objData):
    # Param #
    dataFileName = 'gridData.ysd'
    outFileName1 = 'gridOut1.ysd'
    outFileName2 = 'gridOut2.ysd'
    # Param #
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Create data file
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName)
    objDataWriter.writeArray(aryST)
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName1):
        os.remove(outFileName1)
    if os.path.exists(outFileName2):
        os.remove(outFileName2)
    # Create command string
    cmdBME = 'calcGrid.exe ' + dataFileName + ' ' + outFileName1 + ' ' + \
             outFileName2
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName1):
            raise TgisError('')
        if not os.path.exists(outFileName2):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')

    # Read outputfile
    objDataReader = MatDataReader(outFileName1)
    sptlGridX = objDataReader.getNthCol(0,1)
    sptlGridY = objDataReader.getNthCol(1,1)
    objDataReader = MatDataReader(outFileName2)
    tempGrid = objDataReader.getNthCol(0,1)
    return [sptlGridX,sptlGridY,tempGrid]

def removeMean(objData,gridX,gridY,sptlMean,gridT,tempMean,flgLogTran):
    # Param #
    dataFileName1 = 'rmeanData1.ysd'
    dataFileName2 = 'rmeanData2.ysd'
    dataFileName3 = 'rmeanData3.ysd'
    outFileName = 'rmeanOut.ysd'
    # Param #
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Get data
    if not flgLogTran:
        aryVal = objData.getVal2AsAry()
    else:
        aryVal = objData.getVal3AsAry()
    # Create data file1
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName1)
    objDataWriter.writeArray(aryST)
    objDataWriter.closeFile()
    # Create data file2
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName2)
    objDataWriter.writeArray(transpose(array([gridX,gridY,sptlMean])))
    objDataWriter.closeFile()
    # Create data file2
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName3)
    objDataWriter.writeArray(transpose(array([gridT,tempMean])))
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName):
        os.remove(outFileName)
    # Create command string
    cmdBME = 'removeMean.exe ' + dataFileName1 + ' ' + dataFileName2 + ' ' + \
             dataFileName3 + ' ' + outFileName
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('')

    # Read outputfile
    objDataReader = MatDataReader(outFileName)
    mstI = objDataReader.getNthCol(0,1)
    newVal = aryVal - array(mstI)[:,newaxis]
    return ravel(newVal).tolist(),mstI

def calculateCovByNum(objData,gridX,gridY,gridT,numSptlLag,numTempLag):
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if not isinstance(numSptlLag,int):
        raise TgisError('')
    if not isinstance(numTempLag,int):
        raise TgisError('')
    # Calculate maximum distance
    sptlDist = (sqrt((max(gridX) - min(gridX))**2 + \
                     (max(gridY) - min(gridY))**2)) * 0.5
    tempDist = (max(gridT) - min(gridT)) * 0.75
    # Create lag
    sptlLag = arange(0,sptlDist + sptlDist/float((numSptlLag-1)),\
                     sptlDist/float((numSptlLag-1)))
    tempLag = arange(0,tempDist + tempDist/float((numTempLag-1)),\
                     tempDist/float((numTempLag-1)))
    # Create lag tolerance
    sptlLagTol = ones(sptlLag.shape[0]) * sptlDist/float((numSptlLag-1)) * 0.5
    sptlLagTol[0] = 0.0
    tempLagTol = ones(tempLag.shape[0]) * tempDist/float((numTempLag-1)) * 0.5
    tempLagTol[0] = 0.0
    sptlCov = calculateCov(objData,gridX,gridY,gridT,sptlLag.tolist(),\
                            sptlLagTol.tolist(),[0.0],[0.0])
    sptlCovVal = []
    sptlLagVal = []
    sptlLagTolVal = []
    for idx,item in enumerate(sptlCov):
        if string.strip(item) != 'NaN':
            sptlCovVal.append(float(item))
            sptlLagVal.append(sptlLag[idx])
            sptlLagTolVal.append(sptlLagTol[idx])

    tempCov = calculateCov(objData,gridX,gridY,gridT,[0.0],[0.0],\
                            tempLag.tolist(),tempLagTol.tolist())
    tempCovVal = []
    tempLagVal = []
    tempLagTolVal = []
    for idx,item in enumerate(tempCov):
        if string.strip(item) != 'NaN':
            tempCovVal.append(float(item))
            tempLagVal.append(tempLag[idx])
            tempLagTolVal.append(tempLagTol[idx])
    return [sptlLagVal,sptlLagTolVal,sptlCovVal,tempLagVal,tempLagTolVal,tempCovVal]

def calculateCovByLag(objData,gridX,gridY,gridT,sptlLag,sptlLagTol,\
                      tempLag,tempLagTol):
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    if not isinstance(sptlLag,list):
        sptlLag = sptlLag.tolist()
        sptlLagTol = sptlLagTol.tolist()
    if not isinstance(tempLag,list):
        tempLag = tempLag.tolist()
        tempLagTol = tempLagTol.tolist()
    covByLag = calculateCov(objData,gridX,gridY,gridT,\
                             sptlLag,sptlLagTol,tempLag,tempLagTol)
    covVal = []
    lagVal = []
    lagTolVal = []
    for idx,item in enumerate(covByLag):
        if string.strip(item) != 'NaN':
            covVal.append(float(item))
            if tempLag == [0.0]:
                lagVal.append(sptlLag[idx])
                lagTolVal.append(sptlLagTol[idx])
            else:
                lagVal.append(tempLag[idx])
                lagTolVal.append(tempLagTol[idx])
    return [lagVal,lagTolVal,covVal]

def calculateCov(objData,gridX,gridY,gridT,sptlLag,sptlLagTol,\
                 tempLag,tempLagTol):
    # Param #
    dataFileName1 = 'covData1.ysd'
    dataFileName2 = 'covData2.ysd'
    dataFileName3 = 'covData3.ysd'
    outFileName = 'covOut.ysd'
    # Param #
    # Param check
    if not isinstance(objData,GeoData):
        raise TgisError('')
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Get data
    aryVal = objData.getVal1AsAry()
    aryConc = concatenate((aryST,aryVal),1)
    # Create data file1
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName1)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()
    # Create data file2
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName2)
    objDataWriter.writeArray(transpose(array([sptlLag,sptlLagTol])))
    objDataWriter.closeFile()
    # Create data file2
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName3)
    objDataWriter.writeArray(transpose(array([tempLag,tempLagTol])))
    objDataWriter.closeFile()

    # Delete output file
    if os.path.exists(outFileName):
        os.remove(outFileName)
    # Create command string
    cmdBME = 'calcCov.exe ' + dataFileName1 + ' ' + dataFileName2 + ' ' + \
             dataFileName3 + ' ' + outFileName
    # Execute command string
    retCode = os.system(cmdBME)
    if retCode == 0:
        # If outputfile does not exist, raise error
        if not os.path.exists(outFileName):
            raise TgisError('')
    else:
        # If return code is not zero, raise error
        raise TgisError('Covariance Function Failed') #### P Jat added error text

    # Read outputfile
    objDataReader = MatDataReader(outFileName)
    cov = objDataReader.getNthCol(0,2)
    return cov

def calcCovModel(distVect,covModel,covParam1,covParam2):
    if not isinstance(covModel,str):
        raise TgisError('')
    if not isinstance(covParam1,float):
        raise TgisError('')
    if not isinstance(covParam2,float):
        raise TgisError('')
    if covModel == "exponentialC":
        try:
            covValue = exponentialC(distVect,[covParam1,covParam2])
        except:
            raise TgisError('')
    elif covModel == "gaussianC":
        try:
            covValue = gaussianC(distVect,[covParam1,covParam2])
        except:
            raise TgisError('')
    elif covModel == "sphericalC":
        try:
            covValue = sphericalC(distVect,[covParam1,covParam2])
        except:
            raise TgisError('')
    elif covModel == "holecosC":
        try:
            covValue = holecosC(distVect,[covParam1,covParam2])
        except:
            raise TgisError('')
    elif covModel == "holesinC":
        try:
            covValue = holesinC(distVect,[covParam1,covParam2])
        except:
            raise TgisError('')
    else:
        raise TgisError('')
    return covValue

def exponentialC(D,param):
    C=param[0]*exp(-3*D/param[1])
    return C

def gaussianC(D,param):
    C=param[0]*exp(-(sqrt(3)*D/param[1])**2)
    return C

def holecosC(D,param):
    C=param[0]*cos(pi*D/param[1])
    return C

def holesinC(D,param):
    isnull=find(ravel(D)==0)
    D2=copy.copy(D)
    put(D2,isnull,NaN)
    C=param[0]*sin(pi*D/param[1])/((pi*D2)/param[1])
    put(C,isnull,param[0])
    return C

def sphericalC(D,param):
    C=param[0]*(1.-((3./2.)*(D/param[1])-(1./2)*(D/param[1])**3))
    index=find(D>param[1])
    put(C,index,0)
    return C

def calcGraphBuff(minVal,maxVal):
    buffVal = abs(maxVal - minVal) / buffDeno
    if buffVal < epsVal:
        buffVal = epsVal
    maxLim = maxVal + buffVal
    minLim = minVal - buffVal
    return [minLim,maxLim]

def createBMEDataFiles(objData,gridX,gridY,sptlMean,gridT,tempMean):
    # Constant
    dataFileName1 = 'bmeData.ysd'
    dataFileName2 = 'bmeSMean.ysd'
    dataFileName3 = 'bmeTMean.ysd'
    # Get ST coordinate
    aryST = objData.getSTAsAry()
    # Get data type
    aryVal1 = objData.getVal1AsAry()
    # Get val1
    aryVal2 = objData.getVal2AsAry()
    # Get val2
    aryVal3 = objData.getVal3AsAry()
    # Get Sys ID
    arySysId = objData.getSysIdAsAry()
    # Create data file
    aryConc = concatenate((aryST,arySysId,aryVal1,aryVal2,aryVal3),1)
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName1)
    objDataWriter.writeArray(aryConc)
    objDataWriter.closeFile()
    # Create data file2
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName2)
    objDataWriter.writeArray(transpose(array([gridX,gridY,sptlMean])))
    objDataWriter.closeFile()
    # Create data file3
    objDataWriter = CsvWriter()
    objDataWriter.openFile(dataFileName3)
    objDataWriter.writeArray(transpose(array([gridT,tempMean])))
    objDataWriter.closeFile()

def bmeEstMap(paramString):
    # Constant
    dataFileName = 'bmeData.ysd'
    smeanFileName = 'bmeSMean.ysd'
    tmeanFileName = 'bmeTMean.ysd'
    paramFileName = 'bmeMParam.ysd'
    outputFileName1 = 'bmeMOut1.ysd'
    outputFileName2 = 'bmeMOut2.ysd'
    outputFileName3 = 'bmeMOut3.ysd'
    try:
        # Create param file
        objDataWriter = DataWriter()
        objDataWriter.openFile(paramFileName)
        objDataWriter.writeStr(paramString)
        objDataWriter.closeFile()
        # Delete outputfile1
        if os.path.exists(outputFileName1):
            os.remove(outputFileName1)
        # Delete outputfile2
        if os.path.exists(outputFileName2):
            os.remove(outputFileName2)
        # Delete outputfile3
        if os.path.exists(outputFileName3):
            os.remove(outputFileName3)
        # Create command string        
        cmdBME = 'bmeEst.exe ' + dataFileName + ' ' + \
                 paramFileName + ' ' + \
                 smeanFileName + ' ' + tmeanFileName + ' ' + \
                 outputFileName1 + ' ' + outputFileName2  + ' ' + \
                 outputFileName3
        
        # Execute command string
        retCode = os.system(cmdBME)        
        if retCode == 0:
            # If outputfile does not exist, raise error
            if not os.path.exists(outputFileName1):
                raise TgisError('')
            if not os.path.exists(outputFileName2):
                raise TgisError('')
        else:
            # If return code is not zero, raise error
            raise TgisError('')
        
        # Read outputfile
        objDataReader = MatDataReader(outputFileName1)
        estPtsX = objDataReader.getNthCol(0,1)
        estPtsY = objDataReader.getNthCol(1,1)
        estMean = objDataReader.getNthCol(2,1)
        estVar = objDataReader.getNthCol(3,1)
        objDataReader = MatDataReader(outputFileName2)        
        smPtsX = objDataReader.getNthCol(0,1)
        smPtsY = objDataReader.getNthCol(1,1)
        smMean = objDataReader.getNthCol(2,1)
        smVar = objDataReader.getNthCol(3,1)
        if os.path.exists(outputFileName3):
            objDataReader = MatDataReader(outputFileName3)
            nanPtsX = objDataReader.getNthCol(0,1)
            nanPtsY = objDataReader.getNthCol(1,1)
        else:
            nanPtsX = []
            nanPtsY = []
        return [estPtsX,estPtsY,estMean,estVar,smPtsX,smPtsY,smMean,smVar,nanPtsX,nanPtsY]
    except:
        raise TgisWarning('BME estimation failed. ' + \
                          'Modify estimation parameters and try again.' + \
                          '\n \n(Error: ' + str(sys.exc_info()[1]) + ')')

def bmeEstStat(paramString):
    # Constant
    dataFileName = 'bmeData.ysd'
    smeanFileName = 'bmeSMean.ysd'
    tmeanFileName = 'bmeTMean.ysd'
    paramFileName = 'bmeSParam.ysd'
    outputFileName1 = 'bmeSOut1.ysd'
    outputFileName2 = 'bmeSOut2.ysd'
    try:
        # Create param file
        objDataWriter = DataWriter()
        objDataWriter.openFile(paramFileName)
        objDataWriter.writeStr(paramString)
        objDataWriter.closeFile()
        # Delete outputfile1
        if os.path.exists(outputFileName1):
            os.remove(outputFileName1)
        if os.path.exists(outputFileName2):
            os.remove(outputFileName2)
        # Create command string
        cmdBME = 'bmeEstStat.exe ' + dataFileName + ' ' + \
                 paramFileName + ' ' + \
                 smeanFileName + ' ' + tmeanFileName + ' ' + \
                 outputFileName1 + ' ' + outputFileName2
        # Execute command string
        retCode = os.system(cmdBME)
        if retCode == 0:
            # If outputfile does not exist, raise error
            if not os.path.exists(outputFileName1):
                raise TgisError('')
        else:
            # If return code is not zero, raise error
            raise TgisError('')
        # Read outputfile1
        objDataReader = MatDataReader(outputFileName1)
        estPtsT = objDataReader.getNthCol(0,1)
        estMean = objDataReader.getNthCol(1,1)
        estVar = objDataReader.getNthCol(2,1)
        dataLimi = []
        dataLimi2 = []
        dataProb = []
        dataProb2 = []
        dataTime = []
        # Read outputfile2
        if os.path.exists(outputFileName2):
            objDataReader = MatDataReader(outputFileName2)
            colNum = objDataReader.getColNum()
            dataVal = objDataReader.getColVal()
            for item in dataVal:
                dataTime.append(float(item[0]))
                dataLimi.append(item[1:(((colNum-1)/2)+1)])
                dataProb.append(item[(((colNum-1)/2)+1):])
            for item in dataLimi:
                dataLimi2.append(map(lambda x: float(x), item))
            for item in dataProb:
                dataProb2.append(map(lambda x: float(x), item))
        return [estPtsT,estMean,estVar,dataTime,dataLimi2,dataProb2]

    except:
        raise TgisWarning('BME estimation failed. ' + \
                          'Modify estimation parameters and try again.' + \
                          '\n \n(Error: ' + str(sys.exc_info()[1]) + ')')
