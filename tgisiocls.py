"""TGIS tool: TGIS I/O class
Name: tgisiocls.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

import csv
import string
import sys
import traceback

from numpy import *
from types import *
from tgiserrcls import *

class GeoEASReader:
    def __init__(self,fileName):
        # Open data file
        dataFile = open(fileName,'r')
        # Read file title
        self.__fileTitle = dataFile.readline().rstrip()
        # Read the number of columns
        self.__colNum = int(dataFile.readline())
        # Read column names
        self.__colName = []
        for i in xrange(self.__colNum):
            colName = dataFile.readline().rstrip()
            self.__colName.append(colName)
        # Read data values
        self.__colVal = []
        lineNum = self.__colNum + 2
        for line in dataFile.readlines():
            # Splits the data
            tempVal = line.strip().split()
            lineNum += 1
            # If the number of columns is not equal to _colNum, raise error
            if len(tempVal)!=self.__colNum:
                msgErr = 'Line ' + str(lineNum) + ' of file ' + \
                         dataFile.name + ' should have ' + \
                         str(self.__colNum) + ' columns'
                raise TgisError(msgErr)
            self.__colVal.append(tempVal)
        # Close data file
        dataFile.close()

    def getColName(self):
        return self.__colName

    def getColNum(self):
        return self.__colNum

    def getColVal(self):
        return self.__colVal

    def getRowNum(self):
        return len(self.__colVal)

    def getNthCol(self,numCol,flgType):
        if not isinstance(numCol,int):
            raise TgisError('Column number must be integer')
        if numCol > self.getColNum()-1:
            raise TgisError('Column number must be smaller than ' + \
                             str(self.getColNum()-1))
        if not ((flgType == 1) or (flgType == 2)):
            raise TgisError('Type flag must be 1 or 2')
        nthCol = []
        for item in self.__colVal:
            if flgType == 1:
                nthCol.append(float(item[numCol]))
            elif flgType == 2:
                nthCol.append(item[numCol])
        return nthCol

class CsvReader:
    def __init__(self,fileName):
        # Open data file
        dataFile = open(fileName,'r')
        csvFile = csv.reader(dataFile)
        # Read column names
        self.__colName = csvFile.next()
        # Read the number of columns
        self.__colNum = len(self.__colName)
        # Read data values
        self.__colVal = []
        lineNum = 1
        for item in csvFile:
            lineNum += 1
            # If the number of columns is not equal to _colNum, raise error
            if len(item)!=self.__colNum:
                msgErr = 'Line ' + str(lineNum) + ' of file ' + \
                         dataFile.name + ' should have ' + \
                         str(self.__colNum) + ' columns'
                raise TgisError(msgErr)
            self.__colVal.append(item)
        # Close data file
        dataFile.close()

    def getColName(self):
        return self.__colName

    def getColNum(self):
        return self.__colNum

    def getColVal(self):
        return self.__colVal

    def getRowNum(self):
        return len(self.__colVal)

    def getNthCol(self,numCol,flgType):
        if not isinstance(numCol,int):
            raise TgisError('Column number must be integer')
        if numCol > self.getColNum()-1:
            raise TgisError('Column number must be smaller than ' + \
                             str(self.getColNum()-1))
        if not ((flgType == 1) or (flgType == 2)):
            raise TgisError('Type flag must be 1 or 2')
        nthCol = []
        for item in self.__colVal:
            if flgType == 1:
                nthCol.append(float(item[numCol]))
            elif flgType == 2:
                nthCol.append(item[numCol])
        return nthCol

class CsvWriter:
    def __init__(self):
        self.__dataFile = None

    def openFile(self,fileName):
        self.__dataFile = open(fileName,'w')
        self.__csvFile = csv.writer(self.__dataFile)

    def openAppend(self,FileName):
        self.__dataFile = open(fileName,'a')
        self.__csvFile = csv.writer(self.__dataFile)

    def closeFile(self):
        self.__dataFile.close()

    def writeArray(self,aryData):
        if not isinstance(aryData,ndarray):
            raise TgisError('')
        for item in aryData:
            self.__csvFile.writerow(item)

    def writeStr(self,strData):
        if not isinstance(strData,str):
            raise TgisError('')
        self.__csvFile.writerow(strData)

class DataWriter:
    def __init__(self):
        self.__dataFile = None

    def openFile(self,fileName):
        self.__dataFile = open(fileName,'w')

    def openAppend(self,FileName):
        self.__dataFile = open(fileName,'a')

    def closeFile(self):
        self.__dataFile.close()

    def writeStr(self,strData):
        if not isinstance(strData,str):
            raise TgisError('')
        self.__dataFile.write(strData)

    def writeList(self,listData):
        if not isinstance(listData,list):
            raise TgisError('')
        for item in listData:
            self.__dataFile.write(item + '\n')
          

class MatDataReader:
    def __init__(self,fileName):
        # Open data file
        dataFile = open(fileName,'r')
        csvFile = csv.reader(dataFile)
        # Read column names
        initVal = csvFile.next()
        # Read the number of columns
        self.__colNum = len(initVal)
        # Read data values
        self.__colVal = [initVal]
        lineNum = 0
        for item in csvFile:
            lineNum += 1
            # If the number of columns is not equal to _colNum, raise error
            if len(item)!=self.__colNum:
                msgErr = 'Line ' + str(lineNum) + ' of file ' + \
                         dataFile.name + ' should have ' + \
                         str(self.__colNum) + ' columns'
                raise TgisError(msgErr)
            self.__colVal.append(item)
        # Close data file
        dataFile.close()

    def getColNum(self):
        return self.__colNum

    def getColVal(self):
        return self.__colVal

    def getRowNum(self):
        return len(self.__colVal)

    def getNthCol(self,numCol,flgType):
        if not isinstance(numCol,int):
            raise TgisError('Column number must be integer')
        if numCol > self.getColNum()-1:
            raise TgisError('Column number must be smaller than ' + \
                             str(self.getColNum()-1))
        if not ((flgType == 1) or (flgType == 2)):
            raise TgisError('Type flag must be 1 or 2')
        nthCol = []
        for item in self.__colVal:
            if flgType == 1:
                nthCol.append(float(item[numCol]))
            elif flgType == 2:
                nthCol.append(item[numCol])
        return nthCol

