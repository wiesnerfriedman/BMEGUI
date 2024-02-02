"""TGIS tool: TGIS data class
Name: tgisdatacls.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

from numpy import *
from types import *
import copy

class GeoData:
    def __init__(self):
        self.__coordX = None
        self.__coordY = None
        self.__timeEvnt = None
        self.__Id = None
        self.__sysId = None
        self.__val1 = None
        self.__val2 = None
        self.__val3 = None
#------P jat set val4, and Val5
        self.__val4 = None
        self.__val5 = None

#------------------------------

    def getX(self):
        return self.__coordX

    def getXAsAry(self):
        if self.__coordX == None:
            return None
        else:
            return array(self.__coordX)[:,newaxis]

    def getY(self):
        return self.__coordY

    def getYAsAry(self):
        if self.__coordY == None:
            return None
        else:
            return array(self.__coordY)[:,newaxis]

    def getT(self):
        return self.__timeEvnt

    def getTAsAry(self):
        if self.__timeEvnt == None:
            return None
        else:
            return array(self.__timeEvnt)[:,newaxis]

    def getSptlCoord(self):
        if (self.__coordX == None) and (self.__coordY == None):
            return None
        elif (self.__coordX != None) and (self.__coordY == None):
            raise TgisError('')
        elif (self.__coordX == None) and (self.__coordY != None):
            raise TgisError('')
        else:
            if len(self.__coordX) != len(self.__coordY):
                raise TgisError('')
            else:
                return [self.__coordX,self.__coordY]

    def getSptlAsAry(self):
        if (self.__coordX == None) and (self.__coordY == None):
            return None
        elif (self.__coordX != None) and (self.__coordY != None):
            if len(self.__coordX) != len(self.__coordY):
                raise TgisError('')
            else:
                return transpose(array([self.__coordX,self.__coordY]))
        else:
            raise TgisError('')

    def getSTCoord(self):
        if (self.__coordX == None) and (self.__coordY == None) and \
           (self.__timeEvnt == None):
            return None
        elif (self.__coordX != None) and (self.__coordY != None) and \
             (self.__timeEvnt != None):
            if (len(self.__coordX) != len(self.__coordY)) or \
               (len(self.__coordX) != len(self.__timeEvnt)):
                raise TgisError('')
            else:
                return [self.__coordX,self.__coordY,self.__timeEvnt]
        else:
            raise TgisError('')

    def getSTAsAry(self):
        if (self.__coordX == None) and (self.__coordY == None) and \
           (self.__timeEvnt == None):
            return None
        elif (self.__coordX != None) and (self.__coordY != None) and \
             (self.__timeEvnt != None):
            if (len(self.__coordX) != len(self.__coordY)) or \
               (len(self.__coordX) != len(self.__timeEvnt)):
                raise TgisError('')
            else:
                return transpose(array([self.__coordX,self.__coordY,\
                                        self.__timeEvnt]))
        else:
            raise TgisError('')

    def getId(self):
        return self.__Id

    def getSysId(self):
        return self.__sysId

    def getSysIdAsAry(self):
        if self.__sysId == None:
            return None
        else:
            return array(self.__sysId)[:,newaxis]

    def getVal1(self):
        return self.__val1

    def getVal1AsAry(self):
        if self.__val1 == None:
            return None
        else:
            return array(self.__val1)[:,newaxis]

    def getVal2(self):
        return self.__val2

    def getVal2AsAry(self):
        if self.__val2 == None:
            return None
        else:
            return array(self.__val2)[:,newaxis]

    def getVal3(self):
        return self.__val3
    
    def getVal3AsAry(self):
        if self.__val3 == None:
            return None
        else:
            return array(self.__val3)[:,newaxis]
        
#--------P jat added next two functions-------------------
    def getVal4(self):
        return self.__val4
    
    def getVal4AsAry(self):
        if self.__val4 == None:
            return None
        else:
            return array(self.__val4)[:,newaxis]
        
    
    def getVal5(self):
        return self.__val5
    
    def getVal5AsAry(self):
        if self.__val5 == None:
            return None
        else:
            return array(self.__val5)[:,newaxis]
#--------------------------------------------------------------

    def setX(self,listData):
        if self.__coordX != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__coordX = listData

    def setY(self,listData):
        if self.__coordY != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__coordY = listData

    def setT(self,listData):
        if self.__timeEvnt != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__timeEvnt = listData

    def setId(self,listData):
        if self.__Id != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__Id = listData

    def setSysId(self,listData):
        if self.__sysId != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__sysId = listData

    def setVal1(self,listData):
        if self.__val1 != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__val1 = listData

    def setVal2(self,listData):
        if self.__val2 != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__val2 = listData

    def setVal3(self,listData):
        if self.__val3 != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__val3 = listData

#---- P jat added next two functions ------------------
    def setVal4(self,listData):
        if self.__val4 != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__val4 = listData

    def setVal5(self,listData):
        if self.__val5 != None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        self.__val5 = listData

#--------------------------------------------------------
    def validCoord(self):
        if self.__coordX == None:
            raise TgisError('')
        if self.__coordY == None:
            raise TgisError('')
        if self.__timeEvnt == None:
            raise TgisError('')
        if (len(self.__coordX) == len(self.__coordY)) and \
           (len(self.__coordX) == len(self.__timeEvnt)):
            return True
        else:
            return False

    def updateX(self,listData):
        if self.__coordX == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__coordX)!=len(listData):
            raise TgisError('')
        self.__coordX = listData

    def updateY(self,listData):
        if self.__coordY == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__coordY)!=len(listData):
            raise TgisError('')
        self.__coordY = listData

    def updateT(self,listData):
        if self.__timeEvnt == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__timeEvnt)!=len(listData):
            raise TgisError('')
        self.__timeEvnt = listData

    def updateId(self,listData):
        if self.__Id == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__Id)!=len(listData):
            raise TgisError('')
        self.__Id = listData

    def updateVal1(self,listData):
        if self.__val1 == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__val1)!=len(listData):
            raise TgisError('')
        self.__val1 = listData

    def updateVal2(self,listData):
        if self.__val2 == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__val2)!=len(listData):
            raise TgisError('')
        self.__val2 = listData

    def updateVal3(self,listData):
        if self.__val3 == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__val3)!=len(listData):
            raise TgisError('')
        self.__val3 = listData


#----- P jat added next two functions -------
    def updateVal4(self,listData):
        if self.__val4 == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__val4)!=len(listData):
            raise TgisError('')
        self.__val4 = listData

    def updateVal5(self,listData):
        if self.__val5 == None:
            raise TgisError('')
        if not isinstance(listData,ListType):
            raise TgisError('')
        if len(self.__val5)!=len(listData):
            raise TgisError('')
        self.__val5 = listData
#--------------------------------------------------
