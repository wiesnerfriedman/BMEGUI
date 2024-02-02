"""TGIS tool: TGIS GUI functions
Name: tgisgui.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

###  Import libraries  ###
# Import standard libraries
import sys
import string
import os
from types import *
import fpformat
import copy
import shelve

# Import COM dispatch library
# This commented line is for ArcGIS9.1
#from win32com.client import Dispatch
# This line is for ArcGIS9.2
#import arcgisscripting      ###### P JAT--- This line is only for ArcGIS
                            # if running without ArcGIS 'BLOCK this line
# Import TGIS graph library 
from tgisgraph import *

# Import GTK libraries
import pygtk
import gtk
import gtk.glade

# Import math libraries
from numpy import *
from pylab import find
from pylab import meshgrid as pylabmesh
from pylab import hist as pylabhist

# Import TGIS application libraries
from tgisdatacls import *
from tgiserrcls import *
from tgissys import *
from tgisgen import *
from tgiserrmsg import *


#################################################
#P Jat (open and read Mask File) ----------------
import copy
from tgiserrmsg import *
import scipy.stats
import pylab as p
import numpy as np
#################################################



#-------P Jat: Add STAT lib from numpy for R-square cal
from scipy import stats
#from ProgressBar import*   ### for Progress Bar
#------------------------------------------------

##############P JAt (for web page) ##############
from win32com.client import Dispatch
from time import sleep
import webbrowser
################################################




class TgisGui:
    def __init__(self,flgStdAlone,userConst,workDir=None,dataFile=None, dataFileRT=None):
        """Constructor"""
        self.textColor = 'jet'
        self.GridStat = True
        self.selectedMaskFile='.'  #  default Mask File Name initialization
        self.incMask =0
        self.incMap =0
        self.numCurScreen = 0
        self.flgStdAlone = flgStdAlone
        self.userConst = userConst
        if not self.flgStdAlone:
            # Create GP object
            # This commented line is for ArcGIS9.1
            #self.GP = Dispatch('esriGeoprocessing.GPDispatch.1')
            # This line is for ArcGIS9.2
            self.GP = arcgisscripting.create()

            # Set workspace
            self.GP.workSpace = workDir
            self.workingSpace = self.GP.workSpace
            # Set raw data file name
            self.dataFileName = dataFile
            # Set river topology file name -------------- P Jat May 11, 2011
            self.dataFileNameRT = dataFileRT
        else:
            # Set workspace
            self.workingSpace = workDir
            # Set raw data file name
            self.dataFileName = dataFile
            # Set river topology file name -------------- P Jat May 11, 2011
            self.dataFileNameRT = dataFileRT
       
        
        # Create parameter file name
        numSlaChar =  string.rfind(self.dataFileName,'\\')
        numDotChar = string.rfind(self.dataFileName,'.')
        numSlaCharRT = string.rfind(self.dataFileNameRT,'\\') # P Jat May 11, 2011
        numDotCharRT = string.rfind(self.dataFileNameRT,'.')  # P Jat May 11, 2011
        if (numSlaChar == -1) and (numDotChar == -1):
            self.paramFileHeader = self.dataFileName
        elif (numSlaChar == -1) and (not numDotChar == -1):
            self.paramFileHeader = self.dataFileName[0:numDotChar]
        elif (not numSlaChar == -1) and (numDotChar == -1):
            self.paramFileHeader = self.dataFileName[numSlaChar+1:]
        else:
            self.paramFileHeader = self.dataFileName[numSlaChar+1:numDotChar]
        dataParamFileName = self.workingSpace + \
                            '\\' + self.paramFileHeader + extParamFile

        #------------------- P Jat- May 11, 2011----------------------------------------
        if (numSlaCharRT == -1) and (numDotCharRT == -1):
            self.paramFileHeaderRT = self.dataFileNameRT
        elif (numSlaCharRT == -1) and (not numDotCharRT == -1):
            self.paramFileHeaderRT = self.dataFileNameRT[0:numDotCharRT]
        elif (not numSlaCharRT == -1) and (numDotCharRT == -1):
            self.paramFileHeaderRT = self.dataFileNameRT[numSlaCharRT+1:]
        else:
            self.paramFileHeaderRT = self.dataFileNameRT[numSlaCharRT+1:numDotCharRT]
        dataParamFileNameRT = self.workingSpace + \
                              '\\' + self.paramFileHeaderRT + extParamFile
        #--------------------end P Jat- May 11, 2011------------------------------------


        # Open parameter file
        self.fileParam = shelve.open(dataParamFileName,'c')
        # Open River Topology File------------------------P Jat (May 11, 2011)
        self.fileParamRT = shelve.open(dataParamFileNameRT,'c')
        # Get time stamp of data file
        self.timeStamp = os.path.getmtime(self.dataFileName)
        # Create WATCH cursor
        self.cursorWatch = gtk.gdk.Cursor(gtk.gdk.WATCH)
        # Create first screen
        self.createScreen1()

    def createScreen1(self):
        """Create box1"""
        
        try:            
            # Set box1 object tree
            windowName = 'box1'
            self.numCurScreen = 1
            self.windowTree1 = gtk.glade.XML(gladeFile,windowName)
            # Create window and low window objects
            self.box1 = self.windowTree1.get_widget(windowName)
            self.lowWindow1 = self.box1.window
            # Change cursor to Watch
            self.lowWindow1.set_cursor(self.cursorWatch)
            # Get widget objects
            lblWorkDir = self.windowTree1.get_widget('lblWorkDir')
            lblDataFile = self.windowTree1.get_widget('lblDataFile')
            self.lblDataType = self.windowTree1.get_widget('lblDataType')
            self.lblVal1 = self.windowTree1.get_widget('lblVal1')
            self.lblVal2 = self.windowTree1.get_widget('lblVal2')
            self.lblVal3 = self.windowTree1.get_widget('lblVal3')   #### P Jat ##########
            self.lblVal4 = self.windowTree1.get_widget('lblVal4')   #### P Jat ##########

         
            
            self.cmbX = self.windowTree1.get_widget('cmbX')
            self.cmbY = self.windowTree1.get_widget('cmbY')
            self.cmbTime = self.windowTree1.get_widget('cmbTime')
            self.cmbId = self.windowTree1.get_widget('cmbId')
            self.cmbData = self.windowTree1.get_widget('cmbData')
            self.cmbVal1 = self.windowTree1.get_widget('cmbVal1')
            self.cmbVal2 = self.windowTree1.get_widget('cmbVal2')            
            self.cmbVal3 = self.windowTree1.get_widget('cmbVal3')   #### P Jat ##########
            self.cmbVal4 = self.windowTree1.get_widget('cmbVal4')   #### P Jat ##########
            
            self.chkDataType = self.windowTree1.get_widget('chkDataType')
            self.entSpaceUnit = self.windowTree1.get_widget('entSpaceUnit')
            self.entTimeUnit = self.windowTree1.get_widget('entTimeUnit')
            self.entDataUnit = self.windowTree1.get_widget('entDataUnit')
            self.entAgentName = self.windowTree1.get_widget('entAgentName')
            self.butQuit1 = self.windowTree1.get_widget('butQuit1')
            self.butNext1 = self.windowTree1.get_widget('butNext1')
            # Set default values
            lblDataFile.set_text(self.dataFileName)
            lblWorkDir.set_text(self.workingSpace)
            self.lblDataType.set_text('Data Field')
            self.lblVal1.set_text('')
            self.lblVal2.set_text('')
            self.lblVal3.set_text('')                   #### P Jat ##########
            self.lblVal4.set_text('')                   #### P Jat ##########
            
            self.entSpaceUnit.set_text('spaceUnit')
            self.entTimeUnit.set_text('timeUnit')
            self.entDataUnit.set_text('dataUnit')
            self.entAgentName.set_text('agentX')
            self.cmbVal1.hide()
            self.cmbVal2.hide()
            self.cmbVal3.hide()     #### P Jat ##########     
            self.cmbVal4.hide()     #### P Jat ##########


########            self.comboboxTest = self.windowTree1.get_widget('comboboxTest')             
########            self.comboboxTest.connect('changed', self.changed_color)
########            self.comboboxTest.set_active(0)




            
         
            # Map event handler
            self.butQuit1.connect('clicked',self.askQuit)
            self.butNext1.connect('clicked',self.goBox2)
            self.chkDataType.connect('toggled',self.chgBox1)
            self.box1.connect('destroy',self.justQuit)
            #self.box1.connect('delete-event',self.destroy,self.box1)

            # Get column name from data file
            self.dataFile = createDataFileReader(self.dataFileName)
            colNum = self.dataFile.getColNum()
            if colNum < 4:
                raise TgisError(errMsg0101)
            if colNum == 4 or colNum == 6:           ##### original>      if colNum == 4 or colNum == 5: ####################################
                self.chkDataType.set_sensitive(0)
            colName = self.dataFile.getColName()
           
            # Create list for combo box
            self.lsColName = gtk.ListStore(str)
            for item in colName:
                self.lsColName.append([item])
            if colNum < 6:
                self.listCmb = [self.cmbX,self.cmbY,self.cmbTime,self.cmbData]
            else:
                self.listCmb = [self.cmbX,self.cmbY,self.cmbTime,\
                                self.cmbData,self.cmbVal1,self.cmbVal2,self.cmbVal3,self.cmbVal4]      #### P Jat ############################
                
            #else:
            #    self.listCmb = [self.cmbX,self.cmbY,self.cmbTime,\
            #                    self.cmbData,self.cmbVal1,self.cmbVal2]      #### Original
                
            numItem = self.lsColName.get_iter_first()
            # Set combo box
            for cmb in self.listCmb:                    
                cmb.set_model(self.lsColName)
                self.cell = gtk.CellRendererText()
                cmb.pack_start(self.cell,True)
                cmb.set_active_iter(numItem)
                numItem = self.lsColName.iter_next(numItem)
                               
            # Create list for combo box (ID field)
            self.lsColNameId = gtk.ListStore(str)
            self.lsColNameId.append(['Automatic ID'])
            for item in colName:                                
                self.lsColNameId.append([item])
            numItemId = self.lsColNameId.get_iter_first()
            
            # Set combo box (ID field)
            self.cmbId.set_model(self.lsColNameId)
            self.cell = gtk.CellRendererText()
            self.cmbId.pack_start(self.cell,True)
            self.cmbId.set_active_iter(numItemId)
            
            # Get parameters from param file
            try:
                listNumCol = self.fileParam['listNumCol']
                numColId = self.fileParam['numColId']
                self.flgUseDataType = self.fileParam['flgUseDataType']
                self.sptlUnit = self.fileParam['sptlUnit']
                self.tempUnit = self.fileParam['tempUnit']
                self.dataUnit = self.fileParam['dataUnit']
                self.agentName = self.fileParam['agentName']
                self.objRawData = self.fileParam['objRawData']
                self.objHardData = self.fileParam['objHardData']
                self.objAveHard = self.fileParam['objAveHard1']
                self.flgNewSession = False
            except:
                self.flgNewSession = True
            # If parameters exist, set values
            if not self.flgNewSession:
                self.cmbX.set_active(listNumCol[0])
                self.cmbY.set_active(listNumCol[1])      
                self.cmbTime.set_active(listNumCol[2])
                self.cmbData.set_active(listNumCol[3])
                self.cmbVal4.set_active(listNumCol[7])
                                
                if self.flgUseDataType:
                    self.chkDataType.set_active(True)
                    self.cmbVal1.set_active(listNumCol[4])    ############################################ No change
                    self.cmbVal2.set_active(listNumCol[5])
                    self.cmbVal3.set_active(listNumCol[6])    ##### P JAT #################
                    self.cmbVal4.set_active(listNumCol[7])    ##### P JAT #################
                    
                self.cmbId.set_active(numColId)
                self.entSpaceUnit.set_text(self.sptlUnit)
                self.entTimeUnit.set_text(self.tempUnit)
                self.entDataUnit.set_text(self.dataUnit)
                self.entAgentName.set_text(self.agentName)
            # Change cursor to None
            self.lowWindow1.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def chgButtons(self):
        """Change buttons"""
        if self.numCurScreen == 0:
            pass
        elif self.numCurScreen == 1:
            self.butQuit1.set_sensitive(1)
            self.butNext1.set_sensitive(0)
        elif self.numCurScreen == 2:
            self.butQuit2.set_sensitive(1)
            self.butNext2.set_sensitive(0)
            self.butBack2.set_sensitive(1)
        elif self.numCurScreen == 3:
            self.butQuit3.set_sensitive(1)
            self.butNext3.set_sensitive(0)
            self.butBack3.set_sensitive(1)
        elif self.numCurScreen == 4:
            self.butQuit4.set_sensitive(1)
            self.butNext4.set_sensitive(0)
            self.butBack4.set_sensitive(1)
        elif self.numCurScreen == 5:
            self.butQuit5.set_sensitive(1)
            self.butNext5.set_sensitive(0)
            self.butBack5.set_sensitive(1)

    def chgCursor(self):
        """Change cursor"""
        if self.numCurScreen == 0:
            pass
        elif self.numCurScreen == 1:
            self.lowWindow1.set_cursor(None)
        elif self.numCurScreen == 2:
            self.lowWindow2.set_cursor(None)
        elif self.numCurScreen == 3:
            self.lowWindow3.set_cursor(None)
        elif self.numCurScreen == 4:
            self.lowWindow4.set_cursor(None)
        elif self.numCurScreen == 5:
            self.lowWindow5.set_cursor(None)
        elif self.numCurScreen == 6:
            self.lowWindow6.set_cursor(None)

    def addGPMessage(self,errMsg, severity):
        """Display error message on ArcGIS window"""
        if severity == 0:
            # Add message
            self.GP.AddMessage(errMsg)
        elif severity == 1:
            # Add warining message
            self.GP.AddWarning(errMsg)
        elif severity == 2:
            # Add error message
            self.GP.AddError(errMsg)

    def dispErrMsg(self,errMsg=''):
        """Display error message"""
        # Output traceback info
        errFileName = self.workingSpace + \
                      '\\' + strErrFile
        errFile = open(errFileName,'a')
        traceback.print_exc(file=errFile)
        errFile.close()
        # Set error message dialog object tree
        errorDialogName = "diagError"
        self.errorDialogTree = gtk.glade.XML(gladeFile,errorDialogName)
        # Get widget objects
        self.errorDialog = \
                        self.errorDialogTree.get_widget(errorDialogName)
        self.lblErrorMsg = self.errorDialogTree.get_widget('lblErrorMsg')
        # Set error message
        errMsg = errMsg + str(sys.exc_info()[1])
        self.lblErrorMsg.set_text(errMsg)
        if not self.flgStdAlone:
            self.addGPMessage(errMsg,2)
        # Display error message dialog
        bolResp=self.errorDialog.run()
        if bolResp==gtk.RESPONSE_OK:
            # Destroy error message dialog
            self.errorDialog.destroy()
        elif bolResp==gtk.RESPONSE_CLOSE:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()           

    def dispRegMsg(self,regMsg=''):
        """Display regular message"""
        # Set message dialog object tree
        msgDialogName = "diagMsg"
        self.msgDialogTree = gtk.glade.XML(gladeFile,msgDialogName)
        # Get widget objects
        self.msgDialog = \
                        self.msgDialogTree.get_widget(msgDialogName)
        self.lblDiagMsg = self.msgDialogTree.get_widget('lblDiagMsg')
        # Set error message
        self.lblDiagMsg.set_text(regMsg)
        # Display error message dialog
        bolResp=self.msgDialog.run()
        if bolResp==gtk.RESPONSE_OK:
            # Destroy error message dialog
            self.msgDialog.destroy()

    def askQuit(self,widget):
        """Display quit message"""
        # Change cursor to None
        self.chgCursor
        # Set quit message dialog object tree
        dialogName = "diagQuit"
        self.quitDialogTree = gtk.glade.XML(gladeFile,dialogName)
        # Get widget objects
        self.quitDialog = \
                self.quitDialogTree.get_widget(dialogName)
        # Display quit message dialog
        bolResp = self.quitDialog.run()
        if bolResp == gtk.RESPONSE_CANCEL:
            # Destroy ask dialog
            self.quitDialog.destroy()
        elif bolResp == gtk.RESPONSE_OK:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()


    def chgBox1(self,widget):
        """Show/hide combo boxes"""        
        # Change cursor to Watch
        self.lowWindow1.set_cursor(self.cursorWatch)
        if self.chkDataType.get_active():
            # Show combo box
            self.lblDataType.set_text('Data Type')
            self.lblVal1.set_text('Value1 Field')
            self.lblVal2.set_text('Value2 Field')
            self.lblVal3.set_text('Value3 Field')   #### P JAT ################   
            self.lblVal4.set_text('Value4 Field')   #### P JAT ################   
            
            self.cmbVal1.show()
            self.cmbVal2.show()
            self.cmbVal3.show()                     #### P JAT ################ 
            self.cmbVal4.show()                     #### P JAT ################ 
            self.flgUseDataType = True
        else:
            # Hide combo box
            self.lblDataType.set_text('Data Field')
            self.lblVal1.set_text('')
            self.lblVal2.set_text('')
            self.lblVal3.set_text('')               #### P JAT ################ 
            self.lblVal4.set_text('')               #### P JAT ################ 
            
            self.cmbVal1.hide()
            self.cmbVal2.hide()
            self.cmbVal3.hide()                     #### P JAT ################ 
            self.cmbVal4.hide()                     #### P JAT ################ 
            self.flgUseDataType = False
        # Change cursor to None
        self.lowWindow1.set_cursor(None)

    def selectMethod2(self,dupInfo):
        """Display select method message"""
        # Change cursor to None
        self.chgCursor
        # Set quit message dialog object tree
        dialogName = "diagSelect"
        selectDialogTree = gtk.glade.XML(gladeFile,dialogName)
        # Get widget objects
        selectDialog = selectDialogTree.get_widget(dialogName)
        self.lblQuestion = selectDialogTree.get_widget('lblQuestion')
        self.label512 = selectDialogTree.get_widget('label512')   #---------------- BME Solution Text --------- P Jat
        self.textview3 = selectDialogTree.get_widget('textview3') #-------------- BME error handling -------------- P Jat
        self.label523 = selectDialogTree.get_widget('label523')   #-- 'Error 1 or 2 ----- on Error hand Window-----

        radOpt1 = selectDialogTree.get_widget('radOpt1')
        lblOpt1 = selectDialogTree.get_widget('lblOpt1')
        radOpt2 = selectDialogTree.get_widget('radOpt2')
        lblOpt2 = selectDialogTree.get_widget('lblOpt2')
        radOpt1.set_active(1)

        # Set default messages
        msgOpt1 = 'Accept Data Correction by BMEGUI'
        msgOpt2 = 'Quit Application and Correct Data Manually'
        lblOpt1.set_text(msgOpt1)
        lblOpt2.set_text(msgOpt2)
        
        self.curIdx = 0

        # Construct messages
        numDup = len(dupInfo)
        curId = dupInfo[self.curIdx][0]

        msgQuestion = 'Station ' + str(curId) + ' assigned to' # p jat ------------------------------
        msgQuestion = msgQuestion  + str(len(dupInfo[self.curIdx][1])) + ' different locations.' # There are ' + str(len(dupLoc)) + '\n such station(s).' #---- P jat-----------------------------
        msgQuestion = string.strip(msgQuestion)

        aveLoc = mean(array(dupInfo[self.curIdx][1]),0).tolist()
        BMEsol = ' To correct this error, BMEGUI will assign station ' + str(curId) + ' \n' + \
                 'to new location (' + str(aveLoc[0]) + ', ' + str(aveLoc[1]) + ' )'

##        self.dataFile = createDataFileReader(self.dataFileName)
##        colNum = self.dataFile.getColName()
##        BMEtable = '( '+ colNum[0] + ' , '+ colNum[1] +' )\n'

        colNum = self.dataFile.getColName()
        numColX = self.cmbX.get_active()
        numColY = self.cmbY.get_active()

        BMEtable = '( '+ colNum[numColX] + '        ,       '+ colNum[numColY] +')\n'
        for item in dupInfo[self.curIdx][1]:
            BMEtable = BMEtable + '( ' + str(item[0]) + '       ,        ' + str(item[1]) + ' ) \n'
        BMEtable = string.strip(BMEtable)                    

        # Set messages
        self.lblQuestion.set_text(msgQuestion)
        self.label512.set_text(BMEsol)                            #-------------- BME solution text -------------- P Jat
        
        textbuffer = self.textview3.get_buffer()                  #-------------- BME error handling -------------- P Jat
        textbuffer.set_text(BMEtable)                          #-------------- BME error handling -------------- P Jat

        self.label523.set_text('Error ' + str(self.curIdx + 1) + ' of ' + str(numDup))

        #--------------------- Next, Previos, and All Buttons----P Jat------------------
        self.button67 = selectDialogTree.get_widget('button67') # --- 'NEXT>>' button -------
        self.button68 = selectDialogTree.get_widget('button68') # --- '<<Previous' button -------
        #self.buton66 = selectDialogTree.get_widget('button66') # --- 'ALL'  button -------
        self.button67.connect('clicked',self.moveError,True,dupInfo)                  # Call for 'NEXT'/'BACK' buttons
        self.button68.connect('clicked',self.moveError,False,dupInfo)

        if self.curIdx == 0:
            self.button68.set_sensitive(0)
            self.button67.set_sensitive(1)
        elif self.curIdx == numDup-1:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(0)
        else:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(1)

        if len(dupInfo)==1:
            self.button68.set_sensitive(0)   ## P jat
            self.button67.set_sensitive(0)   ## P jat        

        # Display quit message dialog
        bolResp = selectDialog.run()
        if radOpt1.get_active():
            # Destroy ask dialog
            selectDialog.destroy()
            return True
        else:
            # Destroy ask dialog
            selectDialog.destroy()
            return False

    def moveError(self, widget, flgNext, dupInfo):
        if flgNext:
            self.curIdx = self.curIdx + 1
        else:
            self.curIdx = self.curIdx - 1
                
        numDup = len(dupInfo)
        curId = dupInfo[self.curIdx][0]

        msgQuestion = 'Station ' + str(curId) + ' assigned to' # p jat ------------------------------
        msgQuestion = msgQuestion  + str(len(dupInfo[self.curIdx][1])) + ' different locations.' # There are ' + str(len(dupLoc)) + '\n such station(s).' #---- P jat-----------------------------
        msgQuestion = string.strip(msgQuestion)
        self.lblQuestion.set_text(msgQuestion)

        aveLoc = mean(array(dupInfo[self.curIdx][1]),0).tolist()
        BMEsol = ' To correct this error, BMEGUI will assign station ' + str(curId) + ' \n' + \
                 'to new location (' + str(aveLoc[0]) + ', ' + str(aveLoc[1]) + ' )'
        self.label512.set_text(BMEsol)                            #-------------- BME solution text -------------- P Jat
        
##        self.dataFile = createDataFileReader(self.dataFileName)
##        colNum = self.dataFile.getColName()
##        BMEtable = '( '+ colNum[0] + ' , '+ colNum[1] +' )\n'

        colNum = self.dataFile.getColName()
        numColX = self.cmbX.get_active()
        numColY = self.cmbY.get_active()

        BMEtable = '( '+ colNum[numColX] + '        ,       '+ colNum[numColY] +')\n'
        for item in dupInfo[self.curIdx][1]:
            BMEtable = BMEtable + '( ' + str(item[0]) + '       ,       ' + str(item[1]) + ' ) \n'
        BMEtable = string.strip(BMEtable)                    
        textbuffer = self.textview3.get_buffer()                  #-------------- BME error handling -------------- P Jat
        textbuffer.set_text(BMEtable)                          #-------------- BME error handling -------------- P Jat

        self.label523.set_text('Error ' + str(self.curIdx + 1) + ' of ' + str(numDup))

        if self.curIdx == 0:
            self.button68.set_sensitive(0)
            self.button67.set_sensitive(1)
        elif self.curIdx == numDup-1:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(0)
        else:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(1)

    def prevError(self, widget, curIdx):
        print curIdx
       
        


















#######################  New for same Location but multiple IDs ###
    def selectMethod3(self,dupIdInfo):    
        """Display select method message"""
        # Change cursor to None
        self.chgCursor
        # Set quit message dialog object tree
        dialogName = "diagSelect"
        selectDialogTree = gtk.glade.XML(gladeFile,dialogName)
        # Get widget objects
        selectDialog = selectDialogTree.get_widget(dialogName)
        self.lblQuestion = selectDialogTree.get_widget('lblQuestion')
        self.label512 = selectDialogTree.get_widget('label512')   #---------------- BME Solution Text --------- P Jat
        self.textview3 = selectDialogTree.get_widget('textview3') #-------------- BME error handling -------------- P Jat
        self.label523 = selectDialogTree.get_widget('label523')   #-- 'Error 1 or 2 ----- on Error hand Window-----

        radOpt1 = selectDialogTree.get_widget('radOpt1')
        lblOpt1 = selectDialogTree.get_widget('lblOpt1')
        radOpt2 = selectDialogTree.get_widget('radOpt2')
        lblOpt2 = selectDialogTree.get_widget('lblOpt2')
        radOpt1.set_active(1)

        # Set default messages
        msgOpt1 = 'Accept Data Correction by BMEGUI'
        msgOpt2 = 'Quit Application and Correct Data Manually'
        lblOpt1.set_text(msgOpt1)
        lblOpt2.set_text(msgOpt2)

       
        self.curIdx2 = 0

        # Construct messages
        numIdDup = len(dupIdInfo)
        curId2 = dupIdInfo[self.curIdx2][0]
        msgQuestion = 'Location ' + str(curId2) + ' assigned to' # p jat ------------------------------
        msgQuestion = msgQuestion  + ' '+ str(len(dupIdInfo[self.curIdx2][1])) + ' different stations IDs.' # There are ' + str(len(dupLoc)) + '\n such station(s).' #---- P jat-----------------------------
        msgQuestion = string.strip(msgQuestion)
        BMEsol = ' To correct this error, BMEGUI will assign location ' + str(curId2) + ' \n' + \
                 'to stations ID (' + str(max(dupIdInfo[self.curIdx2][1])) + ' )'

        colNum = self.dataFile.getColName()
        numColX = self.cmbX.get_active()
        numColY = self.cmbY.get_active()

        BMEtable = '( '+ colNum[numColX] + '        ,       '+ colNum[numColY] +',      ID )\n'
        for item in dupIdInfo[self.curIdx2][1]:
            BMEtable = BMEtable + '( ' + str(curId2[0]) + '     ,       '+ str(curId2[1]) +'    '+ str(dupIdInfo[self.curIdx2][1]) +' ) \n'
        BMEtable = string.strip(BMEtable)                    


        # Set messages
        self.lblQuestion.set_text(msgQuestion)
        self.label512.set_text(BMEsol)                            #-------------- BME solution text -------------- P Jat
        
        textbuffer = self.textview3.get_buffer()                  #-------------- BME error handling -------------- P Jat
        textbuffer.set_text(BMEtable)                          #-------------- BME error handling -------------- P Jat

        self.label523.set_text('Error ' + str(self.curIdx2 + 1) + ' of ' + str(numIdDup))


        #--------------------- Next, Previos, and All Buttons----P Jat------------------
        self.button67 = selectDialogTree.get_widget('button67') # --- 'NEXT>>' button -------
        self.button68 = selectDialogTree.get_widget('button68') # --- '<<Previous' button -------
        #self.buton66 = selectDialogTree.get_widget('button66') # --- 'ALL'  button -------
        self.button67.connect('clicked',self.moveErrorId,True,dupIdInfo)                  # Call for 'NEXT'/'BACK' buttons
        self.button68.connect('clicked',self.moveErrorId,False,dupIdInfo)

        if self.curIdx2 == 0:
            self.button68.set_sensitive(0)
            self.button67.set_sensitive(1)
        elif self.curIdx == numIdDup-1:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(0)
        else:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(1) 

        # Display quit message dialog
        bolResp = selectDialog.run()
        if radOpt1.get_active():
            # Destroy ask dialog
            selectDialog.destroy()
            return True
        else:
            # Destroy ask dialog
            selectDialog.destroy()
            return False

    def moveErrorId(self, widget, flgNext, dupIdInfo):
        if flgNext:
            self.curIdx2 = self.curIdx2 + 1
        else:
            self.curIdx2 = self.curIdx2 - 1

       

        # Construct messages
        numIdDup = len(dupIdInfo)
        curId2 = dupIdInfo[self.curIdx2][0]
        msgQuestion = 'Location ' + str(curId2) + ' assigned to' # p jat ------------------------------
        msgQuestion = msgQuestion  + ' '+ str(len(dupIdInfo[self.curIdx2][1])) + ' different stations IDs.' # There are ' + str(len(dupLoc)) + '\n such station(s).' #---- P jat-----------------------------
        msgQuestion = string.strip(msgQuestion)
        BMEsol = ' To correct this error, BMEGUI will assign location ' + str(curId2) + ' \n' + \
                 'to stations ID (' + str(max(dupIdInfo[self.curIdx2][1])) + ' )'

        colNum = self.dataFile.getColName()
        numColX = self.cmbX.get_active()
        numColY = self.cmbY.get_active()

        BMEtable = '( '+ colNum[numColX] + '        ,       '+ colNum[numColY] +',      ID )\n'
        for item in dupIdInfo[self.curIdx2][1]:
            BMEtable = BMEtable + '( ' + str(curId2[0]) + '     ,       '+ str(curId2[1]) +'    '+ str(dupIdInfo[self.curIdx2][1]) +' ) \n'
        BMEtable = string.strip(BMEtable)                    
        self.label512.set_text(BMEsol)                            #-------------- BME solution text -------------- P Jat

        textbuffer = self.textview3.get_buffer()                  #-------------- BME error handling -------------- P Jat
        textbuffer.set_text(BMEtable)                          #-------------- BME error handling -------------- P Jat

        self.label523.set_text('Error ' + str(self.curIdx2 + 1) + ' of ' + str(numIdDup))

        if self.curIdx2 == 0:
            self.button68.set_sensitive(0)
            self.button67.set_sensitive(1)
        elif self.curIdx2 == numIdDup-1:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(0)
        else:
            self.button68.set_sensitive(1)
            self.button67.set_sensitive(1)

    def prevErrorId(self, widget, curIdx2):
        print curIdx2   

##################################################################






        

    def selectMethod(self,*arg):
        """Display select method message"""
        # Change cursor to None
        self.chgCursor
        # Set quit message dialog object tree
        dialogName = "diagSelect"
        selectDialogTree = gtk.glade.XML(gladeFile,dialogName)
        # Get widget objects
        selectDialog = selectDialogTree.get_widget(dialogName)
        lblQuestion = selectDialogTree.get_widget('lblQuestion')
        radOpt1 = selectDialogTree.get_widget('radOpt1')
        lblOpt1 = selectDialogTree.get_widget('lblOpt1')
        radOpt2 = selectDialogTree.get_widget('radOpt2')
        lblOpt2 = selectDialogTree.get_widget('lblOpt2')

        #####len(self.dupLoc)

        self.label512 = selectDialogTree.get_widget('label512')   #---------------- BME Solution Text --------- P Jat
        self.label512.set_text(arg[3])                            #-------------- BME solution text -------------- P Jat
        
        self.textview3 = selectDialogTree.get_widget('textview3') #-------------- BME error handling -------------- P Jat
        textbuffer = self.textview3.get_buffer()                  #-------------- BME error handling -------------- P Jat
        textbuffer.set_text(arg[4])                          #-------------- BME error handling -------------- P Jat
        

        self.label523 = selectDialogTree.get_widget('label523')   #-- 'Error 1 or 2 ----- on Error hand Window-----
        self.label523.set_text('Error ' +  ' of ' + '5')
        #label523.set_text(arg[5])

        

        #--------------------- Next, Previos, and All Buttons----P Jat------------------
        self.button67 = selectDialogTree.get_widget('button67') # --- 'NEXT>>' button -------
        self.button68 = selectDialogTree.get_widget('button68') # --- '<<Previous' button -------
        #self.buton66 = selectDialogTree.get_widget('button66') # --- 'ALL'  button -------
        self.button67.connect('clicked',self.BMEGUIerrorH)                  # Call for 'NEXT'/'BACK' buttons
        self.button68.connect('clicked',self.BMEGUIerrorH)

        
        lblQuestion.set_text(arg[0])
        lblOpt1.set_text(arg[1])
        lblOpt2.set_text(arg[2])
        radOpt1.set_active(1)
        # Display quit message dialog
        bolResp = selectDialog.run()
        if radOpt1.get_active():
            # Destroy ask dialog
            selectDialog.destroy()
            return True
        else:
            # Destroy ask dialog
            selectDialog.destroy()
            return False


    def goBox2(self,widget):
        """Move to box2"""
        try:
            # Delete Mask Files
            if os.path.exists('maskX.txt'):
                os.remove('maskX.txt')
            if os.path.exists('maskY.txt'):
                os.remove('maskY.txt')
            if os.path.exists('maskFileName.txt'):
                os.remove('maskFileName.txt')
                
            # Delete Map Files
            if os.path.exists('mapX.txt'):
                os.remove('mapX.txt')
            if os.path.exists('mapY.txt'):
                os.remove('mapY.txt')
            if os.path.exists('plotMapFileName.txt'):
                os.remove('plotMapFileName.txt')


                
            # Delete River Network Error Record file ('ErrorRiverNet.txt')
            if os.path.exists('ErrorRiverNet.txt'):
                os.remove('ErrorRiverNet.txt')

            if os.path.exists('stdPathRT.txt'):
                cmdChechRiver = 'checkRiverNetworkContinuity.exe ' + self.workingSpace
                retCode = os.system(cmdChechRiver)
                if retCode==0:
                    print
                else:
                    raise TgisError('Error in River Network test')
                file = open('ErrorRiverNet.txt')
                line = file.readline()
                if line=='0.000000\n':
                    print 'No Error in River Network'
                else:
                    RiverErrorMsg ='  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~' +\
                                    '               River Network Error :         ' +\
                                    ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~' +\
                                    '  All River Reaches are not connected.       ' +\
                                    '  See "River_Network_Error.png" figure ' + \
                                    '  at : ' + self.workingSpace
                    riverNetworkerror = 'errorDial.exe ' + self.workingSpace
                    #os.system(riverNetworkerror) # Error function in MATLAB 
                    raise TgisError(RiverErrorMsg)
                    gtk.main_quit()
                 
        
            # Change cursor to Watch
            self.lowWindow1.set_cursor(self.cursorWatch)
            # If not new session, check if parameters has been changed
            if not self.flgNewSession:
                # Get column name from combo box
                listNumCol = []
                if not self.flgUseDataType:
                    listTempCmb = [self.cmbX,self.cmbY,self.cmbTime,self.cmbData]
                else:
                    listTempCmb = [self.cmbX,self.cmbY,self.cmbTime,\
                                   self.cmbData,self.cmbVal1,self.cmbVal2,self.cmbVal3,self.cmbVal4]   #### P jat
                for item in listTempCmb:
                    numCol = item.get_active()
                    listNumCol.append(numCol)
                # Get station ID from combo box
                numColId = self.cmbId.get_active()
                # Get value in param file
                listNumColOld = self.fileParam['listNumCol']
                numColIdOld = self.fileParam['numColId']
                flgUseDataTypeOld = self.fileParam['flgUseDataType']
                # Compare values
                if not listNumColOld == listNumCol:
                    self.flgNewSession = True
                if not numColIdOld == numColId:
                    self.flgNewSession = True
                if not flgUseDataTypeOld == self.flgUseDataType:
                    self.flgNewSession = True
                if self.flgNewSession:
                    msgQuestion = 'Parameter has changed.'
                    msgOpt1 = 'Delete all parameters and estimation files ' + \
                              'and continue using new parameters'
                    msgOpt2 = 'Quit Application and use different working space'
                    flgParamChg = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    if flgParamChg:
                        self.delEstFiles(self.paramFileHeader,extResStatFile)
                        self.delEstFiles(self.paramFileHeader,extResMapFile)
                    else:
                        raise DummyError
            # If new session, create data objects
            if self.flgNewSession:
                # Create raw data object
                self.objRawData = GeoData()
                # Flag use data type
                if self.chkDataType.get_active():
                    self.flgUseDataType = True
                else:
                    self.flgUseDataType = False
                # Get column name from combo box
                listNumCol = []
                if not self.flgUseDataType:
                    listTempCmb = [self.cmbX,self.cmbY,self.cmbTime,self.cmbData]
                else:
                    listTempCmb = [self.cmbX,self.cmbY,self.cmbTime,\
                                   self.cmbData,self.cmbVal1,self.cmbVal2,self.cmbVal3,self.cmbVal4]   #### P Jat
                for item in listTempCmb:
                    numCol = item.get_active()
                    listNumCol.append(numCol)
                if self.dataFile.getNthCol(numCol,1) == []:
                    raise TgisError(errMsg0102)
                for numCmb,item in enumerate(self.listCmb):
                    numCol = item.get_active()
                    if numCmb == 0:
                        self.objRawData.setX(self.dataFile.getNthCol(numCol,1))
                    elif numCmb == 1:
                        self.objRawData.setY(self.dataFile.getNthCol(numCol,1))
                    elif numCmb == 2:
                        self.objRawData.setT(self.dataFile.getNthCol(numCol,1))
                    elif numCmb == 3:
                        self.objRawData.setVal1(self.dataFile.getNthCol(numCol,1))
                    if self.flgUseDataType:
                        if numCmb == 4:
                            self.objRawData.setVal2(self.dataFile.\
                                                    getNthCol(numCol,1))
                        elif numCmb == 5:
                            self.objRawData.setVal3(self.dataFile.\
                                                    getNthCol(numCol,1))
                        elif numCmb == 6:
                            self.objRawData.setVal4(self.dataFile.\
                                                    getNthCol(numCol,1))        ########## P Jat#
                     
                        elif numCmb == 7:
                            self.objRawData.setVal5(self.dataFile.\
                                                    getNthCol(numCol,1))        ########## P Jat
                # Get station ID from combo box
                numColId = self.cmbId.get_active()
                if numColId == 0:
                    # Create Automatic ID
                    self.objRawData.setId(createId(self.objRawData))
                else:
                    # Set user defined ID
                    self.objRawData.setId(self.dataFile.getNthCol(numColId-1,2))
                # Find duplicated Location
                dupLoc = findDupliLoc(self.objRawData)                               
                if len(dupLoc) != 0:
                    flgRemvLoc = self.selectMethod2(dupLoc)
                    if flgRemvLoc:
                        arySptl = removeDupliLoc(self.objRawData)
                        self.objRawData.updateX(arySptl[:,0].tolist())
                        self.objRawData.updateY(arySptl[:,1].tolist())
                    else:
                        raise DummyError
                    
                    
                # Find duplicated ID
                dupId = findDupliId(self.objRawData)
                if len(dupId) != 0:
                    flgRemvId = self.selectMethod3(dupId)
                    if flgRemvId:
                        listId = removeDupliId(self.objRawData)
                        self.objRawData.updateX(arySptl[:,0].tolist())
                        self.objRawData.updateId(listId)
                    else:
                        raise DummyError
                    

                    #----------------- Original Codes ----Start---------------------------------------------------------------------------------------------
                    #msgQuestion = 'Different station IDs are assigned to the ' + \
                    #              'same location. ' + \
                    #              'There are ' + str(len(dupId)) + \
                    #              ' such location(s). \n' + \
                    #              '(Example) \n' + \
                    #              'Location: (' + str(dupId[0][0][0]) + ' , ' + \
                    #              str(dupId[0][0][1]) +') \n' + 'ID: '
                    # #----------------- Original Codes ----end------------------------------------------------------------------------------------------

                    #for item in dupId[0][1]:
                    #    msgQuestion = msgQuestion + str(item) + '  '
                    #msgOpt1 = 'Correct data using system defaults'
                    #msgOpt2 = 'Quit Application and correct data manually'
                    #flgRemvId = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    #if flgRemvId:
                    #    listId = removeDupliId(self.objRawData)
                    #    self.objRawData.updateId(listId)
                    #else:
                    #    raise DummyError



                    
                # Create system Id
                sysId = createSystemId(self.objRawData)
                self.objRawData.setSysId(sysId)
                # Update value field for non type data
                if not self.chkDataType.get_active():
                    dataVal = self.objRawData.getVal1()
                    typeVal = zeros(len(self.objRawData.getX())).tolist()
                    typeVal = map(lambda x: int(x), typeVal)
                    self.objRawData.updateVal1(typeVal)
                    self.objRawData.setVal2(dataVal)
                    self.objRawData.setVal3(dataVal)
                    self.objRawData.setVal4(dataVal)        ############### P jat
                    self.objRawData.setVal5(dataVal)        ############### P Jat
                # Construct hard data object
                self.objHardData = GeoData()
                self.objHardData.setX(self.objRawData.getX())
                self.objHardData.setY(self.objRawData.getY())
                self.objHardData.setT(self.objRawData.getT())
                self.objHardData.setId(self.objRawData.getId())
                self.objHardData.setSysId(self.objRawData.getSysId())
                if self.chkDataType.get_active():
                    self.objHardData.setVal1(self.objRawData.getVal1())
                    hardVal = hardenData(self.objRawData)
                    self.objHardData.setVal2(hardVal)
                else:
                    typeVal = zeros(len(self.objRawData.getX())).tolist()
                    typeVal = map(lambda x: int(x), typeVal)
                    self.objHardData.setVal1(typeVal)
                    self.objHardData.setVal2(self.objRawData.getVal2())





                    
##                # Eliminate duplicated measurement from hardened data               ################################# $$$$$$$$$$$$$$$$$$$$$
                
                self.objAveHard = aveDupli(self.objHardData)
                if len(self.objAveHard.getX()) != len(self.objHardData.getX()):
                    print 
##                    msgQuestion = 'There are duplicated measurements'
##                    msgOpt1 = 'Average duplicated measurements'
##                    msgOpt2 = 'Quit Application and remove duplicated measurements'
##                    #flgRemvId = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)     # ---Original
##                    if flgRemvId:
##                        pass
##                    else:
##                        raise DummyError





                # Station ID ans System ID list
                self.uniqueId = unique(self.objHardData.getId()).tolist()
                self.uniqueSysId = []
                for item in self.uniqueId:
                    self.uniqueSysId.append(self.objHardData.getSysId()\
                                       [self.objHardData.getId().index(item)])
                aveSysId = self.objAveHard.getSysId()
                aveId = []
                for idx,item in enumerate(aveSysId):
                     aveId.append(self.uniqueId[self.uniqueSysId.index(item)])
                self.objAveHard.setId(aveId)
                # Store parameter
                self.fileParam['listNumCol'] = listNumCol
                self.fileParam['numColId'] = numColId
                self.fileParam['flgUseDataType'] = self.flgUseDataType
                self.fileParam['objRawData'] = self.objRawData
                self.fileParam['objHardData'] = self.objHardData
                self.fileParam['objAveHard1'] = self.objAveHard
            else:
                # Station ID ans System ID list
                self.uniqueId = unique(self.objHardData.getId()).tolist()
                self.uniqueSysId = []
                for item in self.uniqueId:
                    self.uniqueSysId.append(self.objHardData.getSysId()\
                                       [self.objHardData.getId().index(item)])
            # Get unit names
            self.sptlUnit = self.entSpaceUnit.get_text()
            self.tempUnit = self.entTimeUnit.get_text()
            self.dataUnit = self.entDataUnit.get_text()
            self.agentName = self.entAgentName.get_text()
            self.fileParam['sptlUnit'] = self.sptlUnit
            self.fileParam['tempUnit'] = self.tempUnit
            self.fileParam['dataUnit'] = self.dataUnit
            self.fileParam['agentName'] = self.agentName
            # Change cursor to None
            self.lowWindow1.set_cursor(None)
            # Hide box1
            self.box1.hide()

            # Set box2 object tree
            windowName = "box2"
            self.numCurScreen = 2
            self.windowTree2 = gtk.glade.XML(gladeFile,windowName)
            # Get window and low window objects
            self.box2 = self.windowTree2.get_widget(windowName)
            self.lowWindow2 = self.box2.window
            # Change cursor to Watch
            self.lowWindow2.set_cursor(self.cursorWatch)
            # Get widget objects
            self.entRawMean = self.windowTree2.get_widget('entRawMean')
            self.entLogMean = self.windowTree2.get_widget('entLogMean')
            self.entRawStd = self.windowTree2.get_widget('entRawStd')
            self.entLogStd = self.windowTree2.get_widget('entLogStd')
            self.entRawSkew = self.windowTree2.get_widget('entRawSkew')
            self.entLogSkew = self.windowTree2.get_widget('entLogSkew')
            self.entRawKurt = self.windowTree2.get_widget('entRawKurt')
            self.entLogKurt = self.windowTree2.get_widget('entLogKurt')
            self.entLogZeroDiv = self.windowTree2.get_widget('entLogZeroDiv')
            self.entLogZeroVal = self.windowTree2.get_widget('entLogZeroVal')
            butRedrawHist = self.windowTree2.get_widget('butRedrawHist')
            self.chkLogTran = self.windowTree2.get_widget('chkLogTran')
            self.butQuit2 = self.windowTree2.get_widget('butQuit2')
            self.butNext2 = self.windowTree2.get_widget('butNext2')
            self.butBack2 = self.windowTree2.get_widget('butBack2')
            self.radLogZero1 = self.windowTree2.get_widget('radLogZero1')
            self.radLogZero2 = self.windowTree2.get_widget('radLogZero2')
            self.noteHist = self.windowTree2.get_widget('noteHist')
            # Set default values
            uniVal = unique(self.objAveHard.getVal2()).tolist()
            if len(uniVal) == 1 and uniVal[0] == 0.0:
                self.radLogZero2.set_active(1)
                self.entLogZeroDiv.set_text('')
                self.entLogZeroVal.set_text(str(self.userConst['defLogZeroVal']))
                self.radLogZero1.set_sensitive(0)
            else:
                self.radLogZero1.set_active(1)
                self.entLogZeroVal.set_text('')
                self.entLogZeroDiv.set_text(str(self.userConst['defLogZeroDiv']))
            # Map event handler
            self.box2.connect('destroy',self.justQuit)
            self.butQuit2.connect('clicked',self.askQuit)
            self.butNext2.connect('clicked',self.goBox3)
            self.chkLogTran.connect('toggled',self.chgHistTab)
            self.butBack2.connect('clicked',self.backScreen)
            butRedrawHist.connect('clicked',self.redrawHist)
            self.radLogZero1.connect('toggled',self.chgRadLog)
            # Define canvas object for raw histogram
            self.areaHistRaw = self.windowTree2.get_widget("hboxHistRaw")
            [self.figHistRaw,self.plotHistRaw] = createFigObj()
            [self.cavHistRaw,self.tbHistRaw] = \
                           createCavObj(self.figHistRaw,self.box2)
            # Define canvas object for raw histogram
            self.areaHistLog = self.windowTree2.get_widget("hboxHistLog")
            [self.figHistLog,self.plotHistLog] = createFigObj()
            [self.cavHistLog,self.tbHistLog] = \
                           createCavObj(self.figHistLog,self.box2)
            # Get parameters from param file
            try:
                self.objAveHard = self.fileParam['objAveHard2']
                self.logZeroType = self.fileParam['logZeroType']
                self.logZeroVal = self.fileParam['logZeroVal']
                self.flgLogTran = self.fileParam['flgLogTran']
            except:
                self.flgNewSession = True
            # If parameters exist, set values
            if self.flgNewSession:
                # Get log zero default setting
                if self.radLogZero1.get_active():
                    self.logZeroType = 0
                    self.logZeroVal = float(self.entLogZeroDiv.get_text())
                else:
                    self.logZeroType = 1
                    self.logZeroVal = float(self.entLogZeroVal.get_text())
                # Log-transform data
                try:
                    self.objAveHard.setVal3(logTran(self.objAveHard.getVal2(),\
                                            self.logZeroType,self.logZeroVal))
                except:
                    self.objAveHard.updateVal3(logTran(self.objAveHard.getVal2(),\
                                            self.logZeroType,self.logZeroVal))
            else:
                if self.logZeroType == 0:
                    self.radLogZero1.set_active(1)
                    self.entLogZeroDiv.set_text(str(self.logZeroVal))
                else:
                    self.radLogZero2.set_active(1)
                    self.entLogZeroVal.set_text(str(self.logZeroVal))
                if self.flgLogTran == 1:
                    self.noteHist.set_current_page(1)
                    self.chkLogTran.set_active(1)
            # Plot Histgram
            self.plotHistBar()
            # Change cursor to None
            self.lowWindow2.set_cursor(None)

        except DummyError:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def plotHistBar(self):
        # Calculate basic statistics
        [hardMean,hardStd,hardSkew,hardKurt,hardMax,hardMin] = \
            calcStat(self.objAveHard.getVal2())
        [logMean,logStd,logSkew,logKurt,logMax,logMin] = \
            calcStat(self.objAveHard.getVal3())
        # Display basic statistics
        self.entRawMean.set_text(str(hardMean))
        self.entRawStd.set_text(str(hardStd))
        self.entRawSkew.set_text(str(hardSkew))
        self.entRawKurt.set_text(str(hardKurt))
        self.entLogMean.set_text(str(logMean))
        self.entLogStd.set_text(str(logStd))
        self.entLogSkew.set_text(str(logSkew))
        self.entLogKurt.set_text(str(logKurt))
        # Calculate frequency for histogram
        rawHistVal = pylabhist(self.objAveHard.getVal2(),\
                               self.userConst['numHistBins'])
        rawCnt = rawHistVal[0].tolist()
        rawVal = rawHistVal[1].tolist()
        rawFreq = map(lambda x: float(x)/float((sum(rawCnt))), rawCnt)
        logHistVal = pylabhist(self.objAveHard.getVal3(),\
                               self.userConst['numHistBins'])
        logCnt = logHistVal[0].tolist()
        logVal = logHistVal[1].tolist()
        logFreq = map(lambda x: float(x)/float((sum(logCnt))), logCnt)
        # Define bar width
        if len(rawVal) == 1:
            rawWidth = abs(rawVal[0])/2
            logWidth = abs(logVal[0])/2
        else:
            rawWidth = abs(max(rawVal)-min(rawVal))/\
                       self.userConst['numHistBins']
            logWidth = abs(max(logVal)-min(logVal))/\
                       self.userConst['numHistBins']
        # Define canvas object for raw histogram
        [self.figHistRaw,self.plotHistRaw] = createFigObj()
        # Define canvas object for raw histogram
        [self.figHistLog,self.plotHistLog] = createFigObj()
        # Plot raw histogram
        xlabelString = self.agentName + ' (' + self.dataUnit + ')'
        ylabelString = strRawHistY
        titleString = strRawHistTitle + self.agentName
        setPlotLabels(self.plotHistRaw,xlabelString,\
                      ylabelString,titleString,True)
        
        self.plotHistRaw.bar(rawVal,rawFreq,
                             width=rawWidth,color="b")
        [self.cavHistRaw,self.tbHistRaw] = \
           plotGraph(self.figHistRaw,self.plotHistRaw,\
                     self.cavHistRaw,self.tbHistRaw,\
                     self.areaHistRaw,self.box2)
        # Plot log histogram
        xlabelString = self.agentName + ' (log-' + self.dataUnit + ')'
        ylabelString = strLogHistY
        titleString = strLogHistTitle + self.agentName
        setPlotLabels(self.plotHistLog,xlabelString,\
                      ylabelString,titleString,True)
        self.plotHistLog.bar(logVal,logFreq,
                             width=logWidth,color="r")
        [self.cavHistLog,self.tbHistLog] = \
           plotGraph(self.figHistLog,self.plotHistLog,\
                     self.cavHistLog,self.tbHistLog,\
                     self.areaHistLog,self.box2)

    def chgHistTab(self,widget):
        """Switch tab to Log-histogram"""
        if self.chkLogTran.get_active():
            self.noteHist.set_current_page(1)
            self.flgLogTran = 1
        else:
            self.noteHist.set_current_page(0)
            self.flgLogTran = 0

    def chgRadLog(self,widget):
        """ """
        if self.radLogZero1.get_active():
            self.entLogZeroVal.set_text('')
        else:
            self.entLogZeroDiv.set_text('')

    def redrawHist(self,widget):
        """Redraw histogram"""
        try:
            # Change cursor to Watch
            self.lowWindow2.set_cursor(self.cursorWatch)
            # Get log zero default setting
            if self.radLogZero1.get_active():
                self.logZeroType = 0
                try:
                    self.logZeroVal = float(self.entLogZeroDiv.get_text())
                except:
                    raise TgisWarning(errMsg0201)
                if self.logZeroVal <= 0:
                    raise TgisWarning(errMsg0202)
            else:
                self.logZeroType = 1
                try:
                    self.logZeroVal = float(self.entLogZeroVal.get_text())
                except:
                    raise TgisWarning(errMsg0203)
                if self.logZeroVal <= 0:
                    raise TgisWarning(errMsg0204)
            # Log-transform data
            self.objAveHard.updateVal3(logTran(self.objAveHard.getVal2(),\
                                       self.logZeroType,self.logZeroVal))
            self.plotHistBar()
            # Change cursor to None
            self.lowWindow2.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def goBox3(self,widget):
        """Move to box3"""
        try:
            t = arange(0.0, 1.0+0.01, 0.01)
            s = cos(2*2*pi*t)
            t[41:60] = nan
            s[41:60] = nan
            p.plot(t, s, '-', lw=2)               
            p.show()
            # Change cursor to Watch
            self.lowWindow2.set_cursor(self.cursorWatch)

            # If new session, create data objects
            if not self.flgNewSession:
                # Get value in param file
                flgLogTranOld = self.fileParam['flgLogTran']
                logZeroTypeOld = self.fileParam['logZeroType']
                logZeroValOld = self.fileParam['logZeroVal']
                # Compare values
                if (self.flgLogTran != flgLogTranOld) or \
                   (self.logZeroType != logZeroTypeOld) or \
                   (self.logZeroVal != logZeroValOld):
                    self.flgNewSession = True
                if self.flgNewSession:
                    msgQuestion = 'Parameter has changed.'
                    msgOpt1 = 'Delete all parameters and estimation files ' + \
                              'and continue using new parameters'
                    msgOpt2 = 'Quit Application and use different working space'
                    flgParamChg = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    if flgParamChg:
                        self.delEstFiles(self.paramFileHeader,extResStatFile)
                        self.delEstFiles(self.paramFileHeader,extResMapFile)
                    else:
                        raise DummyError
            # If new session, create data objects
            if self.flgNewSession:
                # Get data transformation setting
                if not self.chkLogTran.get_active():
                    self.flgLogTran = 0
                else:
                    self.flgLogTran = 1
                # Store parameter
                self.fileParam['objAveHard2'] = self.objAveHard
                self.fileParam['logZeroType'] = self.logZeroType
                self.fileParam['logZeroVal'] = self.logZeroVal
                self.fileParam['flgLogTran'] = self.flgLogTran
            # Get Log Zero for BME estimation
            if not "logZero" in dir(self):
                self.logZero = getLogZero(self.objAveHard.getVal2(),\
                                          self.logZeroType,self.logZeroVal)
            # Change cursor to None
            self.lowWindow2.set_cursor(None)
            # Hide box2
            self.box2.hide()

            # Set box3 object tree
            windowName = "box3"
            self.numCurScreen = 3
            self.windowTree3 = gtk.glade.XML(gladeFile,windowName)
            # Get window and low window objects
            self.box3 = self.windowTree3.get_widget(windowName)
            self.lowWindow3 = self.box3.window
            # Change cursor to Watch
            self.lowWindow3.set_cursor(self.cursorWatch)
            # Get widget objects
            self.chkAggData = self.windowTree3.get_widget('chkAggData')
            self.entAggPeriod = self.windowTree3.get_widget('entAggPeriod')
            lblAggUnit = self.windowTree3.get_widget('lblAggUnit')
            self.butAggData = self.windowTree3.get_widget('butAggData')
            self.noteExp = self.windowTree3.get_widget('noteExp')
            self.butExpTimeNext = self.windowTree3.get_widget('butExpTimeNext')
            self.butExpTimeBack = self.windowTree3.get_widget('butExpTimeBack')
            self.cmbExpTime = self.windowTree3.get_widget('cmbExpTime')
            self.butExpStatNext = self.windowTree3.get_widget('butExpStatNext')
            self.butExpStatBack = self.windowTree3.get_widget('butExpStatBack')
            self.cmbExpId = self.windowTree3.get_widget('cmbExpId')
            self.entExpSysId = self.windowTree3.get_widget('entExpSysId')
            lblExpMapUnit = self.windowTree3.get_widget('lblExpMapUnit')
            self.butQuit3 = self.windowTree3.get_widget('butQuit3')
            self.butNext3 = self.windowTree3.get_widget('butNext3')
            self.butBack3 = self.windowTree3.get_widget('butBack3')
            self.entExpStatFrom = self.windowTree3.get_widget('entExpStatFrom')
            self.entExpStatTo = self.windowTree3.get_widget('entExpStatTo')
            lblExpTimeUnitFrom = \
                    self.windowTree3.get_widget('lblExpTimeUnitFrom')
            lblExpTimeUnitTo = self.windowTree3.get_widget('lblExpTimeUnitTo')
            lblStatLocX = self.windowTree3.get_widget('lblStatLocX')
            lblStatLocY = self.windowTree3.get_widget('lblStatLocY')
            self.entStatLocX = self.windowTree3.get_widget('entStatLocX')
            self.entStatLocY = self.windowTree3.get_widget('entStatLocY')
            self.entExpTimeN = self.windowTree3.get_widget('entExpTimeN')
            self.entExpTimeS = self.windowTree3.get_widget('entExpTimeS')
            self.entExpTimeE = self.windowTree3.get_widget('entExpTimeE')
            self.entExpTimeW = self.windowTree3.get_widget('entExpTimeW')
            lblMapUnitN = self.windowTree3.get_widget('lblMapUnitN')
            lblMapUnitS = self.windowTree3.get_widget('lblMapUnitS')
            lblMapUnitE = self.windowTree3.get_widget('lblMapUnitE')
            lblMapUnitW = self.windowTree3.get_widget('lblMapUnitW')
            butZoomExpStat = self.windowTree3.get_widget('butZoomExpStat')
            butZoomExpTime = self.windowTree3.get_widget('butZoomExpTime')
            butDefExpStat = self.windowTree3.get_widget('butDefExpStat')
            butDefExpTime = self.windowTree3.get_widget('butDefExpTime')
            butExpPtsLayer = self.windowTree3.get_widget('butExpPtsLayer')
            # Set default values
            lblAggUnit.set_text(self.tempUnit)
            lblExpMapUnit.set_text(self.tempUnit)
            lblExpTimeUnitFrom.set_text(self.tempUnit)
            lblExpTimeUnitTo.set_text(self.tempUnit)
            lblStatLocX.set_text(self.sptlUnit)
            lblStatLocY.set_text(self.sptlUnit)
            lblMapUnitN.set_text(self.sptlUnit)
            lblMapUnitS.set_text(self.sptlUnit)
            lblMapUnitE.set_text(self.sptlUnit)
            lblMapUnitW.set_text(self.sptlUnit)
            # Set non-aggregated data
            self.aggPeriod = 1.0
            self.flgAggData = 0
            self.entAggPeriod.set_sensitive(0)
            self.butAggData.set_sensitive(0)
            if self.flgStdAlone:
                butExpPtsLayer.set_sensitive(0)

            # Define canvas object for exploratory ID plot
            self.areaExpId = self.windowTree3.get_widget("vboxExpId")
            [self.figExpId,self.plotExpId] = createFigObj()
            [self.cavExpId,self.tbExpId] = \
                           createCavObj(self.figExpId,self.box3)
            # Define canvas object for exploratory station location
            self.areaStatLoc = self.windowTree3.get_widget("vboxStatLoc")
            [self.figStatLoc,self.plotStatLoc] = createFigObj()
            [self.cavStatLoc,self.tbStatLoc] = \
                           createCavObj(self.figStatLoc,self.box3)
            # Define canvas object for exploratory Time plot
            self.areaExpTime = self.windowTree3.get_widget("hboxExpTime")
            [self.figExpTime,self.plotExpTime] = createFigObj()
            [self.cavExpTime,self.tbExpTime] = \
                           createCavObj(self.figExpTime,self.box3)
            # Map event handler
            self.box3.connect('destroy',self.justQuit)
            self.butQuit3.connect('clicked',self.askQuit)
            self.butNext3.connect('clicked',self.goBox4)
            self.cmbExpId.connect('changed',self.replotId)
            self.cmbExpTime.connect('changed',self.replotTime)
            self.butExpTimeNext.connect('clicked',self.nextTimePlot)
            self.butExpTimeBack.connect('clicked',self.backTimePlot)
            self.butExpStatNext.connect('clicked',self.nextStatPlot)
            self.butExpStatBack.connect('clicked',self.backStatPlot)
            self.entExpSysId.connect('changed',self.chgStatPlot)
            self.chkAggData.connect('toggled',self.activateAggField)
            butZoomExpStat.connect('clicked',self.zoomExpStat)
            butDefExpStat.connect('clicked',self.defaultExpStat)
            butZoomExpTime.connect('clicked',self.zoomExpTime)
            butDefExpTime.connect('clicked',self.defaultExpTime)
            butExpPtsLayer.connect('clicked',self.createExpPtsLayer)
            self.butAggData.connect('clicked',self.aggData)
            self.butBack3.connect('clicked',self.backScreen)

            # Get parameters from param file
            try:
                self.objAveHardBkup = self.fileParam['objAveHardBkup']
                self.aggPeriod = self.fileParam['aggPeriod']
                self.flgAggData = self.fileParam['flgAggData']
                self.objAveHard = self.fileParam['objAveHard3']
            except:
                self.flgNewSession = True
            # If parameters exist, set values
            if not self.flgNewSession:
                if self.flgAggData == 1:
                    self.chkAggData.set_active(1)
                    self.entAggPeriod.set_sensitive(0)
                    self.entAggPeriod.set_text(str(self.aggPeriod))
                    self.butAggData.set_sensitive(0)
                    self.butAggData.set_label('Data Aggregated')
            else:
                # Buckup self.objAveHard
                self.objAveHardBkup = copy.deepcopy(self.objAveHard)
            # Set combo box
            self.setExpCmbBox()
            # Plot exploratory ID plot
            self.createExpIdPlot()
            self.createStatLocPlot()
            self.createExpTimePlot()
            # Check button status
            self.chkNextStat()
            self.chkBackStat()
            self.chkNextTime()
            self.chkBackTime()
            # Change cursor to None
            self.lowWindow3.set_cursor(None)

        except DummyError:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def setExpCmbBox(self):
        # Create ID list for combo box
        self.lsExpId = gtk.ListStore(str)
        for idx,item in enumerate(self.uniqueId):
            strItem = item + '(' + str(self.uniqueSysId[idx]) + ')'
            self.lsExpId.append([strItem])
        numItemId = self.lsExpId.get_iter_first()
        # Set ID combo box
        self.cmbExpId.set_model(self.lsExpId)
        self.cell1 = gtk.CellRendererText()
        self.cmbExpId.pack_start(self.cell1,True)
        self.cmbExpId.set_active_iter(numItemId)
        # Create time event list for combo box
        self.lsExpTime = gtk.ListStore(str)
        for item in unique(self.objAveHard.getT()).tolist():
            self.lsExpTime.append([str(item)])
        numItemId = self.lsExpTime.get_iter_first()
        # Set time combo box
        self.cmbExpTime.set_model(self.lsExpTime)
        self.cell2 = gtk.CellRendererText()
        self.cmbExpTime.pack_start(self.cell2,True)
        self.cmbExpTime.set_active_iter(numItemId)
        # Get system Id from list
        self.entExpSysId.set_text\
             (str(self.uniqueSysId[self.cmbExpId.get_active()]))

    def replotId(self,widget):
        # Check button status
        self.chkNextStat()
        self.chkBackStat()
        # Create new plot
        self.createExpIdPlot()
        self.createStatLocPlot()
        # Set system Id from list
        self.entExpSysId.set_text\
             (str(self.uniqueSysId[self.cmbExpId.get_active()]))

    def chkNextStat(self):
        if self.cmbExpId.get_active() == len(self.uniqueId)-1:
            self.butExpStatNext.set_sensitive(0)
        else:
            self.butExpStatNext.set_sensitive(1)

    def chkBackStat(self):
        if self.cmbExpId.get_active() == 0:
            self.butExpStatBack.set_sensitive(0)
        else:
            self.butExpStatBack.set_sensitive(1)

    def nextStatPlot(self,widget):
        self.cmbExpId.set_active(self.cmbExpId.get_active()+1)

    def backStatPlot(self,widget):
        self.cmbExpId.set_active(self.cmbExpId.get_active()-1)

    def chgStatPlot(self,widget):
        try:
            # Get system ID
            if self.entExpSysId.get_text() == "":
                raise DummyError
            try:
                sysId = int(self.entExpSysId.get_text())
            except:
                raise TgisWarning(errMsg0301)
            if (sysId < 0) or (sysId > len(self.uniqueId)):
                raise TgisWarning(errMsg0302)
            # Set combobox item
            self.cmbExpId.set_active(sysId-1)
            # Check button status
            self.chkNextStat()
            self.chkBackStat()

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except DummyError:
            pass

    def replotTime(self,widget):
        # Check button status
        self.chkNextTime()
        self.chkBackTime()
        # Create new plot
        self.createExpTimePlot()

    def chkNextTime(self):
        if self.cmbExpTime.get_active() == \
           len(unique(self.objAveHard.getT()).tolist())-1:
            self.butExpTimeNext.set_sensitive(0)
        else:
            self.butExpTimeNext.set_sensitive(1)

    def chkBackTime(self):
        if self.cmbExpTime.get_active() == 0:
            self.butExpTimeBack.set_sensitive(0)
        else:
            self.butExpTimeBack.set_sensitive(1)

    def nextTimePlot(self,widget):
        self.cmbExpTime.set_active(self.cmbExpTime.get_active()+1)

    def backTimePlot(self,widget):
        self.cmbExpTime.set_active(self.cmbExpTime.get_active()-1)

    def createExpIdPlot(self):
        """Create exploratory ID plot"""
        # Plot time series
        curSysId = self.uniqueSysId[self.cmbExpId.get_active()]
        idxSysId = find(self.objAveHardBkup.getSysIdAsAry().flatten()==curSysId).\
                   tolist()
        expTime = take(self.objAveHardBkup.getTAsAry(),idxSysId)
        expType = take(self.objAveHardBkup.getVal1AsAry(),idxSysId)
        if self.flgLogTran:
            expVal = take(self.objAveHardBkup.getVal3AsAry(),idxSysId)
        else:
            expVal = take(self.objAveHardBkup.getVal2AsAry(),idxSysId)
        idxSoftId = find(expType.flatten()>0.0).tolist()
        expSoftTime = take(expTime,idxSoftId)
        expSoftVal = take(expVal,idxSoftId)
        idxHardId = find(expType.flatten()==0.0).tolist()
        expHardTime = take(expTime,idxHardId)
        expHardVal = take(expVal,idxHardId)
        aryExp = array([expTime,expVal]).transpose()
        aryExp = aryExp[argsort(aryExp[:,0])]
        expTime = aryExp[:,0]
        expVal = aryExp[:,1]
        # Define canvas object for exploratory ID plot
        [self.figExpId,self.plotExpId] = createFigObj()
        # Plot raw histogram
        xlabelString = strExpIdX + "(" + self.tempUnit + ")"
        if self.flgLogTran:
            ylabelString = self.agentName + "(log-" + self.dataUnit + ")"
        else:
            ylabelString = self.agentName + "(" + self.dataUnit + ")"
        titleString = strExpIdTitle + self.uniqueId[self.cmbExpId.get_active()]
        setPlotLabels(self.plotExpId,xlabelString,\
                      ylabelString,titleString,True)
        self.plotExpId.plot(expTime,expVal,'-b')
        self.plotExpId.hold(True)
        self.plotExpId.plot(expHardTime,expHardVal,'bo',markersize=10)
        self.plotExpId.plot(expSoftTime,expSoftVal,'r^',markersize=10)
        # Create xlim and ylim
        self.defXlimExpId = calcGraphBuff(min(self.objAveHardBkup.getT()),\
                                          max(self.objAveHardBkup.getT()))
        self.defYlimExpId = calcGraphBuff(min(expVal),max(expVal))
        # Plot quality standard
        if self.userConst['flgQstdPlot']:
            if self.flgLogTran:
                self.plotExpId.plot(self.defXlimExpId,\
                                    [log(self.userConst['qstdVal']),\
                                     log(self.userConst['qstdVal'])],'-k')
                if log(self.userConst['qstdVal']) <= self.defYlimExpId[0]:
                    self.defYlimExpId = \
                      calcGraphBuff(log(self.userConst['qstdVal']),max(expVal))
                if log(self.userConst['qstdVal']) >= self.defYlimExpId[1]:
                    self.defYlimExpId = \
                      calcGraphBuff(min(expVal),log(self.userConst['qstdVal']))
            else:
                self.plotExpId.plot(self.defXlimExpId,\
                                    [self.userConst['qstdVal'],\
                                     self.userConst['qstdVal']],'-k')
                if self.userConst['qstdVal'] <= self.defYlimExpId[0]:
                    self.defYlimExpId = \
                      calcGraphBuff(self.userConst['qstdVal'],max(expVal))
                if self.userConst['qstdVal'] >= self.defYlimExpId[1]:
                    self.defYlimExpId = \
                      calcGraphBuff(min(expVal),self.userConst['qstdVal'])
        # Set xlim and ylim
        self.plotExpId.set_xlim(self.defXlimExpId)
        self.plotExpId.set_ylim(self.defYlimExpId)
          
        # Create plot
        [self.cavExpId,self.tbExpId] = \
           plotGraph(self.figExpId,self.plotExpId,\
                     self.cavExpId,self.tbExpId,\
                     self.areaExpId,self.box3)

    def createStatLocPlot(self):
        """Create map of the station"""
        # Plot map of the station
        curSysId = self.uniqueSysId[self.cmbExpId.get_active()]
        idxSysId = self.objAveHard.getSysId().index(curSysId)
        expCoord = self.objAveHard.getSptlAsAry()
        expSysLoc = expCoord[idxSysId,:][newaxis,:]
        # Define canvas object for exploratory ID plot
        [self.figStatLoc,self.plotStatLoc] = createFigObj()
        # Plot raw histogram
        xlabelString = self.sptlUnit
        ylabelString = self.sptlUnit
        titleString = strStatLocTitle
        setPlotLabels(self.plotStatLoc,xlabelString,\
                      ylabelString,titleString,True)
        self.plotStatLoc.plot(expCoord[:,0],expCoord[:,1],'kx',markersize=10)
        self.plotStatLoc.hold(True)
        self.plotStatLoc.plot(expSysLoc[:,0],expSysLoc[:,1],'ro',markersize=15)
        # Create xlim and ylim
        [minCoordX,maxCoordX] = calcGraphBuff(min(expCoord[:,0]),\
                                              max(expCoord[:,0]))
        [minCoordY,maxCoordY] = calcGraphBuff(min(expCoord[:,1]),\
                                              max(expCoord[:,1]))
        # Set xlim and ylim
        self.plotStatLoc.set_xlim([minCoordX,maxCoordX])
        self.plotStatLoc.set_ylim([minCoordY,maxCoordY])
        self.plotStatLoc.hold(False)
        strCom = "%." + str(numSptlDeci) + "f"
        self.entStatLocX.set_text(str(eval(strCom %expSysLoc[:,0])))
        self.entStatLocY.set_text(str(eval(strCom %expSysLoc[:,1])))
        [self.cavStatLoc,self.tbStatLoc] = \
           plotGraph(self.figStatLoc,self.plotStatLoc,\
                     self.cavStatLoc,self.tbStatLoc,\
                     self.areaStatLoc,self.box3)

    def createExpTimePlot(self):
        """Create exploratory ID plot"""
        # Set color map
        setColorMap(self.userConst['strColorMap'])
        # Get data
        curTime = unique(self.objAveHard.getT()).\
                  tolist()[self.cmbExpTime.get_active()]
        idxTime = find(self.objAveHard.getTAsAry().flatten()==curTime).tolist()
        expX = take(self.objAveHard.getXAsAry(),idxTime)
        expY = take(self.objAveHard.getYAsAry(),idxTime)
        if self.flgLogTran:
            expVal = take(self.objAveHard.getVal3AsAry(),idxTime)
        else:
            expVal = take(self.objAveHard.getVal2AsAry(),idxTime)
        if self.flgLogTran:
            allVal = self.objAveHard.getVal3()
        else:
            allVal = self.objAveHard.getVal2()
        expCoord = self.objAveHard.getSptlAsAry()
        self.defXlimExpTime = calcGraphBuff(min(expCoord[:,0]),\
                                            max(expCoord[:,0]))
        self.defYlimExpTime = calcGraphBuff(min(expCoord[:,1]),\
                                            max(expCoord[:,1]))
        if unique(allVal).shape[0] == 1:
            if float(allVal[0])==0.0:
                minVal = -1.0
                maxVal = 1.0
            else:
                minVal = float(allVal[0]) - float(abs(allVal[0])/5)
                maxVal = float(allVal[0]) + float(abs(allVal[0])/5)
        else:
            minVal = float(min(allVal))
            maxVal = float(max(allVal))
        # Define canvas object for exploratory ID plot
        [self.figExpTime,self.plotExpTime] = createFigObj()
        # Plot raw histogram
        xlabelString = self.sptlUnit
        ylabelString = self.sptlUnit
        titleString = strExpTimeTitle + \
                      str(curTime) + " " + self.tempUnit
        if self.flgLogTran:
            titleString = titleString + " (log-" + self.dataUnit + ")"
        else:
            titleString = titleString + " (" + self.dataUnit + ")"
        setPlotLabels(self.plotExpTime,xlabelString,\
                      ylabelString,titleString,True)
        scExpTimePlot = self.plotExpTime.scatter(expX,expY,100,expVal,\
                                                 vmin=minVal, vmax=maxVal )
        self.figExpTime.colorbar(mappable = scExpTimePlot)
        # Set xlim and ylim
        self.plotExpTime.set_xlim(self.defXlimExpTime)
        self.plotExpTime.set_ylim(self.defYlimExpTime)
        [self.cavExpTime,self.tbExpTime] = \
           plotGraph(self.figExpTime,self.plotExpTime,\
                     self.cavExpTime,self.tbExpTime,\
                     self.areaExpTime,self.box3)

    def zoomExpStat(self,widget):
        """Zoom in/out time series at station"""
        try:
            # Check input parameters
            try:
                xlimfrom = float(self.entExpStatFrom.get_text())
                xlimto = float(self.entExpStatTo.get_text())
            except:
                raise TgisWarning(errMsg0303)
            if xlimfrom >= xlimto:
                raise TgisWarning(errMsg0304)
            # Create new time series at station
            self.plotExpId.set_xlim([xlimfrom,xlimto])
            [self.cavExpId,self.tbExpId] = \
               plotGraph(self.figExpId,self.plotExpId,\
                         self.cavExpId,self.tbExpId,\
                         self.areaExpId,self.box3)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def defaultExpStat(self,widget):
        # Set xlim and ylim
        self.plotExpId.set_xlim(self.defXlimExpId)
        self.plotExpId.set_ylim(self.defYlimExpId)
        # Create plot
        [self.cavExpId,self.tbExpId] = \
           plotGraph(self.figExpId,self.plotExpId,\
                     self.cavExpId,self.tbExpId,\
                     self.areaExpId,self.box3)

    def zoomExpTime(self,widget):
        """Zoom in/out map of the data"""
        try:
            # Check input parameters
            try:
                xlimWest = float(self.entExpTimeW.get_text())
                xlimEast = float(self.entExpTimeE.get_text())
                ylimSouth = float(self.entExpTimeS.get_text())
                ylimNorth = float(self.entExpTimeN.get_text())
            except:
                raise TgisWarning(errMsg0305)
            if xlimWest >= xlimEast:
                raise TgisWarning(errMsg0306)
            if ylimSouth >= ylimNorth:
                raise TgisWarning(errMsg0306)
            # Set xlim and ylim
            self.plotExpTime.set_xlim([xlimWest,xlimEast])
            self.plotExpTime.set_ylim([ylimSouth,ylimNorth])
            [self.cavExpTime,self.tbExpTime] = \
               plotGraph(self.figExpTime,self.plotExpTime,\
                         self.cavExpTime,self.tbExpTime,\
                         self.areaExpTime,self.box3)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def defaultExpTime(self,widget):
        self.plotExpTime.set_xlim(self.defXlimExpTime)
        self.plotExpTime.set_ylim(self.defYlimExpTime)
        [self.cavExpTime,self.tbExpTime] = \
           plotGraph(self.figExpTime,self.plotExpTime,\
                     self.cavExpTime,self.tbExpTime,\
                     self.areaExpTime,self.box3)

    def chkGPFile(self,strHeader,strExt):
        featureList = os.listdir(self.GP.workspace)
        numList = []
        for item in featureList:
            if (item[0:len(strHeader)] == strHeader) and \
               (item[-(len(strExt)):] == strExt):
                numList.append(item[len(strHeader):-(len(strExt))])
        if len(numList) == 0:
            defNum = 1
            retFileName = strHeader + "%.3d" %defNum + strExt
            return retFileName
        else:
            newList = []
            for item in numList:
                try:
                    newList.append(int(item))
                except:
                    pass
            if len(newList) == 0:
                defNum = 1
                retFileName = strHeader + "%.3d" %defNum + strExt
                return retFileName
            else:
                defNum = max(newList) + 1
                retFileName = strHeader + "%.3d" %defNum + strExt
                return retFileName

    def createExpPtsLayer(self,widget):
        """Display select method message"""
        try:
            # Change cursor to Watch
            self.lowWindow3.set_cursor(self.cursorWatch)
            # Get parameters
            strHeader = self.userConst['expPLHeader']
            dataCoord = self.userConst['dataCoord']
            projCoord = self.userConst['projCoord']
            flgDataCoord = self.userConst['flgDataCoord']
            flgProjCoord = self.userConst['flgProjCoord']
            # Get point layer file name
            strExt = ".lyr"
            lyrFileName = self.chkGPFile(strHeader,strExt)
            # Create output table
            lyrFileHeader = lyrFileName[0:-len(strExt)]
            lyrTableName = self.GP.workspace + "/" + lyrFileHeader
            self.GP.CreateTable(self.GP.workspace,lyrFileHeader)
            # Set data fields
            self.GP.AddField(lyrTableName,"X","double")
            self.GP.AddField(lyrTableName,"Y","double")
            self.GP.AddField(lyrTableName,"T","double")
            self.GP.AddField(lyrTableName,"Val","double")
            # Get data
            curTime = unique(self.objAveHard.getT()).\
                      tolist()[self.cmbExpTime.get_active()]
            idxTime = find(self.objAveHard.getTAsAry().flatten()==curTime).tolist()
            expX = take(self.objAveHard.getXAsAry(),idxTime)
            expY = take(self.objAveHard.getYAsAry(),idxTime)
            expT = ones(expX.shape)*curTime
            if self.flgLogTran:
                expVal = take(self.objAveHard.getVal3AsAry(),idxTime)
            else:
                expVal = take(self.objAveHard.getVal2AsAry(),idxTime)
            # Insert data values
            curTable = self.GP.InsertCursor(lyrTableName)
            for i in xrange(len(expX)):
                rowTable = curTable.NewRow()
                rowTable.X = expX[i]
                rowTable.Y = expY[i]
                rowTable.T = expT[i]
                rowTable.Val = expVal[i]
                curTable.InsertRow(rowTable)
            del curTable
            # Create and save layer file
            if flgDataCoord:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader,dataCoord)
            else:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader)
            self.GP.SaveToLayerFile(lyrFileHeader,lyrFileName)
            # Set projection
            if flgProjCoord:
                self.GP.project (lyrFileName, lyrFileHeader + "_proj",projCoord)
            strMsg = "Point layer file (" + lyrFileHeader + \
                     ") has been created"
            self.dispRegMsg(strMsg)
            self.addGPMessage(strMsg,0)
            # Change cursor to None
            self.lowWindow3.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def activateAggField(self,widget):
        """Activate data aggregation field"""
        try:
            # Set cursor to Watch
            self.lowWindow3.set_cursor(self.cursorWatch)
            if self.chkAggData.get_active():
                # Activate data aggregation field
                self.entAggPeriod.set_sensitive(1)
                self.butAggData.set_sensitive(1)
            else:
                # Deactivate data aggregation field
                self.entAggPeriod.set_sensitive(0)
                self.entAggPeriod.set_text('')
                self.butAggData.set_label('Aggregate Data')
                self.butAggData.set_sensitive(0)
                self.flgAggData = 0
                self.objAveHard = copy.deepcopy(self.objAveHardBkup)
                # Set combo box
                self.setExpCmbBox()
                # Plot exploratory ID plot
                self.createExpIdPlot()
                self.createStatLocPlot()
                self.createExpTimePlot()
            # Change cursor to None
            self.lowWindow3.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def aggData(self,widget):
        """Aggregate data"""
        try:
            # Set cursor to Watch
            self.lowWindow3.set_cursor(self.cursorWatch)
            # Get Aggregation period
            try:
                self.aggPeriod = float(self.entAggPeriod.get_text())
            except:
                raise TgisWarning(errMsg0308)
            if self.aggPeriod <= 0.0:
                raise TgisWarning(errMsg0309)
            self.objAveHard = aggregateData(self.objAveHard,\
                                            self.aggPeriod)
            aveSysId = self.objAveHard.getSysId()
            aveId = []
            for idx,item in enumerate(aveSysId):
                 aveId.append(self.uniqueId[self.uniqueSysId.index(item)])
            self.objAveHard.setId(aveId)
            # Set combo box
            self.setExpCmbBox()
            # Plot exploratory ID plot
            self.createExpIdPlot()
            self.createStatLocPlot()
            self.createExpTimePlot()
            # Change button
            self.butAggData.set_sensitive(0)
            self.butAggData.set_label('Data Aggregated')
            self.entAggPeriod.set_sensitive(0)
            self.flgAggData = 1
            # Set cursor to None
            self.lowWindow3.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def goBox4(self,widget):
        """Move to box4"""
        try:
            # Change cursor to Watch
            self.lowWindow3.set_cursor(self.cursorWatch)
            # If new session, create data objects
            if not self.flgNewSession:
                # Get value in param file
                aggPeriodOld = self.fileParam['aggPeriod']
                flgAggDataOld = self.fileParam['flgAggData']
                # Compare values
                if (self.flgAggData != flgAggDataOld) or \
                   (self.aggPeriod != aggPeriodOld):
                    self.flgNewSession = True
                if self.flgNewSession:
                    msgQuestion = 'Parameter has changed.'
                    msgOpt1 = 'Delete all parameters and estimation files ' + \
                              'and continue using new parameters'
                    msgOpt2 = 'Quit Application and use different working space'
                    flgParamChg = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    if flgParamChg:
                        self.delEstFiles(self.paramFileHeader,extResStatFile)
                        self.delEstFiles(self.paramFileHeader,extResMapFile)
                    else:
                        raise DummyError
            # If new session, create data objects
            if self.flgNewSession:
                # Store parameter
                self.fileParam['objAveHardBkup'] = self.objAveHardBkup
                self.fileParam['objAveHard3'] = self.objAveHard
                self.fileParam['flgAggData'] = self.flgAggData
                self.fileParam['aggPeriod'] = self.aggPeriod
            # Change cursor to None
            self.lowWindow3.set_cursor(None)
            # Hide box3
            self.box3.hide()

            # Set box4 object tree
            windowName = "box4"
            self.numCurScreen = 4
            self.windowTree4 = gtk.glade.XML(gladeFile,windowName)
            # Get window and low window objects
            self.box4 = self.windowTree4.get_widget(windowName)
            self.lowWindow4 = self.box4.window
            #Change cursor to Watch
            self.lowWindow4.set_cursor(self.cursorWatch)
            # Get widget objects
            self.radMeanTrnd1 = self.windowTree4.get_widget('radMeanTrnd1')
            self.radMeanTrnd2 = self.windowTree4.get_widget('radMeanTrnd2')
            self.entSptlRadi = self.windowTree4.get_widget('entSptlRadi')
            self.entSptlRnge = self.windowTree4.get_widget('entSptlRnge')
            self.entTempRadi = self.windowTree4.get_widget('entTempRadi')
            self.entTempRnge = self.windowTree4.get_widget('entTempRnge')
            lblSptlRadiUnit = self.windowTree4.get_widget('lblSptlRadiUnit')
            lblSptlRngeUnit = self.windowTree4.get_widget('lblSptlRngeUnit')
            lblTempRadiUnit = self.windowTree4.get_widget('lblTempRadiUnit')
            lblTempRngeUnit = self.windowTree4.get_widget('lblTempRngeUnit')
            self.frmMeanTrnd = self.windowTree4.get_widget('frmMeanTrnd')
            self.butQuit4 = self.windowTree4.get_widget('butQuit4')
            self.butNext4 = self.windowTree4.get_widget('butNext4')
            self.butBack4 = self.windowTree4.get_widget('butBack4')
            butCalcMean = self.windowTree4.get_widget('butCalcMean')
            butZoomTempMean = self.windowTree4.get_widget('butZoomTempMean')
            butZoomSptlRawMean = self.windowTree4.get_widget('butZoomSptlRawMean')
            butZoomSptlSmMean = self.windowTree4.get_widget('butZoomSptlSmMean')
            butSmLayer = self.windowTree4.get_widget('butSmLayer')
            butRawLayer = self.windowTree4.get_widget('butRawLayer')
            self.entTempMeanFrom = \
                self.windowTree4.get_widget('entTempMeanFrom')
            self.entTempMeanTo = self.windowTree4.get_widget('entTempMeanTo')
            self.entSptlRawMeanN = self.windowTree4.get_widget('entSptlRawMeanN')
            self.entSptlRawMeanS = self.windowTree4.get_widget('entSptlRawMeanS')
            self.entSptlRawMeanE = self.windowTree4.get_widget('entSptlRawMeanE')
            self.entSptlRawMeanW = self.windowTree4.get_widget('entSptlRawMeanW')
            self.entSptlSmMeanN = self.windowTree4.get_widget('entSptlSmMeanN')
            self.entSptlSmMeanS = self.windowTree4.get_widget('entSptlSmMeanS')
            self.entSptlSmMeanE = self.windowTree4.get_widget('entSptlSmMeanE')
            self.entSptlSmMeanW = self.windowTree4.get_widget('entSptlSmMeanW')
            lblSmUnitN = self.windowTree4.get_widget('lblSmUnitN')
            lblSmUnitS = self.windowTree4.get_widget('lblSmUnitS')
            lblSmUnitE = self.windowTree4.get_widget('lblSmUnitE')
            lblSmUnitW = self.windowTree4.get_widget('lblSmUnitW')
            lblRawUnitN = self.windowTree4.get_widget('lblRawUnitN')
            lblRawUnitS = self.windowTree4.get_widget('lblRawUnitS')
            lblRawUnitE = self.windowTree4.get_widget('lblRawUnitE')
            lblRawUnitW = self.windowTree4.get_widget('lblRawUnitW')
            lblTempMeanFrom = self.windowTree4.get_widget('lblTempMeanFrom')
            lblTempMeanTo = self.windowTree4.get_widget('lblTempMeanTo')
            butDefSptlRawMean = self.windowTree4.get_widget('butDefSptlRawMean')
            butDefSptlSmMean = self.windowTree4.get_widget('butDefSptlSmMean')
            butDefTempMean = self.windowTree4.get_widget('butDefTempMean')
            # Set default values
            lblSmUnitN.set_text(self.sptlUnit)
            lblSmUnitS.set_text(self.sptlUnit)
            lblSmUnitE.set_text(self.sptlUnit)
            lblSmUnitW.set_text(self.sptlUnit)
            lblRawUnitN.set_text(self.sptlUnit)
            lblRawUnitS.set_text(self.sptlUnit)
            lblRawUnitE.set_text(self.sptlUnit)
            lblRawUnitW.set_text(self.sptlUnit)
            lblTempMeanFrom.set_text(self.tempUnit)
            lblTempMeanTo.set_text(self.tempUnit)
            lblSptlRadiUnit.set_text(self.sptlUnit)
            lblSptlRngeUnit.set_text(self.sptlUnit)
            lblTempRadiUnit.set_text(self.tempUnit)
            lblTempRngeUnit.set_text(self.tempUnit)
            self.radMeanTrnd1.set_active(True)
            self.frmMeanTrnd.set_sensitive(0)
            # Set flag
            self.flgRemvMean = 0
            # Deactivate button
            if self.flgStdAlone:
                butSmLayer.set_sensitive(0)
                butRawLayer.set_sensitive(0)
            # Define canvas object for temporal mean plot
            self.areaTempMean = self.windowTree4.get_widget("hboxTempMean")
            [self.figTempMean,self.plotTempMean] = createFigObj()
            [self.cavTempMean,self.tbTempMean] = \
                           createCavObj(self.figTempMean,self.box4)
            # Define canvas object for spatial raw mean plot
            self.areaSptlRawMean = \
                            self.windowTree4.get_widget("hboxSptlRawMean")
            [self.figSptlRawMean,self.plotSptlRawMean] = createFigObj()
            [self.cavSptlRawMean,self.tbSptlRawMean] = \
                           createCavObj(self.figSptlRawMean,self.box4)
            # Define canvas object for spatial smoothed mean plot
            self.areaSptlSmMean = self.windowTree4.get_widget("hboxSptlSmMean")
            [self.figSptlSmMean,self.plotSptlSmMean] = createFigObj()
            [self.cavSptlSmMean,self.tbSptlSmMean] = \
                           createCavObj(self.figSptlSmMean,self.box4)
            # Check data
            if len(unique(self.objAveHard.getT()).tolist()) == 1:
                self.flgOneTempPt = True
            else:
                self.flgOneTempPt = False
            if len(unique(self.objAveHard.getX()).tolist()) == 1 and \
               len(unique(self.objAveHard.getY()).tolist()) == 1:
                self.flgOneSptlPt = True
            else:
                self.flgOneSptlPt = False
            if len(unique(self.objAveHard.getX()).tolist()) <= 2 and \
               len(unique(self.objAveHard.getY()).tolist()) <= 2:
                self.flgNoMean = True
            else:
                self.flgNoMean = False
            if self.flgOneTempPt or self.flgNoMean:
                self.radMeanTrnd1.set_sensitive(0)
                self.radMeanTrnd2.set_sensitive(0)
            # Get parameters from param file
            try:
                self.sptlRadi = self.fileParam['sptlRadi']
                self.sptlRnge = self.fileParam['sptlRnge']
                self.tempRadi = self.fileParam['tempRadi']
                self.tempRnge = self.fileParam['tempRnge']
                self.flgRemvMean = self.fileParam['flgRemvMean']
                self.sptlGridX = self.fileParam['sptlGridX']
                self.sptlGridY = self.fileParam['sptlGridY']
                self.sptlRawMean = self.fileParam['sptlRawMean']
                self.sptlSmMean = self.fileParam['sptlSmMean']
                self.tempGrid = self.fileParam['tempGrid']
                self.tempRawMean = self.fileParam['tempRawMean']
                self.tempSmMean = self.fileParam['tempSmMean']
                self.objStrfAgg = self.fileParam['objStrfAgg']
            except:
                self.flgNewSession = True
            # If parameters exist, set values
            if self.flgNewSession:
                # Calculate default parameters
                self.sptlRadi = sqrt((max(self.objAveHard.getX()) - \
                                      min(self.objAveHard.getX()))**2 + \
                                     (max(self.objAveHard.getY()) - \
                                      min(self.objAveHard.getY()))**2)/3.0
                self.sptlRnge = self.sptlRadi/3.0
                self.tempRadi = (max(self.objAveHard.getT()) - \
                                 min(self.objAveHard.getT()))/3.0
                self.tempRnge = self.tempRadi/3.0
            else:
                if self.flgRemvMean == 1:
                    self.frmMeanTrnd.set_sensitive(1)
                    self.radMeanTrnd2.set_active(True)
                    # Create figure objects
                    self.createTempMeanPlot()
                    self.createSptlMeanPlot()
            self.entSptlRadi.set_text(fpformat.fix(self.sptlRadi,numParamDeci))
            self.entTempRadi.set_text(fpformat.fix(self.tempRadi,numParamDeci))
            self.entSptlRnge.set_text(fpformat.fix(self.sptlRnge,numParamDeci))
            self.entTempRnge.set_text(fpformat.fix(self.tempRnge,numParamDeci))
            # Map event handler
            self.box4.connect('destroy',self.justQuit)
            self.butQuit4.connect('clicked',self.askQuit)
            self.radMeanTrnd1.connect('toggled',self.activateMeanFrm)
            butCalcMean.connect('clicked',self.recalcMean)
            butZoomTempMean.connect('clicked',self.zoomTempMean)
            butZoomSptlRawMean.connect('clicked',self.zoomSptlRawMean)
            butZoomSptlSmMean.connect('clicked',self.zoomSptlSmMean)
            butDefTempMean.connect('clicked',self.defaultTempMean)
            butDefSptlRawMean.connect('clicked',self.defaultSptlRawMean)
            butDefSptlSmMean.connect('clicked',self.defaultSptlSmMean)
            butRawLayer.connect('clicked',self.createSptlMeanLayer,True)
            butSmLayer.connect('clicked',self.createSptlMeanLayer,False)
            self.butBack4.connect('clicked',self.backScreen)
            self.butNext4.connect('clicked',self.goBox5)
            # Change cursor to None
            self.lowWindow4.set_cursor(None)

        except DummyError:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def activateMeanFrm(self,widget):
        """Activate (deactivate) mean Trend Analysis"""
        if self.radMeanTrnd1.get_active():
            # Set flag
            self.flgRemvMean = 0
            # Deactivate mean trend analysis
            self.frmMeanTrnd.set_sensitive(0)
        else:
            # Activate mean trend analysis
            self.frmMeanTrnd.set_sensitive(1)
            # Set flag
            self.flgRemvMean = 1
            # Plot temporal and spatial mean trend
            self.plotMeanTrnd()

    def recalcMean(self,widget):
        """Recalculate mean trend"""
        self.plotMeanTrnd()

    def plotMeanTrnd(self):
        """Plot temporal and spatial mean trend"""
        try:
            # Change cursor to Watch
            self.lowWindow4.set_cursor(self.cursorWatch)
            # Get parameters for mean trend calculation
            self.sptlRadi = self.entSptlRadi.get_text()
            self.tempRadi = self.entTempRadi.get_text()
            self.sptlRnge = self.entSptlRnge.get_text()
            self.tempRnge = self.entTempRnge.get_text()
            # Check input parameters
            if not self.sptlRadi or not self.tempRadi or\
               not self.sptlRnge or not self.tempRnge:
                raise TgisWarning(errMsg0401)
            try:
                self.sptlRadi = float(self.sptlRadi)
                self.tempRadi = float(self.tempRadi)
                self.sptlRnge = float(self.sptlRnge)
                self.tempRnge = float(self.tempRnge)
            except:
                raise TgisWarning(errMsg0402)
            if self.sptlRadi <= 0 or self.tempRadi <= 0 or \
               self.sptlRnge <= 0 or self.tempRnge <= 0:
                raise TgisWarning(errMsg0403)
            # Calculate mean trend
            [self.sptlGridX,self.sptlGridY,self.sptlRawMean,self.sptlSmMean,\
             self.tempGrid,self.tempRawMean,self.tempSmMean] = \
                calcMean(self.objAveHard,\
                         [self.sptlRadi,self.sptlRnge,\
                          self.tempRadi,self.tempRnge],\
                         self.flgLogTran)
            # Create figure objects
            self.createTempMeanPlot()
            self.createSptlMeanPlot()
            # Change cursor to None
            self.lowWindow4.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def createTempMeanPlot(self):
        """Create exploratory ID plot"""
        # Define canvas object for exploratory ID plot
        [self.figTempMean,self.plotTempMean] = createFigObj()
        # Plot raw histogram
        xlabelString = strTempMeanX + "(" + self.tempUnit + ")"
        if self.flgLogTran:
            ylabelString = self.agentName + "(log-" + self.dataUnit + ")"
        else:
            ylabelString = self.agentName + "(" + self.dataUnit + ")"
        titleString = strTempMeanTitle
        setPlotLabels(self.plotTempMean,xlabelString,\
                      ylabelString,titleString,True)
        self.plotTempMean.plot(self.tempGrid,self.tempRawMean,'--b')
        self.plotExpId.hold(True)
        self.plotTempMean.plot(self.tempGrid,self.tempSmMean,'-b')
        # Check number of data
        self.defYlimTempMean = calcGraphBuff(min(self.tempRawMean),\
                                             max(self.tempRawMean))
        maxTime = max(self.tempGrid)
        minTime = min(self.tempGrid)
        if maxTime == minTime:
            self.defXlimTempMean = calcGraphBuff(minTime,maxTime)
        else:
            self.defXlimTempMean = [minTime,maxTime]
        # Plot quality standard
        if self.userConst['flgQstdPlot']:
            if self.flgLogTran:
                self.plotTempMean.plot(self.defXlimTempMean,\
                                       [log(self.userConst['qstdVal']),\
                                        log(self.userConst['qstdVal'])],'-k')
                if log(self.userConst['qstdVal']) <= self.defYlimTempMean[0]:
                    self.defYlimTempMean = \
                      calcGraphBuff(log(self.userConst['qstdVal']),\
                                    max(self.tempRawMean))
                if log(self.userConst['qstdVal']) >= self.defYlimTempMean[1]:
                    self.defYlimTempMean = \
                      calcGraphBuff(min(self.tempRawMean),\
                                    log(self.userConst['qstdVal']))
            else:
                self.plotTempMean.plot(self.defXlimTempMean,\
                                       [self.userConst['qstdVal'],\
                                        self.userConst['qstdVal']],'-k')
                if self.userConst['qstdVal'] <= self.defYlimTempMean[0]:
                    self.defYlimTempMean = \
                      calcGraphBuff(self.userConst['qstdVal'],\
                                    max(self.tempRawMean))
                if self.userConst['qstdVal'] >= self.defYlimTempMean[1]:
                    self.defYlimTempMean = \
                      calcGraphBuff(min(self.tempRawMean),\
                                    self.userConst['qstdVal'])
        # Set xlim and ylim
        self.plotTempMean.set_xlim(self.defXlimTempMean)
        self.plotTempMean.set_ylim(self.defYlimTempMean)
        # Create plot
        [self.cavTempMean,self.tbTempMean] = \
           plotGraph(self.figTempMean,self.plotTempMean,\
                     self.cavTempMean,self.tbTempMean,\
                     self.areaTempMean,self.box4)

    def createSptlMeanPlot(self):
        """Create exploratory ID plot"""
        # Use hot as color map
        setColorMap(self.userConst['strColorMap'])
        # Set boundary
        self.defXlimSptlMean = calcGraphBuff(min(self.sptlGridX),\
                                             max(self.sptlGridX))
        self.defYlimSptlMean = calcGraphBuff(min(self.sptlGridY),\
                                             max(self.sptlGridY))
        if unique(self.sptlRawMean).shape[0] == 1:
            if float(self.sptlRawMean[0])==0.0:
                minValRaw = -1.0
                maxValRaw = 1.0
            else:
                minValRaw = float(self.sptlRawMean[0]) - \
                            float(abs(self.sptlRawMean[0])/5)
                maxValRaw = float(self.sptlRawMean[0]) + \
                            float(abs(self.sptlRawMean[0])/5)
        else:
            minValRaw = float(min(self.sptlRawMean))
            maxValRaw = float(max(self.sptlRawMean))
        if unique(self.sptlSmMean).shape[0] == 1:
            if float(self.sptlSmMean[0])==0.0:
                minValSm = -1.0
                maxValSm = 1.0
            else:
                minValSm = float(self.sptlSmMean[0]) - \
                           float(abs(self.sptlSmMean[0])/5)
                maxValSm = float(self.smSptlMean[0]) + \
                           float(abs(self.sptlSmMean[0])/5)
        else:
            minValSm = float(min(self.sptlSmMean))
            maxValSm = float(max(self.sptlSmMean))
        # Define canvas object for exploratory ID plot
        [self.figSptlRawMean,self.plotSptlRawMean] = createFigObj()
        [self.figSptlSmMean,self.plotSptlSmMean] = createFigObj()
        # Plot raw histogram
        xlabelString = self.sptlUnit
        ylabelString = self.sptlUnit
        titleStringRaw = strSptlRawTitle
        titleStringSm = strSptlSmTitle
        if self.flgLogTran:
            titleStringRaw = titleStringRaw + "(log-" + self.dataUnit + ")"
            titleStringSm = titleStringSm + "(log-" + self.dataUnit + ")"
        else:
            titleStringRaw = titleStringRaw + "(" + self.dataUnit + ")"
            titleStringSm = titleStringSm + "(" + self.dataUnit + ")"
        setPlotLabels(self.plotSptlRawMean,xlabelString,\
                      ylabelString,titleStringRaw,True)
        setPlotLabels(self.plotSptlSmMean,xlabelString,\
                      ylabelString,titleStringSm,True)
        expX = array(self.sptlGridX)
        expY = array(self.sptlGridY)
        expRaw = array(self.sptlRawMean)
        expSm = array(self.sptlSmMean)
        scSptlRawMeanPlot = self.plotSptlRawMean.scatter(expX,expY,100,expRaw,\
                                                         vmin=minValRaw,\
                                                         vmax=maxValRaw)
        scSptlSmMeanPlot = self.plotSptlSmMean.scatter(expX,expY,100,expSm,\
                                                       vmin=minValSm,\
                                                       vmax=maxValSm)
        self.figSptlRawMean.colorbar(mappable = scSptlRawMeanPlot)
        self.figSptlSmMean.colorbar(mappable = scSptlSmMeanPlot)
        # Set xlim and ylim
        self.plotSptlRawMean.set_xlim(self.defXlimSptlMean)
        self.plotSptlRawMean.set_ylim(self.defYlimSptlMean)
        self.plotSptlSmMean.set_xlim(self.defXlimSptlMean)
        self.plotSptlSmMean.set_ylim(self.defYlimSptlMean)
        [self.cavSptlRawMean,self.tbSptlRawMean] = \
           plotGraph(self.figSptlRawMean,self.plotSptlRawMean,\
                     self.cavSptlRawMean,self.tbSptlRawMean,\
                     self.areaSptlRawMean,self.box4)
        [self.cavSptlSmMean,self.tbSptlSmMean] = \
           plotGraph(self.figSptlSmMean,self.plotSptlSmMean,\
                     self.cavSptlSmMean,self.tbSptlSmMean,\
                     self.areaSptlSmMean,self.box4)

    def zoomTempMean(self,widget):
        """Zoom in/out time series at station"""
        try:
            # Check input parameters
            try:
                xlimfrom = float(self.entTempMeanFrom.get_text())
                xlimto = float(self.entTempMeanTo.get_text())
            except:
                raise TgisWarning(errMsg0404)
            if xlimfrom >= xlimto:
                raise TgisWarning(errMsg0405)
            # Create new time series at station
            self.plotTempMean.set_xlim([xlimfrom,xlimto])
            [self.cavTempMean,self.tbTempMean] = \
               plotGraph(self.figTempMean,self.plotTempMean,\
                         self.cavTempMean,self.tbTempMean,\
                         self.areaTempMean,self.box4)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def defaultTempMean(self,widget):
        # Set xlim and ylim
        self.plotTempMean.set_xlim(self.defXlimTempMean)
        self.plotTempMean.set_ylim(self.defYlimTempMean)
        [self.cavTempMean,self.tbTempMean] = \
           plotGraph(self.figTempMean,self.plotTempMean,\
                     self.cavTempMean,self.tbTempMean,\
                     self.areaTempMean,self.box4)

    def zoomSptlRawMean(self,widget):
        """Zoom in/out map of the data"""
        try:
            # Check input parameters
            try:
                xlimWest = float(self.entSptlRawMeanW.get_text())
                xlimEast = float(self.entSptlRawMeanE.get_text())
                ylimSouth = float(self.entSptlRawMeanS.get_text())
                ylimNorth = float(self.entSptlRawMeanN.get_text())
            except:
                raise TgisWarning(errMsg0406)
            if xlimWest >= xlimEast:
                raise TgisWarning(errMsg0407)
            if ylimSouth >= ylimNorth:
                raise TgisWarning(errMsg0408)
            # Set xlim and ylim
            self.plotSptlRawMean.set_xlim([xlimWest,xlimEast])
            self.plotSptlRawMean.set_ylim([ylimSouth,ylimNorth])
            [self.cavSptlRawMean,self.tbSptlRawMean] = \
               plotGraph(self.figSptlRawMean,self.plotSptlRawMean,\
                         self.cavSptlRawMean,self.tbSptlRawMean,\
                         self.areaSptlRawMean,self.box4)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def defaultSptlRawMean(self,widget):
        self.plotSptlRawMean.set_xlim(self.defXlimSptlMean)
        self.plotSptlRawMean.set_ylim(self.defYlimSptlMean)
        [self.cavSptlRawMean,self.tbSptlRawMean] = \
           plotGraph(self.figSptlRawMean,self.plotSptlRawMean,\
                     self.cavSptlRawMean,self.tbSptlRawMean,\
                     self.areaSptlRawMean,self.box4)

    def zoomSptlSmMean(self,widget):
        """Zoom in/out map of the data"""
        try:
            # Check input parameters
            try:
                xlimWest = float(self.entSptlSmMeanW.get_text())
                xlimEast = float(self.entSptlSmMeanE.get_text())
                ylimSouth = float(self.entSptlSmMeanS.get_text())
                ylimNorth = float(self.entSptlSmMeanN.get_text())
            except:
                raise TgisWarning(errMsg0406)
            if xlimWest >= xlimEast:
                raise TgisWarning(errMsg0407)
            if ylimSouth >= ylimNorth:
                raise TgisWarning(errMsg0408)
            # Set xlim and ylim
            self.plotSptlSmMean.set_xlim([xlimWest,xlimEast])
            self.plotSptlSmMean.set_ylim([ylimSouth,ylimNorth])
            [self.cavSptlSmMean,self.tbSptlSmMean] = \
               plotGraph(self.figSptlSmMean,self.plotSptlSmMean,\
                         self.cavSptlSmMean,self.tbSptlSmMean,\
                         self.areaSptlSmMean,self.box4)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def defaultSptlSmMean(self,widget):
        self.plotSptlSmMean.set_xlim(self.defXlimSptlMean)
        self.plotSptlSmMean.set_ylim(self.defYlimSptlMean)
        [self.cavSptlSmMean,self.tbSptlSmMean] = \
           plotGraph(self.figSptlSmMean,self.plotSptlSmMean,\
                     self.cavSptlSmMean,self.tbSptlSmMean,\
                     self.areaSptlSmMean,self.box4)

    def createSptlMeanLayer(self,widget,flgSptlRawMean):
        """Display select method message"""
        try:
            # Change cursor to Watch
            self.lowWindow4.set_cursor(self.cursorWatch)
            # Get parameters
            if flgSptlRawMean:
                strHeader = self.userConst['sptlRawMeanHeader']
            else:
                strHeader = self.userConst['sptlSmMeanHeader']
            dataCoord = self.userConst['dataCoord']
            projCoord = self.userConst['projCoord']
            flgDataCoord = self.userConst['flgDataCoord']
            flgProjCoord = self.userConst['flgProjCoord']
            # Get point layer file name
            strExt = ".lyr"
            lyrFileName = self.chkGPFile(strHeader,strExt)
            # Create output table
            lyrFileHeader = lyrFileName[0:-len(strExt)]
            lyrTableName = self.GP.workspace + "/" + lyrFileHeader
            self.GP.CreateTable(self.GP.workspace,lyrFileHeader)
            # Set data fields
            self.GP.AddField(lyrTableName,"X","double")
            self.GP.AddField(lyrTableName,"Y","double")
            self.GP.AddField(lyrTableName,"Val","double")
            # Insert data values
            curTable = self.GP.InsertCursor(lyrTableName)
            for i in xrange(len(self.sptlGridX)):
                rowTable = curTable.NewRow()
                rowTable.X = self.sptlGridX[i]
                rowTable.Y = self.sptlGridY[i]
                if flgSptlRawMean:
                    rowTable.Val = self.sptlRawMean[i]
                else:
                    rowTable.Val = self.sptlSmMean[i]
                curTable.InsertRow(rowTable)
            del curTable
            # Create and save layer file
            if flgDataCoord:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader,dataCoord)
            else:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader)
            self.GP.SaveToLayerFile(lyrFileHeader,lyrFileName)
            # Set projection
            if flgProjCoord:
                self.GP.project (lyrFileName, lyrFileHeader + "_proj",projCoord)
            strMsg = "Point layer file (" + lyrFileHeader + \
                     ") has been created"
            self.dispRegMsg(strMsg)
            self.addGPMessage(strMsg,0)
            # Change cursor to None
            self.lowWindow4.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def goBox5(self,widget):
        """Move to box5"""
        # Update the value of the progress bar so that we get
        # some movement
        def progress_timeout(pbobj):
            new_val = pbobj.pbar.get_fraction() + 0.01
            #print new_val
            if new_val > 1.0:
                new_val = 0.0
                # Set the new value
            pbobj.pbar.set_fraction(new_val)

            # As this is a timeout function, return TRUE so that it
            # continues to get called
            return True

        class ProgressBar:
            ###def __init__(self):

            def destroy_progress(self, widget, data=None):
                gobject.source_remove(self.timer)
                self.timer = 0
                gtk.main_quit()
            # Callback that toggles the activity mode of the progress
            # bar
            def toggle_activity_mode(self, widget, data=None):
                if widget.get_active():
                    self.pbar.pulse()
                else:
                    self.pbar.set_fraction(0.0)
                
            def __init__(self):
                self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
                self.window.set_resizable(False)

                self.window.connect("destroy", self.destroy_progress)
                self.window.set_title("BMEGUI")
                self.window.set_border_width(5)

                vbox = gtk.VBox(False, 5)
                vbox.set_border_width(10)
                self.window.add(vbox)
                vbox.show()
          
                # Create a centering alignment object
                align = gtk.Alignment(0.5, 0.5, 0, 0)
                vbox.pack_start(align, False, False, 5)
                align.show()

                # Create the ProgressBar
                self.pbar = gtk.ProgressBar()

                align.add(self.pbar)
                self.pbar.set_text("                   Calculating Covariance                      ")
                self.pbar.show()

                # Add a timer callback to update the value of the progress bar
                self.timer = gobject.timeout_add (1, progress_timeout, self)

                separator = gtk.HSeparator()
                vbox.pack_start(separator, False, False, 0)
                separator.show()

                # rows, columns, homogeneous
                table = gtk.Table(2, 2, False)
                vbox.pack_start(table, False, True, 0)
                table.show()

                # Add a button to exit the program
                button = gtk.Button("close")
                button.connect("clicked", self.destroy_progress)
                vbox.pack_start(button, False, False, 0)


                self.window.show()
                
        def main():
            gtk.main()
            return 0

        if __name__ == "__main__":
            ProgressBar()
            main()
        try:
            # Change cursor to Watch
            self.lowWindow4.set_cursor(self.cursorWatch)
            # If new session, create data objects
            if not self.flgNewSession:
                # Get value in param file
                sptlRadiOld = self.fileParam['sptlRadi']
                sptlRngeOld = self.fileParam['sptlRnge']
                tempRadiOld = self.fileParam['tempRadi']
                tempRngeOld = self.fileParam['tempRnge']
                flgRemvMeanOld = self.fileParam['flgRemvMean']
                # Compare values
                if (self.flgRemvMean != flgRemvMeanOld) or \
                   (self.sptlRadi != sptlRadiOld) or \
                   (self.sptlRnge != sptlRngeOld) or \
                   (self.tempRadi != tempRadiOld) or \
                   (self.tempRnge != tempRngeOld):
                    self.flgNewSession = True
                if self.flgNewSession:
                    msgQuestion = 'Parameter has changed.'
                    msgOpt1 = 'Delete all parameters and estimation files ' + \
                              'and continue using new parameters'
                    msgOpt2 = 'Quit Application and use different working space'
                    flgParamChg = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    if flgParamChg:
                        self.delEstFiles(self.paramFileHeader,extResStatFile)
                        self.delEstFiles(self.paramFileHeader,extResMapFile)
                    else:
                        raise DummyError
            # If new session, create data objects
            if self.flgNewSession:
                self.objStrfAgg = GeoData()
                self.objStrfAgg.setX(self.objAveHard.getX())
                self.objStrfAgg.setY(self.objAveHard.getY())
                self.objStrfAgg.setT(self.objAveHard.getT())
                self.objStrfAgg.setId(self.objAveHard.getId())
                self.objStrfAgg.setSysId(self.objAveHard.getSysId())
                if self.flgRemvMean == 0:
                    [self.sptlGridX,self.sptlGridY,self.tempGrid] = \
                                                 createGrid(self.objAveHard)
                    self.sptlRawMean = zeros(len(self.sptlGridX)).tolist()
                    self.sptlSmMean = zeros(len(self.sptlGridX)).tolist()
                    self.tempRawMean = zeros(len(self.tempGrid)).tolist()
                    self.tempSmMean = zeros(len(self.tempGrid)).tolist()
                    # Set new STRF data
                    if self.flgLogTran:
                        self.objStrfAgg.setVal1(self.objAveHard.getVal3())
                    else:
                        self.objStrfAgg.setVal1(self.objAveHard.getVal2())
                    self.objStrfAgg.setVal2\
                             (zeros(len(self.objStrfAgg.getX())).tolist())
                else:
                    # Remove mean trend from Agg data and Raw data
                    [strfVal,mstI] = removeMean(self.objAveHard,\
                                                self.sptlGridX,self.sptlGridY,\
                                                self.sptlSmMean,self.tempGrid,\
                                                self.tempSmMean,self.flgLogTran)
                    self.objStrfAgg.setVal1(strfVal)
                    self.objStrfAgg.setVal2(mstI)
                # Store parameter
                self.fileParam['sptlRadi'] = self.sptlRadi
                self.fileParam['sptlRnge'] = self.sptlRnge
                self.fileParam['tempRadi'] = self.tempRadi
                self.fileParam['tempRnge'] = self.tempRnge
                self.fileParam['flgRemvMean'] = self.flgRemvMean
                self.fileParam['sptlGridX'] = self.sptlGridX
                self.fileParam['sptlGridY'] = self.sptlGridY
                self.fileParam['tempGrid'] = self.tempGrid
                self.fileParam['sptlRawMean'] = self.sptlRawMean
                self.fileParam['sptlSmMean'] = self.sptlSmMean
                self.fileParam['tempRawMean'] = self.tempRawMean
                self.fileParam['tempSmMean'] = self.tempSmMean
                self.fileParam['objStrfAgg'] = self.objStrfAgg
            # Change cursor to None
            self.lowWindow4.set_cursor(None)
            # Hide box4
            self.box4.hide()

            # Set box5 object tree
            windowName = "box5"
            self.numCurScreen = 5
            self.windowTree5 = gtk.glade.XML(gladeFile,windowName)
            # Create window and low window objects
            self.box5 = self.windowTree5.get_widget(windowName)
            self.lowWindow5 = self.box5.window
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Get widget objects
            self.entNumSptlLag = self.windowTree5.get_widget('entNumSptlLag')
            self.entNumTempLag = self.windowTree5.get_widget('entNumTempLag')
            self.entNumCovSt = self.windowTree5.get_widget('entNumCovSt')
            self.noteCovMdl = self.windowTree5.get_widget('noteCovMdl')
            self.entCovPrm1 = self.windowTree5.get_widget('entCovPrm1')
            self.cmbSptlCovMdl1 = self.windowTree5.get_widget('cmbSptlCovMdl1')
            self.cmbTempCovMdl1 = self.windowTree5.get_widget('cmbTempCovMdl1')
            self.entSptlCovPrm1 = self.windowTree5.get_widget('entSptlCovPrm1')
            self.entTempCovPrm1 = self.windowTree5.get_widget('entTempCovPrm1')
            self.entCovPrm2 = self.windowTree5.get_widget('entCovPrm2')
            self.cmbSptlCovMdl2 = self.windowTree5.get_widget('cmbSptlCovMdl2')
            self.cmbTempCovMdl2 = self.windowTree5.get_widget('cmbTempCovMdl2')
            self.entSptlCovPrm2 = self.windowTree5.get_widget('entSptlCovPrm2')
            self.entTempCovPrm2 = self.windowTree5.get_widget('entTempCovPrm2')
            self.entCovPrm3 = self.windowTree5.get_widget('entCovPrm3')
            self.cmbSptlCovMdl3 = self.windowTree5.get_widget('cmbSptlCovMdl3')
            self.cmbTempCovMdl3 = self.windowTree5.get_widget('cmbTempCovMdl3')
            self.entSptlCovPrm3 = self.windowTree5.get_widget('entSptlCovPrm3')
            self.entTempCovPrm3 = self.windowTree5.get_widget('entTempCovPrm3')
            self.entCovPrm4 = self.windowTree5.get_widget('entCovPrm4')
            self.cmbSptlCovMdl4 = self.windowTree5.get_widget('cmbSptlCovMdl4')
            self.cmbTempCovMdl4 = self.windowTree5.get_widget('cmbTempCovMdl4')
            self.entSptlCovPrm4 = self.windowTree5.get_widget('entSptlCovPrm4')
            self.entTempCovPrm4 = self.windowTree5.get_widget('entTempCovPrm4')
            self.vboxCov1 = self.windowTree5.get_widget('vboxCov1')
            self.vboxCov2 = self.windowTree5.get_widget('vboxCov2')
            self.vboxCov3 = self.windowTree5.get_widget('vboxCov3')
            self.vboxCov4 = self.windowTree5.get_widget('vboxCov4')
            self.lblCovPage1 = self.windowTree5.get_widget('lblCovPage1')
            self.lblCovPage2 = self.windowTree5.get_widget('lblCovPage2')
            self.lblCovPage3 = self.windowTree5.get_widget('lblCovPage3')
            self.lblCovPage4 = self.windowTree5.get_widget('lblCovPage4')
            butEditSptlLag = self.windowTree5.get_widget('butEditSptlLag')
            butEditTempLag = self.windowTree5.get_widget('butEditTempLag')
            butCalcSptlCov = self.windowTree5.get_widget('butCalcSptlCov')
            butCalcTempCov = self.windowTree5.get_widget('butCalcTempCov')
            butClearCovPlot = self.windowTree5.get_widget('butClearCovPlot')
            self.butPlotCovMdl = self.windowTree5.get_widget('butPlotCovMdl')
            self.butAutoCovFit = self.windowTree5.get_widget('butAutoCovFit') # P Jat: Added new button for AUTOMATIC COVARIANCE PARAMETERS fitting
            self.lblCovUnit1 = self.windowTree5.get_widget('lblCovUnit1')
            self.lblCovUnit2 = self.windowTree5.get_widget('lblCovUnit2')
            self.lblCovUnit3 = self.windowTree5.get_widget('lblCovUnit3')
            self.lblCovUnit4 = self.windowTree5.get_widget('lblCovUnit4')
            lblSptlCovPrmUnit1 = self.windowTree5.\
                                 get_widget('lblSptlCovPrmUnit1')
            lblTempCovPrmUnit1 = self.windowTree5.\
                                 get_widget('lblTempCovPrmUnit1')
            lblSptlCovPrmUnit2 = self.windowTree5.\
                                 get_widget('lblSptlCovPrmUnit2')
            lblTempCovPrmUnit2 = self.windowTree5.\
                                 get_widget('lblTempCovPrmUnit2')
            lblSptlCovPrmUnit3 = self.windowTree5.\
                                 get_widget('lblSptlCovPrmUnit3')
            lblTempCovPrmUnit3 = self.windowTree5.\
                                 get_widget('lblTempCovPrmUnit3')
            lblSptlCovPrmUnit4 = self.windowTree5.\
                                 get_widget('lblSptlCovPrmUnit4')
            lblTempCovPrmUnit4 = self.windowTree5.\
                                 get_widget('lblTempCovPrmUnit4')
            self.butQuit5 = self.windowTree5.get_widget('butQuit5')
            self.butNext5 = self.windowTree5.get_widget('butNext5')
            self.butBack5 = self.windowTree5.get_widget('butBack5')
            # Set default values
            self.numSptlLag = self.userConst['defNumSptlLag']
            self.numTempLag = self.userConst['defNumTempLag']
            self.entNumSptlLag.set_text(str(self.numSptlLag))
            self.entNumTempLag.set_text(str(self.numTempLag))
            lblSptlCovPrmUnit1.set_text(self.sptlUnit)
            lblTempCovPrmUnit1.set_text(self.tempUnit)
            lblSptlCovPrmUnit2.set_text(self.sptlUnit)
            lblTempCovPrmUnit2.set_text(self.tempUnit)
            lblSptlCovPrmUnit3.set_text(self.sptlUnit)
            lblTempCovPrmUnit3.set_text(self.tempUnit)
            lblSptlCovPrmUnit4.set_text(self.sptlUnit)
            lblTempCovPrmUnit4.set_text(self.tempUnit)
            #-$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ P Jat: Change here for NUMBER OF default COV MODELS----------------------------------
            #self.automaticCovFit()         ### P Jat: Call this function for setting DEFAULT COV MODELS parameters
            self.entNumCovSt.set_text('1')
            #------------------------------------------------------------------------------------------------------------------------
            
                       
            msgCovUnit = '  (Variance = ' + \
                         str(var(self.objStrfAgg.getVal1())) + ')'
            self.lblCovUnit1.set_text(msgCovUnit)
            self.lblCovUnit2.set_text(msgCovUnit)
            self.lblCovUnit3.set_text(msgCovUnit)
            self.lblCovUnit4.set_text(msgCovUnit)

            # Define canvas object for spatial covariance
            self.areaSptlCov = self.windowTree5.get_widget("hboxSptlCov")
            [self.figSptlCov,self.plotSptlCov] = createFigObj()
            [self.cavSptlCov,self.tbSptlCov] = \
                           createCavObj(self.figSptlCov,self.box5)
            # Define canvas object for spatial covariance
            self.areaTempCov = self.windowTree5.get_widget("hboxTempCov")
            [self.figTempCov,self.plotTempCov] = createFigObj()
            [self.cavTempCov,self.tbTempCov] = \
                           createCavObj(self.figTempCov,self.box5)

            # Get parameters from param file
            try:
                self.numCovSt = self.fileParam['numCovSt']
                self.sptlLag = self.fileParam['sptlLag']
                self.sptlLagTol = self.fileParam['sptlLagTol']
                self.sptlCovariance = self.fileParam['sptlCovariance']
                self.tempLag = self.fileParam['tempLag']
                self.tempLagTol = self.fileParam['tempLagTol']
                self.tempCovariance = self.fileParam['tempCovariance']
                self.numSptlLag = self.fileParam['numSptlLag']
                self.numTempLag = self.fileParam['numTempLag']
                self.sill1 = self.fileParam['sill1']
                self.sptlRnge1 = self.fileParam['sptlRnge1']
                self.tempRnge1 = self.fileParam['tempRnge1']
                self.sill2 = self.fileParam['sill2']
                self.sptlRnge2 = self.fileParam['sptlRnge2']
                self.tempRnge2 = self.fileParam['tempRnge2']
                self.sill3 = self.fileParam['sill3']
                self.sptlRnge3 = self.fileParam['sptlRnge3']
                self.tempRnge3 = self.fileParam['tempRnge3']
                self.sill4 = self.fileParam['sill4']
                self.sptlRnge4 = self.fileParam['sptlRnge4']
                self.tempRnge4 = self.fileParam['tempRnge4']
                self.covMdl1 = self.fileParam['covMdl1']
                self.covMdl2 = self.fileParam['covMdl2']
                self.covMdl3 = self.fileParam['covMdl3']
                self.covMdl4 = self.fileParam['covMdl4']         
            except:
                self.flgNewSession = True
            # If parameters exist, set values
            if not self.flgNewSession:
                self.entNumCovSt.set_text(str(self.numCovSt))
                if self.numCovSt >= 1:
                    strSptlMdl1 = self.covMdl1[0:string.find(self.covMdl1,'/')]
                    strTempMdl1 = self.covMdl1[string.find(self.covMdl1,'/')+1:]
                    self.cmbSptlCovMdl1.set_active(self.chkModel(strSptlMdl1))
                    self.cmbTempCovMdl1.set_active(self.chkModel(strTempMdl1))
                    self.entCovPrm1.set_text(str(self.sill1))
                    self.entSptlCovPrm1.set_text(str(self.sptlRnge1))
                    self.entTempCovPrm1.set_text(str(self.tempRnge1))
                if self.numCovSt >= 2:
                    strSptlMdl2 = self.covMdl2[0:string.find(self.covMdl2,'/')]
                    strTempMdl2 = self.covMdl2[string.find(self.covMdl2,'/')+1:]
                    self.cmbSptlCovMdl2.set_active(self.chkModel(strSptlMdl2))
                    self.cmbTempCovMdl2.set_active(self.chkModel(strTempMdl2))
                    self.entCovPrm2.set_text(str(self.sill2))
                    self.entSptlCovPrm2.set_text(str(self.sptlRnge2))
                    self.entTempCovPrm2.set_text(str(self.tempRnge2))
                if self.numCovSt >= 3:
                    strSptlMdl3 = self.covMdl3[0:string.find(self.covMdl3,'/')]
                    strTempMdl3 = self.covMdl3[string.find(self.covMdl3,'/')+1:]
                    self.cmbSptlCovMdl3.set_active(self.chkModel(strSptlMdl3))
                    self.cmbTempCovMdl3.set_active(self.chkModel(strTempMdl3))
                    self.entCovPrm3.set_text(str(self.sill3))
                    self.entSptlCovPrm3.set_text(str(self.sptlRnge3))
                    self.entTempCovPrm3.set_text(str(self.tempRnge3))
                if self.numCovSt >= 4:
                    strSptlMdl4 = self.covMdl4[0:string.find(self.covMdl4,'/')]
                    strTempMdl4 = self.covMdl4[string.find(self.covMdl4,'/')+1:]
                    self.cmbSptlCovMdl4.set_active(self.chkModel(strSptlMdl4))
                    self.cmbTempCovMdl4.set_active(self.chkModel(strTempMdl4))
                    self.entCovPrm4.set_text(str(self.sill4))
                    self.entSptlCovPrm4.set_text(str(self.sptlRnge4))
                    self.entTempCovPrm4.set_text(str(self.tempRnge4))
                # Plot covariance
                self.createSptlCovPlot()
                self.createTempCovPlot()
                self.plotMdl()
                # If there is only one pt, disable buttons
                if self.flgOneTempPt or self.flgOneSptlPt:
                    butCalcSptlCov.set_sensitive(0)
                    self.entNumSptlLag.set_sensitive(0)
                    butCalcTempCov.set_sensitive(0)
                    self.entNumTempLag.set_sensitive(0)
            else:
                # Plot temporal and spatial experimental covariance
                if self.flgOneTempPt or self.flgOneSptlPt:
                    butCalcSptlCov.set_sensitive(0)
                    self.entNumSptlLag.set_sensitive(0)
                    butCalcTempCov.set_sensitive(0)
                    self.entNumTempLag.set_sensitive(0)
                    self.calcZeroSptlCov()
                    self.calcZeroTempCov()
                else:
                    self.plotExpCov()
            # Get default covariance settings
            self.numCovSt = int(self.entNumCovSt.get_text())
            pageNum = 4 - self.numCovSt
            for i in range(pageNum):
                self.noteCovMdl.remove_page(-1)
            # Map event handler
            self.box5.connect('destroy',self.justQuit)
            self.butQuit5.connect('clicked',self.askQuit)
            butEditSptlLag.connect('clicked',self.editSptlCov)
            butEditTempLag.connect('clicked',self.editTempCov)
            butClearCovPlot.connect('clicked',self.clearCovPlot)
            butCalcSptlCov.connect('clicked',self.calcSptlCov)
            butCalcTempCov.connect('clicked',self.calcTempCov)
            self.entNumCovSt.connect('changed',self.chgCovMdlTab)
            self.butPlotCovMdl.connect('clicked',self.plotCovMdl)
            self.butAutoCovFit.connect('clicked',self.automaticCovFit)  # P Jat: Call function 'automaticCovFit' for automatic Covariance Parameters fitting
            self.butNext5.connect('clicked',self.goBox6)
            self.butBack5.connect('clicked',self.backScreen)
            # Change cursor to None
            self.lowWindow5.set_cursor(None)

        except DummyError:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def chkModel(self,covModel):
        if covModel == "exponentialC":
            return 0
        elif covModel == "gaussianC":
            return 1
        elif covModel == "sphericalC":
            return 2
        elif covModel == "holecosC":
            return 3
        elif covModel == "holesinC":
            return 4

    def plotExpCov(self):
        """Plot experimental covariance"""
        try:
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Get the number of lags
            self.numSptlLag = self.entNumSptlLag.get_text()
            self.numTempLag = self.entNumTempLag.get_text()
            # Check the number of lags
            if not self.numSptlLag:
                raise TgisWarning(errMsg0501)
            if not self.numTempLag:
                raise TgisWarning(errMsg0502)
            try:
                self.numSptlLag = int(self.numSptlLag)
                self.numTempLag = int(self.numTempLag)
            except:
                raise TgisWarning(errMsg0503)
            # Calculate experimental covariance
            [self.sptlLag,self.sptlLagTol,self.sptlCovariance,\
             self.tempLag,self.tempLagTol,self.tempCovariance] = \
                calculateCovByNum(self.objStrfAgg,self.sptlGridX,\
                                  self.sptlGridY,self.tempGrid,\
                                  self.numSptlLag,self.numTempLag)
            # Plot covariance
            self.createSptlCovPlot()
            self.createTempCovPlot()
            # Change cursor to None
            self.lowWindow5.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

    def createSptlCovPlot(self):
        """Create spatial covariance plot"""
        # Define canvas object for spatial covariance plot
        [self.figSptlCov,self.plotSptlCov] = createFigObj()
        # Plot spatial covariance
        xlabelString = "Spatial Lag (" + self.sptlUnit + ")"
        ylabelString = "Covariance"
        titleString = "Spatial component"
        setPlotLabels(self.plotSptlCov,xlabelString,\
                      ylabelString,titleString,True)
        self.plotSptlCov.plot(self.sptlLag,self.sptlCovariance,'or',\
                              markersize=10)
        # Create xlim and ylim
        maxSptlLag = max(self.sptlLag)
        minSptlLag = min(self.sptlLag)
        self.defXlimSptlCov = calcGraphBuff(minSptlLag,maxSptlLag)
        self.sptlCovYlimMin = [min(self.sptlCovariance),0]
        self.sptlCovYlimMax = [max(self.sptlCovariance)]
        self.defYlimSptlCov = calcGraphBuff(min(self.sptlCovYlimMin),\
                                            max(self.sptlCovYlimMax))
        # Set xlim and ylim
        self.plotSptlCov.set_xlim(self.defXlimSptlCov)
        self.plotSptlCov.set_ylim(self.defYlimSptlCov)
        [self.cavSptlCov,self.tbSptlCov] = \
           plotGraph(self.figSptlCov,self.plotSptlCov,\
                     self.cavSptlCov,self.tbSptlCov,\
                     self.areaSptlCov,self.box5)

    def createTempCovPlot(self):
        """Create spatial covariance plot"""
        # Define canvas object for spatial covariance plot
        [self.figTempCov,self.plotTempCov] = createFigObj()
        # Plot spatial covariance
        xlabelString = "Temporal Lag (" + self.tempUnit + ")"
        ylabelString = "Covariance"
        titleString = "Temporal component"
        setPlotLabels(self.plotTempCov,xlabelString,\
                      ylabelString,titleString,True)
        self.plotTempCov.plot(self.tempLag,self.tempCovariance,'or',\
                              markersize=10)
        # Create xlim and ylim
        maxTempLag = max(self.tempLag)
        minTempLag = min(self.tempLag)
        self.defXlimTempCov = calcGraphBuff(minTempLag,maxTempLag)
        self.tempCovYlimMin = [min(self.tempCovariance),0]
        self.tempCovYlimMax = [max(self.tempCovariance)]
        self.defYlimTempCov = calcGraphBuff(min(self.tempCovYlimMin),\
                                            max(self.tempCovYlimMax))
        # Set xlim and ylim
        self.plotTempCov.set_xlim(self.defXlimTempCov)
        self.plotTempCov.set_ylim(self.defYlimTempCov)
        [self.cavTempCov,self.tbTempCov] = \
           plotGraph(self.figTempCov,self.plotTempCov,\
                     self.cavTempCov,self.tbTempCov,\
                     self.areaTempCov,self.box5)

    def chgCovMdlTab(self,widget):
        """Change the number of covariance model tabs"""
        try:
            # Check the number of tabs
            if self.entNumCovSt.get_text() == "":
                raise DummyError
            try:
                self.numCovSt = int(self.entNumCovSt.get_text())
            except:
                raise TgisWarning(errMsg0504)
            if self.numCovSt > 4 or self.numCovSt < 1:
                raise TgisWarning(errMsg0504)
            # Change the number of tabs
            curPageNum = self.noteCovMdl.get_n_pages()
            if curPageNum > self.numCovSt:
                for i in range(curPageNum-self.numCovSt):
                    self.noteCovMdl.remove_page(-1)
            elif curPageNum < self.numCovSt:
                for i in range(self.numCovSt - curPageNum):
                    if curPageNum + i + 1 == 2:
                        self.noteCovMdl.append_page(self.vboxCov2,\
                                                    self.lblCovPage2)
                    elif curPageNum + i + 1 == 3:
                        self.noteCovMdl.append_page(self.vboxCov3,\
                                                    self.lblCovPage3)
                    elif curPageNum + i + 1 == 4:
                        self.noteCovMdl.append_page(self.vboxCov4,\
                                                    self.lblCovPage4)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except DummyError:
            pass

    def editSptlCov(self,widget):
        """Calculate spatial covariance by lag and tolerance"""
        try:
            # Set spatial covariance dialog object tree
            dialogName = "diagSptlCovLag"
            covSptlLagDialogTree = gtk.glade.XML(gladeFile,dialogName)
            # Create dialog and low window objects
            covSptlLagDialog = covSptlLagDialogTree.get_widget(dialogName)
            lowCovSptlLagDialog = covSptlLagDialog.window
            # Get widget objects
            entSptlLag = covSptlLagDialogTree.get_widget('entSptlLag')
            entSptlLagTol = covSptlLagDialogTree.get_widget('entSptlLagTol')
            # Get spatial lag and tolerance
            txtSptlLag = ''
            txtSptlLagTol = ''
            if 'sptlLag' in dir(self):
                for i in range(len(self.sptlLag)):
                    txtSptlLag = txtSptlLag + ',' + str(self.sptlLag[i])
                    txtSptlLagTol = txtSptlLagTol + ',' + str(self.sptlLagTol[i])
            # Set spatial lag and tolerance
            entSptlLag.set_text(txtSptlLag.strip(','))
            entSptlLagTol.set_text(txtSptlLagTol.strip(','))
            # Display spatial covariance dialog
            bolResp = covSptlLagDialog.run()
            if bolResp == gtk.RESPONSE_CANCEL:
                # Destroy dialog
                covSptlLagDialog.destroy()
            elif bolResp == gtk.RESPONSE_OK:
                # Change cursor to Watch
                lowCovSptlLagDialog.set_cursor(self.cursorWatch)
                # Get lag and tolerance
                sptlLag = entSptlLag.get_text()
                sptlLagTol = entSptlLagTol.get_text()
                # Check lag and tolerance
                if len(sptlLag) == 0 or len(sptlLagTol) == 0:
                    raise TgisWarning(errMsg0505)
                try:
                    sptlLag = string.split(sptlLag,",")
                    sptlLagTol = string.split(sptlLagTol,",")
                except:
                    raise TgisWarning(errMsg0506)
                if len(sptlLag) != len(sptlLagTol):
                    raise TgisWarning(errMsg0507)
                try:
                    sptlLag = map(float,sptlLag)
                    sptlLagTol = map(float,sptlLagTol)
                except:
                    raise TgisWarning(errMsg0508)
                self.sptlLag = sptlLag
                self.sptlLagTol = sptlLagTol
                # Calculate spatial covariance
                [self.sptlLag,self.sptlLagTol,self.sptlCovariance] = \
                    calculateCovByLag(self.objStrfAgg,self.sptlGridX,\
                                      self.sptlGridY,self.tempGrid,\
                                      self.sptlLag,self.sptlLagTol,\
                                      [0.0],[0.0])
                # Plot spatial covariance
                self.createSptlCovPlot()
                # Change cursor to None
                lowCovSptlLagDialog.set_cursor(None)
                # Destroy dialog
                covSptlLagDialog.destroy()

        except TgisWarning:
            # Change cursor to None
            lowCovSptlLagDialog.set_cursor(None)
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))
            # Destroy dialog
            covSptlLagDialog.destroy()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def editTempCov(self,widget):
        """Calculate temporal covariance by lag and tolerance"""
        try:
            # Set temporal covariance dialog object tree
            dialogName = "diagTempCovLag"
            covTempLagDialogTree = gtk.glade.XML(gladeFile,dialogName)
            # Create dialog and low window objects
            covTempLagDialog = covTempLagDialogTree.get_widget(dialogName)
            lowCovTempLagDialog = covTempLagDialog.window
            # Get widget objects
            entTempLag = covTempLagDialogTree.get_widget('entTempLag')
            entTempLagTol = covTempLagDialogTree.get_widget('entTempLagTol')
            # Get spatial lag and tolerance
            txtTempLag = ''
            txtTempLagTol = ''
            if 'tempLag' in dir(self):
                for i in range(len(self.tempLag)):
                    txtTempLag = txtTempLag + ',' + str(self.tempLag[i])
                    txtTempLagTol = txtTempLagTol + ',' + str(self.tempLagTol[i])
            # Set spatial lag and tolerance
            entTempLag.set_text(txtTempLag.strip(','))
            entTempLagTol.set_text(txtTempLagTol.strip(','))
            # Display spatial covariance dialog
            bolResp = covTempLagDialog.run()
            if bolResp == gtk.RESPONSE_CANCEL:
                # Destroy dialog
                covTempLagDialog.destroy()
            elif bolResp == gtk.RESPONSE_OK:
                # Change cursor to Watch
                lowCovTempLagDialog.set_cursor(self.cursorWatch)
                # Get lag and tolerance
                tempLag = entTempLag.get_text()
                tempLagTol = entTempLagTol.get_text()
                # Check lag and tolerance
                if len(tempLag) == 0 or len(tempLagTol) == 0:
                    raise TgisWarning(errMsg0505)
                try:
                    tempLag = string.split(tempLag,",")
                    tempLagTol = string.split(tempLagTol,",")
                except:
                    raise TgisWarning(errMsg0506)
                if len(tempLag) != len(tempLagTol):
                    raise TgisWarning(errMsg0507)
                try:
                    tempLag = map(float,tempLag)
                    tempLagTol = map(float,tempLagTol)
                except:
                    raise TgisWarning(errMsg0508)
                self.tempLag = tempLag
                self.tempLagTol = tempLagTol
                # Calculate spatial covariance
                [self.tempLag,self.tempLagTol,self.tempCovariance] = \
                    calculateCovByLag(self.objStrfAgg,self.sptlGridX,\
                                      self.sptlGridY,self.tempGrid,\
                                      [0.0],[0.0],\
                                      self.tempLag,self.tempLagTol)
                # Plot spatial covariance
                self.createTempCovPlot()
                # Change cursor to None
                lowCovTempLagDialog.set_cursor(None)
                # Destroy dialog
                covTempLagDialog.destroy()

        except TgisWarning:
            # Change cursor to None
            lowCovTempLagDialog.set_cursor(None)
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))
            # Destroy dialog
            covTempLagDialog.destroy()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def calcZeroSptlCov(self):
        """Calculate spatial covariance by lag and tolerance"""
        try:
            self.sptlLag = [0.0]
            self.sptlLagTol = [0.0]
            # Calculate spatial covariance
            [self.sptlLag,self.sptlLagTol,self.sptlCovariance] = \
                calculateCovByLag(self.objStrfAgg,self.sptlGridX,\
                                  self.sptlGridY,self.tempGrid,\
                                  self.sptlLag,self.sptlLagTol,\
                                  [0.0],[0.0])
            # Plot spatial covariance
            self.createSptlCovPlot()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def calcZeroTempCov(self):
        """Calculate temporal covariance by lag and tolerance"""
        try:
            self.tempLag = [0.0]
            self.tempLagTol = [0.0]
            # Calculate spatial covariance
            [self.tempLag,self.tempLagTol,self.tempCovariance] = \
                calculateCovByLag(self.objStrfAgg,self.sptlGridX,\
                                  self.sptlGridY,self.tempGrid,\
                                  [0.0],[0.0],\
                                  self.tempLag,self.tempLagTol)
            # Plot spatial covariance
            self.createTempCovPlot()

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def calcSptlCov(self,widget):
        """Calculate spatial covariance by the number of lags"""
        try:
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Get the number of lags
            self.numSptlLag = self.entNumSptlLag.get_text()
            # Check the number of lags
            if not self.numSptlLag:
                raise TgisWarning(errMsg0501)
            try:
                self.numSptlLag = int(self.numSptlLag)
            except:
                raise TgisWarning(errMsg0503)
            # Calculate experimental covariance
            [self.sptlLag,self.sptlLagTol,self.sptlCovariance,\
             dummy1,dummy2,dummy3] = \
                calculateCovByNum(self.objStrfAgg,self.sptlGridX,\
                                  self.sptlGridY,self.tempGrid,\
                                  self.numSptlLag,self.numTempLag)
            # Plot covariance
            self.createSptlCovPlot()
            # Change cursor to None
            self.lowWindow5.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def calcTempCov(self,widget):
        """Calculate temporal covariance by the number of lags"""
        try:
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Get the number of lags
            self.numTempLag = self.entNumTempLag.get_text()
            # Check the number of lags
            if not self.numTempLag:
                raise TgisWarning(errMsg0502)
            try:
                self.numTempLag = int(self.numTempLag)
            except:
                raise TgisWarning(errMsg0503)
            # Calculate experimental covariance
            [dummy1,dummy2,dummy3,\
             self.tempLag,self.tempLagTol,self.tempCovariance] = \
                calculateCovByNum(self.objStrfAgg,self.sptlGridX,\
                                  self.sptlGridY,self.tempGrid,\
                                  self.numSptlLag,self.numTempLag)
            # Plot covariance
            self.createTempCovPlot()
            # Change cursor to None
            self.lowWindow5.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def clearCovPlot(self,widget):
        """Clear experimental covariance"""
        try:
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Plot covariance
            self.createSptlCovPlot()
            self.createTempCovPlot()
            # Change cursor to None
            self.lowWindow5.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()




















    #---------------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------@@@@----        
    # ----P Jat:  Added this function to fit automatic covariance model to experimental cov models            
    def automaticCovFit(self, widget):
        #------------------ Find best model and its parameters based on r-square ----
        ##rangeZZ = arange(0,max(self.sptlLag),max(self.sptlLag)/len(self.sptlLag))
        rangeZZ = arange(0,max(self.sptlLag)*5,(self.sptlLag[1]-self.sptlLag[0])/3)
        sptlCovMdlZ1 = 'exponentialC'
        ##nR = int(len(rangeZZ)/2)
        nR = int(len(rangeZZ))
        #print nR
        #print "========================"
        #print rangeZZ

        
        #rLag = self.sptlLag
        #Cr   = self.sptlCovariance
        #print rLag
        #print "========================"
        #print Cr
        #cmdBME = 'removeDupli.exe ' + dataFileName + ' ' + outFileName
        #sptOpt =  'covFit.exe ' +  self.sptlLag + self.sptlCovariance
        ##dataFileName = 'lagData.ysd'
        ##outFileName = 'lagOut.ysd'
        
        # Create data file
        ##aryConc = concatenate((self.sptlLag,self.sptlCovariance),1)
        ##objDataWriter = CsvWriter()
        ##objDataWriter.openFile(dataFileName)
        ##objDataWriter.writeArray(aryConc)
        ##objDataWriter.closeFile()

        rangeZZ2 = log(exp(self.sptlLag))        
        r2spt=[]                            # asure all r-square values for EXP covariance function
        sptModelINT =[];
        for i in range(2,nR):
            #sptlCov1 = calcCovModel(sptlDist,sptlCovMdl1,self.sill1,self.sptlRnge1)            
            ##modelZ2 = calcCovModel(rangeZZ,sptlCovMdlZ1,self.sptlCovariance[0],rangeZZ[len(rangeZZ)-i-1])
            modelZ2 = calcCovModel(rangeZZ2,sptlCovMdlZ1,self.sptlCovariance[0],rangeZZ[i])
            #gradient, intercept, r_value, p_value, std_err = stats.linregress(self.sptlCovariance,modelZ2)
            #r2spt.append(r_value**2)
            r2spt.append(min(abs(rangeZZ2-self.sptlCovariance)))
            sptModelINT.append('exponentialC')

        print #r2spt
            
        ##sptOpt = r2spt.index(max(r2spt))    # Max r-squared INDEX
        sptOpt = r2spt.index(max(r2spt))    # Max r-squared INDEX
        
        sptlCovMdlZ2 = 'gaussianC'
        for i in range(1,nR):            
            modelZ2 = calcCovModel(rangeZZ,sptlCovMdlZ2,self.sptlCovariance[0],rangeZZ[len(rangeZZ)-i-1])
            gradient, intercept, r_value, p_value, std_err = stats.linregress(self.sptlCovariance,modelZ2)
            sptModelINT.append('gaussianC')
            r2spt.append(r_value**2)






        rangeZZT = arange(0,max(self.tempLag),max(self.tempLag)/len(self.tempLag))
        tempCovMdlZ1 = 'exponentialC'
        nR = int(len(rangeZZT)/2)

        r2temp=[]                    # store all r-square values
        tempModelINT =[];
        for i in range(1,nR):            
            modelZ2T = calcCovModel(rangeZZT,tempCovMdlZ1,self.tempCovariance[0],rangeZZT[len(rangeZZT)-i-1])
            gradient, intercept, r_value, p_value, std_err = stats.linregress(self.tempCovariance,modelZ2T)
            tempModelINT.append('exponentialC')
            r2temp.append(r_value**2)          
        
        tempCovMdlZ2 = 'gaussianC'       
        for i in range(1,nR):            
            modelZ2T = calcCovModel(rangeZZT,tempCovMdlZ2,self.tempCovariance[0],rangeZZT[len(rangeZZT)-i-1])
            gradient, intercept, r_value, p_value, std_err = stats.linregress(self.tempCovariance,modelZ2T)
            r2temp.append(r_value**2)
            tempModelINT.append('gaussianC')
        tempOpt = r2temp.index(max(r2temp))    # Max r-squared INDEX
 

##        
##        print sptModelINT[sptOpt]
##        print rangeZZ[len(rangeZZ)-sptOpt]        
##        print tempModelINT[tempOpt]
##        print rangeZZT[len(rangeZZT)-tempOpt]
##        print self.tempCovariance[0]

##        sptlCovMdl1 = 'exponentialC'
##        tempCovMdl1 = 'exponentialC'
##        covPrm1 = 1.175
##        sptlCovPrm1 = .2
##        tempCovPrm1 =5
        
##        self.entCovPrm1.set_text(str(covPrm1))
##        self.entSptlCovPrm1.set_text(str(sptlCovPrm1))
##        self.entTempCovPrm1.set_text(str(tempCovPrm1))
##        self.cmbSptlCovMdl1.set_active(self.chkModel(sptlCovMdl1))
##        self.cmbTempCovMdl1.set_active(self.chkModel(tempCovMdl1))
        
        self.entCovPrm1.set_text(str(self.tempCovariance[0]))
        self.entSptlCovPrm1.set_text(str(rangeZZ[len(rangeZZ)-sptOpt]))
        self.entTempCovPrm1.set_text(str(rangeZZT[len(rangeZZT)-tempOpt]))
        self.cmbSptlCovMdl1.set_active(self.chkModel(sptModelINT[sptOpt]))
        self.cmbTempCovMdl1.set_active(self.chkModel(tempModelINT[tempOpt]))
                 
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------@@@@--    

























        
    def plotCovMdl(self,widget):
        """Plot covariance model"""
        self.plotMdl()

    def plotMdl(self):
        """Plot covariance model"""
        try:
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Get covariance model settings
            self.numCovSt = int(self.entNumCovSt.get_text())                    
            if self.numCovSt >= 1:
                sptlCovMdl1 = self.cmbSptlCovMdl1.get_active_text()
                tempCovMdl1 = self.cmbTempCovMdl1.get_active_text()
                covPrm1 = self.entCovPrm1.get_text()
                sptlCovPrm1 = self.entSptlCovPrm1.get_text()
                tempCovPrm1 = self.entTempCovPrm1.get_text()
            if self.numCovSt >= 2:
                sptlCovMdl2 = self.cmbSptlCovMdl2.get_active_text()
                tempCovMdl2 = self.cmbTempCovMdl2.get_active_text()
                covPrm2 = self.entCovPrm2.get_text()
                sptlCovPrm2 = self.entSptlCovPrm2.get_text()
                tempCovPrm2 = self.entTempCovPrm2.get_text()
            if self.numCovSt >= 3:
                sptlCovMdl3 = self.cmbSptlCovMdl3.get_active_text()
                tempCovMdl3 = self.cmbTempCovMdl3.get_active_text()
                covPrm3 = self.entCovPrm3.get_text()
                sptlCovPrm3 = self.entSptlCovPrm3.get_text()
                tempCovPrm3 = self.entTempCovPrm3.get_text()
            if self.numCovSt == 4:
                sptlCovMdl4 = self.cmbSptlCovMdl4.get_active_text()
                tempCovMdl4 = self.cmbTempCovMdl4.get_active_text()
                covPrm4 = self.entCovPrm4.get_text()
                sptlCovPrm4 = self.entSptlCovPrm4.get_text()
                tempCovPrm4 = self.entTempCovPrm4.get_text()
            # Check covariance model settings
            if self.numCovSt >= 1:
                if len(sptlCovMdl1) == 0 or len(tempCovMdl1) == 0:
                    raise TgisWarning(errMsg0509)                  
            if self.numCovSt >= 2:
                if len(sptlCovMdl2) == 0 or len(tempCovMdl2) == 0:
                    raise TgisWarning(errMsg0510)
            if self.numCovSt >= 3:
                if len(sptlCovMdl3) == 0 or len(tempCovMdl3) == 0:
                    raise TgisWarning(errMsg0511)
            if self.numCovSt == 4:
                if len(sptlCovMdl4) == 0 or len(tempCovMdl4) == 0:
                    raise TgisWarning(errMsg0512)
            try:
                if self.numCovSt >= 1:
                    self.sill1 = float(covPrm1)
                    self.sptlRnge1 = float(sptlCovPrm1)
                    self.tempRnge1 = float(tempCovPrm1)
                if self.numCovSt >= 2:
                    self.sill2 = float(covPrm2)
                    self.sptlRnge2 = float(sptlCovPrm2)
                    self.tempRnge2 = float(tempCovPrm2)
                else:
                    self.sill2 = 0.0
                    self.sptlRnge2 = 0.0
                    self.tempRnge2 = 0.0
                if self.numCovSt >= 3:
                    self.sill3 = float(covPrm3)
                    self.sptlRnge3 = float(sptlCovPrm3)
                    self.tempRnge3 = float(tempCovPrm3)
                else:
                    self.sill3 = 0.0
                    self.sptlRnge3 = 0.0
                    self.tempRnge3 = 0.0
                if self.numCovSt == 4:
                    self.sill4 = float(covPrm4)
                    self.sptlRnge4 = float(sptlCovPrm4)
                    self.tempRnge4 = float(tempCovPrm4)
                else:
                    self.sill4 = 0.0
                    self.sptlRnge4 = 0.0
                    self.tempRnge4 = 0.0
            except:
                raise TgisWarning(errMsg0513)
            # Create covariance parameters for estimation
            if self.numCovSt >= 1:
                self.covMdl1 = sptlCovMdl1 + '/' + tempCovMdl1
            if self.numCovSt >= 2:
                self.covMdl2 = sptlCovMdl2 + '/' + tempCovMdl2
            else:
                self.covMdl2 = '0'
            if self.numCovSt >= 3:
                self.covMdl3 = sptlCovMdl3 + '/' + tempCovMdl3
            else:
                self.covMdl3 = '0'
            if self.numCovSt >= 4:
                self.covMdl4 = sptlCovMdl4 + '/' + tempCovMdl4
            else:
                self.covMdl4 = '0'
            # If sptlLag (or tempLag) contains only one pt, create additional pt
            if max(self.sptlLag) == min(self.sptlLag):
                sptlLag2 = [0]
                if max(self.sptlLag) == 0:
                    sptlLag2.append(max(calcGraphBuff(min(self.sptlLag),\
                                                      max(self.sptlLag))))
                else:
                    sptlLag2.append(max(self.sptlLag))
                sptlDist = arange(min(sptlLag2),\
                                  max(sptlLag2)+\
                                  (max(sptlLag2)-min(sptlLag2))/20,\
                                  (max(sptlLag2)-min(sptlLag2))/100)
            else:
                sptlDist = arange(min(self.sptlLag),\
                                  max(self.sptlLag)+(max(self.sptlLag)-\
                                                      min(self.sptlLag))/20,\
                                  (max(self.sptlLag)-min(self.sptlLag))/100)

            if max(self.tempLag) == min(self.tempLag):
                tempLag2 = [0]
                if max(self.tempLag) == 0:
                    tempLag2.append(max(calcGraphBuff(min(self.tempLag),\
                                                      max(self.tempLag))))
                else:
                    tempLag2.append(max(self.tempLag))
                tempDist = arange(min(tempLag2),\
                                  max(tempLag2)+\
                                  (max(tempLag2)-min(tempLag2))/20,\
                                  (max(tempLag2)-min(tempLag2))/100)
            else:
                tempDist = arange(min(self.tempLag),\
                                  max(self.tempLag)+(max(self.tempLag)-\
                                                      min(self.tempLag))/20,\
                             (max(self.tempLag)-min(self.tempLag))/100)
            # Calculate covariance model values
            if self.numCovSt >= 1:
                sptlCov1 = calcCovModel(sptlDist,sptlCovMdl1,\
                                        self.sill1,self.sptlRnge1)
                tempCov1 = calcCovModel(tempDist,tempCovMdl1,\
                                        self.sill1,self.tempRnge1)
                
            if self.numCovSt >= 2:
                sptlCov2 = calcCovModel(sptlDist,sptlCovMdl2,\
                                        self.sill2,self.sptlRnge2)
                tempCov2 = calcCovModel(tempDist,tempCovMdl2,\
                                        self.sill2,self.tempRnge2)
                sptlCov1 = sptlCov1 + sptlCov2
                tempCov1 = tempCov1 + tempCov2
            if self.numCovSt >= 3:
                sptlCov3 = calcCovModel(sptlDist,sptlCovMdl3,\
                                        self.sill3,self.sptlRnge3)
                tempCov3 = calcCovModel(tempDist,tempCovMdl3,\
                                        self.sill3,self.tempRnge3)
                sptlCov1 = sptlCov1 + sptlCov3
                tempCov1 = tempCov1 + tempCov3
            if self.numCovSt == 4:
                sptlCov4 = calcCovModel(sptlDist,sptlCovMdl4,\
                                        self.sill4,self.sptlRnge4)
                tempCov4 = calcCovModel(tempDist,tempCovMdl4,\
                                        self.sill4,self.tempRnge4)
                sptlCov1 = sptlCov1 + sptlCov4
                tempCov1 = tempCov1 + tempCov4
            # Plot covariance model
            self.plotSptlCov.plot(sptlDist,sptlCov1)
            hold(True)
            # Set defYlimSptlCov
            self.sptlCovYlimMin.append(min(sptlCov1))
            self.sptlCovYlimMax.append(max(sptlCov1))
            self.defYlimSptlCov = calcGraphBuff(min(self.sptlCovYlimMin),\
                                                max(self.sptlCovYlimMax))
            # Set xlim and ylim
            self.plotSptlCov.set_xlim(self.defXlimSptlCov)
            self.plotSptlCov.set_ylim(self.defYlimSptlCov)

            # Create plot
            [self.cavSptlCov,self.tbSptlCov] = \
               plotGraph(self.figSptlCov,self.plotSptlCov,\
                         self.cavSptlCov,self.tbSptlCov,\
                         self.areaSptlCov,self.box5)
            self.plotTempCov.plot(tempDist,tempCov1)
            hold(True)
            # Set defYlimSptlCov
            self.tempCovYlimMin.append(min(tempCov1))
            self.tempCovYlimMax.append(max(tempCov1))
            self.defYlimTempCov = calcGraphBuff(min(self.tempCovYlimMin),\
                                                max(self.tempCovYlimMax))
            # Set xlim and ylim
            self.plotTempCov.set_xlim(self.defXlimTempCov)
            self.plotTempCov.set_ylim(self.defYlimTempCov)
            # Create plot
            [self.cavTempCov,self.tbTempCov] = \
               plotGraph(self.figTempCov,self.plotTempCov,\
                         self.cavTempCov,self.tbTempCov,\
                         self.areaTempCov,self.box5)
            # Change coursor to None
            self.lowWindow5.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()
    



            
    #---------------------------------------------------------------------------------------------
    #-------------- Get data for mask  ( P Jat; June 2011)       
    def getMaskData(self, widget,data=None):        
        if widget.get_active():
            self.lblMaskFile.set_sensitive(1)
            if os.path.exists('maskX.txt'):
                print ' Mask File Exist'
                self.incMask=1
                fileSelected=0
                x =[]
                y =[]
                file = open("maskX.txt")
                for line in file.xreadlines():
                    x.append(float(line.rstrip('\n')))
                file.close()

                file = open("maskY.txt")
                for line in file.xreadlines():
                    y.append(float(line.rstrip('\n')))
                file.close()
                
                self.entEstLimE.set_text(fpformat.fix(max(x),numParamDeci))
                self.entEstLimW.set_text(fpformat.fix(min(x),numParamDeci))
                self.entEstLimN.set_text(fpformat.fix(max(y),numParamDeci))
                self.entEstLimS.set_text(fpformat.fix(min(y),numParamDeci))                

            else:
                print 'open file selection dialog'
                dialog = gtk.FileChooserDialog(" Please Select Mask File (Shape or .csv File)..",
                                               None, gtk.FILE_CHOOSER_ACTION_OPEN,
                                               (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                                gtk.STOCK_OPEN, gtk.RESPONSE_OK))
                dialog.set_default_response(gtk.RESPONSE_OK)
                filter = gtk.FileFilter()
                filter.set_name("All files")
                filter.add_pattern("*")
                dialog.add_filter(filter)
                filter = gtk.FileFilter()
                filter.set_name("Images")
                filter.add_mime_type("image/png")
                filter.add_mime_type("image/jpeg")
                filter.add_mime_type("image/gif")
                filter.add_pattern("*.png")
                filter.add_pattern("*.jpg")
                filter.add_pattern("*.gif")
                filter.add_pattern("*.tif")
                filter.add_pattern("*.xpm")
                dialog.add_filter(filter)
                response = dialog.run()
                if response == gtk.RESPONSE_OK:
                    print dialog.get_filename(), 'selected'
                    self.selectedMaskFile = str(dialog.get_filename())     # make it Global
                    self.lblMaskFile.set_text(self.selectedMaskFile)            #######
                    #--------------------- P Jat Write file with file address
                    # Create param file
                    paramFileName='maskFileName.txt'
                    objDataWriter = DataWriter()
                    objDataWriter.openFile('maskFileName.txt')
                    print dialog.get_filename()
                    objDataWriter.writeStr(dialog.get_filename())
                    objDataWriter.closeFile()
                    fileSelected=1
              
                elif response == gtk.RESPONSE_CANCEL:
                    print 'Closed, no files selected'
                    self.chkInclMask.set_active(False)
                    dialog.destroy()
                    fileSelected=0
                dialog.destroy()

                # Call MATLAB file ('maskDataGen.exe') which read file and
                # generate X, Y coordinates and saves in directory
            if fileSelected==1:
                retCode = os.system('maskDataGen.exe')
                print '============================================'
                print retCode
                if retCode==0:
                    self.incMask=1
                    x =[]
                    y =[]
                    file = open("maskX.txt")
                    for line in file.xreadlines():
                        x.append(float(line.rstrip('\n')))
                    file.close()

                    file = open("maskY.txt")
                    for line in file.xreadlines():
                        y.append(float(line.rstrip('\n')))
                    file.close()
                

                    self.entEstLimE.set_text(fpformat.fix(max(x),numParamDeci))
                    self.entEstLimW.set_text(fpformat.fix(min(x),numParamDeci))
                    self.entEstLimN.set_text(fpformat.fix(max(y),numParamDeci))
                    self.entEstLimS.set_text(fpformat.fix(min(y),numParamDeci))           
                if not retCode==0:                    
                    raise TgisError('Error in Mask file')
                
        else:
            self.incMask =0
            self.lblMaskFile.set_sensitive(0)
            print 'Do not use mask'











    #---------------------------------------------------------------------------------------------
    #-------------- Get data for map  ( P Jat; June 2011)  
    def getMapData(self, widget,data=None):        
        if widget.get_active():            
            if os.path.exists('mapX.txt'):
                print ' Map File Exist'
##                self.incMap=1
##                fileSelected=0
##                xxmap =[]
##                yymap =[]
##                file = open("mapX.txt")
##                for line in file.xreadlines():
##                    xxmap.append(float(line.rstrip('\n')))
##                file.close()
##
##                file = open("mapY.txt")
##                for line in file.xreadlines():
##                    yymap.append(float(line.rstrip('\n')))
##                file.close() 
            else:
                print 'open file selection dialog'
                dialog = gtk.FileChooserDialog(" Please Select Map File (Shape or .csv File)..",
                                               None, gtk.FILE_CHOOSER_ACTION_OPEN,
                                               (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                                gtk.STOCK_OPEN, gtk.RESPONSE_OK))
                dialog.set_default_response(gtk.RESPONSE_OK)
                filter = gtk.FileFilter()
                filter.set_name("All files")
                filter.add_pattern("*")
                dialog.add_filter(filter)
                filter = gtk.FileFilter()
                filter.set_name("Images")
                filter.add_mime_type("image/png")
                filter.add_mime_type("image/jpeg")
                filter.add_mime_type("image/gif")
                filter.add_pattern("*.png")
                filter.add_pattern("*.jpg")
                filter.add_pattern("*.gif")
                filter.add_pattern("*.tif")
                filter.add_pattern("*.xpm")
                dialog.add_filter(filter)
                response = dialog.run()
                if response == gtk.RESPONSE_OK:
                    print dialog.get_filename(), 'selected'
                    self.selectedMapFile = str(dialog.get_filename())           # make it Global                    
                    #--------------------- P Jat Write file with file address
                    # Create param file
                    paramFileName='plotMapFileName.txt'
                    objDataWriter = DataWriter()
                    objDataWriter.openFile('plotMapFileName.txt')
                    print dialog.get_filename()
                    objDataWriter.writeStr(dialog.get_filename())
                    objDataWriter.closeFile()
                    fileSelected=1
              
                elif response == gtk.RESPONSE_CANCEL:
                    print 'Closed, no files selected'
                    self.chkInclMap.set_active(False)
                    dialog.destroy()
                    fileSelected=0
                dialog.destroy()

                # Call MATLAB file ('mapDataGen.exe') which read file and
                # generate X, Y coordinates and saves in directory
            if fileSelected==1:
                retCode = os.system('mapDataGen.exe')
                print '============================================'
                print retCode
                if retCode==0:
                    self.incMap=1                 
                if not retCode==0:                    
                    raise TgisError('Error in Map file')
                
        else:
            self.incMap =0
            print 'Do not plot Map'



















    def goBox6(self,widget):
        """Move to box6"""
        try:
            t = arange(0.0, 1.0+0.01, 0.01)
            s = cos(2*2*pi*t)
            t[41:60] = nan
            s[41:60] = nan

            figure(6)
            subplot(1,1,1)
            # masked_array = np.ma.array (t, mask=np.isnan(t))
            print t
            print s
            plot(t, s, '-', lw=2)

            xlabel('time (s)')
            ylabel('voltage (mV)')
            title('A sine wave with a gap of NaNs between 0.4 and 0.6')
            grid(True)
            xlabel('time (s)')
            ylabel('more nans')
            grid(True)                
            show()




             
            # Change cursor to Watch
            self.lowWindow5.set_cursor(self.cursorWatch)
            # Check covariance models
            listMdl = ['covMdl1','covMdl2','covMdl3','covMdl4']
            for idx,item in enumerate(listMdl):
                if not item in dir(self):
                    msg = 'Covariance model of component ' + str(idx+1)  + \
                          ' is not defined. Select model, ' + \
                          'and press the "Plot Model" button'
                    raise TgisWarning(msg)
            # Check sills
            listMdl = ['sill1','sill2','sill3','sill4']
            for idx,item in enumerate(listMdl):
                if not item in dir(self):
                    msg = 'Sill of component ' + str(idx+1)  + \
                          ' is not defined. Input parameters, ' + \
                          'and press the "Plot Model" button'
                    raise TgisWarning(msg)
            # Check spatial range
            listMdl = ['sptlRnge1','sptlRnge2','sptlRnge3','sptlRnge4']
            for idx,item in enumerate(listMdl):
                if not item in dir(self):
                    msg = 'Spatial range of component ' + str(idx+1)  + \
                          ' is not defined. Input parameters, ' + \
                          'and press the "Plot Model" button'
                    raise TgisWarning(msg)
            # Check temporal range
            listMdl = ['tempRnge1','tempRnge2','tempRnge3','tempRnge4']
            for idx,item in enumerate(listMdl):
                if not item in dir(self):
                    msg = 'Temporal range of  component ' + str(idx+1)  + \
                          ' is not defined. Input parameters, ' + \
                          'and press the "Plot Model" button'
                    raise TgisWarning(msg)

            if not self.flgNewSession:
                # Get value in param file
                numCovStOld = self.fileParam['numCovSt']
                sill1Old = self.fileParam['sill1']
                sptlRnge1Old = self.fileParam['sptlRnge1']
                tempRnge1Old = self.fileParam['tempRnge1']
                sill2Old = self.fileParam['sill2']
                sptlRnge2Old = self.fileParam['sptlRnge2']
                tempRnge2Old = self.fileParam['tempRnge2']
                sill3Old = self.fileParam['sill3']
                sptlRnge3Old = self.fileParam['sptlRnge3']
                tempRnge3Old = self.fileParam['tempRnge3']
                sill4Old = self.fileParam['sill4']
                sptlRnge4Old = self.fileParam['sptlRnge4']
                tempRnge4Old = self.fileParam['tempRnge4']
                covMdl1Old = self.fileParam['covMdl1']
                covMdl2Old = self.fileParam['covMdl2']
                covMdl3Old = self.fileParam['covMdl3']
                covMdl4Old = self.fileParam['covMdl4']
                # Compare values
                if (self.numCovSt != numCovStOld) or \
                   (self.sill1 != sill1Old) or \
                   (self.sptlRnge1 != sptlRnge1Old) or \
                   (self.tempRnge1 != tempRnge1Old) or \
                   (self.sill2 != sill2Old) or \
                   (self.sptlRnge2 != sptlRnge2Old) or \
                   (self.tempRnge2 != tempRnge2Old) or \
                   (self.sill3 != sill3Old) or \
                   (self.sptlRnge3 != sptlRnge3Old) or \
                   (self.tempRnge3 != tempRnge3Old) or \
                   (self.sill4 != sill4Old) or \
                   (self.sptlRnge4 != sptlRnge4Old) or \
                   (self.tempRnge4 != tempRnge4Old) or \
                   (self.covMdl1 != covMdl1Old) or \
                   (self.covMdl2 != covMdl2Old) or \
                   (self.covMdl3 != covMdl3Old) or \
                   (self.covMdl4 != covMdl4Old):
                    self.flgNewSession = True
                if self.flgNewSession:
                    msgQuestion = 'Parameter has changed.'
                    msgOpt1 = 'Delete all parameters and estimation files ' + \
                              'and continue using new parameters'
                    msgOpt2 = 'Quit Application and use different working space'
                    flgParamChg = self.selectMethod(msgQuestion,msgOpt1,msgOpt2)
                    if flgParamChg:
                        self.delEstFiles(self.paramFileHeader,extResStatFile)
                        self.delEstFiles(self.paramFileHeader,extResMapFile)
                    else:
                        raise DummyError
            if self.flgNewSession:
                # Store parameter
                self.fileParam['numCovSt'] = self.numCovSt
                self.fileParam['sptlLag'] = self.sptlLag
                self.fileParam['sptlLagTol'] = self.sptlLagTol
                self.fileParam['sptlCovariance'] = self.sptlCovariance
                self.fileParam['tempLag'] = self.tempLag
                self.fileParam['tempLagTol'] = self.tempLagTol
                self.fileParam['tempCovariance'] = self.tempCovariance
                self.fileParam['numSptlLag'] = self.numSptlLag
                self.fileParam['numTempLag'] = self.numTempLag
                self.fileParam['sill1'] = self.sill1
                self.fileParam['sptlRnge1'] = self.sptlRnge1
                self.fileParam['tempRnge1'] = self.tempRnge1
                self.fileParam['sill2'] = self.sill2
                self.fileParam['sptlRnge2'] = self.sptlRnge2
                self.fileParam['tempRnge2'] = self.tempRnge2
                self.fileParam['sill3'] = self.sill3
                self.fileParam['sptlRnge3'] = self.sptlRnge3
                self.fileParam['tempRnge3'] = self.tempRnge3
                self.fileParam['sill4'] = self.sill4
                self.fileParam['sptlRnge4'] = self.sptlRnge4
                self.fileParam['tempRnge4'] = self.tempRnge4
                self.fileParam['covMdl1'] = self.covMdl1
                self.fileParam['covMdl2'] = self.covMdl2
                self.fileParam['covMdl3'] = self.covMdl3
                self.fileParam['covMdl4'] = self.covMdl4

            # Change cursor to None
            self.lowWindow5.set_cursor(None)
            self.box5.hide()

            # Set box6 object tree
            windowName="box6"
            self.numCurScreen = 6
            self.windowTree6 = gtk.glade.XML(gladeFile,windowName)
            # Create window and low window objects
            self.box6 = self.windowTree6.get_widget(windowName)
            self.lowWindow6 = self.box6.window
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get widget objects
            self.entMaxSptlDist1 = self.windowTree6.\
                                   get_widget('entMaxSptlDist1')
            self.entMaxTempDist1 = self.windowTree6.\
                                   get_widget('entMaxTempDist1')
            self.entSTMetric1 = self.windowTree6.get_widget('entSTMetric1')
            self.entMaxDataPts1 = self.windowTree6.get_widget('entMaxDataPts1')
            self.entMaxSoftPts1 = self.windowTree6.get_widget('entMaxSoftPts1')
            self.cmbEstId = self.windowTree6.get_widget('cmbEstId')
            self.entEstSysId = self.windowTree6.get_widget('entEstSysId')
            self.entEstTime = self.windowTree6.get_widget('entEstTime')
            self.entMaxSptlDist2 = self.windowTree6.\
                                   get_widget('entMaxSptlDist2')
            self.entMaxTempDist2 = self.windowTree6.\
                                   get_widget('entMaxTempDist2')
            self.entSTMetric2 = self.windowTree6.get_widget('entSTMetric2')
            self.entMaxDataPts2 = self.windowTree6.get_widget('entMaxDataPts2')
            self.entMaxSoftPts2 = self.windowTree6.get_widget('entMaxSoftPts2')
            self.noteBMEEst = self.windowTree6.get_widget('noteBMEEst')
            self.noteBMEMap = self.windowTree6.get_widget('noteBMEMap')
            self.noteBMEStat = self.windowTree6.get_widget('noteBMEStat')
            butPlotMap = self.windowTree6.get_widget('butPlotMap')
            butPlotStation = self.windowTree6.get_widget('butPlotStation')
            butQuit6 = self.windowTree6.get_widget('butQuit6')
            butBack6 = self.windowTree6.get_widget('butBack6')
            butShowMap = self.windowTree6.get_widget('butShowMap')
            butDelMap = self.windowTree6.get_widget('butDelMap')
            butShowStat = self.windowTree6.get_widget('butShowStat')
            butDelStat = self.windowTree6.get_widget('butDelStat')
            self.butCreatePoint = self.windowTree6.get_widget('butCreatePoint')
            self.butCreateRaster = self.windowTree6.get_widget('butCreateRaster')
            self.butCloseTab = self.windowTree6.get_widget('butCloseTab')
            self.tvMap = self.windowTree6.get_widget('tvMap')
            self.tvStation = self.windowTree6.get_widget('tvStation')
            self.vboxMapBase = self.windowTree6.get_widget('vboxMapBase')
            self.hboxBMEMap = self.windowTree6.get_widget('hboxBMEMap')
            self.entNumEstPtsX = self.windowTree6.get_widget('entNumEstPtsX')
            self.entNumEstPtsY = self.windowTree6.get_widget('entNumEstPtsY')
            self.chkInclDataPts = self.windowTree6.get_widget('chkInclDataPts')
            self.chkInclVoronoi = self.windowTree6.get_widget('chkInclVoronoi')

            #--------------  Add CHECK BUTTON for mask (P Jat; June 2011)
            self.chkInclMask = self.windowTree6.get_widget('chkInclMask')
            self.lblMaskFile = self.windowTree6.get_widget('lblMaskFile')######            
            #default values (P Jat; June, 2011)
            self.chkInclMask.set_active(False)
            self.inclMask =0            
            self.chkInclMask.connect('clicked',self.getMaskData,True)

            #--------------  Add CHECK BUTTON for map (P Jat; June 2011)
            self.chkInclMap = self.windowTree6.get_widget('chkInclMap')
            #default values (P Jat; June, 2011)
            self.chkInclMap.set_active(False)
            self.inclMap =0
            self.chkInclMap.connect('clicked',self.getMapData,True)

################# Closed (options were as MENU but letter changed as "Combo Box Entry"--
######            #------- Extra functionality (Color, Grid, Help)
######            self.menubar1 = self.windowTree6.get_widget('menubar1')
######            #self.menuitem11 = self.windowTree1.get_widget('menuitem11')
######            #self.menuitem11.connect('activate',self.goBox2)
######            self.color1 = self.windowTree6.get_widget('color1')
######            self.jet2 = self.windowTree6.get_widget('jet2')
######            self.jet2.connect('activate',self.jetColor)
######            
######
######            self.hsv2 = self.windowTree6.get_widget('hsv2')
######            self.hsv2.connect('activate',self.hsvColor)
######            
######            self.hot2 = self.windowTree6.get_widget('hot2')
######            self.hot2.connect('activate',self.hotColor)
######
######            self.cool2 = self.windowTree6.get_widget('cool2')
######            self.cool2.connect('activate',self.coolColor)
######
######            self.spring2 = self.windowTree6.get_widget('spring2')
######            self.spring2.connect('activate',self.springColor)
######
######            self.summer2 = self.windowTree6.get_widget('summer2')
######            self.summer2.connect('activate',self.summerColor)
######
######            self.gray2 = self.windowTree6.get_widget('gray2')
######            self.gray2.connect('activate',self.grayColor)
######
######
######            #----------- Grid (Yes/No) on the map --------
######            self.grid1 = self.windowTree6.get_widget('grid1')
######            
######            self.on2 = self.windowTree6.get_widget('on2')
######            self.on2.connect('activate',self.onGrid)
######
######            self.off2 = self.windowTree6.get_widget('off2')
######            self.off2.connect('activate',self.offGrid)
######
######
######      #----------- Help  --------------------------
            self.help1 = self.windowTree6.get_widget('help1')
            
            self.bmegui_manual2 = self.windowTree6.get_widget('bmegui_manual2')
            #self.OpenManual = os.startfile('Matplotlib.pdf')
            self.bmegui_manual2.connect('activate',self.BMEGUIManual)

            self.bmegui_home_page2 = self.windowTree6.get_widget('bmegui_home_page2')
            #self.bmegui_home_page1.connect('activate',self.goBox5)
            self.bmegui_home_page2.connect('activate',self.HomePage)

            self.contact2 = self.windowTree6.get_widget('contact2')
            self.contact2.connect('activate',self.HomePage)










            self.comboboxColor = self.windowTree6.get_widget('comboboxColor')
            self.comboboxColor.connect('changed', self.changed_color)
            self.comboboxColor.set_active(0)
            
            self.comboboxGrid  = self.windowTree6.get_widget('comboboxGrid')
            self.comboboxGrid.connect('changed', self.changed_grid)
            self.comboboxGrid.set_active(0)

#############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


























            
       
###############################################################################################################
###############################################################################################################
###############################################################################################################
            self.entEstLimW = self.windowTree6.get_widget('entEstLimW')
            self.entEstLimE = self.windowTree6.get_widget('entEstLimE')
            self.entEstLimN = self.windowTree6.get_widget('entEstLimN')
            self.entEstLimS = self.windowTree6.get_widget('entEstLimS')

            # P Jat: Added these line (if river net is analysing diasble Voronoi
            if os.path.isfile('stdPathRT.txt')==True:           # P Jat
                self.chkInclVoronoi.set_sensitive(0)            # P Jat
                self.entEstLimW.set_sensitive(0)                # P Jat
                self.entEstLimE.set_sensitive(0)                # P Jat
                self.entEstLimN.set_sensitive(0)                # P Jat
                self.entEstLimS.set_sensitive(0)                # P Jat
                self.entNumEstPtsX.set_sensitive(0)             # P Jat
                self.entNumEstPtsY.set_sensitive(0)             # P Jat
            else:
                print 'River networ is not being analyzed'

                
            self.entNumDispPtsX = self.windowTree6.get_widget('entNumDispPtsX')
            self.entNumDispPtsY = self.windowTree6.get_widget('entNumDispPtsY')
            self.entBMEEstFrom = self.windowTree6.get_widget('entBMEEstFrom')
            self.lblBMEEstFrom = self.windowTree6.get_widget('lblBMEEstFrom')
            self.entBMEEstTo = self.windowTree6.get_widget('entBMEEstTo')
            self.lblBMEEstTo = self.windowTree6.get_widget('lblBMEEstTo')
            self.entScale = self.windowTree6.get_widget('entScale')
            self.cmbOrder1 = self.windowTree6.get_widget('cmbOrder1')
            self.cmbOrder2 = self.windowTree6.get_widget('cmbOrder2')
            self.butDispNan = self.windowTree6.get_widget('butDispNan')
            self.imgDispNan = self.windowTree6.get_widget('imgDispNan')
            self.lblDispNan = self.windowTree6.get_widget('lblDispNan')

            # Set default values
            self.noteBMEStat.set_scrollable(True)
            self.noteBMEMap.set_scrollable(True)

#################################################################################
#################################################################################
############### Remove ArcGIS related options ###################################
## --------Original Code ------------------------------------           
##            if self.flgStdAlone:
##                self.butCreateRaster.set_sensitive(0)
##                self.butCreatePoint.set_sensitive(0)
## --------Original Code ------------------------------------

            #---------- Modified code ----------------------
            print self.flgStdAlone
            if self.flgStdAlone:
                self.butCreateRaster.set_sensitive(1)
                self.butCreatePoint.set_sensitive(1)
            #-----------------------------------------------
                
            self.butDispNan.set_sensitive(0)
            # Create ListStore objects
            self.lsMap = gtk.ListStore(str,str)
            self.lsStation = gtk.ListStore(str,str)
            # Create TreeView objects for map
            self.tvMap.set_model(self.lsMap)
            strTitle = 'Plot ID'
            self.tvcMap = gtk.TreeViewColumn(strTitle)
            self.tvMap.append_column(self.tvcMap)
            strTitle = 'Estimation Time'
            self.tvcMap2 = gtk.TreeViewColumn(strTitle)
            self.tvMap.append_column(self.tvcMap2)
            self.cellMap = gtk.CellRendererText()
            self.tvcMap.pack_start(self.cellMap, True)
            self.tvcMap.add_attribute(self.cellMap, 'text', 0)
            self.tvcMap2.pack_start(self.cellMap, True)
            self.tvcMap2.add_attribute(self.cellMap, 'text', 1)
            # Create TreeView objects for station plot
            self.tvStation.set_model(self.lsStation)
            strTitle = 'Plot ID'
            self.tvcStation = gtk.TreeViewColumn(strTitle)
            self.tvStation.append_column(self.tvcStation)
            strTitle = 'Station ID'
            self.tvcStation2 = gtk.TreeViewColumn(strTitle)
            self.tvStation.append_column(self.tvcStation2)
            self.cellStation = gtk.CellRendererText()
            self.tvcStation.pack_start(self.cellStation, True)
            self.tvcStation.add_attribute(self.cellStation, 'text', 0)
            self.tvcStation2.pack_start(self.cellStation, True)
            self.tvcStation2.add_attribute(self.cellStation, 'text', 1)
            # Set data fields
            lstResStatNum = self.chkResultFile(self.paramFileHeader,\
                                               extResStatFile)
            for item in lstResStatNum:
                resStatFileName = self.workingSpace + \
                    '\\' + self.paramFileHeader + item + extResStatFile
                resFile = shelve.open(resStatFileName,'r')
                sysId = resFile['stationId']
                resFile.close()
                self.lsStation.append([item,self.uniqueId[sysId-1]])
            # Set data fields
            lstResMapNum = self.chkResultFile(self.paramFileHeader,
                                              extResMapFile)
            for item in lstResMapNum:
                resMapFileName = self.workingSpace + \
                    '\\' + self.paramFileHeader + item + extResMapFile
                resFile = shelve.open(resMapFileName,'r')
                estTime = resFile['estTime']
                resFile.close()
                self.lsMap.append([item,str(estTime)])
            # Calculate default BME estimation parameters
            maxDataPtsVal = self.userConst['defMaxDataPts']
            maxSoftPtsVal = self.userConst['defMaxSoftPts']
            numEstPtsX = self.userConst['defNumEstPtsX']
            numEstPtsY = self.userConst['defNumEstPtsY']
            maxSptlDist = sqrt((max(self.sptlGridX) - min(self.sptlGridX))**2 +\
                               (max(self.sptlGridY) - min(self.sptlGridY))**2)
            maxTempDist = max(self.tempGrid) - min(self.tempGrid)
            # Calculate S/T metric based on sills and ranges
            weightBase = self.sill1 + self.sill2 + self.sill3 + self.sill4
            weightVal1 = self.sill1/weightBase
            weightVal2 = self.sill2/weightBase
            weightVal3 = self.sill3/weightBase
            weightVal4 = self.sill4/weightBase
            stMetricVal = (weightVal1 * self.sptlRnge1 + \
                           weightVal2 * self.sptlRnge2 + \
                           weightVal3 * self.sptlRnge3 + \
                           weightVal4 * self.sptlRnge4)/ \
                          (weightVal1 * self.tempRnge1 + \
                           weightVal2 * self.tempRnge2 + \
                           weightVal3 * self.tempRnge3 + \
                           weightVal4 * self.tempRnge4)
            # Set BME estimation parameters
            self.entMaxSptlDist1.set_text(fpformat.fix(maxSptlDist,numParamDeci))
            self.entMaxTempDist1.set_text(fpformat.fix(maxTempDist,numParamDeci))
            self.entSTMetric1.set_text(fpformat.fix(stMetricVal,numParamDeci))
            self.entMaxDataPts1.set_text(str(maxDataPtsVal))
            self.entMaxSoftPts1.set_text(str(maxSoftPtsVal))
            self.cmbOrder1.set_active(0)
            self.entMaxSptlDist2.set_text(fpformat.fix(maxSptlDist,numParamDeci))
            self.entMaxTempDist2.set_text(fpformat.fix(maxTempDist,numParamDeci))
            self.entSTMetric2.set_text(fpformat.fix(stMetricVal,numParamDeci))
            self.entMaxDataPts2.set_text(str(maxDataPtsVal))
            self.entMaxSoftPts2.set_text(str(maxSoftPtsVal))
            self.cmbOrder2.set_active(0)
            self.lblBMEEstFrom.set_text(self.tempUnit)
            self.lblBMEEstTo.set_text(self.tempUnit)
            self.entBMEEstFrom.set_text(str(min(self.objRawData.getT())))
            self.entBMEEstTo.set_text(str(max(self.objRawData.getT())))
            self.entScale.set_text(str(self.userConst['defScale']))
            # Calculate default estimation grid range
            defXlimEst = calcGraphBuff(min(self.sptlGridX),max(self.sptlGridX))
            defYlimEst = calcGraphBuff(min(self.sptlGridY),max(self.sptlGridY))

            ############################################################################################################################
            ############################################################################################################################
            ############################################ P Jat (June 2011) #################################################################
            ############################################################################################################################
            print self.incMask                      
            #if os.path.exists('maskX.txt'):
            if self.incMask ==1:
                print '============================================================'
                print self.incMask
                print 'Yes mask file exist'
                
                x =[]
                y =[]
                file = open("maskX.txt")
                for line in file.xreadlines():
                    x.append(float(line.rstrip('\n')))
                file.close()

                file = open("maskY.txt")
                for line in file.xreadlines():
                    y.append(float(line.rstrip('\n')))
                file.close()
                self.entEstLimE.set_text(fpformat.fix(max(x),numParamDeci))
                self.entEstLimW.set_text(fpformat.fix(min(x),numParamDeci))
                self.entEstLimN.set_text(fpformat.fix(max(y),numParamDeci))
                self.entEstLimS.set_text(fpformat.fix(min(y),numParamDeci))
            else:
                self.entEstLimE.set_text(fpformat.fix(defXlimEst[1],numParamDeci))
                self.entEstLimW.set_text(fpformat.fix(defXlimEst[0],numParamDeci))
                self.entEstLimN.set_text(fpformat.fix(defYlimEst[1],numParamDeci))
                self.entEstLimS.set_text(fpformat.fix(defYlimEst[0],numParamDeci))
                print 'Mask file does not exist'
            ############################################################################################################################
            ############################################################################################################################
            ############################################ P Jat (June 2011) #################################################################
            ############################################################################################################################               
##            # Set estimation parameter
##            self.entEstLimE.set_text(fpformat.fix(defXlimEst[1],numParamDeci)) ####### Original code 
##            self.entEstLimW.set_text(fpformat.fix(defXlimEst[0],numParamDeci)) ####### Original code 
##            self.entEstLimN.set_text(fpformat.fix(defYlimEst[1],numParamDeci)) ####### Original code 
##            self.entEstLimS.set_text(fpformat.fix(defYlimEst[0],numParamDeci)) ####### Original code 
            self.entNumEstPtsX.set_text(str(numEstPtsX))
            self.entNumEstPtsY.set_text(str(numEstPtsY))
            self.entNumDispPtsX.set_text(str(self.userConst['defNumDispPtsX']))
            self.entNumDispPtsY.set_text(str(self.userConst['defNumDispPtsY']))
            # Create list for plotted maps ans station plot
            self.plotedMapId = []
            self.plotedNaNId_M = []
            self.plotedNaNId_V = []
            self.plotedStationId = []

            # Create ID list for combo box
            self.lsEstId = gtk.ListStore(str)
            for idx,item in enumerate(self.uniqueId):
                strItem = item + '(' + str(self.uniqueSysId[idx]) + ')'
                self.lsEstId.append([strItem])
            numItemId = self.lsEstId.get_iter_first()
            # Set ID combo box
            self.cmbEstId.set_model(self.lsEstId)
            self.cellEst = gtk.CellRendererText()
            self.cmbEstId.pack_start(self.cellEst,True)
            self.cmbEstId.set_active_iter(numItemId)
            # Get system Id from list
            self.entEstSysId.set_text\
                 (str(self.uniqueSysId[self.cmbEstId.get_active()]))

            # Create data files for BME estimation
            createBMEDataFiles(self.objRawData,self.sptlGridX,self.sptlGridY,\
                              self.sptlSmMean,self.tempGrid,self.tempSmMean)
            # Map event handlar
            self.box6.connect('destroy',self.justQuit)
            butQuit6.connect('clicked',self.askQuit)
            self.tvStation.connect('cursor-changed',self.selectStPlot)
            self.tvMap.connect('cursor-changed',self.selectMapPlot)
            butPlotMap.connect('clicked',self.plotBMEMap)
            butPlotStation.connect('clicked',self.plotBMEStation)
            self.cmbEstId.connect('changed',self.chgEstId)
            self.entEstSysId.connect('changed',self.chgEstSysId)
            butShowMap.connect('clicked',self.showBMEMap)
            butDelMap.connect('clicked',self.delBMEMap)
            butShowStat.connect('clicked',self.showBMEStation)
            butDelStat.connect('clicked',self.delBMEStation)
            self.butCloseTab.connect('clicked',self.closeTab)
            self.butCreatePoint.connect('clicked',self.createBMEPtLayer)
            self.butCreateRaster.connect('clicked',self.createBMERasterLayer)
            butBack6.connect('clicked',self.backScreen)
            self.butDispNan.connect('clicked',self.dispNan)
            self.noteBMEMap.connect('switch_page',self.chgMapTab)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except DummyError:
            # Close param file
            self.fileParam.close()
            # Quit application
            gtk.main_quit()

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def chgEstId(self,widget):
        self.entEstSysId.set_text\
             (str(self.uniqueSysId[self.cmbEstId.get_active()]))

    def chgEstSysId(self,widget):
        try:
            # Get system ID
            if self.entEstSysId.get_text() == "":
                raise DummyError
            try:
                sysId = int(self.entEstSysId.get_text())
            except:
                raise TgisWarning(errMsg0601)
            if (sysId < 0) or (sysId > len(self.uniqueId)):
                raise TgisWarning(errMsg0602)
            # Set combobox item
            self.cmbEstId.set_active(sysId-1)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except DummyError:
            pass

    def chkResultFile(self,strHeader,strExt):
        featureList = os.listdir(self.workingSpace)
        numList = []
        for item in featureList:
            if (item[0:len(strHeader)] == strHeader) and \
               (item[-(len(strExt)):] == strExt):
                numList.append(item[len(strHeader):-(len(strExt))])
        retList = []
        if len(numList) == 0:
            return retList
        else:
            for item in numList:
                if len(item) == numFileNumDeci:
                    try:
                        int(item)
                        retList.append(item)
                    except:
                        pass
                else:
                    pass
            return retList

    def delEstFiles(self,strHeader,strExt):
        featureList = os.listdir(self.workingSpace)
        numList = []
        itemList = []
        for item in featureList:
            if (item[0:len(strHeader)] == strHeader) and \
               (item[-(len(strExt)):] == strExt):
                numList.append(item[len(strHeader):-(len(strExt))])
                itemList.append(item)
        if not len(numList) == 0:
            for idx,item in enumerate(numList):
                if len(item) == numFileNumDeci:
                    try:
                        int(item)
                        os.remove(os.path.join(self.workingSpace,itemList[idx]))
                    except:
                        pass

    def selectMapPlot(self,widget):
        """Extract BME estimation parameters"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            treeselection = self.tvMap.get_selection()
            (model, numCol) = treeselection.get_selected()
            plotId = self.lsMap.get_value(numCol,0)
            # Get BME estimation parameters
            resMapFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResMapFile
            resFile = shelve.open(resMapFileName,'r')
            estTime = resFile['estTime']
            maxSptlDist = resFile['maxSptlDist']
            maxTempDist = resFile['maxTempDist']
            stMetricVal = resFile['stMetric']
            maxDataPtsVal = resFile['maxDataPts']
            maxSoftPtsVal = resFile['maxSoftPts']
            estLimE = resFile['estLimE']
            estLimW = resFile['estLimW']
            estLimN = resFile['estLimN']
            estLimS = resFile['estLimS']
            numEstPtsX = resFile['numEstPtsX']
            numEstPtsY = resFile['numEstPtsY']
            inclDataPts = resFile['inclDataPts']
            inclVoronoi = resFile['inclVoronoi']
            numDispPtsX = resFile['numDispPtsX']
            numDispPtsY = resFile['numDispPtsY']
            numOrder = resFile['numOrder']
            resFile.close()
            # Set BME estimation parameters
            self.entMaxSptlDist1.set_text(str(maxSptlDist))
            self.entMaxTempDist1.set_text(str(maxTempDist))
            self.entSTMetric1.set_text(str(stMetricVal))
            self.entMaxDataPts1.set_text(str(maxDataPtsVal))
            self.entMaxSoftPts1.set_text(str(maxSoftPtsVal))
            self.entEstTime.set_text(str(estTime))
            self.entEstLimE.set_text(str(estLimE))
            self.entEstLimW.set_text(str(estLimW))
            self.entEstLimN.set_text(str(estLimN))
            self.entEstLimS.set_text(str(estLimS))
            self.entNumEstPtsX.set_text(str(numEstPtsX))
            self.entNumEstPtsY.set_text(str(numEstPtsY))
            self.chkInclDataPts.set_active(inclDataPts)
            self.chkInclVoronoi.set_active(inclVoronoi)

            ###self.chkInclMask.set_active(inclMask)            
            ###############################################################################           
            self.entNumDispPtsX.set_text(str(numDispPtsX))
            self.entNumDispPtsY.set_text(str(numDispPtsY))
            self.cmbOrder1.set_active(numOrder)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def showBMEMap(self,widget):
        """Show selected map"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            try:
                treeselection = self.tvMap.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsMap.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            if int(plotId) in self.plotedMapId:
                raise TgisWarning('Plot ID: ' + str(plotId) + \
                                      ' is already plotted.')
            # Get BME estimation parameters
            resMapFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResMapFile
            resFile = shelve.open(resMapFileName,'r')
            estTime = resFile['estTime']
            estPtsX = resFile['estPtsX']
            estPtsY = resFile['estPtsY']
            estMean = resFile['estMean']
            estVar = resFile['estVar']
            smPtsX = resFile['smPtsX']
            smPtsY = resFile['smPtsY']
            smMean = resFile['smMean']
            smVar = resFile['smVar']
            nanPtsX = resFile['nanPtsX']
            nanPtsY = resFile['nanPtsY']
            resFile.close()
            # Create new tab and plot BME mean estimates
            strEstTime = 'Plot ID: ' + str(plotId) + '(Mean)'
            lblEstTime = gtk.Label(strEstTime)
            areaBMEMap = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEMap,plotBMEMap] = createFigObj()
            [cavBMEMap,tbBMEMap] = createCavObj(figBMEMap,self.box6)
            # Plot BME mean estimates
            xlabelString = self.sptlUnit
            ylabelString = self.sptlUnit

            if self.flgLogTran:
                unitString = "(log-" + self.dataUnit + ")"
            else:
                unitString = "(" + self.dataUnit + ")"

            titleString = 'BME Mean Estimates on ' + str(estTime) + \
                          ' ' + self.tempUnit + ' ' + unitString
            setPlotLabels(plotBMEMap,xlabelString,\
                          ylabelString,titleString,self.GridStat)
            # Set color map
            setColorMap(self.userConst['strColorMap'])
            # Resize BME mean estimates
            [meshX,meshY] = pylabmesh(array(unique(smPtsX).tolist()),
                                      array(unique(smPtsY).tolist()))
            meshMean = transpose(resize(smMean,(unique(smPtsX).shape[0],\
                                                unique(smPtsY).shape[0])))
            meshVar = transpose(resize(smVar,(unique(smPtsX).shape[0],\
                                              unique(smPtsY).shape[0])))
            # Set vmin and vmax
            if len(unique(smMean).tolist()) == 1:
                if float(smMean[0]) == 0.0:
                    minVal = -1.0
                    maxVal = 1.0
                else:
                    minVal = float(smMean[0]) - float(abs(smMean[0])/5)
                    maxVal = float(smMean[0]) + float(abs(smMean[0])/5)
            else:
                minVal = float(min(smMean))
                maxVal = float(max(smMean))

            # Plot map            
            estmap = plotBMEMap.pcolor(meshX,meshY,meshMean,cmap=get_cmap(self.textColor),shading='interp',\
                                       vmin=minVal,vmax=maxVal)
            ###figBMEMap.colorbar(mappable = estmap)            
            plotBMEMap.hold(True)
            plotBMEMap.plot(nanPtsX,nanPtsY,'ko',markersize=10)
            if len(nanPtsX) != 0:
                self.plotedNaNId_M.append(int(plotId))            
            # Set xlim and ylim
            plotBMEMap.set_xlim([min(smPtsX),max(smPtsX)])
            plotBMEMap.set_ylim([min(smPtsY),max(smPtsY)])
            [cavBMEMap,tbBMEMap] = \
               plotGraph(figBMEMap,plotBMEMap,\
                         cavBMEMap,tbBMEMap,\
                         areaBMEMap,self.box6)            
            areaBMEMap.show()
            lblEstTime.show()
            self.noteBMEMap.append_page(areaBMEMap,lblEstTime)

            # Create new tab and plot BME error variance
            strEstTime = 'Plot ID: ' + str(plotId) + '(Error)'
            lblEstTime = gtk.Label(strEstTime)
            areaBMEErr = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEErr,plotBMEErr] = createFigObj()
            [cavBMEErr,tbBMEErr] = createCavObj(figBMEErr,self.box6)
            # Plot BME mean estimates
            xlabelString = self.sptlUnit
            ylabelString = self.sptlUnit
            titleString = 'BME Error Variance on ' + str(estTime) + \
                          ' ' + self.tempUnit + ' ' + unitString
            setPlotLabels(plotBMEErr,xlabelString,\
                          ylabelString,titleString,self.GridStat)
            
                        
            # Set color map
            setColorMap(self.userConst['strColorMap'])
            # Set vmin and vmax
            if len(unique(smVar).tolist()) == 1:
                if float(smVar[0]) == 0.0:
                    minVal = -1.0
                    maxVal = 1.0
                else:
                    minVal = float(smVar[0]) - float(abs(smVar[0])/5)
                    maxVal = float(smVar[0]) + float(abs(smVar[0])/5)
            else:
                minVal = float(min(smVar))
                maxVal = float(max(smVar))
            # Plot map 
            varmap = plotBMEErr.pcolor(meshX,meshY,meshVar,cmap=get_cmap(self.textColor),shading='interp',\
                                       vmin=minVal,vmax=maxVal)
            ###figBMEErr.colorbar(mappable = varmap)           
            plotBMEErr.hold(True)
            plotBMEErr.plot(nanPtsX,nanPtsY,'ko',markersize=10)
            if len(nanPtsX) != 0:
                self.plotedNaNId_V.append(int(plotId))
            # Set xlim and ylim
            plotBMEErr.set_xlim([min(smPtsX),max(smPtsX)])
            plotBMEErr.set_ylim([min(smPtsY),max(smPtsY)])
            [cavBMEErr,tbBMEErr] = \
               plotGraph(figBMEErr,plotBMEErr,\
                         cavBMEErr,tbBMEErr,\
                         areaBMEErr,self.box6)            
            areaBMEErr.show()
            lblEstTime.show()
            self.noteBMEMap.append_page(areaBMEErr,lblEstTime)

           

            # Add plot ID
            self.plotedMapId.append(int(plotId))

            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def delBMEMap(self,widget):
        """Delete selected station plot"""
        try:
            # Get plot ID
            try:
                treeselection = self.tvMap.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsMap.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            # Get BME estimation parameters
            resMapFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResMapFile

            # Set ask dialog object tree
            dialogName = "diagAsk"
            self.askDialogTree = gtk.glade.XML(gladeFile,dialogName)
            # Get widget objects
            self.askDialog = \
                self.askDialogTree.get_widget(dialogName)
            lblAskMsg = self.askDialogTree.get_widget('lblAskMsg')
            lblAskMsg.set_text('Following plot will be deleted.\n' + \
                               'Plot Id: ' + str(plotId) + '\n' + \
                               'Do you want to continue?')
            # Display ask dialog
            bolResp = self.askDialog.run()
            if bolResp == gtk.RESPONSE_CANCEL:
                # Destroy ask dialog
                self.askDialog.destroy()
            elif bolResp == gtk.RESPONSE_OK:
                # Remove plot ID from TreeView
                self.lsMap.remove(numCol)
                # Delete estimation file
                os.remove(resMapFileName)
                # Remove plot ID
                if int(plotId) in self.plotedMapId:
                    self.plotedMapId.remove(int(plotId))
                if int(plotId) in self.plotedNaNId_M:
                    self.plotedNaNId_M.remove(int(newNumber))
                if int(plotId) in self.plotedNaNId_V:
                    self.plotedNaNId_V.remove(int(newNumber))

                # Remove tab
                numPages = self.noteBMEMap.get_n_pages()
                numPages = numPages - 1
                numRemvPage = []
                for i in range(numPages):
                    pageOnNote = self.noteBMEMap.get_nth_page(i+1)
                    strPageTab = \
                        self.noteBMEMap.get_tab_label_text(pageOnNote)
                    plotIdOnTab = string.split(strPageTab,':')
                    plotIdOnTab = string.split(plotIdOnTab[1],'(')
                    plotIdOnTab = int(string.strip(plotIdOnTab[0]))
                    if plotIdOnTab == int(plotId):
                        numRemvPage.append(i+1)
                for i in range(len(numRemvPage)):
                    self.noteBMEMap.remove_page(numRemvPage[i]-i)
                # Destroy ask dialog
                self.askDialog.destroy()

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def selectStPlot(self,widget):
        """Extract BME estimation parameters"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            try:
                treeselection = self.tvStation.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsStation.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            # Get BME estimation parameters
            resStatFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResStatFile
            resFile = shelve.open(resStatFileName,'r')
            maxSptlDist = resFile['maxSptlDist']
            maxTempDist = resFile['maxTempDist']
            stMetricVal = resFile['stMetric']
            maxDataPtsVal = resFile['maxDataPts']
            maxSoftPtsVal = resFile['maxSoftPts']
            stationId = resFile['stationId']
            estFrom = resFile['estFrom']
            estTo = resFile['estTo']
            scaleVal = resFile['scaleVal']
            numOrder = resFile['numOrder']
            resFile.close()
            # Set BME estimation parameters
            self.entMaxSptlDist2.set_text(str(maxSptlDist))
            self.entMaxTempDist2.set_text(str(maxTempDist))
            self.entSTMetric2.set_text(str(stMetricVal))
            self.entMaxDataPts2.set_text(str(maxDataPtsVal))
            self.entMaxSoftPts2.set_text(str(maxSoftPtsVal))
            self.entEstSysId.set_text(str(stationId))
            self.entBMEEstFrom.set_text(str(estFrom))
            self.entBMEEstTo.set_text(str(estTo))
            self.entScale.set_text(str(scaleVal))
            self.cmbOrder2.set_active(numOrder)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def showBMEStation(self,widget):
        """Show selected station plot"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            try:
                treeselection = self.tvStation.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsStation.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            if int(plotId) in self.plotedStationId:
                raise TgisWarning('Plot ID: ' + str(plotId) + \
                                      ' is already plotted.')
            try:
                scaleVal = float(self.entScale.get_text())
            except:
                raise TgisWarning(errMsg0603)
            if scaleVal <= 0.0:
                raise TgisWarning(errMsg0604)
            # Get BME estimation parameters
            resStatFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResStatFile
            resFile = shelve.open(resStatFileName,'r')
            curSysId = resFile['stationId']
            estPtsT = resFile['estPtsT']
            estMean = resFile['estMean']
            estVar = resFile['estVar']
            dataTime = resFile['dataTime']
            dataLimi = resFile['dataLimi']
            dataProb = resFile['dataProb']
            resFile.close()

            idxSysId = find(self.objHardData.getSysIdAsAry().\
                            flatten() == int(curSysId)).tolist()
            if self.flgLogTran:
                aryVal = array(logTran(self.objHardData.getVal2(),\
                               self.logZeroType,self.logZeroVal))[:,newaxis]
            else:
                aryVal = array(self.objHardData.getVal2())[:,newaxis]
            estTime = take(self.objHardData.getTAsAry(),idxSysId)
            estType = take(self.objHardData.getVal1AsAry(),idxSysId)
            estVal = take(aryVal,idxSysId)
            idxHardId = find(estType.flatten()==0.0).tolist()
            estHardTime = take(estTime,idxHardId)
            estHardVal = take(estVal,idxHardId)
            idxSoftIntId = find(estType.flatten()==1.0).tolist()
            estSoftIntTime = take(estTime,idxSoftIntId)
            estSoftIntVal = take(estVal,idxSoftIntId)
            idxSoftGauId = find(estType.flatten()==2.0).tolist()
            estSoftGauTime = take(estTime,idxSoftGauId)
            estSoftGauVal = take(estVal,idxSoftGauId)
            # Create upper and lower CI
            sqrtVar = map(lambda x: sqrt(x), estVar)
            upperCI = (array(estMean) + array(sqrtVar)).tolist()
            lowerCI = (array(estMean) - array(sqrtVar)).tolist()

            # Create new tab and plot BME mean estimates
            strStatId = 'Plot ID: ' + str(plotId)
            lblStatId = gtk.Label(strStatId)
            areaBMEStat = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEStat,plotBMEStat] = createFigObj()
            [cavBMEStat,tbBMEStat] = createCavObj(figBMEStat,self.box6)
            # Temp ylim
            tempYlimMin = []
            tempYlimMax = []
            tempYlimMin.append(min(lowerCI))
            tempYlimMax.append(max(upperCI))
            # Plot BME mean estimates
            xlabelString = self.tempUnit
            if self.flgLogTran:
                ylabelString = 'log-' + self.agentName + ' (' + \
                               'log-' + self.dataUnit + ')'
            else:
                ylabelString = self.agentName + ' (' + self.dataUnit + ')'
            titleString = 'BME Mean Estimate at Station ' + \
                          str(self.uniqueId[curSysId-1])
            setPlotLabels(plotBMEStat,xlabelString,\
                          ylabelString,titleString,self.GridStat)
            # Plot BME mean estimates
            plotBMEStat.plot(estPtsT,estMean,'-b',\
                             label = 'BME Mean Estimate')
            plotBMEStat.hold(True)
            plotBMEStat.plot(estPtsT,upperCI,'--g')
            plotBMEStat.plot(estPtsT,lowerCI,'--g',label = '68% CI')
            if len(estHardVal) != 0:
                plotBMEStat.plot(estHardTime,estHardVal,'bo',markersize=10)
            if len(estSoftIntVal) != 0:
                plotBMEStat.plot(estSoftIntTime.tolist(),\
                                 estSoftIntVal.tolist(),'r^',markersize=10)
            if len(estSoftGauVal) != 0:
                plotBMEStat.plot(estSoftGauTime,estSoftGauVal,'rs',markersize=10)
            # Plot data
            if len(dataTime) != 0:
                for idx,item in enumerate(dataTime):
                    idxDataProb = find(array(dataProb[idx])!=0.0).tolist()
                    valDataProb = take(array(dataProb[idx]),idxDataProb).tolist()
                    valDataLimi = take(array(dataLimi[idx]),idxDataProb).tolist()
                    valDataProb = map(lambda x: x*scaleVal,valDataProb)
                    tempYlimMin.append(min(valDataLimi))
                    tempYlimMax.append(max(valDataLimi))
                    if max(valDataProb)==min(valDataProb):
                        bufLen = (max(estPtsT) - min(estPtsT))/self.userConst['defBufLen']
                        plotBMEStat.plot([item,item+bufLen],\
                             [min(valDataLimi),min(valDataLimi)],'-k')
                        plotBMEStat.plot([item,item+bufLen],\
                             [max(valDataLimi),max(valDataLimi)],'-k')
                        plotBMEStat.plot([item+bufLen,item+bufLen],\
                             [min(valDataLimi),max(valDataLimi)],'-k')
                    else:
                        plotBMEStat.plot(map(lambda x: x+item,valDataProb),\
                                         valDataLimi,'-k')
            # Plot quality standard
            if self.userConst['flgQstdPlot']:
                if self.flgLogTran:
                    plotBMEStat.plot([min(estPtsT),max(estPtsT)],\
                                     [log(self.userConst['qstdVal']),\
                                      log(self.userConst['qstdVal'])],'-k')
                    tempYlimMin.append(log(self.userConst['qstdVal']))
                    tempYlimMax.append(log(self.userConst['qstdVal']))
                else:
                    plotBMEStat.plot([min(estPtsT),max(estPtsT)],\
                                     [self.userConst['qstdVal'],\
                                      self.userConst['qstdVal']],'-k')
                    tempYlimMin.append(self.userConst['qstdVal'])
                    tempYlimMax.append(self.userConst['qstdVal'])
            # Create xlim and ylim
            self.defXlimBMEStat = calcGraphBuff(min(estPtsT),max(estPtsT))
            self.defYlimBMEStat = calcGraphBuff(min(tempYlimMin),max(tempYlimMax))
            # Set xlim and ylim
            plotBMEStat.set_xlim(self.defXlimBMEStat)
            plotBMEStat.set_ylim(self.defYlimBMEStat)
            [cavBMEStat,tbBMEStat] = \
               plotGraph(figBMEStat,plotBMEStat,\
                         cavBMEStat,tbBMEStat,\
                         areaBMEStat,self.box6)
            areaBMEStat.show()
            lblStatId.show()
            self.noteBMEStat.append_page(areaBMEStat,lblStatId)
            # Add plot ID
            self.plotedStationId.append(int(plotId))
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def delBMEStation(self,widget):
        """Delete selected station plot"""
        try:
            # Get station ID
            try:
                treeselection = self.tvStation.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsStation.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            # Create estimation file name
            dataStationFileName = self.workingSpace + '\\' + \
                                  self.paramFileHeader + \
                                  plotId + '.yse'
            # Set ask dialog object tree
            dialogName = "diagAsk"
            self.askDialogTree = gtk.glade.XML(gladeFile,dialogName)
            # Get widget objects
            self.askDialog = \
                self.askDialogTree.get_widget(dialogName)
            lblAskMsg = self.askDialogTree.get_widget('lblAskMsg')
            lblAskMsg.set_text('Following plot will be deleted.\n' + \
                               'Plot Id: ' + str(plotId) + '\n' + \
                               'Do you want to continue?')
            # Display ask dialog
            bolResp = self.askDialog.run()
            if bolResp == gtk.RESPONSE_CANCEL:
                # Destroy ask dialog
                self.askDialog.destroy()
            elif bolResp == gtk.RESPONSE_OK:
                # Remove plot ID from TreeView
                self.lsStation.remove(numCol)
                # Delete estimation file
                os.remove(dataStationFileName)
                # Remove plot ID
                if int(plotId) in self.plotedStationId:
                    self.plotedStationId.remove(int(plotId))
                # Remove tab
                numPages = self.noteBMEStat.get_n_pages()
                numPages = numPages - 1
                numRemvPage = []
                for i in range(numPages):
                    pageOnNote = self.noteBMEStat.get_nth_page(i+1)
                    strPageTab = \
                        self.noteBMEStat.get_tab_label_text(pageOnNote)
                    plotIdOnTab = string.split(strPageTab,':')
                    plotIdOnTab = int(string.strip(plotIdOnTab[1]))
                    if plotIdOnTab == int(plotId):
                        numRemvPage.append(i+1)
                for i in range(len(numRemvPage)):
                    self.noteBMEStat.remove_page(numRemvPage[i])
                # Destroy ask dialog
                self.askDialog.destroy()

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def plotBMEMap(self,widget):
        """Plot BME estimation map"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get estimation time
            try:
                estTime = float(self.entEstTime.get_text())
            except:
                raise TgisWarning(errMsg0605)
            # Create parameters
            try:
                maxSptlDist = float(self.entMaxSptlDist1.get_text())
                maxTempDist = float(self.entMaxTempDist1.get_text())
                stMetric = float(self.entSTMetric1.get_text())
                maxDataPts = int(self.entMaxDataPts1.get_text())
                maxSoftPts = int(self.entMaxSoftPts1.get_text())
                numEstPtsX = int(self.entNumEstPtsX.get_text())
                numEstPtsY = int(self.entNumEstPtsY.get_text())
                estLimE = float(self.entEstLimE.get_text())
                estLimW = float(self.entEstLimW.get_text())
                estLimN = float(self.entEstLimN.get_text())
                estLimS = float(self.entEstLimS.get_text())
                numDispPtsX = int(self.entNumDispPtsX.get_text())
                numDispPtsY = int(self.entNumDispPtsY.get_text())
                numOrder = self.cmbOrder1.get_active()
            except:
                raise TgisWarning(errMsg0606)
            if estLimE <= estLimW:
                raise TgisWarning(errMsg0607)
            if estLimN <= estLimS:
                raise TgisWarning(errMsg0608)
            if self.chkInclDataPts.get_active():
                inclDataPts = 1
            else:
                inclDataPts = 0
            if self.chkInclVoronoi.get_active():
                inclVoronoi = 1
            else:
                inclVoronoi = 0

            #------------ For Mask ----------------------------
            if self.chkInclMask.get_active():                
                #self.chkInclMask.set_sensitive(0)
                #self.lblMaskFile.set_sensitive(1)
                self.inclMask = 1
            else:                 
                self.inclMask = 0
            #------------ For Mask ----------------------------

            #------------ For Map ----------------------------
            if self.chkInclMap.get_active():                
                self.inclMap = 1
            else:                 
                self.inclMap = 0
            #------------ For Mask ----------------------------                
            ###########################################################################
                
            # Get log zero settings
            if self.logZeroType == 0:
                aryVal = array(self.objAveHardBkup.getVal2())
                detecLimit = min(take(aryVal,find(aryVal>0.0)))
                zeroVal = detecLimit/self.logZeroVal
            elif self.logZeroType == 1:
                zeroVal = self.logZeroVal
            # Create order value
            if numOrder == 0:
                orderVal = 99
            elif numOrder == 1:
                orderVal = 0
            elif numOrder == 2:
                orderVal = 1
            elif numOrder == 3:
                orderVal = 2
            # Create parameter string
            paramString = str(estTime) + ' ' + \
                          str(maxSptlDist) + ' ' + \
                          str(maxTempDist) + ' ' + \
                          str(stMetric) + ' ' + \
                          str(maxDataPts) + ' ' + \
                          str(maxSoftPts) + ' ' +  \
                          str(numEstPtsX) + ' ' +  \
                          str(numEstPtsY) + ' ' +  \
                          str(estLimE) + ' ' +  \
                          str(estLimW) + ' ' +  \
                          str(estLimN) + ' ' +  \
                          str(estLimS) + ' ' +  \
                          str(inclDataPts) + ' ' +  \
                          str(inclVoronoi) + ' ' +  \
                          str(numDispPtsX) + ' ' +  \
                          str(numDispPtsY) + ' ' +  \
                          self.covMdl1 + ' ' + \
                          str(self.sill1) + ' ' + \
                          str(self.sptlRnge1) + ' ' + \
                          str(self.tempRnge1) + ' ' + \
                          self.covMdl2 + ' ' + \
                          str(self.sill2) + ' ' + \
                          str(self.sptlRnge2) + ' ' + \
                          str(self.tempRnge2) + ' ' + \
                          self.covMdl3 + ' ' + \
                          str(self.sill3) + ' ' + \
                          str(self.sptlRnge3) + ' ' + \
                          str(self.tempRnge3) + ' ' + \
                          self.covMdl4 + ' ' + \
                          str(self.sill4) + ' ' + \
                          str(self.sptlRnge4) + ' ' + \
                          str(self.tempRnge4) + ' ' + \
                          str(zeroVal) + ' ' + \
                          str(self.flgLogTran) + ' ' + \
                          str(self.flgRemvMean) + ' ' + \
                          str(self.userConst['op1Val']) + ' ' + \
                          str(self.userConst['op3Val']) + ' ' + \
                          str(self.userConst['op4Val']) + ' ' + \
                          str(self.userConst['op8Val']) + ' ' + \
                          str(orderVal)
            # Create BME estimation files
            [estPtsX,estPtsY,estMean,estVar,smPtsX,smPtsY,\
             smMean,smVar,nanPtsX,nanPtsY] = \
                     bmeEstMap(paramString)
            # Check NaN value
            if len(nanPtsX) != 0:                
                msg = 'BME estimation failed at ' + str(len(nanPtsX)) + \
                      ' out of ' + str(len(estPtsX)) + ' estimation points.'
                self.dispRegMsg(msg)
            # Set data fields
            lstResMapNum = self.chkResultFile(self.paramFileHeader,
                                              extResMapFile)
            if len(lstResMapNum) == 0:
                newNumber = 1
            else:
                newNumber = max(map(lambda x:int(x),lstResMapNum)) + 1
            cmdFormat = "%0" + str(numFileNumDeci) + "i"
            strFileNum = cmdFormat %newNumber
            if len(strFileNum) != numFileNumDeci:
                raise TgisError('Internal Error (File name creation)')
            resMapFileName = self.workingSpace + '\\' + self.paramFileHeader + \
                             strFileNum + extResMapFile
            resFile = shelve.open(resMapFileName,'c')
            resFile['estTime'] = estTime
            resFile['maxSptlDist'] = maxSptlDist
            resFile['maxTempDist'] = maxTempDist
            resFile['stMetric'] = stMetric
            resFile['maxDataPts'] = maxDataPts
            resFile['maxSoftPts'] = maxSoftPts
            resFile['numEstPtsX'] = numEstPtsX
            resFile['numEstPtsY'] = numEstPtsY
            resFile['estLimE'] = estLimE
            resFile['estLimW'] = estLimW
            resFile['estLimN'] = estLimN
            resFile['estLimS'] = estLimS
            resFile['inclDataPts'] = inclDataPts
            resFile['inclVoronoi'] = inclVoronoi
            resFile['numDispPtsX'] = numDispPtsX
            resFile['numDispPtsY'] = numDispPtsY
            resFile['estPtsX'] = estPtsX
            resFile['estPtsY'] = estPtsY
            resFile['estMean'] = estMean
            resFile['estVar'] = estVar
            resFile['smPtsX'] = smPtsX
            resFile['smPtsY'] = smPtsY
            resFile['smMean'] = smMean
            resFile['smVar'] = smVar
            resFile['numOrder'] = numOrder
            resFile['nanPtsX'] = nanPtsX
            resFile['nanPtsY'] = nanPtsY
            resFile.close()
            # Add plot ID
            self.plotedMapId.append(newNumber)
            # Add estimation time on list
            self.lsMap.append([strFileNum,str(estTime)])

            # Create new tab and plot BME mean estimates
            strEstTime = 'Plot ID: ' + strFileNum + '(Mean)'
            lblEstTime = gtk.Label(strEstTime)
            areaBMEMap = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEMap,plotBMEMap] = createFigObj()
            [cavBMEMap,tbBMEMap] = createCavObj(figBMEMap,self.box6)
            # Plot BME mean estimates
            xlabelString = self.sptlUnit
            ylabelString = self.sptlUnit

            if self.flgLogTran:
                unitString = "(log-" + self.dataUnit + ")"
            else:
                unitString = "(" + self.dataUnit + ")"

            titleString = 'BME Mean Estimates on ' + str(estTime) + \
                          ' ' + self.tempUnit + ' ' + unitString
            setPlotLabels(plotBMEMap,xlabelString,\
                          ylabelString,titleString,self.GridStat)


       
            # Set color map
            setColorMap(self.userConst['strColorMap'])
            # Resize BME mean estimates
            [meshX,meshY] = pylabmesh(array(unique(smPtsX).tolist()),
                                      array(unique(smPtsY).tolist()))
            meshMean = transpose(resize(smMean,(unique(smPtsX).shape[0],\
                                                unique(smPtsY).shape[0])))
            meshVar = transpose(resize(smVar,(unique(smPtsX).shape[0],\
                                              unique(smPtsY).shape[0])))
            # Set vmin and vmax
            if len(unique(smMean).tolist()) == 1:
                if float(smMean[0]) == 0.0:
                    minVal = -1.0
                    maxVal = 1.0
                else:
                    minVal = float(smMean[0]) - float(abs(smMean[0])/5)
                    maxVal = float(smMean[0]) + float(abs(smMean[0])/5)
            else:
                minVal = float(min(smMean))
                maxVal = float(max(smMean))

            # Plot map            
            estmap = plotBMEMap.pcolor(meshX,meshY,meshMean,cmap=get_cmap(self.textColor),shading='interp',\
                                       vmin=minVal,vmax=maxVal)
            ###figBMEMap.colorbar(mappable = estmap)
            plotBMEMap.hold(True)

            ############################################################################################################################
            ############################################################################################################################
            ############################################ P Jat (June 2011) #################################################################
            ############################################################################################################################

            if self.incMap ==1:
                xxmap =[]
                yymap =[]
                file = open("mapX.txt")
                for line in file.xreadlines():
                    dataX =float(line.rstrip('\n')); 
                    if dataX==-9999:        
                        dataXX =None
                        xxmap.append(dataXX)
                    else:
                        dataXX=dataX
                        xxmap.append(dataX)
                file.close()                    

                file = open("mapY.txt")
                for line in file.xreadlines():
                    dataY =float(line.rstrip('\n'));  
                    if dataY==-9999:        
                        dataYY =None
                        yymap.append(dataYY)
                    else:
                        dataYY=dataY
                        yymap.append(dataY)
                file.close()
                plotBMEMap.hold(True)
                plotBMEMap.plot(xxmap,yymap,color='black',lw=2)
                figure(11)
                plot(xxmap,yymap,color='black',lw=2)
                show()               
                
                print 'I am here at line= 5535'
            else:
                print 'Do not plot map'

            #if os.path.exists('maskX.txt'):
            if self.incMask ==1:                         
                x =[]
                y =[]
                file = open("maskX.txt")
                for line in file.xreadlines():
                    x.append(float(line.rstrip('\n')))
                file.close()

                file = open("maskY.txt")
                for line in file.xreadlines():
                    y.append(float(line.rstrip('\n')))
                file.close()
                plotBMEMap.fill(x,y,'w', alpha=0)                
            else:
                print #'Mask file does not exist'
                
            titleString = 'BME Mean Estimates on ' + str(estTime) + \
                          ' ' + self.tempUnit + ' ' + unitString
            setPlotLabels(plotBMEMap,xlabelString,\
                          ylabelString,titleString,self.GridStat)



               
            ############################################################################################################################
            ############################################################################################################################
            ####################################################P Jat (June 2011)#########################################################
            ############################################################################################################################
            plotBMEMap.plot(nanPtsX,nanPtsY,'ko',markersize=10)
            if len(nanPtsX) != 0:
                self.plotedNaNId_M.append(int(newNumber))

            print ' Set limit of map here by min and max, X, Y'
            print min(smPtsX)
            print max(smPtsX)
            print min(smPtsY)
            print max(smPtsY)

            # Set xlim and ylim
            if self.incMap ==1:
                xxmap =[]
                yymap =[]
                file = open("mapX.txt")
                for line in file.xreadlines():
                    dataX =float(line.rstrip('\n')); 
                    if dataX==-9999:        
                        dataXX =None
                        xxmap.append(dataXX)
                    else:
                        dataXX=dataX
                        xxmap.append(dataX)
                file.close()                    

                file = open("mapY.txt")
                for line in file.xreadlines():
                    dataY =float(line.rstrip('\n'));  
                    if dataY==-9999:        
                        dataYY =None
                        yymap.append(dataYY)
                    else:
                        dataYY=dataY
                        yymap.append(dataY)
                file.close()


##                plotBMEMap.set_xlim([min(xxmap),max(xxmap)])
##                plotBMEMap.set_ylim([min(yymap),max(yymap)])
            else:
                print 'wqdqwdqwdwqqqqqqqqqqqqqqqqqqqqqqq'
##                plotBMEMap.set_xlim([min(smPtsX),max(smPtsX)])
##                plotBMEMap.set_ylim([min(smPtsY),max(smPtsY)])
                
##            # Set xlim and ylim
##            plotBMEMap.set_xlim([min(smPtsX),max(smPtsX)])
##            plotBMEMap.set_ylim([min(smPtsY),max(smPtsY)])
                
            [cavBMEMap,tbBMEMap] = \
               plotGraph(figBMEMap,plotBMEMap,\
                         cavBMEMap,tbBMEMap,\
                         areaBMEMap,self.box6)
            areaBMEMap.show()
            lblEstTime.show()
            self.noteBMEMap.append_page(areaBMEMap,lblEstTime)

            # Create new tab and plot BME error variance
            strEstTime = 'Plot ID: ' + strFileNum + '(Error)'
            lblEstTime = gtk.Label(strEstTime)
            areaBMEErr = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEErr,plotBMEErr] = createFigObj()
            [cavBMEErr,tbBMEErr] = createCavObj(figBMEErr,self.box6)
            # Plot BME mean estimates
            xlabelString = self.sptlUnit
            ylabelString = self.sptlUnit
            titleString = 'BME Error Variance on ' + str(estTime) + \
                          ' ' + self.tempUnit + ' ' + unitString
##            setPlotLabels(plotBMEErr,xlabelString,\
##                          ylabelString,titleString,self.GridStat)
            # Set color map
            setColorMap(self.userConst['strColorMap'])
            # Set vmin and vmax
            if len(unique(smVar).tolist()) == 1:
                if float(smVar[0]) == 0.0:
                    minVal = -1.0
                    maxVal = 1.0
                else:
                    minVal = float(smVar[0]) - float(abs(smVar[0])/5)
                    maxVal = float(smVar[0]) + float(abs(smVar[0])/5)
            else:
                minVal = float(min(smVar))
                maxVal = float(max(smVar))

            # Plot map           
            varmap = plotBMEErr.pcolor(meshX,meshY,meshVar,cmap=get_cmap(self.textColor),shading='interp',\
                                       vmin=minVal,vmax=maxVal)
            ###figBMEErr.colorbar(mappable = varmap)
            plotBMEErr.hold(True)
            
            ####################################################################################################################
            ##############################################P Jat (June 2011)##################################################
            ####################################################################################################################
            #if os.path.exists('maskY.txt'):
            if self.incMask ==1:
                plotBMEErr.fill(x,y,'w', alpha=0)
            else:
                print #'Mask file does not exist'

            if self.incMap ==1:
                plotBMEErr.plot(xxmap,yymap)
                print ' I am here at line=5625'
            else:
                print #'Map file does not exist'           
            ####################################################################################################################
            ############################################P Jat (June 2011)#################################################
            ####################################################################################################################
            
            plotBMEErr.plot(nanPtsX,nanPtsY,'ko',markersize=10)
            if len(nanPtsX) != 0:
                self.plotedNaNId_V.append(int(newNumber))


            print ' Set limit of map here by min and max, X, Y line5623'
            print min(smPtsX)
            print max(smPtsX)
            print min(smPtsY)
            print max(smPtsY)

            # Set xlim and ylim
            if self.incMap ==1:
                xxmap =[]
                yymap =[]
                file = open("mapX.txt")
                for line in file.xreadlines():
                    dataX =float(line.rstrip('\n')); 
                    if dataX==-9999:        
                        dataXX =None
                        xxmap.append(dataXX)
                    else:
                        dataXX=dataX
                        xxmap.append(dataX)
                file.close()                    

                file = open("mapY.txt")
                for line in file.xreadlines():
                    dataY =float(line.rstrip('\n'));  
                    if dataY==-9999:        
                        dataYY =None
                        yymap.append(dataYY)
                    else:
                        dataYY=dataY
                        yymap.append(dataY)
                file.close()


                plotBMEErr.set_xlim([min(xxmap),max(xxmap)])
                plotBMEErr.set_ylim([min(yymap),max(yymap)])
            else:
                plotBMEErr.set_xlim([min(smPtsX),max(smPtsX)])
                plotBMEErr.set_ylim([min(smPtsY),max(smPtsY)])


                

                
##            
##            # Set xlim and ylim
##            plotBMEErr.set_xlim([min(smPtsX),max(smPtsX)])
##            plotBMEErr.set_ylim([min(smPtsY),max(smPtsY)])
            setPlotLabels(plotBMEErr,xlabelString,\
                          ylabelString,titleString,self.GridStat)

            [cavBMEErr,tbBMEErr] = \
               plotGraph(figBMEErr,plotBMEErr,\
                         cavBMEErr,tbBMEErr,\
                         areaBMEErr,self.box6)
            areaBMEErr.show()
            lblEstTime.show()
            self.noteBMEMap.append_page(areaBMEErr,lblEstTime)


            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def plotBMEStation(self,widget):
        """Plot BME estimation at monitoring station"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Plot time series
            curSysId = self.uniqueSysId[self.cmbEstId.get_active()]
            # Create parameters
            try:
                maxSptlDist = float(self.entMaxSptlDist2.get_text())
                maxTempDist = float(self.entMaxTempDist2.get_text())
                stMetric = float(self.entSTMetric2.get_text())
                maxDataPts = int(self.entMaxDataPts2.get_text())
                maxSoftPts = int(self.entMaxSoftPts2.get_text())
                estFrom = float(self.entBMEEstFrom.get_text())
                estTo = float(self.entBMEEstTo.get_text())
                scaleVal = float(self.entScale.get_text())
                numOrder = self.cmbOrder2.get_active()
            except:
                raise TgisWarning(errMsg0606)
            if estTo <= estFrom:
                raise TgisWarning(errMsg0609)
            # Get log zero settings
            if self.logZeroType == 0:
                aryVal = array(self.objAveHardBkup.getVal2())
                detecLimit = min(take(aryVal,find(aryVal>0.0)))
                zeroVal = detecLimit/self.logZeroVal
            elif self.logZeroType == 1:
                zeroVal = self.logZeroVal
            # Create order value
            if numOrder == 0:
                orderVal = 99
            elif numOrder == 1:
                orderVal = 0
            elif numOrder == 2:
                orderVal = 1
            elif numOrder == 3:
                orderVal = 2
            # Create parameter string
            paramString = str(curSysId) + ' ' + \
                          str(estFrom) + ' ' + \
                          str(estTo) + ' ' + \
                          str(maxSptlDist) + ' ' + \
                          str(maxTempDist) + ' ' + \
                          str(stMetric) + ' ' + \
                          str(maxDataPts) + ' ' + \
                          str(maxSoftPts) + ' ' +  \
                          str(self.userConst['defNumEstPtsT']) + ' ' + \
                          str(self.userConst['inclDataPtsT']) + ' ' + \
                          self.covMdl1 + ' ' + \
                          str(self.sill1) + ' ' + \
                          str(self.sptlRnge1) + ' ' + \
                          str(self.tempRnge1) + ' ' + \
                          self.covMdl2 + ' ' + \
                          str(self.sill2) + ' ' + \
                          str(self.sptlRnge2) + ' ' + \
                          str(self.tempRnge2) + ' ' + \
                          self.covMdl3 + ' ' + \
                          str(self.sill3) + ' ' + \
                          str(self.sptlRnge3) + ' ' + \
                          str(self.tempRnge3) + ' ' + \
                          self.covMdl4 + ' ' + \
                          str(self.sill4) + ' ' + \
                          str(self.sptlRnge4) + ' ' + \
                          str(self.tempRnge4) + ' ' + \
                          str(zeroVal) + ' ' + \
                          str(self.flgLogTran) + ' ' + \
                          str(self.flgRemvMean) + ' ' + \
                          str(self.userConst['op1Val']) + ' ' + \
                          str(self.userConst['op3Val']) + ' ' + \
                          str(self.userConst['op4Val']) + ' ' + \
                          str(self.userConst['op8Val']) + ' ' + \
                          str(orderVal)
            idxSysId = find(self.objHardData.getSysIdAsAry().\
                            flatten() == int(curSysId)).tolist()
            if self.flgLogTran:
                aryVal = array(logTran(self.objHardData.getVal2(),\
                               self.logZeroType,self.logZeroVal))[:,newaxis]
            else:
                aryVal = array(self.objHardData.getVal2())[:,newaxis]
            estTime = take(self.objHardData.getTAsAry(),idxSysId)
            estType = take(self.objHardData.getVal1AsAry(),idxSysId)
            estVal = take(aryVal,idxSysId)
            idxHardId = find(estType.flatten()==0.0).tolist()
            estHardTime = take(estTime,idxHardId)
            estHardVal = take(estVal,idxHardId)
            idxSoftIntId = find(estType.flatten()==1.0).tolist()
            estSoftIntTime = take(estTime,idxSoftIntId)
            estSoftIntVal = take(estVal,idxSoftIntId)
            idxSoftGauId = find(estType.flatten()==2.0).tolist()
            estSoftGauTime = take(estTime,idxSoftGauId)
            estSoftGauVal = take(estVal,idxSoftGauId)
            # Create BME estimation files
            [estPtsT,estMean,estVar,dataTime,dataLimi,dataProb] = \
                                                        bmeEstStat(paramString)
            # Create upper and lower CI
            sqrtVar = map(lambda x: sqrt(x), estVar)
            upperCI = (array(estMean) + array(sqrtVar)).tolist()
            lowerCI = (array(estMean) - array(sqrtVar)).tolist()
            # Set data fields
            lstResStatNum = self.chkResultFile(self.paramFileHeader,
                                              extResStatFile)
            if len(lstResStatNum) == 0:
                newNumber = 1
            else:
                newNumber = max(map(lambda x:int(x),lstResStatNum)) + 1
            cmdFormat = "%0" + str(numFileNumDeci) + "i"
            strFileNum = cmdFormat %newNumber
            if len(strFileNum) != numFileNumDeci:
                raise TgisError('Internal Error (File Name Creation)')
            resStatFileName = self.workingSpace + '\\' + self.paramFileHeader + \
                             strFileNum + extResStatFile
            resFile = shelve.open(resStatFileName,'c')
            resFile['stationId'] = curSysId
            resFile['maxSptlDist'] = maxSptlDist
            resFile['maxTempDist'] = maxTempDist
            resFile['stMetric'] = stMetric
            resFile['maxDataPts'] = maxDataPts
            resFile['maxSoftPts'] = maxSoftPts
            resFile['estPtsT'] = estPtsT
            resFile['estMean'] = estMean
            resFile['estVar'] = estVar
            resFile['dataTime'] = dataTime
            resFile['dataLimi'] = dataLimi
            resFile['dataProb'] = dataProb
            resFile['estFrom'] = estFrom
            resFile['estTo'] = estTo
            resFile['scaleVal'] = scaleVal
            resFile['numOrder'] = numOrder
            resFile.close()
            # Add plot ID
            self.plotedStationId.append(newNumber)
            # Add estimation time on list
            self.lsStation.append([strFileNum,self.uniqueId[curSysId-1]])

            # Create new tab and plot BME mean estimates
            strStatId = 'Plot ID: ' + strFileNum
            lblStatId = gtk.Label(strStatId)
            areaBMEStat = gtk.HBox(homogeneous=False, spacing=0)
            # Define canvas object for spatial covariance plot
            [figBMEStat,plotBMEStat] = createFigObj()
            [cavBMEStat,tbBMEStat] = createCavObj(figBMEStat,self.box6)
            # Temp ylim
            tempYlimMin = []
            tempYlimMax = []
            tempYlimMin.append(min(lowerCI))
            tempYlimMax.append(max(upperCI))
            # Plot BME mean estimates
            xlabelString = self.tempUnit
            if self.flgLogTran:
                ylabelString = 'log-' + self.agentName + ' (' + \
                               'log-' + self.dataUnit + ')'
            else:
                ylabelString = self.agentName + ' (' + self.dataUnit + ')'
            titleString = 'BME Mean Estimate at Station ' + \
                          str(self.uniqueId[curSysId-1])
            setPlotLabels(plotBMEStat,xlabelString,\
                          ylabelString,titleString,self.GridStat)            
            plotBMEStat.plot(estPtsT,estMean,'-b',\
                             label = 'BME Mean Estimate')
            plotBMEStat.hold(True)
            plotBMEStat.plot(estPtsT,upperCI,'--g')
            plotBMEStat.plot(estPtsT,lowerCI,'--g',label = '68% CI')
            if len(estHardVal) != 0:
                plotBMEStat.plot(estHardTime,estHardVal,'bo',markersize=10)
            if len(estSoftIntVal) != 0:
                plotBMEStat.plot(estSoftIntTime.tolist(),\
                                 estSoftIntVal.tolist(),'r^',markersize=10)
            if len(estSoftGauVal) != 0:
                plotBMEStat.plot(estSoftGauTime,estSoftGauVal,'rs',markersize=10)
            # Plot data
            if len(dataTime) != 0:
                for idx,item in enumerate(dataTime):
                    idxDataProb = find(array(dataProb[idx])!=0.0).tolist()
                    valDataProb = take(array(dataProb[idx]),idxDataProb).tolist()
                    valDataLimi = take(array(dataLimi[idx]),idxDataProb).tolist()
                    valDataProb = map(lambda x: x*scaleVal,valDataProb)
                    tempYlimMin.append(min(valDataLimi))
                    tempYlimMax.append(max(valDataLimi))
                    if max(valDataProb)==min(valDataProb):
                        bufLen = (max(estPtsT) - min(estPtsT))/self.userConst['defBufLen']
                        plotBMEStat.plot([item,item+bufLen],\
                             [min(valDataLimi),min(valDataLimi)],'-k')
                        plotBMEStat.plot([item,item+bufLen],\
                             [max(valDataLimi),max(valDataLimi)],'-k')
                        plotBMEStat.plot([item+bufLen,item+bufLen],\
                             [min(valDataLimi),max(valDataLimi)],'-k')
                    else:
                        plotBMEStat.plot(map(lambda x: x+item,valDataProb),\
                                         valDataLimi,'-k')
            # Plot quality standard
            if self.userConst['flgQstdPlot']:
                if self.flgLogTran:
                    plotBMEStat.plot([min(estPtsT),max(estPtsT)],\
                                     [log(self.userConst['qstdVal']),\
                                      log(self.userConst['qstdVal'])],'-k')
                    tempYlimMin.append(log(self.userConst['qstdVal']))
                    tempYlimMax.append(log(self.userConst['qstdVal']))
                else:
                    plotBMEStat.plot([min(estPtsT),max(estPtsT)],\
                                     [self.userConst['qstdVal'],\
                                      self.userConst['qstdVal']],'-k')
                    tempYlimMin.append(self.userConst['qstdVal'])
                    tempYlimMax.append(self.userConst['qstdVal'])
            # Create xlim and ylim
            self.defXlimBMEStat = calcGraphBuff(min(estPtsT),max(estPtsT))
            self.defYlimBMEStat = calcGraphBuff(min(tempYlimMin),max(tempYlimMax))
            # Set xlim and ylim
            print ' Set limit of map here by min and max, X, Y: line:5856'
            print min(smPtsX)
            print max(smPtsX)
            print min(smPtsY)
            print max(smPtsY)
            plotBMEStat.set_xlim(self.defXlimBMEStat)
            plotBMEStat.set_ylim(self.defYlimBMEStat)
            [cavBMEStat,tbBMEStat] = \
               plotGraph(figBMEStat,plotBMEStat,\
                         cavBMEStat,tbBMEStat,\
                         areaBMEStat,self.box6)
            areaBMEStat.show()
            lblStatId.show()
            self.noteBMEStat.append_page(areaBMEStat,lblStatId)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def closeTab(self,widget):
        """Close current plot tab"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            if self.noteBMEEst.get_current_page() == 0:
                # Get current tab
                strPageTab = self.noteBMEMap.get_tab_label_text(\
                              self.noteBMEMap.get_nth_page(\
                               self.noteBMEMap.get_current_page()))
                # Remove current tab
                if strPageTab == 'Map List':
                    pass
                else:
                    plotId = string.split(strPageTab,':')
                    plotId = string.split(plotId[1],'(')
                    plotId = int(string.strip(plotId[0]))
                    if plotId in self.plotedMapId:
                        self.plotedMapId.remove(plotId)

                    if plotId in self.plotedNaNId_M:
                        self.plotedNaNId_M.remove(plotId)
                    if plotId in self.plotedNaNId_V:
                        self.plotedNaNId_V.remove(plotId)

                    numPages = self.noteBMEMap.get_n_pages()
                    numPages = numPages - 1
                    numRemvPage = []
                    for i in range(numPages):
                        pageOnNote = self.noteBMEMap.get_nth_page(i+1)
                        strPageTab = \
                            self.noteBMEMap.get_tab_label_text(pageOnNote)
                        plotIdOnTab = string.split(strPageTab,':')
                        plotIdOnTab = string.split(plotIdOnTab[1],'(')
                        plotIdOnTab = int(string.strip(plotIdOnTab[0]))
                        if plotIdOnTab == plotId:
                            numRemvPage.append(i+1)
                    for i in range(len(numRemvPage)):
                        self.noteBMEMap.remove_page(numRemvPage[i]-i)
            else:
                # Get current tab
                strPageTab = self.noteBMEStat.get_tab_label_text(\
                              self.noteBMEStat.get_nth_page(\
                               self.noteBMEStat.get_current_page()))
                # Remove current tab
                if strPageTab == 'Plot List':
                    pass
                else:
                    plotId = string.split(strPageTab,':')
                    plotId = int(string.strip(plotId[1]))
                    if plotId in self.plotedStationId:
                        self.plotedStationId.remove(plotId)
                    self.noteBMEStat.\
                              remove_page(self.noteBMEStat.get_current_page())
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def chgMapTab(self,widget,respage,respage_num):
        """Switch tab to Log-histogram"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get current tab
#             strPageTab = self.noteBMEMap.get_tab_label_text(\
#                           self.noteBMEMap.get_nth_page(\
#                            self.noteBMEMap.get_current_page()))
            strPageTab = self.noteBMEMap.get_tab_label_text(\
                          self.noteBMEMap.get_nth_page(\
                           respage_num))
            # Remove current tab
            if strPageTab == 'Map List':
                self.butDispNan.set_sensitive(0)
            else:
                plotId = string.split(strPageTab,':')
                plotId = string.split(plotId[1],'(')
                if plotId[1].startswith('Mean'):
                    flgMeanTab = True
                else:
                    flgMeanTab = False
                plotId = string.strip(plotId[0])

                # Get BME estimation parameters
                resMapFileName = self.workingSpace + '\\' + \
                                  self.paramFileHeader + plotId + extResMapFile
                resFile = shelve.open(resMapFileName,'r')
                nanPtsX = resFile['nanPtsX']
                resFile.close()

                if len(nanPtsX) == 0:
                    self.butDispNan.set_sensitive(0)
                elif len(nanPtsX) != 0 and flgMeanTab and \
                     int(plotId) in self.plotedNaNId_M:
                    self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                    self.lblDispNan.set_text('Hide Failed Estimation Point')
                    self.butDispNan.set_sensitive(1)
                elif len(nanPtsX) != 0 and flgMeanTab and \
                     not (int(plotId) in self.plotedNaNId_M):
                    self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                    self.lblDispNan.set_text('Display Failed Estimation Point')
                    self.butDispNan.set_sensitive(1)
                elif len(nanPtsX) != 0 and not(flgMeanTab) and \
                     int(plotId) in self.plotedNaNId_V:
                    self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                    self.lblDispNan.set_text('Hide Failed Estimation Point')
                    self.butDispNan.set_sensitive(1)
                elif len(nanPtsX) != 0 and not(flgMeanTab) and \
                     not (int(plotId) in self.plotedNaNId_V):
                    self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                    self.lblDispNan.set_text('Display Failed Estimation Point')
                    self.butDispNan.set_sensitive(1)

            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()


    def dispNan(self,widget):
        """Dispaly position of failed estimation points"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get current tab
            strPageTab = self.noteBMEMap.get_tab_label_text(\
                          self.noteBMEMap.get_nth_page(\
                           self.noteBMEMap.get_current_page()))
            # Remove current tab
            if strPageTab == 'Map List':
                pass
            else:
                plotId = string.split(strPageTab,':')
                plotId = string.split(plotId[1],'(')
                if plotId[1].startswith('Mean'):
                    flgMeanTab = True
                else:
                    flgMeanTab = False
                plotId = string.strip(plotId[0])

                # Get BME estimation parameters
                resMapFileName = self.workingSpace + '\\' + \
                                  self.paramFileHeader + plotId + extResMapFile
                resFile = shelve.open(resMapFileName,'r')
                smPtsX = resFile['smPtsX']
                smPtsY = resFile['smPtsY']
                smMean = resFile['smMean']
                smVar = resFile['smVar']
                nanPtsX = resFile['nanPtsX']
                nanPtsY = resFile['nanPtsY']
                resFile.close()

                xlimVal = [min(smPtsX),max(smPtsX)]
                ylimVal = [min(smPtsY),max(smPtsY)]

                # Resize BME mean estimates
                [meshX,meshY] = pylabmesh(array(unique(smPtsX).tolist()),
                                          array(unique(smPtsY).tolist()))
                meshMean = transpose(resize(smMean,(unique(smPtsX).shape[0],\
                                                    unique(smPtsY).shape[0])))
                meshVar = transpose(resize(smVar,(unique(smPtsX).shape[0],\
                                                  unique(smPtsY).shape[0])))

                chNote = self.noteBMEMap.get_children()
                areaObj = chNote[self.noteBMEMap.get_current_page()]
                chBox = areaObj.get_children()
                cavObj = chBox[0]
                tbObj = chBox[1]
                figObj = cavObj.figure
                chFigure = figObj.get_children()
                plotObj = chFigure[1]
                # Set color map
                setColorMap(self.userConst['strColorMap'])

                if flgMeanTab:
                    if int(plotId) in self.plotedNaNId_M:
                        plotObj.hold(False)
                        # Set vmin and vmax
                        if len(unique(smMean).tolist()) == 1:
                            if float(smMean[0]) == 0.0:
                                minVal = -1.0
                                maxVal = 1.0
                            else:
                                minVal = float(smMean[0]) - float(abs(smMean[0])/5)
                                maxVal = float(smMean[0]) + float(abs(smMean[0])/5)
                        else:
                            minVal = float(min(smMean))
                            maxVal = float(max(smMean))

                        # Plot map
                        estmap = plotObj.pcolor(meshX,meshY,meshMean,cmap=get_cmap(self.textColor),shading='interp',\
                                                   vmin=minVal,vmax=maxVal)
                        
                        #--------------------------P Jat (June, 2011)------------------------
                        #if os.path.exists('maskX.txt'):
                        if self.incMask ==1:
                            print #'Yes mask file exist'            
                            x =[]
                            y =[]
                            file = open("maskX.txt")
                            for line in file.xreadlines():
                                x.append(float(line.rstrip('\n')))
                            file.close()

                            file = open("maskY.txt")
                            for line in file.xreadlines():
                                y.append(float(line.rstrip('\n')))
                            file.close()
                            plotObj.hold(True)
                            plotObj.fill(x,y,'w', alpha=0)
                        else:
                            print #'Mask file does not exist'







                        if self.incMap ==1:
                            xxmap =[]
                            yymap =[]
                            file = open("mapX.txt")
                            for line in file.xreadlines():
                                dataX =float(line.rstrip('\n')); 
                                if dataX==-9999:        
                                    dataXX =None
                                    xxmap.append(dataXX)
                                else:
                                    dataXX=dataX
                                    xxmap.append(dataX)
                            file.close()                    

                            file = open("mapY.txt")
                            for line in file.xreadlines():
                                dataY =float(line.rstrip('\n'));  
                                if dataY==-9999:        
                                    dataYY =None
                                    yymap.append(dataYY)
                                else:
                                    dataYY=dataY
                                    yymap.append(dataY)
                            file.close()
                            plotObj.hold(True)
                            plotBMEMap.plot(xxmap,yymap)
                            print 'I am here at line= 6149'
                        else:
                            print 'Do not plot map'
                            
                        #--------------------------End: P Jat (June, 2011)--------------------

                        
                        self.plotedNaNId_M.remove(int(plotId))
                        self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                        self.lblDispNan.set_text('Display Failed Estimation Point')
                    else:
                        plotObj.hold(True)
                        plotObj.plot(nanPtsX,nanPtsY,'ko',markersize=10)
                        #--------------------------P Jat (June, 2011)------------------------
                        #if os.path.exists('maskX.txt'):
                        if self.incMask ==1:
                            print #'Yes mask file exist'            
                            x =[]
                            y =[]
                            file = open("maskX.txt")
                            for line in file.xreadlines():
                                x.append(float(line.rstrip('\n')))
                            file.close()

                            file = open("maskY.txt")
                            for line in file.xreadlines():
                                y.append(float(line.rstrip('\n')))
                            file.close()
                            plotObj.hold(True)
                            plotObj.fill(x,y,'w', alpha=0)
                        else:
                            print #'Mask file does not exist'


                        if self.incMap ==1:
                            xxmap =[]
                            yymap =[]
                            file = open("mapX.txt")
                            for line in file.xreadlines():
                                dataX =float(line.rstrip('\n')); 
                                if dataX==-9999:        
                                    dataXX =None
                                    xxmap.append(dataXX)
                                else:
                                    dataXX=dataX
                                    xxmap.append(dataX)
                            file.close()                    

                            file = open("mapY.txt")
                            for line in file.xreadlines():
                                dataY =float(line.rstrip('\n'));  
                                if dataY==-9999:        
                                    dataYY =None
                                    yymap.append(dataYY)
                                else:
                                    dataYY=dataY
                                    yymap.append(dataY)
                            file.close()
                            plotObj.hold(True)
                            plotBMEMap.plot(xxmap,yymap)
                            print 'I am here at line= 6215'
                        else:
                            print 'Do not plot map'
                            
                        #--------------------------End: P Jat (June, 2011)--------------------
                        
                        self.plotedNaNId_M.append(int(plotId))
                        self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                        self.lblDispNan.set_text('Hide Failed Estimation Point')
                else:
                    if int(plotId) in self.plotedNaNId_V:
                        plotObj.hold(False)
                        # Set vmin and vmax
                        if len(unique(smVar).tolist()) == 1:
                            if float(smVar[0]) == 0.0:
                                minVal = -1.0
                                maxVal = 1.0
                            else:
                                minVal = float(smVar[0]) - float(abs(smVar[0])/5)
                                maxVal = float(smVar[0]) + float(abs(smVar[0])/5)
                        else:
                            minVal = float(min(smVar))
                            maxVal = float(max(smVar))

                        varmap = plotObj.pcolor(meshX,meshY,meshVar,cmap=get_cmap(self.textColor),shading='interp',\
                                                   vmin=minVal,vmax=maxVal)
                        
                        #--------------------------P Jat (June, 2011)------------------------
                        #if os.path.exists('maskX.txt'):
                        if self.incMask ==1:
                            print #'Yes mask file exist'            
                            x =[]
                            y =[]
                            file = open("maskX.txt")
                            for line in file.xreadlines():
                                x.append(float(line.rstrip('\n')))
                            file.close()

                            file = open("maskY.txt")
                            for line in file.xreadlines():
                                y.append(float(line.rstrip('\n')))
                            file.close()
                            plotObj.hold(True)
                            plotObj.fill(x,y,'w', alpha=0)
                        else:
                            print #'Mask file does not exist'


                        if self.incMap ==1:
                            xxmap =[]
                            yymap =[]
                            file = open("mapX.txt")
                            for line in file.xreadlines():
                                dataX =float(line.rstrip('\n')); 
                                if dataX==-9999:        
                                    dataXX =None
                                    xxmap.append(dataXX)
                                else:
                                    dataXX=dataX
                                    xxmap.append(dataX)
                            file.close()                    

                            file = open("mapY.txt")
                            for line in file.xreadlines():
                                dataY =float(line.rstrip('\n'));  
                                if dataY==-9999:        
                                    dataYY =None
                                    yymap.append(dataYY)
                                else:
                                    dataYY=dataY
                                    yymap.append(dataY)
                            file.close()
                            plotObj.hold(True)
                            plotBMEMap.plot(xxmap,yymap)
                            print 'I am here at line= 6288'
                        else:
                            print 'Do not plot map'


                
                        #--------------------------End: P Jat (June, 2011)--------------------
                            
                        self.plotedNaNId_V.remove(int(plotId))
                        self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                        self.lblDispNan.set_text('Display Failed Estimation Point')
                    else:
                        plotObj.hold(True)
                        plotObj.plot(nanPtsX,nanPtsY,'ko',markersize=10)
                        #--------------------------P Jat (June, 2011)------------------------
                        #if os.path.exists('maskX.txt'):
                        if self.incMask ==1:
                            print #'Yes mask file exist'            
                            x =[]
                            y =[]
                            file = open("maskX.txt")
                            for line in file.xreadlines():
                                x.append(float(line.rstrip('\n')))
                            file.close()

                            file = open("maskY.txt")
                            for line in file.xreadlines():
                                y.append(float(line.rstrip('\n')))
                            file.close()
                            plotObj.hold(True)
                            plotObj.fill(x,y,'w', alpha=0)
                        else:
                            print #'Mask file does not exist'


                        if self.incMap ==1:
                            xxmap =[]
                            yymap =[]
                            file = open("mapX.txt")
                            for line in file.xreadlines():
                                dataX =float(line.rstrip('\n')); 
                                if dataX==-9999:        
                                    dataXX =None
                                    xxmap.append(dataXX)
                                else:
                                    dataXX=dataX
                                    xxmap.append(dataX)
                            file.close()                    

                            file = open("mapY.txt")
                            for line in file.xreadlines():
                                dataY =float(line.rstrip('\n'));  
                                if dataY==-9999:        
                                    dataYY =None
                                    yymap.append(dataYY)
                                else:
                                    dataYY=dataY
                                    yymap.append(dataY)
                            file.close()
                            plotObj.hold(True)
                            plotBMEMap.plot(xxmap,yymap)
                            print 'I am here at line= 6349'
                        else:
                            print 'Do not plot map'
                        #--------------------------End: P Jat (June, 2011)--------------------
                        self.plotedNaNId_V.append(int(plotId))
                        self.imgDispNan.set_from_icon_name('gtk-dialog-warning',gtk.ICON_SIZE_BUTTON)
                        self.lblDispNan.set_text('Hide Failed Estimation Point')

                # Set xlim and ylim
                xlimVal_cur = plotObj.set_xlim(xlimVal)
                ylimVal_cur = plotObj.set_ylim(ylimVal)
                print ' Set limit of map here by min and max, X, Y Line:6350'
                print min(smPtsX)
                print max(smPtsX)
                print min(smPtsY)
                print max(smPtsY)
            
                plotObj.hold(False)
                # Create plot
                [cavObj,tbObj] = \
                   plotGraph(figObj,plotObj,cavObj,tbObj,areaObj,self.box6)

            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def createBMEPtLayer(self,widget):
        """Display select method message"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            try:
                treeselection = self.tvMap.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsMap.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            # Get BME estimation parameters
            resMapFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResMapFile
            resFile = shelve.open(resMapFileName,'r')
            estTime = resFile['estTime']
            estPtsX = resFile['estPtsX']
            estPtsY = resFile['estPtsY']
            estMean = resFile['estMean']
            estVar = resFile['estVar']
            resFile.close()
            # Get parameters
            strHeader = self.userConst['bmePtHeader']
            dataCoord = self.userConst['dataCoord']
            projCoord = self.userConst['projCoord']
            flgDataCoord = self.userConst['flgDataCoord']
            flgProjCoord = self.userConst['flgProjCoord']
            # Get point layer file name
            strExt = ".lyr"
            #lyrFileName = self.chkGPFile(strHeader,strExt)
            lyrFileName = strHeader + plotId + strExt
            # Create output table
            lyrFileHeader = lyrFileName[0:-len(strExt)]
            lyrTableName = self.GP.workspace + "/" + lyrFileHeader
#             # Get table name from workspace
#             tbs = self.GP.ListTables()
#             # Loop through the list of feature classes.
#             tbs.reset()
#             tb = tbs.next()
#             while tb:
#                 if string.lower(tb) == string.lower(lyrFileHeader):
#                     raise DummyError
#                 tb = tbs.next()
            self.GP.CreateTable(self.GP.workspace,lyrFileHeader)
            # Set data fields
            self.GP.AddField(lyrTableName,"X","double")
            self.GP.AddField(lyrTableName,"Y","double")
            self.GP.AddField(lyrTableName,"Mean","double")
            self.GP.AddField(lyrTableName,"Var","double")
            # Insert data values
            curTable = self.GP.InsertCursor(lyrTableName)
            for i in xrange(len(estPtsX)):
                rowTable = curTable.NewRow()
                rowTable.X = estPtsX[i]
                rowTable.Y = estPtsY[i]
                rowTable.Mean = estMean[i]
                rowTable.Var = estVar[i]
                curTable.InsertRow(rowTable)
            del curTable
            # Create and save layer file
            if flgDataCoord:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader,dataCoord)
            else:
                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
                                         lyrFileHeader)
            self.GP.SaveToLayerFile(lyrFileHeader,lyrFileName)
            # Set projection
            if flgProjCoord:
                self.GP.project (lyrFileName, lyrFileHeader + "_proj",projCoord)
            strMsg = "Point layer file (" + lyrFileHeader + \
                     ") has been created"
            self.dispRegMsg(strMsg)
            self.addGPMessage(strMsg,0)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except DummyError:
            # Change cursor to None
            self.lowWindow6.set_cursor(None)
            strMsg = "Point layer file (" + lyrFileHeader + \
                     ") already exists"
            self.dispRegMsg(strMsg)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def createBMERasterLayer(self,widget):
        """Display select method message"""
        try:
            # Change cursor to Watch
            self.lowWindow6.set_cursor(self.cursorWatch)
            # Get plot ID
            try:
                treeselection = self.tvMap.get_selection()
                (model, numCol) = treeselection.get_selected()
                plotId = self.lsMap.get_value(numCol,0)
            except:
                raise TgisWarning('Select Plot ID from the list.')
            # Get BME estimation parameters
            resMapFileName = self.workingSpace + '\\' + \
                              self.paramFileHeader + plotId + extResMapFile
            resFile = shelve.open(resMapFileName,'r')
            estTime = resFile['estTime']
            estPtsX = resFile['smPtsX']
            estPtsY = resFile['smPtsY']
            estMean = resFile['smMean']
            estVar = resFile['smVar']
            numDispPtsX = resFile['numDispPtsX']
            numDispPtsY = resFile['numDispPtsY']
            resFile.close()
            # Get parameters
            strHeader = self.userConst['bmeRstHeader']
            dataCoord = self.userConst['dataCoord']
            projCoord = self.userConst['projCoord']
            flgDataCoord = self.userConst['flgDataCoord']
            flgProjCoord = self.userConst['flgProjCoord']
            # Get point layer file name
            strExt = ".lyr"
            #lyrFileName = self.chkGPFile(strHeader,strExt)
            lyrFileName = strHeader + plotId + strExt
            # Create output table
            lyrFileHeader = lyrFileName[0:-len(strExt)]

            #---------------------------- Save X, Y, and BME estimated Mean and Var   ( P Jat; June 2011)-------------------
            strHeader ='BME' #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            arcGridFileName = strHeader + plotId
            if os.path.exists('dataX.txt'):
                os.remove('dataX.txt')
            if os.path.exists('dataY.txt'):
                os.remove('dataY.txt')
            if os.path.exists('dataMean.txt'):
                os.remove('dataMean.txt')
            if os.path.exists('dataVar.txt'):
                os.remove('dataVar.txt')                

                
            myfile = open("dataX.txt", "wb")
            for item in estPtsX:
                myfile.write(str(item)+',')               
            myfile.close()
            
            myfile = open("dataY.txt", "wb")
            for item in estPtsY:
                myfile.write(str(item)+',')               
            myfile.close()
            
            myfile = open("dataMean.txt", "wb")
            for item in estMean:
                myfile.write(str(item)+',')               
            myfile.close()
           
            myfile = open("dataVar.txt", "wb")
            for item in estVar:
                myfile.write(str(item)+',')               
            myfile.close()


            colRowN = [numDispPtsY, numDispPtsX]  # X- Column; Y- Rows
            myfile = open("colRowN.txt", "wb")
            for item in colRowN:
                myfile.write(str(item)+',')               
            myfile.close()


            
            if os.path.exists(self.workingSpace +'/Arc_ASCII_files'):
                print 'yes Arc_ASCII_files Directory exist'
            else:
                arcGridDir =os.mkdir(self.workingSpace+ '/Arc_ASCII_files')

                
            arcGridFileName = self.workingSpace +'/Arc_ASCII_files/'+ arcGridFileName            
            ###arcGridFileName =self.workingSpace +'/'+ arcGridFileName
            cdmArcGridWrite = 'arcgridwrite.exe '+ arcGridFileName
            retCode = os.system(cdmArcGridWrite)                  
            
            #-----------------End -------------------------------------------------------------------------------------------



            
#### -------------------------- P Jat Blocked this Section --------------
#####             # Get table name from workspace
#####             tbs = self.GP.ListTables()
#####             # Loop through the list of feature classes.
#####             tbs.reset()
#####             tb = tbs.next()
#####             while tb:
#####                 if string.lower(tb) == string.lower(lyrFileHeader):
#####                     raise DummyError
#####                 tb = tbs.next()
####            lyrTableName = self.GP.workspace + "/" + lyrFileHeader
####            self.GP.CreateTable(self.GP.workspace,lyrFileHeader)
####            # Set data fields
####            self.GP.AddField(lyrTableName,"X","double")
####            self.GP.AddField(lyrTableName,"Y","double")
####            self.GP.AddField(lyrTableName,"Mean","double")
####            self.GP.AddField(lyrTableName,"Var","double")
####            # Insert data values
####            curTable = self.GP.InsertCursor(lyrTableName)
####            for i in xrange(len(estPtsX)):
####                rowTable = curTable.NewRow()
####                rowTable.X = estPtsX[i]
####                rowTable.Y = estPtsY[i]
####                rowTable.Mean = estMean[i]
####                rowTable.Var = estVar[i]
####                curTable.InsertRow(rowTable)
####            del curTable
####            # Create and save layer file
####            if flgDataCoord:
####                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
####                                         lyrFileHeader,dataCoord)
####            else:
####                self.GP.MakeXYEventLayer(lyrTableName,"X","Y",\
####                                         lyrFileHeader)
####            self.GP.SaveToLayerFile(lyrFileHeader,lyrFileName)
####            # Set projection
####            if flgProjCoord:
####                self.GP.project (lyrFileName, lyrFileHeader + "_proj",projCoord)
####            # Create raster files
####            cellSizeX = (max(estPtsX)-min(estPtsX))/numDispPtsX
####            cellSizeY = (max(estPtsY)-min(estPtsY))/numDispPtsY
####            if cellSizeX >= cellSizeY:
####                cellSize = cellSizeX + cellSizeX/20.0
####            else:
####                cellSize = cellSizeY + cellSizeY/20.0
####            rasterName = strHeader + plotId + "m"
####            self.GP.FeatureToRaster(lyrFileHeader,\
####                                    "Mean",rasterName,cellSize)
####            rasterName = strHeader + plotId + "v"
####            self.GP.FeatureToRaster(lyrFileHeader,\
####                                    "Var",rasterName,cellSize)
####        #------ Close this section , now we are using MATLAB written 'arcASCII (.asc)' file generator
####            strMsg = "Point layer file (" + lyrFileHeader + \
####                     ") and raster files (" + strHeader + plotId + "m," + \
####                     strHeader + plotId + "v" + ")have been created"
##
##            strMsg = "Arc ASCII (.asc) files:       " + strHeader + plotId +"Mean.asc                  "+\
##                     strHeader + plotId + "Var.asc                              " + "       have been created"

            strMsg = "Arc ASCII (.asc) files:       " + strHeader + plotId + "MeanArcASCII" + ".asc            "+\
                     strHeader + plotId + "VarArcASCII" +  ".asc                  " + "         have been created"

            self.dispRegMsg(strMsg)
            
###            self.addGPMessage(strMsg,0)
            # Change cursor to None
            self.lowWindow6.set_cursor(None)

        except TgisWarning:
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispRegMsg(str(sys.exc_info()[1]))

        except DummyError:
            # Change cursor to None
            self.lowWindow6.set_cursor(None)
            strMsg = "Point layer file (" + lyrFileHeader + \
                     ") already exists"
            self.dispRegMsg(strMsg)

        except:
            # Change buttons
            self.chgButtons()
            # Change cursor to None
            self.chgCursor()
            # Display message
            self.dispErrMsg()

    def justQuit(self,widget):
        """Close log file before quit application."""
        try:
            # Close param file
            self.fileParam.close()
        except:
            pass
        # Quit application
        gtk.main_quit()

    def backScreen(self,widget):
        """Back to previous screen"""
        if self.numCurScreen == 2:
            self.box2.hide()
            self.box1.show()
            self.numCurScreen = 1
        elif self.numCurScreen == 3:
            self.box3.hide()
            self.objAveHard = copy.deepcopy(self.objAveHardBkup)
            self.box2.show()
            self.numCurScreen = 2
        elif self.numCurScreen == 4:
            self.box4.hide()
            self.box3.show()
            self.numCurScreen = 3
        elif self.numCurScreen == 5:
            self.box5.hide()
            self.box4.show()
            self.numCurScreen = 4
        elif self.numCurScreen == 6:
            self.box6.hide()
            self.box5.show()
            self.numCurScreen = 5
        self.flgNewSession = False

########### Closed (options were as MENU but letter changed as "Combo Box Entry"--
##    def summerColor(self,widget):
##        self.textColor = 'summer'
##        
##
##    def jetColor(self,widget):
##        self.textColor = 'jet'
##        self.jet2.set_sensitive(0)
##
##    def coolColor(self,widget):
##        self.textColor = 'cool'
##
##    def hsvColor(self,widget):
##        self.textColor = 'hsv'
##        
##    def hotColor(self,widget):
##        self.textColor = 'hot'
##
##    def springColor(self,widget):
##        self.textColor = 'spring'
##
##    def grayColor(self,widget):
##        self.textColor = 'gray'
##        
##    def onGrid(self,widget):
##        self.GridStat = True
##
##    def offGrid(self,widget):
##        self.GridStat = False

    def BMEGUIManual(self, widget):
        os.startfile('BMEGUI2.1.1_UserManual_v01.pdf')
           

    def HomePage(self, widget):
        webbrowser.open("http://www.unc.edu/depts/case/BMElab/")
        
  
    def changed_color(self, entry):
        self.textColor=str(entry.get_active_text())        
        print 'I like', self.textColor, 'color'
        return


    def changed_grid(self, entry):
        GridSelection=str(entry.get_active_text())
        if str(GridSelection)=='ON':
            self.GridStat = True
        else:
            self.GridStat = False
        print  self.GridStat
            
            
     
