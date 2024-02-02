"""TGIS tool: Stand alone file select
Name: stdalone.py
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

# Import GTK libraries
import pygtk
import gtk
import gtk.glade

from tgisiocls import *
from tgisgen import *
from pylab import *   ##############################
gladeFile = 'stdalone.glade'

class StdAlone:

    def __init__(self):
        # Check output file
        if os.path.exists('stdPath.txt'):
            os.remove('stdPath.txt')
        if os.path.exists('stdPathRT.txt'):   # P Jat (May 11,2011)
            os.remove('stdPathRT.txt')        # P Jat (May 11,2011)
            
        # Create WATCH cursor
        self.cursorWatch = gtk.gdk.Cursor(gtk.gdk.WATCH)
        # Set box1 object tree
        windowName = 'window1'
        self.windowTree1 = gtk.glade.XML(gladeFile,windowName)
        # Create window and low window objects
        self.window1 = self.windowTree1.get_widget(windowName)
        self.lowWindow1 = self.window1.window
        # Change cursor to Watch
        #self.lowWindow1.set_cursor(self.cursorWatch)
        # Get widget objects
        self.entWork = self.windowTree1.get_widget('entWork')
        self.entData = self.windowTree1.get_widget('entData')
        self.butWork = self.windowTree1.get_widget('butWork')
        self.butData = self.windowTree1.get_widget('butData')
        self.entRT   = self.windowTree1.get_widget('entRT')    # P Jat May 11, 2011
        self.butRT   = self.windowTree1.get_widget('butRT')    # P jat May 11, 2011
        self.checkbuttonRiverM   = self.windowTree1.get_widget('checkbuttonRiverM') # P Jat May 11, 2011

        #default values (P Jat; May 11, 2011)
        self.checkbuttonRiverM.set_active(False)    # (P Jat; May 11, 2011)
        useRiverNetwork = "No"                      # (P Jat; May 11, 2011)
        self.entRT.set_sensitive(0)                 # (P Jat; May 11, 2011)
        self.butRT.set_sensitive(0)                 # (P Jat; May 11, 2011)
        self.useRiverNetworkFlag =0 #-------
                  
        
        
        self.butCancel = self.windowTree1.get_widget('butCancel')
        self.butOK = self.windowTree1.get_widget('butOK')
        # Map event handler
        self.butCancel.connect('clicked',self.canVal)
        self.butOK.connect('clicked',self.retVal)
        self.butWork.connect('clicked',self.openFileDiag,self.entWork,True)
        self.butData.connect('clicked',self.openFileDiag,self.entData,False)
        self.butRT.connect('clicked',self.openFileDiag,self.entRT,False)   # P Jat May11,2011
        self.checkbuttonRiverM.connect('toggled',self.getRiverData,1)      # P Jat May11, 2011
        self.window1.connect('destroy',self.canVal)
     
    
    def getRiverData(self, widget,data=None):           # (P Jat; May 11, 2011)
        if widget.get_active():                         # (P Jat; May 11, 2011)
            self.entRT.set_sensitive(1)                 # (P Jat; May 11, 2011)
            self.butRT.set_sensitive(1)                 # (P Jat; May 11, 2011)
            self.useRiverNetworkFlag =1
            
        else:                                           # (P Jat; May 11, 2011)
            self.entRT.set_sensitive(0)                 # (P Jat; May 11, 2011)
            self.butRT.set_sensitive(0)                 # (P Jat; May 11, 2011)
            useRiverNetwork = "No"
            self.useRiverNetworkFlag =0
            
            
    
    def openFileDiag(self,widget,entryBox,flgDir):
        # Set error message dialog object tree
        dialogName = "fileSelect"
        self.dialogTree = gtk.glade.XML(gladeFile,dialogName)
        # Get widget objects
        self.fileSelect = self.dialogTree.get_widget(dialogName)
        if flgDir:
            self.fileSelect.set_action(gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER)
        else:
            self.fileSelect.set_action(gtk.FILE_CHOOSER_ACTION_OPEN)
        # Display error message dialog
        bolResp=self.fileSelect.run()
        if bolResp==gtk.RESPONSE_OK:
            # Get filename
            selectedFile = self.fileSelect.get_filename()
            entryBox.set_text(selectedFile)
            # Destroy error message dialog
            self.fileSelect.destroy()
        elif bolResp==gtk.RESPONSE_CANCEL:
            # Destroy error message dialog
            self.fileSelect.destroy()
            
    def retVal(self,widget):
        try:
            # Check file and directory
            if not os.path.isdir(self.entWork.get_text()):
                raise ValueError('Working directory must be a directory')
            if not os.path.isfile(self.entData.get_text()):
                raise ValueError('Data file must be a file')
            # Create output data
            listData = [self.entWork.get_text(),self.entData.get_text()]
            # Check output file
            if os.path.exists('stdPath.txt'):
                os.remove('stdPath.txt')
            # Create data file
            objDataWriter = DataWriter()
            objDataWriter.openFile('stdPath.txt')
            objDataWriter.writeList(listData)
            objDataWriter.closeFile()
            
            #--------------------- P Jat May 11, 2011            
            # Create output data
            listDataRT = [self.entRT.get_text(),self.entRT.get_text()]
            
            # Check output file
            if os.path.exists('stdPathRT.txt'):
                os.remove('stdPathRT.txt')
            # Create data file
            objDataWriter = DataWriter()
            objDataWriter.openFile('stdPathRT.txt')
            objDataWriter.writeList(listDataRT)
            objDataWriter.closeFile()             
    
            if self.useRiverNetworkFlag:        # P Jat: REMOVE stdPathRT if river network
                print 'YES: Use River Network' # is not using (not checked 'river Network' 
            else:                               # check button while loading data
                if os.path.exists('stdPathRT.txt'):
                    os.remove('stdPathRT.txt')
            #-------------------end P Jat --------------

            gtk.main_quit()
            self.window1.hide()
        except:
            print str(sys.exc_info()[1])
            gtk.main_quit()

    def canVal(self,widget):
        # Check output file
        if os.path.exists('stdPath.txt'):
            os.remove('stdPath.txt')
        gtk.main_quit()

app = StdAlone()
val = gtk.main()
