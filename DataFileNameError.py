"""TGIS tool: TGIS GUI functions
Name: DataFileNameError.py
Last Modified: Oct 26, 2011
Developed by P Jat
"""

###  Import libraries  ###
import sys
import os
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
# Import GTK libraries
import pygtk
import gtk
import gtk.glade


def dispErrMsg(self):
    """Display error message"""
    # Set error message dialog object tree
    errorDialogName = "diagError"
    errorDialogTree = gtk.glade.XML(gladeFile,errorDialogName)
    # Get widget objects
    errorDialog = \
                    errorDialogTree.get_widget(errorDialogName)
    lblErrorMsg = errorDialogTree.get_widget('lblErrorMsg')
    # Set error message
    errMsg = "Error: Data file name starts with numeric character. \n\n Please change data file name, starting with non-numeric character"
    lblErrorMsg.set_text(errMsg)
    # Display error message dialog
    bolResp=errorDialog.run()
    if bolResp==gtk.RESPONSE_OK:
        # Destroy error message dialog
        errorDialog.destroy()
    elif bolResp==gtk.RESPONSE_CLOSE:
        # Close param file
        fileParam.close()
        # Quit application
        gtk.main_quit() 

dispErrMsg('Error')
gtk.main_quit()


