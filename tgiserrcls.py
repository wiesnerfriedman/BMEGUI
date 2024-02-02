"""TGIS tool: TGIS error class
Name: tgiserrcls.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""


""" Modified by P Jat/ Feb 12, 2009    """


""" Yasu's Original Code -----------------------

class TgisError(Exception):
    def __init__(self, args=None):
        self.args = args

class DummyError(Exception):
    def __init__(self, args=None):
        self.args = args

class TgisWarning:
    def __init__(self, args=None):
        self.args = args

------------------------------------------------"""




#------------------------- P Jat 02/12/2009 ---------------------------

class TgisError(Exception):
    def __init__(self, value):
        self.value = value
        
    def __str__(self):
        return repr(self.value)
    

class DummyError(Exception):
    def __init__(self, value):
        self.value = value
        
    def __str__(self):
        return repr(self.value)
    
    
class TgisWarning:
    def __init__(self, value):
        self.value = value
        
    def __str__(self):
        return repr(self.value)

#----------------------  P Jat ----------------------------------------


