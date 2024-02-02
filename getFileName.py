#!/usr/bin/env python

import sys
import pygtk
pygtk.require("2.0")
import gtk
import gtk.glade
import gobject
import shelve
import os


class pyWine:
        def __init__(self):              
                save_file_name = self.file_browse(gtk.FILE_CHOOSER_ACTION_SAVE)
                print save_file_name + '.csv'
                      
        def file_browse(self, dialog_action, file_name=""):
                if (dialog_action==gtk.FILE_CHOOSER_ACTION_OPEN):
                        dialog_buttons = (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_OPEN, gtk.RESPONSE_OK)
                else:
                        dialog_buttons = (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_SAVE, gtk.RESPONSE_OK)

                file_dialog = gtk.FileChooserDialog(title="Enter File name", action=dialog_action, buttons=dialog_buttons)

                """set the filename if we are saving"""
                if (dialog_action==gtk.FILE_CHOOSER_ACTION_SAVE):
                        file_dialog.set_current_name(file_name)
                result = ""
                
                if file_dialog.run() == gtk.RESPONSE_OK:
                        result = file_dialog.get_filename()
                file_dialog.destroy()
                return result
        
if __name__ == "__main__":
	wine = pyWine()	
	gtk.main()
