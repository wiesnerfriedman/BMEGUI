#!/usr/bin/env python

# example progressbar.py

import pygtk
pygtk.require('2.0')
import gtk, gobject

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
