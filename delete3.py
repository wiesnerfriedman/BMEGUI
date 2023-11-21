#!/usr/bin/env python
import gtk

from pylab import *
from matplotlib.figure import Figure

# uncomment to select /GTK/GTKAgg/GTKCairo
#from matplotlib.backends.backend_gtk import FigureCanvasGTK as FigureCanvas
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas
#from matplotlib.backends.backend_gtkcairo import FigureCanvasGTKCairo as FigureCanvas

# or NavigationToolbar for classic
#from matplotlib.backends.backend_gtk import NavigationToolbar2GTK as NavigationToolbar
from matplotlib.backends.backend_gtkagg import NavigationToolbar2GTKAgg as NavigationToolbar

import sys

class PyApp():
    def __init__(self):
        
        # Create toplevel window
        window11 = gtk.Window(gtk.WINDOW_TOPLEVEL)
        window11.set_title("BMEGUI: River network plot")
        window11.connect("destroy", gtk.main_quit)
        window11.set_default_size(750,450)
        window11.set_size_request(450, 400)
        window11.set_border_width(2)
        window11.set_position(gtk.WIN_POS_CENTER)

        # Create vbox widget for toplevel window
        vbox = gtk.VBox(False) #vbox = gtk.VBox(False, 2)               
        window11.add(vbox)
        
      
             
             
        # Create textbox and append as new notebook page
        self.filename = ""
        self.textbox = gtk.TextView()
        self.textbox.set_wrap_mode(gtk.WRAP_WORD)
        self.textbox.set_editable(True)
        self.textbox.set_cursor_visible(True)        
        self.textbox.set_border_window_size(gtk.TEXT_WINDOW_LEFT,1)
        self.textbox.set_border_window_size(gtk.TEXT_WINDOW_RIGHT,1)
        self.textbox.set_border_window_size(gtk.TEXT_WINDOW_TOP,1)
        self.textbox.set_border_window_size(gtk.TEXT_WINDOW_BOTTOM,1)

        scrolledwindow = gtk.ScrolledWindow()
        scrolledwindow.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scrolledwindow.add(self.textbox)



        # Create notebook and place the position of the tabs
        notebook = gtk.Notebook()
        notebook.set_tab_pos(gtk.POS_RIGHT)
        vbox.pack_start(notebook)
        

        
        fig = Figure(figsize=(5,4), dpi=100)
        ax = fig.add_subplot(111)
        t = arange(0.0,3.0,0.01)
        s = sin(2*pi*t)
        ax.plot(t,s, label="seno")
        legend()
        ax.grid(True)
        canvas = FigureCanvas(fig)  # a gtk.DrawingArea

        #vbox.pack_start(canvas, True, True, 0)
        vbox.pack_start(canvas)
        toolbar = NavigationToolbar(canvas, window11)
        vbox.pack_start(toolbar, False, False)
        
        # Create and append notebook page
        frame = gtk.Frame()
        frame.set_border_width(10)
        frame.set_size_request(100, 75)        
  
        # Show 
        window11.show_all()
#execfile('delete.py')

if __name__ == "__main__":
    PyApp()
    gtk.main()

