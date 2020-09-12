import subprocess
import time

NORM_THRESHOLD = 128 #The lightness indicating when to switch back from inverted to normal
INV_THRESHOLD = 135 #The lightness indicating when to invert
INVERT_COMMAND = "xcalib -i -a"
INVERTED = False # Gotta add a state since we can't get the screen color after being inverted
MAIN_COMMAND = "import -silent -thumbnail 1x1 -window root -format '%[fx:int(255*r+.5)] %[fx:int(255*g+.5)] %[fx:int(255*b+.5)]' info:-"
PERIOD = 0.5 # Update time

def get_lightness():
    # This procedure take a screen shot then get its average color
    av_rgb = subprocess.check_output(MAIN_COMMAND, shell=True)
    # Formatting
    av_rgb = [float(i) for i in av_rgb.split()]
    #Calculating
    lightness = (min(av_rgb) + max(av_rgb)) / 2
    return lightness

# while True:
#     lightness = get_lightness()
#     print("This is lightness:", lightness)
#     #We use hysteresis to prevent rapid changes
#     if INVERTED:
#         if lightness < NORM_THRESHOLD:
#             subprocess.call(INVERT_COMMAND.split())
#             INVERTED = False
#     else:
#         if lightness > INV_THRESHOLD:
#             subprocess.call(INVERT_COMMAND.split())
#             INVERTED = True
#     time.sleep(PERIOD)

import tkinter as tk
import pyautogui

root= tk.Tk()

canvas1 = tk.Canvas(root, width = 300, height = 300)
canvas1.pack()

def takeScreenshot ():

    myScreenshot = pyautogui.screenshot()
    myScreenshot.save(r'root.png')

myButton = tk.Button(text='Take Screenshot', command=takeScreenshot, bg='green',fg='white',font= 10)
canvas1.create_window(150, 150, window=myButton)

root.mainloop()
