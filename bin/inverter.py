from subprocess import *
from string import *
from time import *

NORM_THRESHOLD = 128 #The lightness indicating when to switch back from inverted to normal
INV_THRESHOLD = 135 #The lightness indicating when to invert
INVERT_COMMAND = "xcalib -i -a"
INVERTED = False # Gotta add a state since we can't get the screen color after being inverted
STABLE_MAIN_COMMAND = "import -thumbnail 2x1 -window root - | convert - -scale 1x1\! -format \'%[fx:int(255*r+.5)] %[fx:int(255*g+.5)] %[fx:int(255*b+.5)]\' info:-"
TEST_MAIN_COMMAND = "import -thumbnail 1x1 -window root -format \'%[fx:int(255*r+.5)] %[fx:int(255*g+.5)] %[fx:int(255*b+.5)]\' info:-"
PERIOD = 0.1 # Update time

def get_lightness():
    # What do we do here? Take a screen shot then get its average color
    av_rgb = check_output(TEST_MAIN_COMMAND, shell=True)

    # Formatting
    av_rgb = [float(i) for i in split(av_rgb)]

    #Calculating
    minimum = min(av_rgb)
    maximum = max(av_rgb)
    lightness = (minimum + maximum) / 2

    return lightness


while True:
    #begin=time()

    lightness = get_lightness()
    #print "This is lightness: " + str(lightness)
    
    #The main evaluation process uses hysteresis (to prevent rapid changes)
    if INVERTED:
        if lightness < NORM_THRESHOLD:
            call(split(INVERT_COMMAND))
            INVERTED = False
    else:
        if lightness > INV_THRESHOLD:
            call(split(INVERT_COMMAND))
            INVERTED = True

    #end=time()
    #print "Time: " + str(end-begin)
    sleep(PERIOD)







