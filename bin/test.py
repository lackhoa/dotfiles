from subprocess import *
from string import *
from time import *

INVERT_COMMAND = "xcalib -i -a"
MAIN_COMMAND = "import -thumbnail 2x1 -window root - | convert - -grayscale rec709luma -scale 1x1\! -format \'%[fx:int(255*r+.5)]\' info:-"
PERIOD = 0.1
# Gotta add a state since we can't get the screen color after being inverted
INVERTED = False

def get_lightness():
    # What do we do here? Take a screen shot then get its average color
    av_rgb = check_output(MAIN_COMMAND, shell=True)
    print "The output is: " + av_rgb

    # Formatting
    av_rgb = float(av_rgb)

    return av_rgb


while True:
    start = time()

    lightness = get_lightness()
    print "This is lightness: " + str(lightness)
    if lightness > 128:
        if not INVERTED:
            # call(split(INVERT_COMMAND))
            INVERTED = True
    elif INVERTED:
        # call(split(INVERT_COMMAND))
        INVERTED = False

    end = time()
    print "This is the time: " + str(end - start)

    sleep(PERIOD)







