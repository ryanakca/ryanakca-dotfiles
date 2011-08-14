#!/usr/bin/python

import os
import random

WALLPAPER_DIR="/home/ryan/.local/share/wallpapers"

wallpapers = os.listdir(WALLPAPER_DIR)

os.system('feh --bg-center %s' % random.choice(wallpapers)) 
