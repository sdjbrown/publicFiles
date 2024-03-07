#!/usr/bin/env python3

#A program for extracting calibration information from microsope photographs taken with Nikon NIS-Elements

# PLEASE NOTE: Nikon NIS-Elements is a product of the Nikon Corporation Healthcare Business Unit. 
# They are in no way affiliated with, nor do they endorse the use of this software.

# Copyright (C) Samuel D.J. Brown 2024

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    The full text of the  GNU General Public License is available 
#     at <https://www.gnu.org/licenses/>.

#Usage:
# nikonCalib.py -f filename

import sys
from bs4 import BeautifulSoup
from optparse import OptionParser
from PIL import Image
    
parser = OptionParser()
parser.add_option("-a", default=False, action="store_true", dest="all")
parser.add_option("-c", default=True, action="store_true", dest= "calib")
parser.add_option("-s", default=True, action="store_true", dest= "scale_bar")
parser.add_option("-d", default=True, action="store_true", dest= "dimensions")
parser.add_option("-i", default=False, action="store_true", dest= "inkscape")
parser.add_option("-f", action="store", type="string", dest= "filename")

(options, args) = parser.parse_args()


with open(options.filename, "rb") as fin:
	img = fin.read()

#e = img.decode("utf-16", "ignore")
#e_xml = BeautifulSoup(e)

#print(e_xml.prettify())

imgObj = Image.open(options.filename)
imgSize = imgObj.size

f = img.decode("utf-8", "ignore")
xmp_start = f.find('<ReportObjects')
xmp_end = f.find('</ReportObjects>')

if xmp_start < 0:
	sys.exit("File does not include relevant section")

fString = f[xmp_start:xmp_end+16]
f_xml = BeautifulSoup(fString, features="lxml")

tag = f_xml.rlxrograticules_node
calib_out = tag.get('pic-calib', {})
scale_bar = 1000/float(calib_out)

if options.dimensions:
	print("The image width is " + str(imgSize[0]) + " px")
	print("The image height is " + str(imgSize[1]) + " px")

if options.calib:
	print("The calibration for the image is " + calib_out + " um/px")
	
if options.scale_bar:
	print("A 1 mm scale bar should be " + str(scale_bar) + " px long")
	
if options.inkscape:
	decreaseRatio = 768/imgSize[0]
	ink_scale = scale_bar * decreaseRatio * 0.5
	print("A 0.5 mm scale bar in Inkscape should be " + str(ink_scale) + " px long")

if options.all:
	print(f_xml.prettify())



