#!/usr/bin/env python3

#Usage:
# nikonCalib.py -f filename

import sys
from bs4 import BeautifulSoup
from optparse import OptionParser
    
parser = OptionParser()
parser.add_option("-a", default=False, action="store_true", dest="all")
parser.add_option("-c", default=True, action="store_true", dest= "calib")
parser.add_option("-s", default=True, action="store_true", dest= "scale_bar")
parser.add_option("-f", action="store", type="string", dest= "filename")

(options, args) = parser.parse_args()


with open(options.filename, "rb") as fin:
	img = fin.read()

#e = img.decode("utf-16", "ignore")
#e_xml = BeautifulSoup(e)

#print(e_xml.prettify())

f = img.decode("utf-8", "ignore")
xmp_start = f.find('<ReportObjects')
xmp_end = f.find('</ReportObjects>')

if xmp_start < 0:
	sys.exit("File does not include relevant section")

fString = f[xmp_start:xmp_end+16]
f_xml = BeautifulSoup(fString)

tag = f_xml.rlxrograticules_node
calib_out = tag.get('pic-calib', {})
scale_bar = 1000/float(calib_out)

if options.calib:
	print("The calibration for the image is " + calib_out + " um/px")
	
if options.scale_bar:
	print("A 1 mm scale bar should be " + str(scale_bar) + " px long")


if options.all:
	print(f_xml.prettify())



