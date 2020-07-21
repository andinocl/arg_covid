# 
# arg_covid_downloads
#
# Downloads daily reports from National MinSalud,
# City of Buenos Aires, Province of Buenos Aires
#
# Requires python3
#


# Select download destination
import subprocess
import sys

def install(package):
	subprocess.check_call([sys.executable, "-m", "pip", "install", package])

import datetime
import time
import os
import shutil
import tempfile
import requests
import urllib.request
import os.path
from tkinter import filedialog
from tkinter import *

root = Tk()
#print("Select a download directory")
#root.filedir = filedialog.askdirectory(initialdir="~/")
root.filedir = "/Users/XXXXX/COVID reports"

print(root.filedir)
# Set up for loop to get data for each file
# @todo build in start date
today=datetime.date.today()
td = today.day
tm = today.month
mos_base_url = "https://www.argentina.gob.ar/sites/default/files/"
caba_base_url = "xx"
pba__base_url = "xx"




for m in range (3,tm+1):
	if m == tm: # don't do a bunch of failed loops if date doesn't exist
		fd = td
	elif m == 1 or m == 3 or m == 5 or m == 7 or m==8 or m==10 or m==12:
		fd = 31
	elif m==4 or m==6 or m==9 or m==30:
		fd = 30
	else:
		fd = 28 # no leap years during this pandemic

	for d in range (1,fd+1):
		
		# First handle national
		this_dir = "minsalud"
		path = os.path.join(root.filedir,this_dir)
		if not os.path.isdir(path):
			os.mkdir(path)

		for i in range (0,2):
			if i==0:
				ampm = "matutino"
				mod_hour = 9
			else:
				ampm = "vespertino"
				mod_hour = 20	
			
			mos_file_name =  '{:02d}'.format(d) + "-" + '{:02d}'.format(m) + "-20-reporte-" + ampm + "-covid-19.pdf"
			mos_url = mos_base_url + mos_file_name
			local_file = os.path.join(path,mos_file_name)
			print(mos_file_name)
			mod_date=datetime.datetime(year=2020, month=m, day=d, hour=mod_hour,minute=0,second=0)
			mod_time=time.mktime(mod_date.timetuple())
			
			# @TODO scrub pdf for data
        	# for now, just download to directory
        	
			if not os.path.isfile(local_file): #write the file if it doesn't yet exist
				r = requests.get(mos_url, stream=True)
				if r.status_code == 200:
					with open(local_file, 'wb') as f:
						f.write(r.content)
					print("Writing " + mos_url + " to " + local_file)
					os.utime(local_file, (mod_time,mod_time)) # will make sorting in directory easier
				else:
					print("No file at: " + mos_url)
			else:
				print("File exists at: " + local_file)




        ## Next Handle PBA

## Next Handle CABA
# Honestly, their data is trash.
# The reports naming logic is somewhat random
# On July 19, the jpg is saved at: 
# https://www.buenosaires.gob.ar/sites/gcaba/files/unnamed_26.jpg
# and there are no previous single-day images available
# setting up to get past weekly reports and then maybe to cron?
#this_dir = "caba"
#path = os.path.join(root.filedir,this_dir)
#if not os.path.isdir(path):
#os.mkdir(path)
      







