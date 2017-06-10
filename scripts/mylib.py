from mpl_toolkits.basemap import Basemap
import numpy as np
import matplotlib.pyplot as plt
import re
import urllib
import os
from datetime import datetime

def frange(x, y, jump, digit=2):
	ls=[]
	while x <= y:
		ls.append(x)
		x += jump
	ls2 = [round(x, digit) for x in ls]
	return(ls2)

def parse_fwf(str, widths):
	ls = []
	curpos = 0
	for i in range(len(widths)):
		ls.append(str[curpos:(widths[i] + curpos)])
		curpos = curpos + widths[i]
	return(ls)

def read_ozone_data(filepath):
	with file(filepath) as f: 
		s = f.read()
	# find out format of data from the first 3 lines\
	if re.search('\r', s) != None:
		s=re.sub('\r', '', s)
	header = s.split('\n')[0:3]
	parlon = re.findall('[\d.]+', header[1])
	parlat = re.findall('[\d.]+', header[2])
	# parlon and parlat are a list of 4 numbers (1. length, 2. Start from West/south, 3. end at east/north, 4.width)
	# To parse the multiline data

	s2 = s.split('\n')[3:]
	s3 = ''.join(s2)
	s4 = re.split('   lat =  [ \\-][ \\-0-9][0-9]\\.5', s3)[0:180]
	nlon = int(parlon[0])
	nlat = int(parlat[0])
	width = ([4]+[3]*24)*(nlon//25)+([4]+[3]*(nlon%25-1))
	#Read
	mtx = [[0 for x in range(nlon)] for y in range(nlat)]
	for i in range(len(s4)):
		tmp1 = parse_fwf(s4[i],width)
		tmp2 = map(int, tmp1)
		mtx[i] = tmp2
	lon = frange(-1 * float(parlon[1]), float(parlon[2]), float(parlon[3]), digit = 3)
	lat = frange(-1 * float(parlat[1]), float(parlat[2]), float(parlat[3]), digit = 3)
	return((mtx, lon, lat))

# plotAntarc take the tupl output from readdata and 
def plot_antarc(tupl, filename, contlevel=10):
	m = Basemap(projection = 'ortho', lon_0 = 0, lat_0 = -90)
	x, y = m(*np.meshgrid(tupl[1], tupl[2]))
	m.contourf(x, y, tupl[0], contlevel, cmap=plt.cm.jet)
	m.drawcoastlines()
	m.drawmapboundary()
	m.contour(x, y, tupl[0], )
	plt.savefig(filename)
	#return()

def plot_antarc2(tupl, filename):
	m = Basemap(projection = 'ortho', lon_0 = 0, lat_0 = -90)
	x, y = m(*np.meshgrid(tupl[1], tupl[2]))
	m.drawcoastlines()
	m.drawmapboundary()
	cf = m.contourf(x, y, tupl[0], 50, cmap=plt.cm.binary)
	m.contour(x, y, tupl[0], [250], colors='red')
	m.colorbar(cf)
	plt.savefig(filename)
	plt.clf()

def plot_global(tupl, filename, title, contlevel=20):
	m = Basemap(projection='moll',lon_0=0,resolution='c')
	x, y = m(*np.meshgrid(tupl[1], tupl[2]))
	m.drawcoastlines()
	m.drawmapboundary()
	cf = m.contourf(x, y, tupl[0], 50, cmap=plt.cm.jet, levels = frange(200,500,20))
	m.colorbar(cf)
	plt.title(title)
	plt.savefig(filename)
	#return()

def readbatchmonthly(datadir, outputdir, contlevel=50):
	years = os.listdir(datadir)
	for y in years:
		yeardir = datadir + '\\' + y
		days = os.listdir(yeardir)
		# nday = len(days)
		month = [int(x[-8:-6]) for x in days]
		unimonth = np.unique(month)
		for mon in unimonth:
			startind = month.index(mon)
			nd = month.count(mon)
			doutput = outputdir + '\\' + str(y) + '-' + str(mon) + '.png' 
			avgm = []
			tupleholder = ()
			for i in range(startind, startind + nd):
				d = days[i]
				daypath = datadir + '\\' + y + '\\' + d
				tmptupl = read_ozone_data(daypath)
				dm = tmptupl[0]
				avgm.append(dm)
				tupleholder = tmptupl
			print(doutput)
			#print(len(avgm))
			#print(len(avgm[0]))
			#print(len(avgm[0][0]))
			wei = [[[1 if i else 0 for i in j] for j in t] for t in avgm]
			mm = np.ma.average(avgm,0,wei).data
			#print(len(mm))
			#print(len(mm[0]))
			mmtupl = (mm, tupleholder[1], tupleholder[2])
			plot_antarc(mmtupl, doutput, contlevel)

def readbatchweekly(datadir, outputdir):
	years = os.listdir(datadir)
	if len(years) > 6: 
		years = years[0:5]
	allyear = []
	for y in years:
		print(y)
		yeardir = datadir + '\\' + y
		dayfiles = os.listdir(yeardir)
		ex = read_ozone_data(datadir + '\\' + y + '\\' + dayfiles[0])
		ex = ex[0]
		nullm = [[0 for i in j] for j in ex]
		# nday = len(days)
		datestr = [x[-12:-4] for x in dayfiles]
		dates = [datetime.strptime(x, '%Y%m%d') for x in datestr]
		ydays = [x.timetuple().tm_yday for x in dates]
		weeks = [x // 7 + 1 for x in ydays] #1:53 weeks
		allwk = []
		for wk in range(1,54):
			if wk not in weeks:
				allwk.append(nullm)
				continue
			startind = weeks.index(wk)
			nd = weeks.count(wk)
			avgw = []
			tupleholder = ()
			for i in range(startind, startind + nd):
				d = dayfiles[i]
				daypath = datadir + '\\' + y + '\\' + d
				tmptupl = read_ozone_data(daypath)
				dm = tmptupl[0]
				avgw.append(dm)
				tupleholder = tmptupl
			#print(len(avgm))
			#print(len(avgm[0]))
			#print(len(avgm[0][0]))
			wei = [[[1 if i else 0 for i in j] for j in t] for t in avgw]
			wm = np.ma.average(avgw,0,wei).data
			allwk.append(wm)
		allyear.append(allwk)
	wei2 = [[[[1 if i else 0 for i in j] for j in l] for l in t] for t in allyear]
	wkly = np.ma.average(allyear,0,wei2).data
			#print(len(mm))
			#print(len(mm[0]))
	i = 1
	for wkm in wkly:
		mmtupl = (wkm, tupleholder[1], tupleholder[2])
		woutput = outputdir + '\\week' + str(i) + '.png' 
		print(outputdir)
		print(woutput)
		plot_global(mmtupl, woutput, title='Week' + str(i))
		i += 1


def readbatchyearly(datadir, outputdir, contlevel=50):
	years = os.listdir(datadir)
	if not os.path.exists(outputdir):
		os.makedirs(outputdir)
	for y in years:
		yeardir = datadir + '\\' + y
		days = os.listdir(yeardir)
		nday = len(days)
		doutput = outputdir + '\\' + y + '.png'
		print(doutput)
		avgy = []
		tupleholder = ()
		for d in days:
			daypath = datadir + '\\' + y + '\\' + d
			tmptupl = read_ozone_data(daypath)
			dm = tmptupl[0]
			avgy.append(dm)
			if d == days[0]:
				tupleholder = tmptupl
			#print(len(avgm))
			#print(len(avgm[0]))
			#print(len(avgm[0][0]))
		wei = [[[1 if i else 0 for i in j] for j in t] for t in avgy]
		my = np.ma.average(avgy,0,wei).data
			#print(len(mm))
			#print(len(mm[0]))
		mytupl = (my, tupleholder[1], tupleholder[2])
		plot_antarc(mytupl, doutput, countlevel)

def readyear(yeardir, outputdir=''):
	days = os.listdir(yeardir)
	avgy=[]
	for d in days:
		daypath = yeardir + '\\' + d
		tmptupl = read_ozone_data(daypath)
		dm = tmptupl[0]
		avgy.append(dm)
		if d == days[0]:
			tupleholder = tmptupl
	wei = [[[1 if i else 0 for i in j] for j in t] for t in avgy]
	my = np.ma.average(avgy,0,wei).data
	flatmy = [i for sl1 in my for i in sl1 if i != 0]
	if outputdir != '':
		if not os.path.exists(outputdir):
			os.makedirs(outputdir)
		plt.hist(flatmy,50)
		plt.title(yeardir[-5:])
		plt.savefig(outputdir+'\\'+'histo_' + yeardir[-5:] + '.png')
		plt.clf()
	mytupl = (my, tupleholder[1], tupleholder[2])
	return mytupl






