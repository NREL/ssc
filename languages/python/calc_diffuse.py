
import site, numpy as np
#site.addsitedir('/srv/data/transfer/shared/samTools/ssc/trunk/python')
site.addsitedir('/srv/data/transfer/shared/samTools/new_ssc/languages/python')
import ssc
ssc = ssc.SSCAPI()

wfile = '25.55_78.55_2011.csv'
#wfile = '723815TY.csv'

lat, lon = float(wfile.split('_')[0]), float(wfile.split('_')[1])
#data = np.loadtxt('/srv/data2/india_weather_files/0012/combined/%s' % wfile, dtype={'names':('year', 'month', 'day', 'hour', 'ghi','dni'), 'formats':('f4','f4', 'f4','f4', 'f4','f4')}, delimiter=',', usecols=(0,1,2,3,4,5))
data = np.loadtxt(wfile, dtype={'names':('year', 'month', 'day', 'hour', 'ghi','dni'), 'formats':('f4','f4', 'f4','f4', 'f4','f4')}, delimiter=',', usecols=(0,1,2,3,4,5))

dat = ssc.ssc_data_create()

ssc.ssc_data_set_number(dat, 'irrad_mode',1)
ssc.ssc_data_set_array(dat, 'beam', data['dni'])
ssc.ssc_data_set_array(dat, 'global', data['ghi'])
ssc.ssc_data_set_array(dat, 'year', data['year'])
ssc.ssc_data_set_array(dat, 'month', data['month'])
ssc.ssc_data_set_array(dat, 'day', data['day'])
ssc.ssc_data_set_array(dat, 'hour', data['hour'])

ssc.ssc_data_set_array(dat, 'minute', np.repeat(29,8760))
ssc.ssc_data_set_number(dat, 'lat', lat)
ssc.ssc_data_set_number(dat, 'lon', lon)
ssc.ssc_data_set_number(dat, 'tz', 5.5)
ssc.ssc_data_set_number(dat, 'sky_model', 2)
ssc.ssc_data_set_number(dat, 'track_mode', 0)
ssc.ssc_data_set_number(dat, 'azimuth', 180)
ssc.ssc_data_set_number(dat, 'tilt', 0)




mod = ssc.ssc_module_create("irradproc")
if ssc.ssc_module_exec(mod, dat) == 0:
    print 'PV Watts V1 simulation error'
    idx = 1
    msg = ssc.ssc_module_log(mod, 0)
    while (msg != None):
        print '\t: ' + msg
        msg = ssc.ssc_module_log(mod, idx)
        idx = idx + 1
    #if there was an error in the model, return 0s
else:
	sky = ssc.ssc_data_get_array(dat, "poa_skydiff")
	ground = ssc.ssc_data_get_array(dat, "poa_gnddiff")
	zenith = ssc.ssc_data_get_array(dat, "sun_zen")
	diffuse = np.array(sky) + np.array(ground)

	file = open('diffuse_perez.csv', 'w')
	file.write('Lat: %s, Lon: %s, TZ: 5.5, Azimuth: 180, Tilt: 0 \n' % (lat, lon))
	file.write('year, month, day, hour, ghi, dni, diffuse, zenith angle \n')
	for x in range(8760):
	    file.write('%s,%s,%s,%s,%s,%s,%s,%s \n' % (data['year'][x], data['month'][x], data['day'][x], data['hour'][x], data['ghi'][x], data['dni'][x], diffuse[x], zenith[x]) )
	file.close()
	
ssc.ssc_module_free(mod)
ssc.ssc_data_free(dat)
