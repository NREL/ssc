
import ssc # contains all Python classes for accessing ssc


# test to list all ssc version and build information
def version_test():
	     ssc_api = ssc.API()
	     print 'ssc version = ' , ssc_api.version()
             print 'ssc build info = ', ssc_api.build_info()


# test to list all modules available in ssc
def module_list():
	     ssc_entry = ssc.Entry()
	     while (ssc_entry.get()):
			  module_name = ssc_entry.name()
			  description = ssc_entry.description()
			  version = ssc_entry.version()
			  print '\nModule: ',module_name,', version: ', version
			  print '    ' , description


def array_test():
	     ssc_data = ssc.Data()
	     arr = [0]*10
	     for i in range(len(arr)):
			  arr[i] = i / 10.0
	     ssc_data.set_array("TestArray", arr)
	       
	     ret_array = ssc_data.get_array("TestArray")
	       
	     print '\nTesting SetArray and GetArray'
	     for i in range(len(ret_array)):
			  print '\treturned array element: ' , i ,' = ' , ret_array[i]


def matrix_test():
	     ssc_data = ssc.Data()
	     matrix = [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]
	     ssc_data.set_matrix("TestMatrix", matrix)
 
	     ret_matrix = ssc_data.get_matrix("TestMatrix")
 
	     print "\nTesting SetMatrix and GetMatrix"

	     nrows = len(ret_matrix)
	     ncols = len(ret_matrix[0])

	     for i in range(nrows):
	         for j in range(ncols):
			  print "\treturned matrix element: (" , i , "," , j , ") = " , ret_matrix[i][j]
	      

def module_and_variables_test():
	     ssc_entry = ssc.Entry()
	     module_index = 0
	     while (ssc_entry.get()):
			  module_name = ssc_entry.name()
			  description = ssc_entry.description()
			  version = ssc_entry.version()
			  print "\nModule: " , module_name , ", version: " , version 
			  print " " + description
			  module_index += 1
 
			  ssc_module = ssc.Module(module_name)
			  ssc_info = ssc.Info(ssc_module)
 
			  while (ssc_info.get()):
				       print "\t" , ssc_info.var_type() , ": \"" , ssc_info.name() , "\" " , " [" , ssc_info.data_type() , "] " + ssc_info.label() , " (" , ssc_info.units() , ")"

def variables_list(module):
	     ssc_module = ssc.Module(module)
	     ssc_info = ssc.Info(ssc_module)
	     
	     print "Variables for " , module
	     while (ssc_info.get()):
			  print "\t" , ssc_info.var_type() , ": \"" , ssc_info.name() , "\" " , " [" , ssc_info.data_type() , "] " + ssc_info.label() , " (" , ssc_info.units() , ")"
	     
	     

def pvwatts_test():
	     data = ssc.Data()
	     data.set_string("solar_resource_file", "../../examples/abilene.tm2") 
	     data.set_number("system_capacity", 4.0)
	     data.set_number("dc_ac_ratio", 1.1)
	     data.set_number("tilt", 20)
	     data.set_number("azimuth", 180)
	     data.set_number( 'inv_eff', 96 );
	     data.set_number( 'losses', 14.0757 );
	     data.set_number( 'array_type', 0 );
	     data.set_number( 'tilt', 20 );
	     data.set_number( 'azimuth', 180 );
	     data.set_number( 'gcr', 0.4 );
	     data.set_number( 'adjust:factor', 1 );
 
	     mod = ssc.Module("pvwattsv5")
	     if (mod.exec_(data)):
			  tot = data.get_number("ac_annual")
			  ac = data.get_array("ac_monthly")
			  for i in range(len(ac)):
				       print "[" , i , "]: " , ac[i] , " kWh"
			  print "AC total: " , tot 
			  print "PVWatts test OK"
	     else:
			  idx = 0
			  msg = mod.log(idx)
			  while (msg is not None):
				       print "Error [", idx," ]: " , msg
				       idx += 1
				       msg = mod.log(idx)
			  print "PVWatts example failed"
	     

def pvwatts_func_test():
	     ssc_module = ssc.Module("pvwattsv1_1ts")
	     ssc_data = ssc.Data()
	     ssc_data.set_number("year", 1970) # general year (tiny effect in sun position)
	     ssc_data.set_number("month", 1) # 1-12
	     ssc_data.set_number("day", 1) #1-number of days in month
	     ssc_data.set_number("hour", 9) # 0-23
	     ssc_data.set_number("minute", 30) # minute of the hour (typically 30 min for midpoint calculation)
	     ssc_data.set_number("lat", 33.4) # latitude, degrees
	     ssc_data.set_number("lon", -112) # longitude, degrees
	     ssc_data.set_number("tz", -7)  # timezone from gmt, hours
	     ssc_data.set_number("time_step", 1)  # time step, hours
 
	     # solar and weather data
	     ssc_data.set_number("beam", 824)  # beam (DNI) irradiance, W/m2
	     ssc_data.set_number("diffuse", 29)  # diffuse (DHI) horizontal irradiance, W/m2
	     ssc_data.set_number("tamb", 9.4)  # ambient temp, degree C
	     ssc_data.set_number("wspd", 2.1)  # wind speed, m/s
	     ssc_data.set_number("snow", 0)  # snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)
 
	     # system specifications
	     ssc_data.set_number("system_size", 4)  # system DC nameplate rating (kW)
	     ssc_data.set_number("derate", 0.77)  # derate factor
	     ssc_data.set_number("track_mode", 0)  # tracking mode 0=fixed, 1=1axis, 2=2axis
	     ssc_data.set_number("azimuth", 180)  # azimuth angle 0=north, 90=east, 180=south, 270=west
	     ssc_data.set_number("tilt", 20)  # tilt angle from horizontal 0=flat, 90=vertical
 
 
	     # previous timestep values of cell temperature and POA
	     ssc_data.set_number("tcell", 6.94)  # calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
	     ssc_data.set_number("poa", 84.5)  # plane of array irradiance (W/m2) from previous time step
 
	     if (ssc_module.exec_(ssc_data)):
			  poa = ssc_data.get_number("poa")
			  tcell = ssc_data.get_number("tcell") 
			  dc = ssc_data.get_number("dc") 
			  ac = ssc_data.get_number("ac") 
			  print "poa: " , poa , " W/m2" 
			  print "tcell: " , tcell , " C" 
			  print "dc: " , dc , " W"
			  print "ac: " , ac , " W" 
			  print "PVWatts func test OK"
	     else:
			  print "PVWatts func test failed"
	     
	     

# ############################################################
# Test program 'main'

ssc_api = ssc.API()
ssc_api.set_print(0)
version_test()
module_list()
module_and_variables_test()
array_test()
matrix_test()
pvwatts_test()
pvwatts_func_test()
variables_list("pvsamv1")