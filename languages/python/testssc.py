
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
	      


# ############################################################
# Test program 'main'


version_test()
#module_list()
#array_test()
matrix_test()
