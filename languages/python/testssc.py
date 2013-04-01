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


# ############################################################
# Test program 'main'


version_test()
module_list()

