# SSC (SAM Simulation Core) Tests

The SSC test project contains unit and integration tests using the GTest framework for the shared, solarpilot, ssc, and tcs libraries.

# Building on Windows with Visual Studio 2013

1. Clone [GTest](https://github.com/google/googletest) 

2. Open ```/googletest/googletest/msvc/2010/gtest-md.sln```
	* Select OK when VS prompts for a One-way upgrade	
3. Build Release and Debug modes for x64 and Win32 architectures.
	* If you encounter an ```multiple CL.EXE write to the same .PDB file, please use /FS error```, try setting ```Multi_processor compilation``` option (in ```C/C++```, ```General```) to ```No``` or changing the ```Intermediate Directories``` for each project in the solution from ```$(Platform)\$(Configuration)\``` to ```$(Platform)\$(Configuration)\$(ProjectName)\```.
4. Check environment variable for GTEST exists and points to ```/googletest/googletest```

5. Build ```/ssc/build_vc2013/ssc.sln```

# Writing Tests
GoogleTest has a [Primer](https://github.com/google/googletest/blob/master/googletest/docs/Primer.md#simple-tests) for using assertions, tests and test fixtures.

# Running Tests via Command Line
1. To run all tests, navigate in ```cmd.exe``` to ```/ssc/build_vc2013/(Platform)/(Config)/``` and call ```ssc_test.exe```
2. To filter tests by name (case-sensitive): ```ssc_test.exe --gtest_filter=PositivePattners:-NegativePatterns```
	* Each pattern separated by ```:``` can contain ```*``` to match any string or ```?``` to match any character
	* For example, ```ssc_test.exe --gtest_filter=*WeatherData*``` will run tests with "WeatherData" anywhere in the name; ```ssc_test.exe --gtest_filter=*WeatherData*:*9999*``` will run tests the above tests minus tests with 9999 anywhere in the name
