This folder implements a slightly modified version of John Dyreby's steam code as a standalone dynamic library with a C interface.

The code has been tested on Win32 using MinGW, Linux, and MacOSX

Last updated 2012.12.5, Aron Dobos


Instructions for compiling on Windows: (32 bit)


1.	Download the Mingw full version at http://sourceforge.net/projects/mingw/  (latest is mingw-get-inst-20120426.exe) 
2.	Run the downloaded executable and select “C Compiler” and “C++ Compiler” and “Fortran Compiler”
3.	Finish install and add c:/mingw/bin to the system PATH
3.5 Reboot computer to update 'path' env variable
4.	Check out latest tcs code from https://efmsvn.nrel.gov/tcs/svn 
5.	Open a command window (Start->Run->”cmd”)
6.	Go to the waterprop folder under your tcs working directory 
7.	Type "mingw32-make" at the command prompt
8.	The waterprop.dll should be created
9.	Open Visual Studio, and from the Tools menu, select the "Visual Studio Command Prompt"
	If using Visual Studio 2012 Express, in the Window Start menu, search for "VS2012 x86 Native Tools Command Prompt", and click that.

10.	Change to the tcs/waterprop folder
11.	Type "mingw32-make vclib32" to create the waterprop32.lib and waterprop32.exp files that are needed for linking in the TCS/typelib project.
12.	The typelib project in TCS is preconfigured to link to waterprop32.lib - so make sure you follow these steps before trying to build TCS.  
13.	Copy waterprop32.dll to tcs/Win32/Release and tcs/Win32/Debug so that when typelib.dll is loaded by the TCS environment, it can find the waterprop32.dll and load it - otherwise typelib.dll will not be loaded.
14.	If releasing waterprop32.dll or typelib.dll, make sure to also release the libgcc_s_dw2-1.dll and libgfortran-3.dll files found in c:\mingw\bin


For 64bit:
1. Download the following MinGW-64.
http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/rubenvb/gcc-4.7-release/x86_64-w64-mingw32-gcc-4.7.2-release-win64_rubenvb.7z
2. Unpack to c:\mingw64
3. Don't add c:\mingw64\bin to the path - it'll get confused with the 32 bit version at c:\mingw\bin

Quick build:
 you can just run runme_VS2012.bat which will rebuild the 32 bit and 64 bit binaries.

Make sure to copy the 64-bit runtime libraries to the x64/Debug and x64/Release folders to allow typelib to find them.
  libgcc_s_sjlj-1.dll
  libgfortran-3.dll
  libquadmath-0.dll

These are found in c:\mingw64\bin



Instructions for compiling on Linux/Mac:

1.	Type 'make' in the tcs/waterprop folder, waterprop32.so should be compiled.  By default, a 32-bit version will be attempted. To compile 64-bit, use "make CFG=64"
