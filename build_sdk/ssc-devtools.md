# Compiling SSC+SSCdev for win32/x64/linux64/osx64 

Using wxWidgets 3 (SVN), LK, and WEX
Aron Dobos – Updated 3 Jan 2012
Steven Janzou – Updated 19 Jan 2014
Paul Gilman - Convert to markdown 22 Sep 2017

## Visual Studio Express for Windows
 
This requires Visual Studio 2013 Express for Desktop

http://www.microsoft.com/visualstudio/eng/products/visual-studio-express-for-windows-desktop

## Mac Notes: XCode 4.6 for OSX 10.8

1.	Download the latest XCode (4.6) from the App Store.
2.	Run XCode
3.	Select XCode->Preferences  menu item
4.	under “Downloads”, download and install all of the command line tools.
5.	Download wxWidgets  from https://sourceforge.net/projects/wxwindows/files/3.0.2/wxWidgets-3.0.2.tar.bz2/download 

## wxWidgets 3.0, win32 and x64.

1.	Download wxWidgets 3.0 from 
https://sourceforge.net/projects/wxwindows/files/3.0.0/wxWidgets-3.0.0.zip/download
2.	Extract to c:\wxWidgets-3.0.0
3.	Open Visual Studio 2013, and open the C:\wxWidgets-3.0.0\build\msw\wx_vc10.sln file
a.	Allow the one-way upgrade, it will take some time to convert all the project files
b.	If the conversion report issues an error for the .sln, ignore it.
4.	Select the ‘Debug’ Configuration, and build, then the ‘Release’ Configuration, and build.
5.	Select the x64 solution platform from the tool bar.
6.	Build both Debug and Release configurations for the x64 platform, all of them should complete successfully.
7.	Set the WXMSW3 environment variable to C:\wxWidgets-3.0.0

## LK Library

1.	Check out LK from https://efmsvn.nrel.gov/lk/svn  into a folder called ‘lk’
2.	Open the lk\vc2013_wx3\ lkvc13wx3.sln project file
3.	Build Release and Debug, both Win32 and x64.
4.	Set the LKDIR environment variable to point to your ‘lk’ folder.

## WEX Library

1.	Check out WEX from https://efmsvn.nrel.gov/wex/svn/trunk into a folder called ‘wex’
2.	Open the wex\vc2013_wx3\ wexvc13wx3.sln project
3.	Build the Debug and Release configurations, both win32 and x64.
4.	Set the WEXDIR environment variable to your wex folder.

Everything should complete successfully.

## Building an SSC SDK release package:

To facilitate easy combining of binaries from all 3 target OSes for SSC, there is a subfolder in the SSC repository called sdk-release.  This subfolder contains compiled binaries from all three platforms, and is updated when there is a new release of the SDK.  The sdk-release folder is intended to be checked out on each target platform, the binaries compiled, copied over, and then checked in to the SVN.  Then, when all the binaries are checked in, the folder can be zipped up for distribution containing all the different platform binaries.

1.	In your c:\Projects\ssc  folder, create a subfolder called sdk-release. 

2.	Right click on it, and check out https://efmsvn.nrel.gov/ssc/svn/sdk-release

3.	On Windows:

	a.	Compile SSC for both win32 and x64, Release configuration.

	b.	Run the make-sdk-win.bat file, which will copy the binaries to the right places in the sdk-release folder.

4.	On OSX: (10.8 64 bit)

	a.	In the ssc/build_osx folder, type make to build SSCtool.app and ssc.dylib 

		i.	This presumes you’ve compiled LK and WEX, as well as wxWidgets SVN with the following command line configuration:
		
		ii.	 ./configure --prefix=/Users/adobos/local/wx3-svn --enable-stl=yes --enable-debug=no --enable-shared=no --with-cocoa --with-libjpeg=builtin --with-libpng=builtin --with-regex=builtin --with-libtiff=builtin --with-zlib=builtin --with-expat=builtin
		
		iii.	make
		
		iv.	make install
		
		v.	sudo ln -s /Users/adobos/local/wx3-svn/lib/wx/config/osx_cocoa-unicode-static-2.9 /usr/bin/wx-config-dsa

		vi.	sudo ln -s /Users/adobos/local/wx3-svn/lib/wx/config/osx_cocoa-unicode-static-2.9 /usr/bin/wx-config-3

		vii.	cd lk/osx_wx3_unicode folder && make
		
		viii.	cd  wex/osx_wx3_unicode folder && make

	b.	Back in the toplevel SSC folder, type “bash make-sdk-osx.sh” to copy the right files to the sdk-release folder.

5.	On Linux: (Assumed CentOS 6.3, 64 bit)

	a.	In the ssc/build_linux folder, type make to build SSCTool binary and ssc.so
	
		i.	This presumes you’ve compiled LK and WEX, as well as wxWidgets with the following command line configuration:
		
		ii.	 ./configure --prefix=/home/adobos/local/wx3-svn --enable-stl=yes --enable-debug=no --enable-shared=no --with-gtk=2 --with-libjpeg=builtin --with-libpng=builtin --with-regex=builtin --with-libtiff=builtin --with-zlib=builtin --with-expat=builtin

		iii.	make

		iv.	make install

		v.	sudo ln -s /home/adobos/local/wx3-svn/lib/wx/config/gtk2-unicode-static-2.9 /usr/bin/wx-config-dsa

		vi.	sudo ln -s /home/adobos/local/wx3-svn/lib/wx/config/gtk2-unicode-static-2.9 /usr/bin/wx-config-3

		vii.	cd lk/linux_wx3_unicode folder && make

		viii.	cd  wex/linux_wx3_unicode folder && make

	b.	Back in the toplevel SSC folder, type “bash make-sdk-linux.sh” to copy the right files to the sdk-release folder and create the dependency and filetype information files.

6.	Back on Windows,

	a.	Update the sdk-release folder to get all the binaries for each of the platforms

	b.	Run the make-sdk-common.bat file to copy the language wrappers, examples, common headers, and documentation files to the release folder

7.	Zip up the sdk-release folder, and post it on the website!  Avoid including any .svn subfolders in the zip file.

Hope that works!



