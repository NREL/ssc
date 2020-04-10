# Making an SDK release

**The SDK build process needs to be updated before the Fall/Winter 2018 Release (on Paul's To Do list):**

* Remove API wrappers.
	
* Update SSC guide to explain SAM code generator.
	
# Instructions as of 7/14/2016	

1.	Create the ‘sdk-release’ folder:
 
2.	Check out https://efmsvn2.nrel.gov/ssc/svn/sdk-release into that folder

3.	Open ssc/doc/ssc_guide.tex and recompile the ssc_guide.pdf using LaTeX.

4.	On Windows, run ‘make-sdk-common.bat’ to update all of the api wrapper files and examples:
 
5.	On all platforms, run the copy script that updates all the binaries for each platform:

	a.	Linux: ./make-sdk-linux.sh

	b.	Windows: make-sdk-win.bat

	c.	OSX: ./make-sdk-osx.sh

6.	Check in changes to the ‘sdk-release’ folder

7.	Update the SVN on Windows to get the latest binaries for OSX and Linux

8.	Select everything in sdk-release and add to a zip file:
 
9.	Rename file to sam-sdk-2016-3-14-r2.zip or whatever the version/revision number is for the release and post online in the right places.
