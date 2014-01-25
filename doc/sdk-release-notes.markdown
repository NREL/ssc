Release Notes
=============

__Note.__ This list shows changes that affect the SDK. For descriptions of changes to individual SSC modules, see the SAM release notes, which you can find on SAM's Help menu or on the [Download page](https://sam.nrel.gov/content/downloads).

January 24, 2014: SDK 2014-9-24, SSC 36
---------------------------------------

This is a major update that adds preliminary versions of the concentrating solar power (CSP) modules to the SDK.


September 20, 2013: SDK 2013-9-20, SSC 33
-----------------------------------------

SDK 2013-9-20 was released on September 20, 2013 with the SSC library version number 33.

* Update sample scripts and wrappers to work with SSC 33

May 3: Update to First Release
------------------------------

* New modules: CSV weather file reader and converter, plane-of-array calibration, utility rate 2 for new OpenEI utility rate database version 2
* Updates to support Linux CentOS6.3 and OS X 10.8
* Update MATLAB, Java, Python, and LK script examples
* Corrected an issue in the annualoutput module with the calculation of annual_e_net_delivered when the energy_curtailment matrix has values other than 1. This only affected the cashloan module, which uses annual_e_net as an input. (The ippppa module uses hourly_e_net_delivered as input and calculates the annual value rather than using the value from annualoutput.)
* Updated windpower module to include wake model improvements.
* Improved the MATLAB wrapper with new classes, SSC package, and a MATLAB GUIDE example.
* Improved the Python wrapper with new classes, and several examples including flat plate PV with residential and commercial markets.
* Fixed various issues with the MATLAB and Java wrappers. They have been tested in Windows 32-bit and 64-bit, OS X 10.8, and CentOS 6.3.
* Updated all wrapper examples to include flat plate PV with residential and commercial markets. They have been tested in Windows 32-bit and 64-bit, OS X 10.8, and CentOS 6.3.
* Updated the Geothermal capacity factor calculation based on system lifetime.
* Updated the radiation processor to handle minutes other than 0, 15, 30, 45 and 60.
* Added OpenEI Version 2 support for utility rate calculations.

February 8, 2013: First Release 
-------------------------------

The first version of the SDK was called Version 0.9.
