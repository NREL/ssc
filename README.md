# SSC (SAM Simulation Core)

Travis-CI status

[![Build Status](https://travis-ci.org/NREL/ssc.svg?branch=develop)](https://travis-ci.org/NREL/ssc)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNREL%2Fssc.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2FNREL%2Fssc?ref=badge_shield)

The SSC Open Source Project repository contains the source code for the technology and financial models contained within the National Renewable Energy Laboratory's System Advisor Model (SAM). For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov).

You could think of SSC as the home for the algorithms behind the SAM desktop program. Most people run the code through the desktop user interface, but SSC can also be run directly using the [SAM Sofware Develoment Kit](https://sam.nrel.gov/sdk). 

SSC requires builds upon four other open-source projects, [Google Test](https://github.com/google/googletest), [LK](https://github.com/nrel/lk), [wxWidgets](https://www.wxwidgets.org/), and [WEX](https://github.com/nrel/wex). However, if you remove SDKtool and TCSconsole from your SSC project, you can build SSC without any other software dependencies. Please see the main [SAM project wiki](https://github.com/NREL/SAM/wiki) for complete build instructions and software dependencies.

However, to simply explore the code and understand the algorithms used in SSC, start by looking in the "SSC" project at the compute modules (files starting with cmod_) to find the compute module for the technology or financial model of interest.

# Contributing

Please see the contribution guidelines in the main [SAM project readme](https://github.com/NREL/SAM/blob/develop/README.md).

# License

SSC is licensed uder a mixed MIT/GPL V3 [license](LICENSE.md).


[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNREL%2Fssc.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2FNREL%2Fssc?ref=badge_large)