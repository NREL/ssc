cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_C_COMPILER gcc)
set(CMAKE_CXX_COMPILER g++)

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(sam_simulation_core)

add_subdirectory(splinter)
add_subdirectory(shared)
add_subdirectory(nlopt)
add_subdirectory(lpsolve)
add_subdirectory(solarpilot)
add_subdirectory(tcs)
add_subdirectory(ssc)
add_subdirectory(sdktool)
add_subdirectory(tcsconsole)
add_subdirectory(test)
