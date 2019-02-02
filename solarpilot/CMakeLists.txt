set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.13)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(solarpilot)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. ../nlopt ..)

set(SOLARPILOT_SRC
	fluxsim.cpp
	API_structures.cpp
	Ambient.cpp
	AutoPilot_API.cpp
	Financial.cpp
	Flux.cpp
	Heliostat.cpp
	IOUtil.cpp
	Land.cpp
	LayoutSimulateThread.cpp
	OpticalMesh.cpp
	Receiver.cpp
	STObject.cpp
	STSimulateThread.cpp
	SolarField.cpp
	Toolbox.cpp
	definitions.cpp
	heliodata.cpp
	interop.cpp
	mod_base.cpp
	optimize.cpp
	solpos.cpp
	string_util.cpp)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_library(solarpilotd STATIC ${SOLARPILOT_SRC})
  set_property(TARGET solarpilotd PROPERTY PREFIX "")

  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  add_library(solarpilot STATIC ${SOLARPILOT_SRC})
  set_property(TARGET solarpilot PROPERTY PREFIX "")
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
  add_compile_options(/W4 /WX)
  add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	if (APPLE)
		set(CMAKE_CXX_STANDARD 11)
		add_compile_options(-arch x86_64 -fno-common)
  		add_compile_definitions(WX_PRECOMP)
	else()
    	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
		add_compile_definitions(__64BIT__)
	endif()
  add_compile_options(-Wall -O3 )
endif(MSVC)
