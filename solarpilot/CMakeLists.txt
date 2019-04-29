#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
endif()

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

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



#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
  add_compile_options(/W3)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O3)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( __64BIT__ __WINDOWS__ _WINDOWS _CRT_SECURE_NO_WARNINGS _MBCS )
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
  		add_compile_definitions(WX_PRECOMP)
	else()
		add_definitions(-D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -std=c++11" )
endif(MSVC)


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_library(solarpilotd STATIC ${SOLARPILOT_SRC})
	set(SOLARPILOT_LIB solarpilotd)

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(solarpilot STATIC ${SOLARPILOT_SRC})
	set(SOLARPILOT_LIB solarpilot)
endif()
	set_property(TARGET ${SOLARPILOT_LIB} PROPERTY PREFIX "")

if (MSVC)
	set_target_properties(${SOLARPILOT_LIB} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
endif()
