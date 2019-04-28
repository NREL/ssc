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

Project(splinter)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. ../ssc)

set(SPLINTER_SRC
	bspline.cpp
	bsplinebasis.cpp
	bsplinebasis1d.cpp
	bsplinebuilder.cpp
	datapoint.cpp
	datatable.cpp
	function.cpp
	knots.cpp
	mykroneckerproduct.cpp
	serializer.cpp
	utilities.cpp)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W2 /wd4267 /wd4244)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O4)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( __64BIT__ __WINDOWS__ _WINDOWS )

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
	add_library(splinterd STATIC ${SPLINTER_SRC})
	set(SPLINTER_LIB splinterd)
	set_property(TARGET splinterd PROPERTY PREFIX "")

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(splinter STATIC ${SPLINTER_SRC})
	set(SPLINTER_LIB splinter)
	set_property(TARGET splinter PROPERTY PREFIX "")
endif()

if (MSVC)
	set_target_properties(${SPLINTER_LIB} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()


