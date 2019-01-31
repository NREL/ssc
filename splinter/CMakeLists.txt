set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(splinter)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(.)

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

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_library(splinterd STATIC ${SPLINTER_SRC})
  set_property(TARGET splinterd PROPERTY PREFIX "")

  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  add_library(splinter STATIC ${SPLINTER_SRC})
  set_property(TARGET splinter PROPERTY PREFIX "")
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
  add_compile_options(/W4 /WX)
  add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
  add_compile_options(-Wall -O3 -arch x86_64 -fno-common)
  add_compile_definitions(WX_PRECOMP)
endif(MSVC)
