set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(sdktool)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
find_package(wxWidgets REQUIRED stc propgrid aui gl html adv core xml net base)
include(${wxWidgets_USE_FILE})

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../ssc ../shared ../splinter)

set(SDKTOOL_SRC
	dataview.cpp
	dllinvoke.cpp
	editvariable.cpp
	scripting.cpp
	sscdev.cpp)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive

unset(WEX_LIB CACHE)
unset(LK_LIB CACHE)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(SDKtoold ${SDKTOOL_SRC})
	set(SDKTOOL_EXE SDKtoold)
	find_library( WEX_LIB
		NAMES wexosxwx3d.a wexuxwx3d.a wexd.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3d.a lkuxwx3d.lib
		PATHS $ENV{LKDIR})

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(SDKtool ${SDKTOOL_SRC})
	set(SDKTOOL_EXE SDKtool)
	find_library( WEX_LIB
		NAMES wexosxwx3.a  wexuxwx3.a wex.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3.a lkuxwx3.a lk.lib
		PATHS $ENV{LKDIR})
endif()

# setup linking dependencies and flags
target_link_libraries(${SDKTOOL_EXE} -lm -lcurl)
target_link_libraries(${SDKTOOL_EXE} ${WEX_LIB} ${LK_LIB}) 
target_link_libraries(${SDKTOOL_EXE} ${wxWidgets_LIBRARIES})


get_target_property(OUT ${SDKTOOL_EXE} LINK_LIBRARIES)
message(STATUS ${OUT})

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	add_compile_options(-Wall -O3 -arch x86_64 -fno-common)
	add_compile_definitions(LK_USE_WXWIDGETS WX_PRECOMP )
	if (APPLE)
		add_compile_definitions(__WXOSX__)
	endif()
endif(MSVC)

# Setup Application
if(APPLE)
	add_custom_command(
		TARGET ${SDKTOOL_EXE}
		POST_BUILD 
		COMMAND SetFile -a t ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}
		COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app
		COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents
		COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/MacOS
		COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/Resources
		COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE} ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/MacOS
		COMMAND cp ${CMAKE_SOURCE_DIR}/build_osx/SDKTool.icns ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/Resources
		COMMAND cp ${CMAKE_SOURCE_DIR}/build_osx/Info-SDKTool.plist ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/Info.plist
		COMMAND echo "Software Development Toolkit for System Advisor Model, National Renewable Energy Laboratory" >> ${CMAKE_CURRENT_SOURCE_DIR}/${SDKTOOL_EXE}.app/Contents/PkgInfo
	)
endif()
