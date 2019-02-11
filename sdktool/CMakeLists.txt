set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.13)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(sdktool)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

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




# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/SDKTool.icns)
	set(SDKTOOL_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/SDKTool.icns)
	set_source_files_properties(${SDKTOOL_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(SDKTOOL_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_vs2013/SDKTool.rc)
endif()

# set build types
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(SDKtoold 
		WIN32
		MACOSX_BUNDLE 
		${SDKTOOL_SRC}
		${SDKTOOL_ICON})
	set(SDKTOOL_EXE SDKtoold)

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(SDKtool 
		WIN32
		MACOSX_BUNDLE 
		${SDKTOOL_SRC}
		${SDKTOOL_ICON})
	set(SDKTOOL_EXE SDKtool)
endif()

# setup linking dependencies and flags

# if running as combined build, make sure to link to projects
if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		target_link_libraries(${SDKTOOL_EXE} lkd wexd)
	else()
		target_link_libraries(${SDKTOOL_EXE} lk wex)
	endif()
# otherwise find compiled library
else()
	unset(WEX_LIB CACHE)
	unset(LK_LIB CACHE)
	# set build types for archive
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		find_library( WEX_LIB
				NAMES wexd.a wexd.lib
				PATHS $ENV{WEXDIR})
		find_library( LK_LIB
				NAMES lkd.a lk.lib
				PATHS $ENV{LKDIR})
	else()
		find_library( WEX_LIB
				NAMES wex.a wex.lib
				PATHS $ENV{WEXDIR})
		find_library( LK_LIB
				NAMES lk.a lk.lib
				PATHS $ENV{LKDIR})
	endif()
	target_link_libraries(${SDKTOOL_EXE} ${WEX_LIB} ${LK_LIB})
endif()

target_link_libraries(${SDKTOOL_EXE} ${wxWidgets_LIBRARIES})
target_link_libraries(${SDKTOOL_EXE} -lm -lcurl)

if (UNIX AND NOT APPLE)
	target_link_libraries(${SDKTOOL_EXE} -ldl -lfontconfig)
endif()


#get_target_property(OUT ${SDKTOOL_EXE} LINK_LIBRARIES)
#message(STATUS ${OUT})

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	if (APPLE)
		set(CMAKE_POSITION_INDEPENDENT_CODE ON)
		add_compile_options(-arch x86_64)
		add_compile_definitions(__WXOSX__)
	else()
		add_compile_definitions(__64BIT__)
	endif()
	add_compile_options(-Wall -Wno-unknown-pragmas -Werror -O3 -fno-common -std=c++11)
	add_compile_definitions(LK_USE_WXWIDGETS WX_PRECOMP )
endif(MSVC)

# create application info for executable
if (APPLE)
	set_target_properties(${SDKTOOL_EXE} PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/Info-SDKTool.plist
	)
endif()


