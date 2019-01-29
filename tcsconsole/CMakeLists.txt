set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_C_COMPILER gcc)
set(CMAKE_CXX_COMPILER g++)

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(tcsconsole)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
find_package(wxWidgets REQUIRED xrc webview stc richtext ribbon propgrid aui gl html qa adv core xml net base)
include(${wxWidgets_USE_FILE})

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../tcs)

set(TCSCONSOLE_SRC
	tcsscript.cpp
	tcsmain.cpp
	tcslayout.cpp)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
set( DEPENDENCIES
	splinter
	shared
	solarpilot
	tcs
	nlopt)

unset(WEX_LIB CACHE)
unset(LK_LIB CACHE)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(TCSConsoled ${TCSCONSOLE_SRC})
	set(TCSCONSOLE_EXE TCSConsoled)
	find_library( WEX_LIB
		NAMES wexosxwx3d.a wexuxwx3d.a wexd.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3d.a lkuxwx3d.lib
		PATHS $ENV{LKDIR})

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(TCSConsole ${TCSCONSOLE_SRC})
	set(TCSCONSOLE_EXE TCSConsole)
	find_library( WEX_LIB
		NAMES wexosxwx3.a  wexuxwx3.a wex.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3.a lkuxwx3.a lk.lib
		PATHS $ENV{LKDIR})
endif()

# setup linking dependencies and flags
# change when making toplevel file
set(SSC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/..")
foreach( name ${DEPENDENCIES} )
	# force redefinition of library paths in case build type changes
	unset(${name}_LIB CACHE)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		find_library( ${name}_LIB
			NAMES ${name}d.a ${name}d.lib
			PATHS ${SSC_DIR}/${name} ${SSC_DIR}/../${name})
	else()
		find_library( ${name}_LIB
			NAMES ${name}.a ${name}.lib
			PATHS ${SSC_DIR}/${name} ${SSC_DIR}/../${name})
	endif()
	target_link_libraries(${TCSCONSOLE_EXE} ${${name}_LIB})
endforeach()
target_link_libraries(${TCSCONSOLE_EXE} -lm -lcurl)
target_link_libraries(${TCSCONSOLE_EXE} ${WEX_LIB} ${LK_LIB}) 
target_link_libraries(${TCSCONSOLE_EXE} ${wxWidgets_LIBRARIES})


get_target_property(OUT ${TCSCONSOLE_EXE} LINK_LIBRARIES)
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
		TARGET ${TCSCONSOLE_EXE}
		POST_BUILD 
		COMMAND SetFile -a t ../${TCSCONSOLE_EXE}
		COMMAND mkdir -p ../${TCSCONSOLE_EXE}.app
		COMMAND mkdir -p ../${TCSCONSOLE_EXE}.app/Contents
		COMMAND mkdir -p ../${TCSCONSOLE_EXE}.app/Contents/MacOS
		COMMAND mkdir -p ../${TCSCONSOLE_EXE}.app/Contents/Resources
		COMMAND cp ../${TCSCONSOLE_EXE} ../${TCSCONSOLE_EXE}.app/Contents/MacOS
		COMMAND cp ../../build_osx/TCSConsole.icns ../${TCSCONSOLE_EXE}.app/Contents/Resources
		COMMAND cp ../../build_osx/Info-TCSConsole.plist ../${TCSCONSOLE_EXE}.app/Contents/Info.plist
		COMMAND echo "TCSConsole, National Renewable Energy Laboratory" >> ../${TCSCONSOLE_EXE}.app/Contents/PkgInfo
	)
endif()
