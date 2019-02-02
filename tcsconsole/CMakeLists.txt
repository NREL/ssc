set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.13)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(tcsconsole)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base)
include(${wxWidgets_USE_FILE})

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../tcs)

set(TCSCONSOLE_SRC
	tcsscript.cpp
	tcsmain.cpp
	tcslayout.cpp)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE TCSConsole.icns)
	set(TCSCONSOLE_ICON ${CMAKE_SOURCE_DIR}/build_osx/TCSConsole.icns)
	set_source_files_properties(${TCSCONSOLE_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(TCSCONSOLE_ICON ${CMAKE_SOURCE_DIR}/build_vs2013/TCSConsole.rc)
endif()


# set build types for archive
set( DEPENDENCIES
	tcs
	nlopt
	solarpilot
	shared
	splinter)

unset(WEX_LIB CACHE)
unset(LK_LIB CACHE)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(TCSConsoled 
		WIN32
		MACOSX_BUNDLE
		${TCSCONSOLE_SRC}
		${TCSCONSOLE_ICON})
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
	add_executable(TCSConsole
		WIN32
		MACOSX_BUNDLE
		${TCSCONSOLE_SRC}
		${TCSCONSOLE_ICON})
	set(TCSCONSOLE_EXE TCSConsole)
	find_library( WEX_LIB
		NAMES wexosxwx3.a  wexuxwx3.a wex.lib
		PATHS $ENV{WEXDIR})
	find_library( LK_LIB
		NAMES lkosxwx3.a lkuxwx3.a lk.lib
		PATHS $ENV{LKDIR})
endif()

# setup linking dependencies and flags
foreach( name ${DEPENDENCIES} )
	# force redefinition of library paths in case build type changes
	unset(${name}_LIB CACHE)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		target_link_libraries(${TCSCONSOLE_EXE} ${name}d)
	else()
		target_link_libraries(${TCSCONSOLE_EXE} ${name})
	endif()
endforeach()
target_link_libraries(${TCSCONSOLE_EXE} -lm -lcurl)
target_link_libraries(${TCSCONSOLE_EXE} ${WEX_LIB} ${LK_LIB}) 
target_link_libraries(${TCSCONSOLE_EXE} ${wxWidgets_LIBRARIES})
if (UNIX AND NOT APPLE)
	target_link_libraries(${TCSCONSOLE_EXE} -lfontconfig -ldl)
endif()


get_target_property(OUT ${TCSCONSOLE_EXE} LINK_LIBRARIES)
message(STATUS ${OUT})

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	if (APPLE)
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
	set_target_properties(${TCSCONSOLE_EXE} PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_SOURCE_DIR}/build_osx/Info-TCSConsole.plist
	)
endif()