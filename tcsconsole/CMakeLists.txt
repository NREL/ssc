#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

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

Project(tcsconsole)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../tcs)

set(TCSCONSOLE_SRC
	tcsscript.cpp
	tcsmain.cpp
	tcslayout.cpp)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W3 /MP)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( LK_USE_WXWIDGETS _CRT_SECURE_NO_DEPRECATE=1)
	add_compile_definitions(_CRT_NON_CONFORMING_SWPRINTFS=1 _SCL_SECURE_NO_WARNINGS=1)
	add_compile_definitions(__WXMSW__ _UNICODE _WINDOWS NOPCH)
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64)
		add_definitions(-D__WXOSX__)
	else()
		add_definitions(-D__64BIT__)
	endif()
	add_compile_options(-Wall -Wno-unknown-pragmas -Werror -fno-common -std=c++11)
	add_definitions(-DLK_USE_WXWIDGETS -DWX_PRECOMP )
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
		add_compile_definitions(_DEBUG)
	else()
		add_compile_options(-O3)
	endif()
endif(MSVC)

#####################################################################################################################
#
# Packages
#
#####################################################################################################################

if (UNIX)
	set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
	find_package(wxWidgets REQUIRED webview xrc stc richtext ribbon propgrid aui gl html qa adv net core xml net base)
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
	if (CMAKE_BUILD_TYPE STREQUAL "Debug")
		set(wxWidgets_CONFIGURATION mswud)
	else()
		set(wxWidgets_CONFIGURATION mswu)
	endif()
	find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base scintilla)
endif()
include(${wxWidgets_USE_FILE})


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/TCSConsole.icns)
	set(TCSCONSOLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/TCSConsole.icns)
	set_source_files_properties(${TCSCONSOLE_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	#set(TCSCONSOLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_vs2013/TCSConsole.rc)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(TCSConsoled
			WIN32
			MACOSX_BUNDLE
			${TCSCONSOLE_SRC}
			${TCSCONSOLE_ICON})
	set(TCSCONSOLE_EXE TCSConsoled)

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(TCSConsole
			WIN32
			MACOSX_BUNDLE
			${TCSCONSOLE_SRC}
			${TCSCONSOLE_ICON})
	set(TCSCONSOLE_EXE TCSConsole)
endif()

if (APPLE)
	set_target_properties(${TCSCONSOLE_EXE} PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/../build_osx/Info-TCSConsole.plist
	)
endif()

if (MSVC)
	set_target_properties(${TCSCONSOLE_EXE} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>
        LINK_FLAGS /SUBSYSTEM:WINDOWS)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

set( DEPENDENCIES
		tcs
		nlopt
		solarpilot
		shared
		splinter)
foreach( name ${DEPENDENCIES} )
	unset(${name}_LIB CACHE)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		target_link_libraries(${TCSCONSOLE_EXE} ${name}d)
	else()
		target_link_libraries(${TCSCONSOLE_EXE} ${name})
	endif()
endforeach()

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		target_link_libraries(${TCSCONSOLE_EXE} lkd wexd)
	else()
		target_link_libraries(${TCSCONSOLE_EXE} lk wex)
	endif()
else()
	unset(WEX_LIB CACHE)
	unset(LK_LIB CACHE)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		find_library( WEX_LIB
			NAMES wexd.a wexd.lib
			PATHS $ENV{WEXDIR})
		find_library( LK_LIB
			NAMES lkd.a lkd.lib
			PATHS $ENV{LKDIR})

		set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
	else()
		find_library( WEX_LIB
			NAMES wex.a wex.lib
			PATHS $ENV{WEXDIR})
		find_library( LK_LIB
			NAMES lk.a lk.lib
			PATHS $ENV{LKDIR})
	endif()
	target_link_libraries(${TCSCONSOLE_EXE} ${WEX_LIB} ${LK_LIB})
endif()

target_link_libraries(${TCSCONSOLE_EXE} ${wxWidgets_LIBRARIES})

if (UNIX)
	target_link_libraries(${TCSCONSOLE_EXE} -lm -lcurl)
elseif (MSVC)
	find_library( CURL_LIB
				NAMES libcurl.lib
				PATHS $ENV{WEXDIR}/build_vs2017/libcurl_ssl_x64/lib)
	target_link_libraries(${TCSCONSOLE_EXE} ${CURL_LIB})
	target_link_libraries(${TCSCONSOLE_EXE} Winhttp)
endif()

if (UNIX AND NOT APPLE)
	target_link_libraries(${TCSCONSOLE_EXE} -lfontconfig -ldl)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

set( LIBCURL_FILES
   dbghelp.dll
   libcurl.dll
   libeay32.dll
   libssh2.dll
   msvcp140.dll
   openssl.exe
   ssleay32.dll
   vcruntime140.dll
)

foreach( file_i ${LIBCURL_FILES})
    add_custom_command(
    TARGET ${TCSCONSOLE_EXE}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND}
    ARGS -E copy $ENV{WEXDIR}/build_vs2017/libcurl_ssl_x64/bin/${file_i} ${CMAKE_CURRENT_SOURCE_DIR}
)
endforeach( file_i )

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	set(DEBUG TRUE)
else()
	set(DEBUG FALSE)
endif()

add_custom_command(
    TARGET ${TCSCONSOLE_EXE}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND}
    ARGS -E copy $ENV{SSCDIR}/ssc/ssc$<$<BOOL:DEBUG>:d>.lib $ENV{SSCDIR}/ssc/ssc$<$<BOOL:DEBUG>:d>.dll ${CMAKE_CURRENT_SOURCE_DIR}
)
