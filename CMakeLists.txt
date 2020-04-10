#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

cmake_minimum_required(VERSION 2.8)

option(skip_tools "Skips the sdktool and tcsconsole builds" OFF)

#
# If project isn't system_advisor_model and skip_tools=1,
#   environment vars LK_LIB and LKD_LIB can be used to specify where to find those libraries
#

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()

if (UNIX AND NOT CMAKE_C_COMPILER)
	set(CMAKE_C_COMPILER gcc)
	set(CMAKE_CXX_COMPILER g++)
endif()
set(CMAKE_CXX_STANDARD 11)

if ( NOT APPLE)
    set(CURL_DIR build_resources/libcurl_ssl_x64)
endif()

Project(sam_simulation_core)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 11)

function(set_no_warnings)
    if(MSVC)
        add_compile_options(/W0)
    endif()
endfunction()

function(set_disabled_warnings)
    if(MSVC)
        add_compile_options(/W4 /w44191 /w44242  /w44266 /w44302 /w44388 /w44826 /w44905 /w44906 /w44928)
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unknown-pragmas -Wno-sign-compare -Wno-unused-variable -Wno-uninitialized -Wno-int-to-pointer-cast -Wno-ignored-attributes -Wno-deprecated")
        if(APPLE)
            add_compile_options(-Wno-unused-private-field -Wunused-private-field)
        else()
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -Wno-misleading-indentation -Wno-int-in-bool-context" )
        endif()
    endif()
endfunction()

function(set_default_compile_options)
    if(MSVC)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj")
        add_compile_options(/MP)
        add_compile_definitions( __64BIT__ __WINDOWS__ _WINDOWS _CRT_SECURE_NO_WARNINGS LPWINAPP )
        foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
            set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
        endforeach()
    else(MSVC)
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11" )
        add_definitions(-D__64BIT__)
        if (APPLE)
            add_compile_options(-arch x86_64 -fno-common)
            add_definitions(-DWX_PRECOMP -D__MACOSX__)
        else()
            add_definitions(-D__UNIX__)
        endif()
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            add_compile_definitions(_DEBUG)
        else()
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
        endif()
    endif()
endfunction()

function(set_tools_compile_options)
    if(MSVC)
        add_compile_options(/W3)
        add_compile_definitions( LK_USE_WXWIDGETS _CRT_SECURE_NO_DEPRECATE=1 )
        add_compile_definitions(_CRT_NON_CONFORMING_SWPRINTFS=1 _SCL_SECURE_NO_WARNINGS=1)
        add_compile_definitions(__WXMSW__ _UNICODE NOPCH )
    else(MSVC)
        if (APPLE)
            add_definitions(-D__WXOSX__)
        endif()
        add_compile_options(-Wall -Wno-deprecated -Wno-unknown-pragmas -Werror -Wno-overloaded-virtual -fno-common -std=c++11)
        add_definitions(-DLK_USE_WXWIDGETS )
    endif(MSVC)
endfunction()


#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

add_subdirectory(splinter)
add_subdirectory(shared)
add_subdirectory(nlopt)
add_subdirectory(lpsolve)
add_subdirectory(solarpilot)
add_subdirectory(tcs)
add_subdirectory(ssc)

if (NOT skip_tools)
	add_subdirectory(sdktool)
	add_subdirectory(tcsconsole)
endif()

if (NOT skip_tests)
	add_subdirectory(test)
endif()
