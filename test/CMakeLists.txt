#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.8)

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

Project(ssc_test)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories($ENV{GTEST}/include . input_cases shared_test ssc_test tcs_test 
						../ssc ../tcs ../solarpilot ../shared ../splinter )

file(GLOB SSC_TESTS ssc_test/*.cpp)
file(GLOB SHARED_TESTS shared_test/*.cpp)
file(GLOB TCS_TESTS tcs_test/*.cpp)
file(GLOB INPUTS_SRC input_cases/*.cpp)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)

if(MSVC)
	add_compile_options(/W3 /wd4244)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O4)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( __64BIT__ __WINDOWS__ _WINDOWS _CRT_SECURE_NO_WARNINGS _MBCS )
	
elseif(APPLE)
	add_compile_options(-Wall -O3 -arch x86_64 -Wno-deprecated)
else()
	add_compile_options(-Wall -O3 -std=c++11 -Wno-strict-aliasing -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-reorder)
endif(MSVC)


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

unset(GTEST_LIB CACHE)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(Testd 
		${SSC_TESTS}
		${SHARED_TEST}
		${TCS_TESTS}
		${INPUTS_SRC}
		main.cpp)
	set(TEST_EXE Testd)
	
	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_executable(Test 
		${SSC_TESTS}
		${SHARED_TEST}
		${TCS_TESTS}
		${INPUTS_SRC}
		main.cpp)
	set(TEST_EXE Test)
endif()

if (MSVC)
	set_target_properties(${TEST_EXE} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	find_library( GTEST_LIB
		NAMES libgtest.a gtestd.lib
		PATHS $ENV{GTDIR}/build/lib $ENV{GTDIR}/build/lib $ENV{GTDIR}/msvc/2010/gtest-md/x64-Debug)
	target_link_libraries(${TEST_EXE} splinterd ${GTEST_LIB} sscd)
else()
	find_library( GTEST_LIB
		NAMES libgtest.a gtest.lib
		PATHS $ENV{GTDIR}/build/lib $ENV{GTDIR}/build/lib $ENV{GTDIR}/msvc/2010/gtest-md/x64-Release)
	target_link_libraries(${TEST_EXE} splinter ${GTEST_LIB} ssc)
endif()

target_link_libraries(${TEST_EXE} -ldl -lpthread)

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
endif()

