set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(ssc_test)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. input_cases shared_test ssc_test tcs_test ../ssc ../tcs ../solarpilot ../shared ../splinter $ENV{GTDIR}/googletest/include)

file(GLOB SSC_TESTS ssc_test/*.cpp)
file(GLOB SHARED_TESTS shared_test/*.cpp)
file(GLOB TCS_TESTS tcs_test/*.cpp)
file(GLOB INPUTS_SRC input_cases/*.cpp)

set(CMAKE_CXX_STANDARD 11)

# clear library paths to re-trigger search given new build
unset(GTEST_LIB CACHE)
unset(SSC_LIB CACHE)
unset(SPLINTER_LIB CACHE)
# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_executable(Testd 
		${SSC_TESTS}
		${SHARED_TEST}
		${TCS_TESTS}
		${INPUTS_SRC}
		main.cpp)
	set(TEST_EXE Testd)
	find_library( GTEST_LIB
		NAMES libgtest.a gtestd.lib
		PATHS $ENV{GTDIR}/build/lib $ENV{GTEST}/build/lib)
	find_library( SSC_LIB
		NAMES sscd.dylib sscd.lib
		PATHS $ENV{SSCDIR}/ssc)
	find_library( SPLINTER_LIB
		NAMES splinterd.a splinterd.lib
		PATHS $ENV{SSCDIR}/splinter)
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
	find_library( GTEST_LIB
		NAMES libgtest.a gtest.lib
		PATHS $ENV{GTDIR}/build/lib $ENV{GTEST}/build/lib)
	find_library( SSC_LIB
		NAMES ssc.dylib ssc.lib
		PATHS $ENV{SSCDIR}/ssc)
	find_library( SPLINTER_LIB
		NAMES splinter.a splinter.lib
		PATHS $ENV{SSCDIR}/splinter)
endif()

# setup linking dependencies and flags
target_link_libraries(${TEST_EXE} -ldl -lpthread)
target_link_libraries(${TEST_EXE} ${GTEST_LIB} ${SSC_LIB} ${SPLINTER_LIB}) 


get_target_property(OUT ${TEST_EXE} LINK_LIBRARIES)
message(STATUS ${OUT})

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W3 /WX)
	add_compile_definitions( _MBCS )
else(MSVC)
	add_compile_options(-Wall -O2 -arch x86_64 -Wno-deprecated)
endif(MSVC)


