set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.13)

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
endif()

# setup linking dependencies and flags
target_link_libraries(${TEST_EXE} splinter ${GTEST_LIB} ssc) 
target_link_libraries(${TEST_EXE} -ldl -lpthread)


get_target_property(OUT ${TEST_EXE} LINK_LIBRARIES)
message(STATUS ${OUT})

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W3 /WX)
	add_compile_definitions( _MBCS )
elseif(APPLE)
	add_compile_options(-Wall -O3 -arch x86_64 -Wno-deprecated)
else()
	add_compile_options(-Wall -O3 -std=c++11 -Wno-strict-aliasing -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-reorder)
	#add_compile_definitions()
endif(MSVC)


