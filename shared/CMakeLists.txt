set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.12)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(shared)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. ../splinter)

set(SHARED_SRC
	lsqfit.cpp
	lib_fuel_cell.cpp
	lib_fuel_cell_dispatch.cpp
	lib_battery.cpp
	lib_battery_dispatch.cpp
	lib_battery_powerflow.cpp
	lib_cec6par.cpp
	lib_financial.cpp
	lib_geothermal.cpp
	lib_iec61853.cpp
	lib_irradproc.cpp
	lib_miniz.cpp
	lib_physics.cpp
	lib_powerblock.cpp
	lib_power_electronics.cpp
	lib_pvinv.cpp
	lib_pv_incidence_modifier.cpp
	lib_pv_io_manager.cpp
	lib_pvmodel.cpp
	lib_pvshade.cpp
	lib_pv_shade_loss_mpp.cpp
	lib_pvwatts.cpp
	lib_sandia.cpp
	lib_shared_inverter.cpp
	lib_snowmodel.cpp
	lib_time.cpp
	lib_util.cpp
	lib_utility_rate.cpp
	lib_weatherfile.cpp
	lib_windfile.cpp
	lib_wind_obos.cpp
	lib_wind_obos_cable_vessel.cpp
	lib_windwakemodel.cpp
	lib_windwatts.cpp
	lib_mlmodel.cpp
	lib_ondinv.cpp)

set_source_files_properties(${SHARED_SRC} PROPERTIES 
	COMPILE_FLAGS "-Wall -Wno-strict-aliasing -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-reorder  -Wno-ignored-attributes -Wno-deprecated")
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_library(sharedd STATIC ${SHARED_SRC})
  set(SHARED_LIB sharedd)
  set_property(TARGET sharedd PROPERTY PREFIX "")

  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  add_library(shared STATIC ${SHARED_SRC})
  set(SHARED_LIB shared)
  set_property(TARGET shared PROPERTY PREFIX "")
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
  add_compile_options(/W4 /WX)
  add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common -Wno-unused-private-field -Wunused-private-field)
  		add_compile_definitions(WX_PRECOMP)
	else()
		add_compile_options(-Werror -Wno-misleading-indentation -Wno-int-in-bool-context)
		add_compile_definitions(__64BIT__)
	endif()
  add_compile_options(-O3 -std=c++11)
endif(MSVC)

if (UNIX)
	target_link_options(${SHARED_LIB} PUBLIC "-lm")
endif()
