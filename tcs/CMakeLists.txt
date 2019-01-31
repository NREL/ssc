set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(tcs)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. ../nlopt ../shared ../splinter ../lpsolve ../solarpilot)

set(TCS_SRC
	tcskernel.cpp
	fmin_callbacks.cpp
	fmin.cpp
	direct_steam_receivers.cpp
	CO2_properties.cpp
	co2_compressor_library.cpp
	nlopt_callbacks.cpp
	numeric_solvers.cpp
	heat_exchangers.cpp
	sco2_pc_csp_int.cpp
	sco2_rec_util.cpp
	sco2_power_cycle.cpp
	sco2_cycle_components.cpp
	water_properties.cpp
	tc_test_type402.cpp
	sam_sco2_recomp_type424.cpp
	sco2_test_type401.cpp
	sco2_recompression_cycle.cpp
	sco2_partialcooling_cycle.cpp
    cavity_calcs.cpp
	atmospheric_aod.cpp
	Heliostat_AzElAod.cpp
    datatest.cpp
    direct_steam_receivers.cpp
    tou_translator.cpp
    htf_props.cpp
    interpolation_routines.cpp
    powerblock.cpp
    pump.cpp
    sam_csp_util.cpp
    thermocline_tes.cpp
    ngcc_powerblock.cpp
	interconnect.cpp
    sam_dsg_controller_type265.cpp
    sam_lf_st_pt_type232.cpp
    sam_mw_csp_SumCalcs.cpp
	sam_mw_pt_heliostatfield_csp_solver.cpp
    sam_mw_gen_Type260_csp_solver.cpp
    sam_mw_lf_type261_steam.cpp
    sam_mw_lf_type261_Wnet.cpp
    sam_mw_lf_type262_salt.cpp
    sam_mw_pt_type222_csp_solver.cpp
    sam_mw_pt_type224_csp_solver.cpp
    sam_mw_pt_type228.cpp
    sam_mw_trough_type250.cpp
	sam_type250_input_generator.cpp
    sam_mw_trough_type251.cpp
    sam_mw_type234.cpp
    sam_iscc_powerblock.cpp
    sam_iscc_parasitics.cpp
    sam_pf_dish_collector_type295.cpp
    sam_pf_dish_engine_type297.cpp
    sam_pf_dish_parasitic_type298.cpp
    sam_pf_dish_receiver_type296.cpp
    sam_trough_model_type805.cpp
    sam_trough_plant_type807.cpp
    sam_trough_storage_type806.cpp
    solarcollector.cpp
    storage_hx.cpp
    sumprod.cpp
	trnsys_weatherreader.cpp
	typelib.cpp
	waterprop.cpp
	weatherreader_csp_solver.cpp
	csp_solver_weatherreader.cpp
	csp_solver_util.cpp
	csp_solver_two_tank_tes.cpp
	csp_solver_tou_block_schedules.cpp
	csp_solver_pt_heliostatfield.cpp
	csp_solver_pc_Rankine_indirect_224.cpp
	csp_solver_pc_gen.cpp
	csp_solver_pc_heat_sink.cpp
	csp_solver_pc_steam_heat_sink.cpp
	csp_solver_pc_sco2.cpp
	csp_solver_mspt_receiver_222.cpp
	csp_solver_mspt_collector_receiver.cpp
	csp_solver_trough_collector_receiver.cpp
	csp_solver_lf_dsg_collector_receiver.cpp
	csp_solver_gen_collector_receiver.cpp
	csp_solver_core.cpp
	csp_solver_mono_eq_methods.cpp
	csp_system_costs.cpp
	csp_solver_pt_sf_perf_interp.cpp
	ud_power_cycle.cpp
	csp_dispatch.cpp
)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_library(tcsd STATIC ${TCS_SRC})
  set_property(TARGET tcsd PROPERTY PREFIX "")

  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  add_library(tcs STATIC ${TCS_SRC})
  set_property(TARGET tcs PROPERTY PREFIX "")
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
  add_compile_options(/W4 /WX)
  add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
  add_compile_options(-Wall -O3 -arch x86_64 -fno-common)
  add_compile_definitions(WX_PRECOMP)
endif(MSVC)
