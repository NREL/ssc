set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_C_COMPILER gcc)
set(CMAKE_CXX_COMPILER g++)

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(ssc)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(. ../splinter ../shared ../nlopt ../lpsolve ../solarpilot ../tcs ../ssc)

set(SSC_SRC
	cmod_singlediode.cpp
	cmod_ui_tes_calcs.cpp
	csp_common.cpp
	cmod_lcoefcr.cpp
	cmod_thirdpartyownership.cpp
	cmod_host_developer.cpp
	cmod_snowmodel.cpp
	cmod_iec61853par.cpp
	cmod_layoutarea.cpp
	cmod_solarpilot.cpp
	cmod_dsg_flux_preprocess.cpp
	cmod_iscc_design_point.cpp
	cmod_sco2_design_point.cpp
	cmod_belpe.cpp
	cmod_biomass.cpp
	cmod_annualoutput.cpp
	cmod_fuelcell.cpp
	cmod_generic_system.cpp
	cmod_wfcheck.cpp
	cmod_pvwattsv5.cpp
	cmod_pvwattsv5_lifetime.cpp
	cmod_windfile.cpp
	cmod_pvwattsv1_1ts.cpp
	cmod_pvwattsv1_poa.cpp
	cmod_battwatts.cpp
	cmod_wfcsv.cpp
	cmod_6parsolve.cpp
	cmod_windpower.cpp
	cmod_windbos.cpp
	cmod_wind_obos.cpp
	cmod_windcsm.cpp
	cmod_pv6parmod.cpp
	cmod_pvsandiainv.cpp
	cmod_pvsamv1.cpp
	cmod_wfreader.cpp
	cmod_irradproc.cpp
	cmod_fossilgen.cpp
	cmod_cashloan.cpp
	cmod_pvwattsv1.cpp
	cmod_pvwattsv0.cpp
	cmod_equpartflip.cpp
	cmod_levpartflip.cpp
	cmod_saleleaseback.cpp
	cmod_singleowner.cpp
	cmod_timeseq.cpp
	cmod_utilityrate.cpp
	cmod_utilityrate2.cpp
	cmod_utilityrate3.cpp
	cmod_utilityrate4.cpp
	cmod_utilityrate5.cpp
	cmod_thermalrate.cpp
	cmod_ippppa.cpp
	cmod_swh.cpp
	cmod_geothermal.cpp
	cmod_geothermal_costs.cpp
	cmod_poacalib.cpp
	core.cpp
	vartab.cpp
	sscapi.cpp
	common.cpp
	common_financial.cpp
	cmod_tcsdirect_steam.cpp
	cmod_tcsgeneric_solar.cpp
	cmod_tcslinear_fresnel.cpp
	cmod_tcsmolten_salt.cpp
	cmod_tcstrough_empirical.cpp
	cmod_tcstrough_physical.cpp
	cmod_tcstrough_physical_csp_solver.cpp
	cmod_trough_physical.cpp
	cmod_trough_physical_iph.cpp
	cmod_iph_to_lcoefcr.cpp
	cmod_linear_fresnel_dsg_iph.cpp
	cmod_tcsdish.cpp
	cmod_tcsiscc.cpp
	cmod_hcpv.cpp
	cmod_tcsmslf.cpp
	cmod_battery.cpp
	cmod_user_htf_comparison.cpp
	cmod_cb_mspt_system_costs.cpp
	cmod_cb_construction_financing.cpp
	cmod_cb_empirical_hce_heat_loss.cpp
	cmod_sco2_design_point.cpp
	cmod_sco2_design_cycle.cpp
	cmod_sco2_csp_system.cpp
	cmod_sco2_csp_ud_pc_tables.cpp
	cmod_sco2_air_cooler.cpp
	cmod_inv_cec_cg.cpp
	cmod_pv_get_shade_loss_mpp.cpp
	tckernel.cpp )

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# set build types for shared library

# change when making toplevel file
set(SSC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/..")
set( DEPENDENCIES
	splinter
	shared
	solarpilot
	tcs
	lpsolve
	nlopt)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_library(sscd SHARED ${SSC_SRC})
	set(SSC_LIB sscd)
	set_property(TARGET sscd PROPERTY PREFIX "")

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(ssc SHARED ${SSC_SRC})
	set(SSC_LIB ssc)
	set_property(TARGET ssc PROPERTY PREFIX "")
endif()

# setup linking dependencies and flags
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
	target_link_libraries(${SSC_LIB} ${${name}_LIB})
endforeach()
target_link_libraries(${SSC_LIB} -lm -lstdc++)


# dltest executable for checking ssc library
if(UNIX)
	add_executable(dltest ../build_osx/dltest.c)
	if (NOT APPLE)
		target_compile_options(dltest PUBLIC -ldl)
	endif()

	add_custom_command(
		TARGET ${SSC_LIB}
		COMMENT "Run dltest"
		DEPENDS ${SSC_LIB}
		POST_BUILD 
		COMMAND dltest ${SSC_LIB}.dylib
	)
endif()

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
	add_compile_options(-Wall -O3 -arch x86_64 -fno-common)
	add_compile_definitions(WX_PRECOMP __64BIT__)
	if(APPLE)
		add_compile_definitions(__MACOSX__)
	endif()
endif(MSVC)
