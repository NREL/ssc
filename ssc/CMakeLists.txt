set(CMAKE_VERBOSE_MAKEFILE ON)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
elseif(MSVC)
    set(CMAKE_SYSTEM_VERSION 8.1 CACHE TYPE INTERNAL FORCE)
endif()

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(ssc)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
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
	cmod_generic_system-builder.cpp
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

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 11)

# set build types for shared library


set( DEPENDENCIES
	solarpilot
	tcs
	nlopt
	shared
	lpsolve
	splinter
	)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_library(sscd SHARED ${SSC_SRC})
	set(SSC_LIB sscd)
	set_property(TARGET sscd PROPERTY PREFIX "")

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(ssc SHARED ${SSC_SRC})
	set(SSC_LIB ssc)
	if(NOT system_advisor_model_EXPORT)
		set_property(TARGET ssc PROPERTY PREFIX "")
	endif()
endif()


# setup linking dependencies and flags
foreach( name ${DEPENDENCIES} )
	# force redefinition of library paths in case build type changes
	unset(${name}_LIB CACHE)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		target_link_libraries(${SSC_LIB} ${name}d)
	else()
		target_link_libraries(${SSC_LIB} ${name})
	endif()
endforeach()
if (UNIX)
	target_link_libraries(${SSC_LIB} -lm -ldl -lstdc++)
endif()

if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /w44191 /w44242  /w44266 /w44302 /w44388 /w44826 /w44905 /w44906 /w44928)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(_DEBUG)
    else()
        add_compile_options(/MD /O4)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( __DLL__ __64BIT__ __WINDOWS__ _WINDOWS _CRT_SECURE_NO_WARNINGS __VISUALC__ )
	set_target_properties(${SSC_LIB} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
		add_compile_definitions(__MACOSX__ WX_PRECOMP __64BIT__)
	else ()
		add_definitions(-D__UNIX__ -D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unknown-pragmas -O3 -std=c++11" )

endif(MSVC)


# dltest executable for checking ssc library
if(UNIX)
	add_executable(dltest ../build_osx/dltest.c)
	if (NOT APPLE)
		target_link_libraries(dltest -ldl -Wl,--no-as-needed)
		add_custom_command(
			TARGET ${SSC_LIB}
			POST_BUILD 
			COMMAND dltest ${CMAKE_CURRENT_SOURCE_DIR}/${SSC_LIB}.so)
	elseif  ()
		add_custom_command(
			TARGET ${SSC_LIB}
			POST_BUILD 
			COMMAND dltest ${SSC_LIB})
	endif()
endif()


# alternately, building for PySAM, Release type
if (system_advisor_model_EXPORT)
	set(CMAKE_MACOSX_RPATH 1)
	if(APPLE)
		set_property(TARGET ${SSC_LIB} PROPERTY PREFIX "lib")
		set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
		add_custom_command(
			TARGET ${SSC_LIB}
			POST_BUILD
			COMMAND install_name_tool -id @rpath/libssc.so $ENV{SSCDIR}/ssc/libssc.so
		)
	endif(APPLE)
endif()