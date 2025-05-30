#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

# For CMake build option `SAMAPI_EXPORT`, see ../CMakeLists.txt

include_directories(. ../splinter ../shared ../nlopt ../lpsolve ../solarpilot ../tcs ../ssc)

set(SSC_SRC
		cmod_6parsolve.cpp
		cmod_analysisperiodchange_eqns.cpp
		cmod_analysisperiodchange_eqns.h
		cmod_battery.cpp
		cmod_battery.h
		cmod_battery_eqns.cpp
		cmod_battery_eqns.h
        cmod_battery_stateful.cpp
        cmod_battery_stateful.h
		cmod_battwatts.cpp
		cmod_battwatts.h
		cmod_belpe.cpp
		cmod_biomass.cpp
		cmod_cashloan.cpp
		cmod_cashloan_heat.cpp
		cmod_cb_construction_financing.cpp
		cmod_cb_empirical_hce_heat_loss.cpp
		cmod_csp_dsg_lf_ui.cpp
        cmod_csp_heatsink.cpp
		cmod_csp_subcomponent.cpp
        cmod_custom_generation.cpp
		cmod_etes_electric_resistance.cpp
		cmod_etes_ptes.cpp
		cmod_equpartflip.cpp
		cmod_financial_eqns.cpp
		cmod_financial_eqns.h
        cmod_fresnel_physical.cpp
        cmod_fresnel_physical_iph.cpp
		cmod_fuelcell.cpp
		cmod_fuelcell.h
		cmod_generic_system-builder.cpp
		cmod_generic_system-builder.h
		cmod_geothermal.cpp
		cmod_geothermal_costs.cpp
        cmod_geothermal_costs_eqns.cpp
        cmod_geothermal_costs_eqns.h
		cmod_grid.cpp
		cmod_grid.h
		cmod_hcpv.cpp
		cmod_host_developer.cpp
		cmod_hybrid.cpp
		cmod_hybrid_steps.cpp
		cmod_iec61853par.cpp
		cmod_inv_cec_cg.cpp
		cmod_iph_to_lcoefcr.cpp
		cmod_irradproc.cpp
		cmod_layoutarea.cpp
		cmod_lcoefcr.cpp
		cmod_levpartflip.cpp
		cmod_linear_fresnel_dsg_iph.cpp
		cmod_merchantplant.cpp
		cmod_merchantplant_eqns.cpp
		cmod_merchantplant_eqns.h
		cmod_mhk_costs.cpp
		cmod_mhk_eqns.cpp
		cmod_mhk_eqns.h
		cmod_mhk_tidal.cpp
		cmod_mhk_wave.cpp
		cmod_mspt_sf_and_rec_isolated.cpp
        cmod_mspt_iph.cpp
        cmod_ptes_design_point.cpp
		cmod_pv6parmod.cpp
		cmod_pv_get_shade_loss_mpp.cpp
		cmod_pvsamv1.cpp
		cmod_pvsamv1.h
		cmod_pvsamv1_eqns.cpp
		cmod_pvsamv1_eqns.h
		cmod_pvsandiainv.cpp
		cmod_pvwattsv5.cpp
		cmod_pvwattsv7.cpp
		cmod_pvwattsv8.cpp
		cmod_saleleaseback.cpp
		cmod_sco2_air_cooler.cpp
		cmod_sco2_csp_system.cpp
		cmod_sco2_csp_ud_pc_tables.cpp
		cmod_singlediode.cpp
		cmod_singleowner.cpp
		cmod_singleowner_heat.cpp
		cmod_communitysolar.cpp
		cmod_snowmodel.cpp
		cmod_solarpilot.cpp
		cmod_swh.cpp
		cmod_tcsgeneric_solar.cpp
		cmod_tcslinear_fresnel.cpp
		cmod_tcsmolten_salt.cpp
		cmod_tcstrough_empirical.cpp
		cmod_test_ud_power_cycle.cpp
		cmod_thermalrate.cpp
        cmod_thermalrate_iph.cpp
		cmod_thirdpartyownership.cpp
		cmod_timeseq.cpp
		cmod_trough_physical.cpp
		cmod_trough_physical_iph.cpp
		cmod_ui_udpc_checks.cpp
		cmod_user_htf_comparison.cpp
		cmod_utilityrate5.cpp
		cmod_utilityrate5.h
		cmod_wavefile.cpp
        	cmod_tidalfile.cpp
		cmod_utilityrate5_eqns.cpp
		cmod_utilityrate5_eqns.h
        	cmod_utilityrateforecast.cpp
        	cmod_utilityrateforecast.h
		cmod_wfcheck.cpp
		cmod_wfcsv.cpp
		cmod_wfreader.cpp
		cmod_wind_obos.cpp
		cmod_windbos.cpp
		cmod_windcsm.cpp
		cmod_windfile.cpp
		cmod_windpower.cpp
		cmod_windpower.h
		cmod_windpower_eqns.cpp
		cmod_windpower_eqns.h
        	cmod_wind_landbosse.cpp
        	cmod_wind_landbosse.h
		common.cpp
		common.h
		common_financial.cpp
		common_financial.h
		core.cpp
		core.h
		csp_common.cpp
		csp_common.h
		ssc_equations.h
        sscapi.cpp
		sscapi.h
		tckernel.cpp
		tckernel.h
		vartab.cpp
		vartab.h
		)

if(MSVC)
	set	(SSC_SRC
		${SSC_SRC}
		vbapi.cpp
		vbapi.h
		)
endif(MSVC)


#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_library(ssc SHARED ${SSC_SRC})
set_target_properties(ssc
    PROPERTIES
    PREFIX ""
    DEBUG_POSTFIX "d"
	)

set_disabled_warnings(ssc)
set_default_compile_options(ssc)

if(MSVC)
    set_additional_compile_options(ssc "-D__DLL__ -D__VISUALC__")
else()
    set(SRC_UNUSED_VARS
            cmod_mhk_eqns.cpp
            cmod_csp_tower_eqns.cpp
            cmod_merchantplant.cpp
            cmod_merchantplant_eqns.cpp
            cmod_pvsamv1_eqns.cpp
            cmod_financial_eqns.cpp
            cmod_windpower_eqns.cpp
            cmod_utilityrate5_eqns.cpp
            cmod_geothermal_costs_eqns.cpp)
    set_source_files_properties(${SRC_UNUSED_VARS} PROPERTIES COMPILE_FLAGS -Wno-unused-variable)
endif()

# dltest executable for checking ssc library
if(UNIX AND NOT SAMAPI_EXPORT)
	add_executable(dltest ../build_resources/dltest.c)
	if (APPLE)
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND codesign --force --deep --sign - $<TARGET_FILE:ssc>
			COMMAND dltest $<TARGET_FILE:ssc>)
	else()
		target_link_libraries(dltest -ldl -Wl,--no-as-needed)
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND dltest $<TARGET_FILE:ssc>)
	endif()
endif()

# libssc for PySAM
if(UNIX)
	if (SAMAPI_EXPORT)
		set(CMAKE_MACOSX_RPATH 1)
		set_property(TARGET ssc PROPERTY PREFIX "lib")
		set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
		if (APPLE)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmacosx-version-min=12")
            add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND install_name_tool -id @rpath/libssc$<$<CONFIG:Debug>:d>.so $<TARGET_FILE:ssc>
			)
        endif()
	endif()
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

set( DEPENDENCIES
	solarpilot_core
	tcs
	nlopt
	shared
	lpsolve
	splinter
	)
foreach( name ${DEPENDENCIES} )
	target_link_libraries(ssc PUBLIC ${name})
endforeach()

if (UNIX)
	find_package(Threads REQUIRED)
	target_link_libraries(ssc PUBLIC Threads::Threads ${CMAKE_DL_LIBS}) 
	find_library(MATH_LIBRARY m)
	if(MATH_LIBRARY)
		target_link_libraries(ssc PUBLIC ${MATH_LIBRARY})
	endif()
    if(ENABLE_COVERAGE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_link_options(ssc PRIVATE --coverage)
        target_link_libraries(ssc PUBLIC gcov)
    endif()    
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
	set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
	set(SSC_H ${CMAKE_CURRENT_LIST_DIR}/sscapi.h)

	set(TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/../test/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	if(DEFINED ENV{SAMNTDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E make_directory $ENV{SAMNTDIR}/deploy/x64
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${SSC_H} $ENV{SAMNTDIR}/deploy/x64
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${TESTDIR}
			)
	endif()

	set (SDKTOOLDIR ${CMAKE_CURRENT_BINARY_DIR}/../sdktool/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
	if (EXISTS ${SDKTOOLDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${SDKTOOLDIR}
			)
	endif()

	set (TCSCONSOLDEDIR ${CMAKE_CURRENT_BINARY_DIR}/../tcsconsole/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
	if (EXISTS ${TCSCONSOLDEDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${TCSCONSOLDEDIR}>
			)
	endif()

	if (SAMAPI_EXPORT)
		if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
			set(APIGENDIR ${CMAKE_CURRENT_BINARY_DIR}/../../sam/api/api_autogen/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
			add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND ${CMAKE_COMMAND}
				ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${APIGENDIR}
				)
		else()
			set(APIGENDIR $ENV{SAMNTDIR}/build/api/api_autogen/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
            if (EXISTS ${APIGENDIR})
			    add_custom_command(
				    TARGET ssc
				    POST_BUILD
				    COMMAND ${CMAKE_COMMAND}
				    ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${APIGENDIR}
				    )
               endif()
		endif()
	endif()

	add_custom_target(build-time-make-directory ALL
		COMMAND ${CMAKE_COMMAND} -E make_directory ${TESTDIR} ${APIGENDIR})
	add_dependencies(ssc build-time-make-directory)

endif()

if(DEFINED ENV{PYSAMDIR})
	if (SAMAPI_EXPORT OR MSVC)
		if (MSVC)
			add_custom_command(
					TARGET ssc
					POST_BUILD
					COMMAND ${CMAKE_COMMAND} -E remove
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:ssc>.lib
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:ssc>.dll
			)
			set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
		endif()
		add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND ${CMAKE_COMMAND}
				ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} $ENV{PYSAMDIR}/files
		)
	endif()
endif()

