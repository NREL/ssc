set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.13)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(lpsolve)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include_directories(.)

set(LPSOLVE_SRC
	colamd.c
	commonlib.c
	fortify.c
	ini.c
	lp_crash.c
	lp_Hash.c
	lp_lib.c
	lp_LUSOL.c
	lp_matrix.c
	lp_MDO.c
	lp_mipbb.c
	lp_MPS.c
	lp_params.c
	lp_presolve.c
	lp_price.c
	lp_pricePSE.c
	lp_report.c
	lp_rlp.c
	lp_scale.c
	lp_simplex.c
	lp_solve.c
	lp_SOS.c
	lp_utils.c
	lp_wlp.c
	lusol.c
	mmio.c
	myblas.c
	yacc_read.c)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_library(lpsolved STATIC ${LPSOLVE_SRC})
	set_property(TARGET lpsolved PROPERTY PREFIX "")

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(lpsolve STATIC ${LPSOLVE_SRC})
	set_property(TARGET lpsolve PROPERTY PREFIX "")
	set(CMAKE_C_FLAGS -DNDEBUG)
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
	add_compile_options(/W4 /WX)
	add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else()
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
	else()
		add_compile_definitions(__64BIT__)
	endif()
	add_compile_options(-Wall -O3 -std=c++11)
	add_compile_definitions(WX_PRECOMP CHECK_SOLUTION YY_NEVER_INTERACTIVE PARSER_LP INVERSE_ACTIVE=INVERSE_LUSOL RoleIsExternalInvEngine)
endif()
