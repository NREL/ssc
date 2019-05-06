#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

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


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W4 /MP)
	if (CMAKE_BUILD_TYPE STREQUAL Debug)
        add_compile_options(/MDd)
        add_compile_definitions(FPUexception NOFORTIFY _DEBUG LP_MAXLINELEN=0)
    else()
        add_compile_options(/MD /O2)
        add_compile_definitions(NDEBUG )
    endif()
    add_compile_definitions( LPWINAPP  _CONSOLE MSDOS CHECK_SOLUTION _SCL_SECURE_NO_DEPRECATE)
	add_compile_definitions( YY_NEVER_INTERACTIVE PARSER_LP INVERSE_ACTIVE=INVERSE_LUSOL )
	add_compile_definitions( RoleIsExternalInvEngine _CRT_SECURE_NO_DEPRECATE _CRT_NONSTDC_NO_DEPRECATE )
else()
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
	else()
		add_definitions(-D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -std=c++11" )
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall WX_PRECOMP CHECK_SOLUTION YY_NEVER_INTERACTIVE PARSER_LP
		INVERSE_ACTIVE=INVERSE_LUSOL RoleIsExternalInvEngine")
endif()


####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	add_library(lpsolved STATIC ${LPSOLVE_SRC})
	set(LPSOLVE_LIB lpsolved)

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
	add_library(lpsolve STATIC ${LPSOLVE_SRC})
	set(LPSOLVE_LIB lpsolve)

	set(CMAKE_C_FLAGS -DNDEBUG)
endif()
set_property(TARGET ${LPSOLVE_LIB} PROPERTY PREFIX "")

if (MSVC)
	set_target_properties(${LPSOLVE_LIB} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${CMAKE_CURRENT_SOURCE_DIR}>)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()

