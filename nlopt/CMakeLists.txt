#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/config.h)
	execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/configure)
	execute_process(COMMAND cp config.h ${CMAKE_CURRENT_SOURCE_DIR})
endif()

include_directories(.)

set(NLOPT_SRC
	auglag.cpp
	bobyqa.cpp
	ccsa_quadratic.cpp
	cdirect.cpp
	cobyla.cpp
	crs.cpp
	deprecated.cpp
	DIRect.cpp
	direct_wrap.cpp
	DIRserial.cpp
	DIRsubrout.cpp
	esch.cpp
	general.cpp
	hybrid.cpp
	isres.cpp
	mlsl.cpp
	mma.cpp
	mssubs.cpp
	mt19937ar.cpp
	newuoa.cpp
	nldrmd.cpp
	optimize.cpp
	options.cpp
	plip.cpp
	plis.cpp
	pnet.cpp
	praxis.cpp
	pssubs.cpp
	qsort_r.cpp
	redblack.cpp
	rescale.cpp
	sbplx.cpp
	slsqp.cpp
	sobolseq.cpp
	stop.cpp

)

if(MSVC)
	list( APPEND NLOPT_SRC sys/time.cpp timer.c)
else()
	list( APPEND NLOPT_SRC timer.c)
endif()


####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_library(nlopt STATIC ${NLOPT_SRC})
set_target_properties(nlopt
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)
if (MSVC)
	set_target_properties(nlopt
		PROPERTIES
		LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()

set_default_compile_options(nlopt)
set_no_warnings(nlopt)
if(MSVC)
    set_additional_compile_options(nlopt "/D__WXMSW__ /D_MBCS")
else(MSVC)
    set_additional_compile_options(nlopt "-Wall")
endif(MSVC)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()

