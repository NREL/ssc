set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 3.7)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")

if(MSVC)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Debug and Release Builds Configured" FORCE)
endif()

Project(nlopt)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file (
  config.h.in
  config.h COPYONLY
 )

include_directories(.)

set(NLOPT_SRC
	auglag.c
	bobyqa.c
	ccsa_quadratic.c
	cdirect.c
	cobyla.c
	crs.c
	deprecated.c
	DIRect.c
	direct_wrap.c
	DIRserial.c
	DIRsubrout.c
	esch.c
	general.c
	hybrid.c
	isres.c
	mlsl.c
	mma.c
	mssubs.c
	mt19937ar.c
	newuoa.c
	nldrmd.c
	optimize.c
	options.c
	plip.c
	plis.c
	pnet.c
	praxis.c
	pssubs.c
	qsort_r.c
	redblack.c
	rescale.c
	sbplx.c
	slsqp.c
	sobolseq.c
	stop.c
	timer.c
)

set(CMAKE_CXX_STANDARD 11)

# set build types for archive
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_library(nloptd STATIC ${NLOPT_SRC})
  set_property(TARGET nloptd PROPERTY PREFIX "")

  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  add_library(nlopt STATIC ${NLOPT_SRC})
  set_property(TARGET nlopt PROPERTY PREFIX "")
endif()



if(MSVC)
# need msvc optimization levels & different configs, windows sdk
  add_compile_options(/W4 /WX)
  add_compile_definitions( _CRT_SECURE_NO_WARNINGS _MBCS __WXMSW__)
else(MSVC)
  add_compile_options(-Wall -Wno-deprecated-declarations -O3 -arch x86_64 -fno-common)
  add_compile_definitions(WX_PRECOMP)
endif(MSVC)
