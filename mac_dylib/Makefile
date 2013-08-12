VPATH=../ssc ../recore

CC = gcc
CXX = g++
CFLAGS = -I../ssc -I../recore -g -D__MACOSX__ -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++

ifeq ($(CFG),)
CFG=64
endif

ifeq ($(CFG),64)
CFLAGS += -D__64BIT__
TARGET = ssc64.dylib
else
CFLAGS += -D__32BIT__ -arch i386
TARGET = ssc.dylib
LDFLAGS += -arch i386
endif

OBJECTS = \
	cmod_pvwattsfunc.o \
	cmod_test_pvshade.o \
	cmod_geothermalui.o \
	cmod_6parsolve.o \
	cmod_windpower.o \
	cmod_annualoutput.o \
	cmod_test_irr.o \
	cmod_pv6parmod.o \
	cmod_pvsandiainv.o \
	cmod_pvsamv1.o \
	cmod_wfreader.o \
	cmod_irradproc.o \
	cmod_fossilgen.o \
	cmod_cashloan.o \
	cmod_pvwattsv1.o \
	cmod_equpartflip.o \
	cmod_levpartflip.o \
	cmod_saleleaseback.o \
	cmod_singleowner.o \
	cmod_timeseq.o \
	cmod_utilityrate.o \
	cmod_utilityrate2.o \
	cmod_ippppa.o \
	cmod_swh.o \
	cmod_geothermal.o \
	cmod_poacalib.o \
	core.o \
	lib_windfile.o \
	lib_pvinv.o \
	lib_cec6par.o \
	lib_pvshade.o \
	lib_sandia.o \
	lib_pvmodel.o \
	lib_irradproc.o \
	lib_powerblock.o \
	lib_financial.o \
	lib_pvwatts.o \
	lib_physics.o \
	lib_util.o \
	vartab.o \
	lib_weatherfile.o \
	lib_windwatts.o \
	lib_geothermal.o \
	lib_sandia.o \
	sscapi.o \
	vtab_common.o

$(TARGET) : $(OBJECTS)
	$(CXX) $(LDFLAGS) -dynamiclib -flat_namespace -o $@ $^
	gcc $(CFLAGS) dltest.c -o dltest -ldl
	./dltest ./$(TARGET)

clean:
	rm -rf $(OBJECTS) $(TARGET) ./dltest

