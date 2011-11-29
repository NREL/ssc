VPATH=../src

CC = gcc
CXX = g++
CFLAGS = -g -arch i386 -D__MACOSX__ -D__32BIT__ -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++ -arch i386

OBJECTS = \
	cmod_6parsolve.o \
	cmod_test_irr.o \
	cmod_wfreader.o \
	cmod_irradproc.o \
	cmod_trntower.o \
	cmod_trnbase.o \
	cmod_trndish.o \
	cmod_ptflux.o \
	cmod_fossilgen.o \
	cmod_cashloan.o \
	cmod_pvwattsv1.o \
	cmod_equpartflip.o \
	cmod_levpartflip.o \
	cmod_saleleaseback.o \
	cmod_singleowner.o \
	cmod_pvwatts.o \
	cmod_timeseq.o \
	cmod_utilityrate.o \
	cmod_windwatts.o \
	cmod_ippppa.o \
	cmod_swhsolopt.o \
	cmod_geohourly.o \
	cmod_trntrough.o \
	core.o \
	lib_irradproc.o \
	lib_powerblock.o \
	lib_financial.o \
	lib_pvwatts.o \
	lib_physics.o \
	lib_util.o \
	lib_vartab.o \
	lib_weatherfile.o \
	lib_windwatts.o \
	lib_geohourly.o \
	lib_sandia.o \
	sscapi.o \
	vtab_common.o

TARGET = ssc.dylib

$(TARGET) : $(OBJECTS)
	$(CXX) $(LDFLAGS) -dynamiclib -flat_namespace -o $@ $^
	cp $(TARGET) ../python/ssc32

clean:
	rm -rf $(OBJECTS) $(TARGET)

