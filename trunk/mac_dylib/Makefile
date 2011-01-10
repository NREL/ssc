VPATH=../src

CC = gcc
CXX = g++
CFLAGS = -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++

OBJECTS = \
	cmod_trntower.o \
	cmod_trnbase.o \
	cmod_trndish.o \
	cmod_ptflux.o \
	cmod_fossilgen.o \
	cmod_cashloan.o \
	cmod_easywatts.o \
	cmod_levpartflip.o \
	cmod_pvwatts.o \
	cmod_stdhrlywf.o \
	cmod_timeseq.o \
	cmod_utilityrate.o \
	cmod_windwatts.o \
	core.o \
	lib_financial.o \
	lib_pvwatts.o \
	lib_util.o \
	lib_vartab.o \
	lib_wfhrly.o \
	lib_windwatts.o \
	lib_getem.o \
	sscapi.o \
	vtab_common.o

TARGET = ssc.dylib

$(TARGET) : $(OBJECTS)
	$(CXX) -dynamiclib -flat_namespace -o $@ $^
	cp $(TARGET) ../python/ssc32

clean:
	rm -rf $(OBJECTS) $(TARGET)

