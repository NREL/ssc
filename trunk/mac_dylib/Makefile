VPATH=../src

CC = gcc
CXX = g++
CFLAGS = -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++

OBJECTS = \
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
	sscapi.o \
	vtab_common.o

TARGET = ssc.dylib

$(TARGET) : $(OBJECTS)
	$(CXX) -dynamiclib -flat_namespace -o $@ $^

clean:
	rm -rf $(OBJECTS) $(TARGET)

