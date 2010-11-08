VPATH=../src

CC = gcc
CXX = g++
CFLAGS = -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++

OBJECTS = \
	cmod_easywatts.o \
	cmod_pvwatts.o \
	cmod_stdhrlywf.o \
	cmod_timeseq.o \
	cmod_windwatts.o \
	core.o \
	lib_pvwatts.o \
	lib_util.o \
	lib_vartab.o \
	lib_wfhrly.o \
	lib_windwatts.o \
	sscapi.o

TARGET = ssc32.so

$(TARGET) : $(OBJECTS)
	$(CXX) -shared -o $@ $^ $(LDFLAGS)

clean:
	rm -rf $(OBJECTS) $(TARGET)

