CC = gcc
CXX = g++
CFLAGS = -arch i386 -I. -Wall -g
CXXFLAGS = $(CFLAGS)

OBJECTS = lib_cec6par.o\
	lib_financial.o\
	lib_geothermal.o\
	lib_irradproc.o\
	lib_physics.o\
	lib_powerblock.o\
	lib_pvmodel.o\
	lib_pvwatts.o\
	lib_sandia.o\
	lib_util.o\
	lib_weatherfile.o\
	lib_windwatts.o

TARGET=../recore.a

$(TARGET):$(OBJECTS)
	ar rs $(TARGET) $(OBJECTS)

clean:
	rm -rf $(TARGET) $(OBJECTS)

