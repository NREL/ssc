VPATH = ../../../shared


CC = gcc
CXX = g++
CFLAGS =  -g -O2 -I. -I$(VPATH) 
LDFLAGS = -std=gnu++11 
CXXFLAGS= $(CFLAGS)


OBJECTS  = \
	lib_pv_shade_loss_mpp.o \
	lib_miniz.o \
	test.o
	
TARGET = shade_db


$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) -g -o $@ $^ $(LDFLAGS)
	
clean:
	del  $(TARGET) $(OBJECTS) $(TARGET).exe
