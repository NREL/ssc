DIR_FILES = ..\..\..\shared

CXX = g++
CFLAGS =  -g -O2 -I. 
CXXFLAGS= $(CFLAGS)


OBJECTS  = \
	lib_pv_shade_loss_mpp.cpp \
	lib_miniz.cpp \
	test.cpp
	
TARGET = shade_db

shade_db:
	copy $(DIR_FILES)\DB8_vmpp_impp_uint8_bin.h
	copy $(DIR_FILES)\lib_pv_shade_loss_mpp.h
	copy $(DIR_FILES)\lib_pv_shade_loss_mpp.cpp
	copy $(DIR_FILES)\lib_miniz.cpp
	copy $(DIR_FILES)\lib_miniz.h
	copy ..\test.cpp
	$(CXX) $(CXXFLAGS) -g -o $(TARGET) $(OBJECTS) 
	
clean:
	del  *.exe *.obj *.cpp *.h

all: clean shade_db