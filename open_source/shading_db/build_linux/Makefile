DIR_FILES = ../../../shared

CXX = g++
CFLAGS =  -g -Wall -I. 
CXXFLAGS= $(CFLAGS)


OBJECTS  = \
	lib_pv_shade_loss_mpp.cpp \
	lib_miniz.cpp \
	test.cpp
	
TARGET = shade_db

shade_db:
	cp $(DIR_FILES)/DB8_vmpp_impp_uint8_bin.h ./
	cp $(DIR_FILES)/lib_pv_shade_loss_mpp.h ./
	cp $(DIR_FILES)/lib_pv_shade_loss_mpp.cpp ./
	cp $(DIR_FILES)/lib_miniz.cpp ./
	cp $(DIR_FILES)/lib_miniz.h ./
	cp ../test.cpp  ./
	$(CXX) $(CXXFLAGS) -g -o $(TARGET) $(OBJECTS) 
	
clean:
	rm -rf $(TARGET) *.cpp *.h

all: clean shade_db
