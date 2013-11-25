VPATH = ..
WEXDIR = ../../../../wex
WEXLIB = $(WEXDIR)/wexuxwx3.a
LKDIR = ../../../lk
LKLIB = $(LKDIR)/lkuxwx3.a

CC = gcc
CXX = g++
CFLAGS = -g -std=c++0x -I. -I$(WEXDIR)/include -I$(LKDIR) -I../../ssc -I../../recore -DLK_USE_WXWIDGETS `wx-config-3 --cflags`
CXXFLAGS = $(CFLAGS)
LDFLAGS = -std=c++0x -lfontconfig $(WEXLIB) $(LKLIB) `wx-config-3 --libs stc` `wx-config-3 --libs aui` `wx-config-3 --libs` -lm 

OBJECTS  = cmform.o \
	dataview.o \
	dllinvoke.o \
	editvariable.o \
	scripting.o \
	sscdev.o \
	statdlg.o \
	
TARGET = SSCdev

$(TARGET): $(OBJECTS) $(WEXLIB) 
	$(CXX) -g -o $@ $^ $(LDFLAGS)
	
clean:
	rm -f $(TARGET) $(OBJECTS)
