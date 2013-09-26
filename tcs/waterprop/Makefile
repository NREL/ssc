# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS


CFLAGS = -Wall -O2

ifdef SystemRoot
	RM = del /Q
	EXT = dll
	ifeq ($(CFG),64)
		BITS = 64
		GFORT = c:/MinGW64/bin/gfortran.exe
		CCCOMP = c:/MinGW64/bin/gcc.exe
	else
		BITS = 32
		GFORT = c:/MinGW/bin/gfortran.exe
		CCCOMP = c:/MinGW/bin/gcc.exe
	endif	
	LDFLAGS = -lgfortran -Wl,--output-def,waterprop$(BITS).def,--out-implib,waterprop$(BITS).a
else
	RM = rm -f
	EXT = dylib
	ifeq ($(CFG),"32")
		CFLAGS += -D__32BIT__ -m32 -fPIC
		LDFLAGS += -arch i386 -m32
		BITS = 32
	else
		CFLAGS += -D__64BIT__ -fPIC
		BITS = 64
	endif
	GFORT = gfortran
	CCCOMP = gcc
endif

ifdef LKDIR
	CFLAGS += -I$(LKDIR) -D__WITHLK__
endif
	

waterprop$(BITS).$(EXT) : waterprop.o module_water_properties.o
	echo $(CFG)
	$(GFORT) $(LDFLAGS) -shared -o $@ $^ -lgfortran 

vclib32 : waterprop$(BITS).def
	lib.exe /machine:i386 /def:waterprop$(BITS).def
	
vclib64 : waterprop$(BITS).def
	lib.exe /machine:x64 /def:waterprop$(BITS).def

module_water_properties.o : module_water_properties.f90
	$(GFORT) -o $@ -ffree-line-length-none -fimplicit-none $(CFLAGS) -c $^

waterprop.o : waterprop.c
	$(CCCOMP) -o $@ -DMAKE_DLL $(CFLAGS) -c $^ 

clean:
	$(RM) waterprop.o module_water_properties.o water_properties.mod waterprop$(BITS).$(EXT)
