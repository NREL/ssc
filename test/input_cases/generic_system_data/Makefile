ifdef SystemRoot
	RM = del /Q
	EXT = .exe
 	CIFLAGS = -I.. -L.
 	LFLAGS = -lssc
#x64	SET PATH=c:\MinGW64\bin;%PATH%
#win32	SET PATH=c:\MinGW\bin;%PATH%
	CCCOMP = gcc
else
    PF = $(shell uname)
    ifneq (,$(findstring Darwin, $(PF)))
        VERS = $(shell sw_vers -productVersion)
 		CIFLAGS = -I..
        EXT = .dylib
    else 
        ifneq (,$findstring(Linux, $(PF)))
	    CIFLAGS = -I.. ./ssc.so
	    EXT = .o
    	endif
    endif
    RM = rm -f
    CCCOMP = gcc
 	LFLAGS = -ldl
endif
PROJ_NAME =  60-min-commercial
JFLAGS = -g
all :
	$(CCCOMP) $(CIFLAGS) -o$(PROJ_NAME) $(PROJ_NAME).c $(LFLAGS)
clean :
	$(RM) $(PROJ_NAME)$(EXT)
help:
	@echo "Please check the settings for your system.Your system may not be supported.Please contact sam.support@nrel.gov"
