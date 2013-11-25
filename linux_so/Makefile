VPATH=../ssc ../recore

CC = gcc
CXX = g++
CFLAGS = -fpermissive -I../ssc -I../recore -D__UNIX__ -fPIC -Wall -O3
CXXFLAGS = $(CFLAGS)
LDFLAGS = -lm -lstdc++

ifeq ($(CFG),)
CFG=64
endif


ifeq ($(CFG),64)
CFLAGS += -D__64BIT__
TARGET=ssc64.so
else
CFLAGS += -D__32BIT__ -m32
TARGET=ssc32.so
LDFLAGS += -m32
endif

all: inform $(TARGET)


OBJECTS = \
	cmod_pvwattsfrompoa.o \
	cmod_pvwattsfunc.o \
	cmod_wfcsv.o \
	cmod_test_pvshade.o \
	cmod_geothermalui.o \
	cmod_6parsolve.o \
	cmod_windpower.o \
	cmod_annualoutput.o \
	cmod_test_irr.o \
	cmod_pv6parmod.o \
	cmod_pvsandiainv.o \
	cmod_pvsamv1.o \
	cmod_wfreader.o \
	cmod_irradproc.o \
	cmod_fossilgen.o \
	cmod_cashloan.o \
	cmod_pvwattsv1.o \
	cmod_equpartflip.o \
	cmod_levpartflip.o \
	cmod_saleleaseback.o \
	cmod_singleowner.o \
	cmod_timeseq.o \
	cmod_utilityrate.o \
	cmod_utilityrate2.o \
	cmod_ippppa.o \
	cmod_swh.o \
	cmod_geothermal.o \
	cmod_poacalib.o \
	core.o \
	lib_windfile.o \
	lib_cec6par.o \
	lib_pvshade.o \
	lib_sandia.o \
	lib_pvmodel.o \
	lib_irradproc.o \
	lib_powerblock.o \
	lib_financial.o \
	lib_pvwatts.o \
	lib_physics.o \
	lib_util.o \
	vartab.o \
	lib_weatherfile.o \
	lib_windwatts.o \
	lib_geothermal.o \
	lib_pvinv.o \
	sscapi.o \
	vtab_common.o

$(TARGET) : $(OBJECTS)
	$(CXX) $(CFLAGS) -shared -o $@ $^ $(LDFLAGS)
	gcc $(CFLAGS) dltest.c -o dltest -ldl
	./dltest ./$(TARGET)

inform:
ifneq ($(CFG),64)
ifneq ($(CFG),32)
	@echo "Invalid configuration "$(CFG)" specified."
	@echo "You must specify a configuration when running make."
	@echo  "make CFG=64"
	@echo
	@echo "Possible choices for CFG are '32' and '64'"
	@exit 1
endif
endif

clean:
	rm -rf $(OBJECTS) ssc32.so ssc64.so 

