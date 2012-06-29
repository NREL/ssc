# The directories containing the source files, separated by ':'
VPATH=.

# To make "debug" the default configuration if invoked with just "make":
#
ifeq ($(CFG),)
CFG=debug
endif

CMLDIR=../../comlib

# The source files: regardless of where they reside in the source tree,
# VPATH will locate them...
retool_SRC = \
	retool.cpp

# Build a Dependency list and an Object list, by replacing the .cpp
# extension to .d for dependency files, and .o for object files.
retool_DEP = $(patsubst %.cpp, deps/retool_%.d, ${retool_SRC})
retool_OBJ = $(patsubst %.cpp, objs.$(CFG)/retool_%.o, ${retool_SRC})

# Your final executable
TARGET=REtool


# What compiler to use for generating dependencies: it will be invoked with -MM
CXXDEP = g++

# What include flags to pass to the compiler
INCLUDEFLAGS= -g -D__WX__ -D__RELEASE__ -D__USENOTEBOOK__ -I. -I../../wxDView/wxDViewLib -I../../lk -I../recore -I../ssc  -I$(CMLDIR) `wx-config-dsa --cflags`

# Separate compile options per configuration
ifeq ($(CFG),debug)
CXXFLAGS += -arch i386 -Os -g -Wall ${INCLUDEFLAGS}
else
CXXFLAGS += -arch i386 -Os -g -Wall ${INCLUDEFLAGS}
endif

# A common link flag for all configurations

LDFLAGS = `wx-config-dsa --libs` `wx-config-dsa --libs stc` -lm $(CMLDIR)/comlib.a ../../wxDView/wxdviewlib.a ../../lk/wxosx_static/lk.a ../mac_dylib/ssc.dylib

all:	inform bin.$(CFG)/${TARGET}

inform:
ifneq ($(CFG),release)
ifneq ($(CFG),debug)
	@echo "Invalid configuration "$(CFG)" specified."
	@echo "You must specify a configuration when running make, e.g."
	@echo  "make CFG=debug"
	@echo  
	@echo  "Possible choices for configuration are 'release' and 'debug'"
	@exit 1
endif
endif
	@echo "Configuration "$(CFG)
	@echo "------------------------"



wxtop_srcdir=`wx-config-dsa --prefix`

bin.$(CFG)/${TARGET}: ${retool_OBJ} ${Group1_OBJ}  $(CMLDIR)/comlib.a ${CORETRACEA} 
	@mkdir -p bin.$(CFG)
	$(CXX) -g -o $@ $^ ${LDFLAGS}
	`wx-config-dsa --rescomp` $@
	SetFile -a C $@
	cp bin.$(CFG)/$(TARGET) ./REtool.app/Contents/MacOS
	cp ../mac_dylib/ssc.dylib ./REtool.app/Contents/Frameworks
	install_name_tool -id @executable_path/../Frameworks/ssc.dylib REtool.app/Contents/Frameworks/ssc.dylib
	install_name_tool -change ssc.dylib @executable_path/../Frameworks/ssc.dylib REtool.app/Contents/MacOS/REtool
	

objs.$(CFG)/retool_%.o: %.cpp
	@mkdir -p objs.$(CFG)
	$(CXX) -c $(CXXFLAGS) -o $@ $<

deps/retool_%.d: %.cpp
	@mkdir -p deps
	@echo Generating dependencies for $<
	@set -e ; $(CXXDEP) -MM $(INCLUDEFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,objs.$(CFG)\/retool_\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

clean:
	@rm -rf deps objs.$(CFG) bin.$(CFG)

# Unless "make clean" is called, include the dependency files
# which are auto-generated. Don't fail if they are missing
# (-include), since they will be missing in the first invocation!
ifneq ($(MAKECMDGOALS),clean)
-include ${retool_}
endif
