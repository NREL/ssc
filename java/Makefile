# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS

CLASS_PATH = ../bin
# Define a virtual path for .class in the bin directory
vpath %.class $(CLASS_PATH)

sources = $(wildcard *.java)
classes = $(sources:.java=.class)


ifdef SystemRoot
	RM = del /Q
	CP = copy /y
	EXT = dll
	ifeq ($(CFG),32)
		BITS = 32
		CCCOMP = c:/MinGW/bin/gcc.exe
		CIFLAGS = -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include"  -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\java.exe
		CP_DLL = ..\Win32\Release\ssc.dll ssc.dll
	else
		BITS = 64
		CCCOMP = c:/MinGW64/bin/gcc.exe
		CIFLAGS = -I"C:\Program Files\Java\jdk1.7.0_10\include"  -I"C:\Program Files\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files\Java\jdk1.7.0_10\bin\java.exe
		CP_DLL = ..\x64\Release\ssc.dll ssc.dll
	endif	
	CP_H = ..\ssc\sscapi.h sscapi.h
	CP_TM2 = ..\examples\abilene.tm2 abilene.tm2
	RM_JAVA = *.class SSC\\*.class *.jar 
	RM_ALL = *.dll *.class SSC\\*.class SSC\\*.dll *.jar *.h  *.tm2
else
	RM = rm -f
	CP = cp -f
	EXT = dylib
	ifeq ($(CFG),"32")
		CFLAGS += -D__32BIT__ -m32 
		LDFLAGS += -arch i386 -m32
		BITS = 32
	else
		CFLAGS += -D__64BIT__
		BITS = 64
		CIFLAGS = -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include"  -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include\win32"
		JC = javac
		JAR = jar
		JAVA = java
		CP_DLL = ../mac_dylib/ssc64.dylib ./ssc.dylib
	endif
	CCCOMP = gcc
	CP_H = ../ssc/sscapi.h ./sscapi.h
	CP_TM2 = ../examples/abilene.tm2 ./abilene.tm2
	RM_JAVA = *.class ./SSC/*.class *.jar 
	RM_ALL = *.dll *.class ./SSC/*.class ./SSC/*.dll *.jar *.h  *.tm2
endif




JFLAGS = -g
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
	TestSSCAPI.java \
	SSC/sscapiJNI.java \
	SSC/API.java \
	SSC/Data.java \
	SSC/Module.java \
	SSC/Entry.java \
	SSC/Info.java 
    

java_all:  classes

classes: $(CLASSES:.java=.class)


jar: classes dll
	$(JAR) cvfm TestSSCAPI.jar Manifest.txt TestSSCAPI.class SSC/*.class		

java_clean:
	$(RM) $(RM_JAVA)


run: all
	$(JAVA) -jar TestSSCAPI.jar

all: jar


# $@ matches the target, $< matches the first dependancy
dll : 
	$(CP) $(CP_H)
	$(CP) $(CP_TM2)
	$(CP) $(CP_DLL)
	$(CCCOMP) -D_JNI_IMPLEMENTATION_ -Wl,--kill-at $(CIFLAGS) -shared sscapi_wrap.c -o SSCAPIJNI.$(EXT) ssc.$(EXT)
	
	
clean :
	$(RM) $(RM_ALL)




