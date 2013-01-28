# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS

CLASS_PATH = ../bin
# Define a virtual path for .class in the bin directory
vpath %.class $(CLASS_PATH)

sources = $(wildcard *.java)
classes = $(sources:.java=.class)


ifdef SystemRoot
	RM = del /Q
	EXT = dll
	ifeq ($(CFG),64)
		BITS = 64
		CCCOMP = c:/MinGW64/bin/gcc.exe
		CIFLAGS = -I"C:\Program Files\Java\jdk1.7.0_10\include"  -I"C:\Program Files\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files\Java\jdk1.7.0_10\bin\java.exe
		CP_DLL = copy /y ..\x64\Release\ssc.dll ssc.dll
		CP_H = copy /y ..\ssc\sscapi.h sscapi.h
		CP_TM2 = copy /y ..\examples\abilene.tm2 abilene.tm2
	else
		BITS = 32
		CCCOMP = c:/MinGW/bin/gcc.exe
		CIFLAGS = -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include"  -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\java.exe
		CP_DLL = copy /y ..\Win32\Release\ssc.dll ssc.dll
		CP_H = copy /y ..\ssc\sscapi.h sscapi.h
		CP_TM2 = copy /y ..\examples\abilene.tm2 abilene.tm2
	endif	
else
	RM = rm -f
	EXT = dylib
	ifeq ($(CFG),"32")
		CFLAGS += -D__32BIT__ -m32 
		LDFLAGS += -arch i386 -m32
		BITS = 32
	else
		CFLAGS += -D__64BIT__
		BITS = 64
	endif
	CCCOMP = gcc
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
	$(RM) *.class SSC/*.class *.jar SSC\\*.jar


run: all
	$(JAVA) -jar TestSSCAPI.jar

all: jar


dll : SSCAPIJNI.dll

# $@ matches the target, $< matches the first dependancy
SSCAPIJNI.dll : 
	$(CP_H)
	$(CP_TM2)
	$(CP_DLL)
	$(CCCOMP) -D_JNI_IMPLEMENTATION_ -Wl,--kill-at $(CIFLAGS) -shared sscapi_wrap.c -o $@ ssc.dll
	
#c:\mingw64\bin\gcc -Wall -D_JNI_IMPLEMENTATION_ -Wl,--kill-at  -I"C:\Program Files\Java\jdk1.7.0_10\include"  -I"C:\Program Files\Java\jdk1.7.0_10\include\win32" -shared sscapi_wrap.c -o sscapiJNI64.dll ssc64.dll	
	
clean :
	$(RM) *.dll *.class SSC\\*.class SSC\\*.dll *.jar SSC\\*.jar *.h




