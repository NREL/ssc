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
	JNILIB = SSCAPIJNI.dll
	ifeq ($(CFG),32)
		BITS = 32
		CCCOMP = c:/MinGW/bin/gcc.exe
#Update based on your Java installation location		

		CIFLAGS = -Wl,--kill-at -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include"  -I"C:\Program Files (x86)\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files (x86)\Java\jdk1.7.0_10\bin\java.exe
		CP_DLL = ..\..\Win32\ssc32.dll ssc.dll
		SSCLIB = ssc.dll
	else
		BITS = 64
		CCCOMP = c:/MinGW64/bin/gcc.exe

#Update based on your Java installation location		
 		CIFLAGS = -Wl,--kill-at -I"C:\Program Files\Java\jdk1.7.0_10\include"  -I"C:\Program Files\Java\jdk1.7.0_10\include\win32"
		JC = C:\Program Files\Java\jdk1.7.0_10\bin\javac.exe
		JAR = C:\Program Files\Java\jdk1.7.0_10\bin\jar.exe
		JAVA = C:\Program Files\Java\jdk1.7.0_10\bin\java.exe
		
		CP_DLL = ..\..\Win64\ssc64.dll ssc.dll
		SSCLIB = ssc.dll
	endif	
	CP_H = ..\..\sscapi.h sscapi.h
	CP_TM2 = ..\..\examples\abilene.tm2 abilene.tm2
	RM_JAVA = *.class SSC\\*.class *.jar 
	RM_ALL = *.dll *.class SSC\\*.class SSC\\*.dll *.jar *.h  *.tm2
else
	RM = rm -f
	CP = cp -f
	EXT = dylib
	JNILIB = libSSCAPIJNI.jnilib
	CFLAGS += -D__64BIT__
	BITS = 64
#Update based on your Java installation location		
	CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Headers -I/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers

	JC = javac
	JAR = jar
	JAVA = java
	CP_DLL = ../mac_dylib/ssc64.dylib ./ssc64.dylib
	SSCLIB = ssc64.dylib
	CCCOMP = gcc
	CP_H = ../../sscapi.h ./sscapi.h
	CP_TM2 = ../../examples/abilene.tm2 ./abilene.tm2
	RM_JAVA = *.class ./SSC/*.class *.jar 
	RM_ALL = *.dylib *.jnilib *.class ./SSC/*.class ./SSC/*.dylib *.jar *.h  *.tm2
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
    


run: all
	$(JAVA) -jar TestSSCAPI.jar

all: jar


java_all:  classes

java_classes: $(CLASSES:.java=.class)


jar: java_classes dll
	$(JAR) cvfm TestSSCAPI.jar Manifest.txt TestSSCAPI.class SSC/*.class		

java_clean:
	$(RM) $(RM_JAVA)


# $@ matches the target, $< matches the first dependancy
dll : 
	$(CP) $(CP_H)
	$(CP) $(CP_TM2)
	$(CP) $(CP_DLL)
	$(CCCOMP) -D_JNI_IMPLEMENTATION_  $(CIFLAGS) -shared sscapi_wrap.c -o $(JNILIB) $(SSCLIB)
	
	
clean :
	$(RM) $(RM_ALL)




