# explicit for all architectures
# mixing architectures so remove object files before archiving
all: makes libraries zip

makes:
	make -f Makefile-shared -j8 ARCH=x86
	rm *.o
	make -f Makefile-shared -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-shared -j8 ARCH=armeabi
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=x86
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armeabi
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=x86
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armeabi
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=x86
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armeabi
	rm *.o
	make -f Makefile-tcs -j8 ARCH=x86
	rm *.o
	make -f Makefile-tcs -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-tcs -j8 ARCH=armeabi
	rm *.o
	make -f Makefile-ssc -j8 ARCH=x86
	rm *.o
	make -f Makefile-ssc -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-ssc -j8 ARCH=armeabi
	rm *.o


libraries:
	mkdir lib
	mkdir lib/x86
	mkdir lib/armeabi
	mkdir lib/armeabi-v7a
	
	cp shared_x86.a lib/x86/shared.a
	cp nlopt_x86.a lib/x86/nlopt.a
	cp lpsolve_x86.a lib/x86/lpsolve.a
	cp solarpilot_x86.a lib/x86/solarpilot.a
	cp tcs_x86.a lib/x86/tcs.a
	cp ssc_x86.a lib/x86/ssc.a
	
	cp shared_armeabi.a lib/armeabi/shared.a
	cp nlopt_armeabi.a lib/armeabi/nlopt.a
	cp lpsolve_armeabi.a lib/armeabi/lpsolve.a
	cp solarpilot_armeabi.a lib/armeabi/solarpilot.a
	cp tcs_armeabi.a lib/armeabi/tcs.a
	cp ssc_armeabi.a lib/armeabi/ssc.a
	
	cp shared_armeabi-v7a.a lib/armeabi-v7a/shared.a
	cp nlopt_armeabi-v7a.a lib/armeabi-v7a/nlopt.a
	cp lpsolve_armeabi-v7a.a lib/armeabi-v7a/lpsolve.a
	cp solarpilot_armeabi-v7a.a lib/armeabi-v7a/solarpilot.a
	cp tcs_armeabi-v7a.a lib/armeabi-v7a/tcs.a
	cp ssc_armeabi-v7a.a lib/armeabi-v7a/ssc.a
	rm *.a

zip:
	zip -r libssc.zip lib

clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	rm -rf lib
	rm libssc.zip

