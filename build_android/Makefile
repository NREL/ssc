# explicit for all architectures
# mixing architectures so remove object files before archiving
all: makes libraries

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
	mkdir x86
	mkdir armeabi
	mkdir armeabi-v7a
	
	cp shared_x86.a x86/shared.a
	cp nlopt_x86.a x86/nlopt.a
	cp lpsolve_x86.a x86/lpsolve.a
	cp solarpilot_x86.a x86/solarpilot.a
	cp tcs_x86.a x86/tcs.a
	cp ssc_x86.a x86/ssc.a
	
	cp shared_armeabi.a armeabi/shared.a
	cp nlopt_armeabi.a armeabi/nlopt.a
	cp lpsolve_armeabi.a armeabi/lpsolve.a
	cp solarpilot_armeabi.a armeabi/solarpilot.a
	cp tcs_armeabi.a armeabi/tcs.a
	cp ssc_armeabi.a armeabi/ssc.a
	
	cp shared_armeabi-v7a.a armeabi-v7a/shared.a
	cp nlopt_armeabi-v7a.a armeabi-v7a/nlopt.a
	cp lpsolve_armeabi-v7a.a armeabi-v7a/lpsolve.a
	cp solarpilot_armeabi-v7a.a armeabi-v7a/solarpilot.a
	cp tcs_armeabi-v7a.a armeabi-v7a/tcs.a
	cp ssc_armeabi-v7a.a armeabi-v7a/ssc.a


clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	rm -rf x86
	rm -rf armeabi
	rm -rf armeabi-v7a
	rm *.a

