# explicit for all architectures
# mixing architectures so remove object files before archiving
all: makes libraries zip

makes:
	make -f Makefile-splinter -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-shared -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-tcs -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-ssc -j8 ARCH=arm64-v8a
	rm *.o


libraries:
	mkdir lib
	mkdir lib/arm64-v8a
	
	cp splinter_arm64-v8a.a lib/arm64-v8a/splinter.a
	cp shared_arm64-v8a.a lib/arm64-v8a/shared.a
	cp nlopt_arm64-v8a.a lib/arm64-v8a/nlopt.a
	cp lpsolve_arm64-v8a.a lib/arm64-v8a/lpsolve.a
	cp solarpilot_arm64-v8a.a lib/arm64-v8a/solarpilot.a
	cp tcs_arm64-v8a.a lib/arm64-v8a/tcs.a
	cp ssc_arm64-v8a.a lib/arm64-v8a/ssc.a
	
	rm *.a

zip:
	zip -r libssc.zip lib

clean:
	make -f Makefile-splinter clean
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	rm -rf lib
	rm libssc.zip

