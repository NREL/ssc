# mixing architectures so remove object files before archiving
all:
	make -f Makefile-shared -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-shared -j4 ARCH=i386
	rm *.o
	make -f Makefile-shared -j4 ARCH=arm64
	rm *.o
	make -f Makefile-nlopt -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-nlopt -j4 ARCH=i386
	rm *.o
	make -f Makefile-nlopt -j4 ARCH=arm64
	rm *.o
	make -f Makefile-lpsolve -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-lpsolve -j4 ARCH=i386
	rm *.o
	make -f Makefile-lpsolve -j4 ARCH=arm64
	rm *.o
	make -f Makefile-solarpilot -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-solarpilot -j4 ARCH=i386
	rm *.o
	make -f Makefile-solarpilot -j4 ARCH=arm64
	rm *.o
	make -f Makefile-tcs -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-tcs -j4 ARCH=i386
	rm *.o
	make -f Makefile-tcs -j4 ARCH=arm64
	rm *.o
	make -f Makefile-ssc -j4 ARCH=x86_64
	rm *.o
	make -f Makefile-ssc -j4 ARCH=i386
	rm *.o
	make -f Makefile-ssc -j4 ARCH=arm64
	rm *.o

clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean

libraries:
	lipo -create shared_i386.a shared_x86_64.a shared_arm64.a -output shared.a	
	lipo -create nlopt_i386.a nlopt_x86_64.a nlopt_arm64.a -output nlopt.a
	lipo -create lpsolve_i386.a lpsolve_x86_64.a lpsolve_arm64.a -output lpsolve.a
	lipo -create solarpilot_i386.a solarpilot_x86_64.a solarpilot_arm64.a -output solarpilot.a
	lipo -create tcs_i386.a tcs_x86_64.a tcs_arm64.a -output tcs.a
	lipo -create ssc_i386.a  ssc_x86_64.a ssc_arm64.a -output ssc.a
