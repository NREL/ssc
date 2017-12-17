all:
	make -f Makefile-shared -j
	make -f Makefile-nlopt -j
	make -f Makefile-lpsolve -j
	make -f Makefile-solarpilot -j
	make -f Makefile-tcs -j
	make -f Makefile-ssc -j
	make -f Makefile-sdktool -j
	make -f Makefile-tcsconsole -j
#	make -f Makefile-gtest -j 

clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	make -f Makefile-sdktool clean
	make -f Makefile-tcsconsole clean
#	make -f Makefile-gtest clean
