all:
	make -f Makefile-shared -j8
	make -f Makefile-nlopt -j8
	make -f Makefile-lpsolve -j8
	make -f Makefile-solarpilot -j8
	make -f Makefile-tcs -j8
	make -f Makefile-ssc -j8
	make -f Makefile-sdktool -j8
	make -f Makefile-tcsconsole -j8
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
