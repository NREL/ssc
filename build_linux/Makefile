all:
	make -s -f Makefile-shared -j4
	make -s -f Makefile-nlopt -j4
	make -s -f Makefile-lpsolve -j4
	make -s -f Makefile-solarpilot -j4
	make -s -f Makefile-tcs -j4
	make -s -f Makefile-ssc -j4
	make -s -f Makefile-sdktool -j4
	make -s -f Makefile-tcsconsole -j4 
	make -s -f Makefile-gtest -j4 

clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	make -f Makefile-sdktool clean
	make -f Makefile-tcsconsole clean
