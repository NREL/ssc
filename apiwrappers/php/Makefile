PHPDIR=/usr/include/php

sscphp.so: sscphp.c
	gcc -shared -O2 -fPIC -I$(PHPDIR) -I$(PHPDIR)/TSRM -I$(PHPDIR)/main -I$(PHPDIR)/ext -I$(PHPDIR)/Zend -o sscphp.so sscphp.c /home/adobos/Projects/ssc/build_linux/ssc.so

clean:
	rm sscphp.so
