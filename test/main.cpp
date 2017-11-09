#include <stdlib.h>
#include <iostream>
#include <gtest/gtest.h>

GTEST_API_ int main(int argc, char **argv) {


	printf("Running main() from gtest_main.cc\n");
	testing::InitGoogleTest(&argc, argv);
	int status =  RUN_ALL_TESTS(); 

	if (!status)
		printf("Tests Pass!\n");
	std::cin.get();
	return status;
}