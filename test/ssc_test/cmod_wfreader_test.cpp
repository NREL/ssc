#include <vector>
#include <string>
#include <gtest/gtest.h>

#include "vartab.h"

TEST(Wfreader_cmod_wfreader, Test) {
    auto mod = ssc_module_create("wfreader");
    auto data = ssc_data_create();
    ssc_data_set_string(data, "file_name", "/Users/dguittet/SAM-Dev/pysam/tests/blythe_ca_33.617773_-114.588261_psmv3_60_tmy.csv");
    ssc_data_set_number(data, "header_only", 1);
    EXPECT_TRUE(ssc_module_exec(mod, data));
}
