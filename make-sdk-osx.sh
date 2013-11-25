cp -v mac_dylib/ssc.dylib sdk-release/osx64/ssc.dylib
mkdir -p sdk-release/osx64/SSCdev.app
cp -v -R dev/osx_wx3/SSCdev.app/* sdk-release/osx64/SSCdev.app
