#/bin/sh

# script to copy binaries to <ssc>/sdk-release folder
cp mac_dylib/ssc64.dylib sdk-release/osx64/ssc64.dylib
mkdir -p sdk-release/osx64/SSCdev.app
cp -R dev/SSCdev.app/* sdk-release/osx64/SSCdev.app


