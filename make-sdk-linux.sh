# script to copy binaries to <ssc>/sdk-release folder
cp -v linux_so/ssc64.so sdk-release/linux64/ssc.so
cp -v dev/linux_wx3/SSCdev sdk-release/linux64/SSCdev
file sdk-release/linux64/SSCdev > sdk-release/linux64/filetypes.txt
file sdk-release/linux64/ssc.so >> sdk-release/linux64/filetypes.txt
echo "Dynamic library dependencies: compiled on CentOS 6.3" > sdk-release/linux64/dependencies.txt
echo "SSCdev:" >> sdk-release/linux64/dependencies.txt
ldd sdk-release/linux64/SSCdev >> sdk-release/linux64/dependencies.txt
echo "ssc.so:" >> sdk-release/linux64/dependencies.txt
ldd sdk-release/linux64/ssc.so >> sdk-release/linux64/dependencies.txt

