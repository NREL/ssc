# script to copy binaries to <ssc>/sdk-release folder
cp -v build_linux/ssc.so sdk-release/linux64/ssc.so
cp -v build_linux/SDKtool sdk-release/linux64/SDKtool
file sdk-release/linux64/SDKtool > sdk-release/linux64/filetypes.txt
file sdk-release/linux64/ssc.so >> sdk-release/linux64/filetypes.txt
echo "Dynamic library dependencies: compiled on CentOS 6.4" > sdk-release/linux64/dependencies.txt
echo "SDKtool:" >> sdk-release/linux64/dependencies.txt
ldd sdk-release/linux64/SDKtool >> sdk-release/linux64/dependencies.txt
echo "ssc.so:" >> sdk-release/linux64/dependencies.txt
ldd sdk-release/linux64/ssc.so >> sdk-release/linux64/dependencies.txt

