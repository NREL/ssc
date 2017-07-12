mkdir sdk-release/android/armeabi
mkdir sdk-release/android/armeabi-v7a
mkdir sdk-release/android/x86
cp -v -R build_android/armeabi/* sdk-release/android/armeabi
cp -v -R build_android/armeabi-v7a/* sdk-release/android/armeabi-v7a
cp -v -R build_android/x86/* sdk-release/android/x86
