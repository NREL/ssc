mkdir ..\Win32
copy /y c:\mingw\bin\libgcc_s_dw2-1.dll ..\win32\Debug
copy /y c:\mingw\bin\libgfortran-3.dll ..\win32\Debug
copy /y c:\mingw\bin\libquadmath-0.dll ..\win32\Debug
copy /y c:\mingw\bin\libgcc_s_dw2-1.dll ..\win32\Release
copy /y c:\mingw\bin\libgfortran-3.dll ..\win32\Release
copy /y c:\mingw\bin\libquadmath-0.dll ..\win32\Release
mingw32-make clean
mingw32-make
mingw32-make vclib32
copy /y waterprop32.dll ..\Win32\Debug
copy /y waterprop32.dll ..\Win32\Release


mkdir ..\x64
copy /y c:\mingw64\bin\libgcc_s_sjlj-1.dll ..\x64\Debug
copy /y c:\mingw64\bin\libgfortran-3.dll ..\x64\Debug
copy /y c:\mingw64\bin\libquadmath-0.dll ..\x64\Debug
copy /y c:\mingw64\bin\libgcc_s_sjlj-1.dll ..\x64\Release
copy /y c:\mingw64\bin\libgfortran-3.dll ..\x64\Release
copy /y c:\mingw64\bin\libquadmath-0.dll ..\x64\Release
mingw32-make CFG=64 clean
mingw32-make CFG=64
mingw32-make CFG=64 vclib64
copy /y waterprop64.dll ..\x64\Debug
copy /y waterprop64.dll ..\x64\Release
