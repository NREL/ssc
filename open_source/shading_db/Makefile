ifdef SystemRoot
	DIR_FILES = ..\..\shared
	CD = 
	RM = del /Q
	CP = copy /y
else
	DIR_FILES = ../../shared
	CD = ./
	RM = rm -rf
	CP = cp -f
endif
	

shade_db:
ifdef SystemRoot
	$(CP) $(DIR_FILES)\DB8_vmpp_impp_uint8_bin.h $(CD)
	$(CP) $(DIR_FILES)\lib_pv_shade_loss_mpp.h $(CD)
	$(CP) $(DIR_FILES)\lib_pv_shade_loss_mpp.cpp $(CD)
	$(CP) $(DIR_FILES)\lib_miniz.cpp $(CD)
	$(CP) $(DIR_FILES)\lib_miniz.h $(CD)
else
	$(CP) $(DIR_FILES)/DB8_vmpp_impp_uint8_bin.h $(CD)
	$(CP) $(DIR_FILES)/lib_pv_shade_loss_mpp.h $(CD)
	$(CP) $(DIR_FILES)/lib_pv_shade_loss_mpp.cpp $(CD)
	$(CP) $(DIR_FILES)/lib_miniz.cpp $(CD)
	$(CP) $(DIR_FILES)/lib_miniz.h $(CD)
endif

clean:
	$(RM) DB8_vmpp_impp_uint8_bin.h
	$(RM) lib_pv_shade_loss_mpp.h
	$(RM) lib_pv_shade_loss_mpp.cpp
	$(RM) lib_miniz.cpp
	$(RM) lib_miniz.h

all: clean shade_db