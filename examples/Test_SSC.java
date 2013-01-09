
public class Test_SSC {
    public static void PVWatts_test()
    {
        System.out.println("\nPVWatts_test begin");
        SSC sscobj = new SSC("pvwattsv1");
        sscobj.Set_String("file_name", "C:\\Projects\\SAM\\documentation\\Wrappers\\ssc\\jni\\ssctest\\AZ Phoenix.tm2" );
        sscobj.Set_Number("system_size", 1.0f );
        sscobj.Set_Number("derate", 0.77f );
        sscobj.Set_Number("track_mode", 0 );
        sscobj.Set_Number("tilt", 20 );
        sscobj.Set_Number("azimuth", 180 );
        if ( sscobj.Exec() )
        {
            int len[] = {0};
            float[] ac = sscobj.Get_Array( "ac", len );
            float sum = 0;
            for( int i=0;i<len[0];i++)
            {
                sum += ac[i];
            }
            System.out.println("length returned: " + len[0]);
            System.out.println("ac total (get array): " + sum);
            System.out.println("PVWatts example passed");
        }
        else
        {
            System.out.println("PVWatts example failed");
        }
        System.out.println("PVWatts_test end");
    }
    
    public static void Static_test()
    {
        System.out.println("\nStatic_test begin");
        SSC sscobj = new SSC();
        System.out.println("ssc version = " + sscobj.Version());
        System.out.println("ssc build info = " + sscobj.Build_Info());
        System.out.println("Static_test end");
    }

    public static void Module_list_test()
    {
        System.out.println("\nModule_list_test begin");
        SSC sscobj = new SSC();
        int index = 0;
        while( sscobj.Module_Info( index++ ) )
        {
            String module_name = sscobj.Module_Name();
            String description = sscobj.Module_Description();
            int version = sscobj.Module_Version();
            System.out.println( "Module: " + module_name + ", version: " + version);
            System.out.println( "    " + description );
        }        
        System.out.println("Module_list_test end");
    }
    
    public static void main(String[] args)
    {
        System.loadLibrary("sscapiJNI64");
        Static_test();
        Module_list_test();
        PVWatts_test();
    }    
}
