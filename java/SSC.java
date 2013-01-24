

public class SSC 
{
  private long ssc_data_ptr;
  private long ssc_module_ptr;
  private long ssc_info_ptr;
  private long ssc_entry_ptr;

  public SSC()
  {
    ssc_data_ptr = sscapiJNI.ssc_data_create();
    ssc_module_ptr = 0;
    ssc_info_ptr = 0;
    ssc_entry_ptr = 0;
  }

  public SSC( String module_name )
  {
    ssc_data_ptr = sscapiJNI.ssc_data_create();
    ssc_module_ptr = sscapiJNI.ssc_module_create(module_name);
    ssc_info_ptr = 0;
    ssc_entry_ptr = 0;
  }
  
   protected void finalize() 
   {
       delete();
   }

  public synchronized void delete() 
  {
    if (ssc_data_ptr != 0) 
    {
        sscapiJNI.ssc_data_free(ssc_data_ptr);
    }
    ssc_data_ptr = 0;
    if (ssc_module_ptr != 0) 
    {
        sscapiJNI.ssc_module_free(ssc_module_ptr);
    }
    ssc_module_ptr = 0;
    ssc_info_ptr = 0;
    ssc_entry_ptr = 0;
  }
  
  
  public int Version()
  {
      return sscapiJNI.ssc_version();
  }
  
  public String Build_Info()
  {
      return sscapiJNI.ssc_build_info();
  }
  
  public void Set_String(String name, String value)
  {
      sscapiJNI.ssc_data_set_string(ssc_data_ptr, name, value);
  }

  public void Set_Number(String name, float value)
  {
      sscapiJNI.ssc_data_set_number(ssc_data_ptr, name, value);
  }

  public float[] Get_Array(String name)
  {
      return sscapiJNI.ssc_data_get_array(ssc_data_ptr, name);
  }

  public float GetNumber(String name)
  {
      float[] value = {0};
      int ret = sscapiJNI.ssc_data_get_number(ssc_data_ptr, name, value);
      if (ret==0)
      {
          value[0] = Float.NaN;
      }
      return value[0];
  }
  
  public boolean Exec()
  {
      return (sscapiJNI.ssc_module_exec(ssc_module_ptr, ssc_data_ptr) != 0);
  }

  public boolean Module_Info(int index)
  {
      ssc_entry_ptr = sscapiJNI.ssc_module_entry(index);
      return (ssc_entry_ptr != 0);
  }
  
  public String Module_Name()
  {
      return sscapiJNI.ssc_entry_name(ssc_entry_ptr);
  }
  
  public String Module_Description()
  {
      return sscapiJNI.ssc_entry_description(ssc_entry_ptr);
  }

  public int Module_Version()
  {
      return sscapiJNI.ssc_entry_version(ssc_entry_ptr);
  }
  
}