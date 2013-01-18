using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace CS_SSC_API
{
    public class SSC 
    {
        private HandleRef ssc_data_ptr;
        private HandleRef ssc_module_ptr;
        private HandleRef ssc_info_ptr;
        private HandleRef ssc_entry_ptr;

      public SSC()
      {
        ssc_data_ptr = new HandleRef(null, sscapiPINVOKE.ssc_data_create());
      }

      public SSC( String module_name )
      {
        ssc_data_ptr = new HandleRef(null, sscapiPINVOKE.ssc_data_create());
        ssc_module_ptr = new HandleRef(null, sscapiPINVOKE.ssc_module_create(module_name));
      }
  
  
  
      public int Version()
      {
          return sscapiPINVOKE.ssc_version();
      }
  
      public String BuildInfo()
      {
          IntPtr buildInfo = sscapiPINVOKE.ssc_build_info();
          return Marshal.PtrToStringAnsi(buildInfo);
      }
  
      public void Set_String(String name, String value)
      {
          sscapiPINVOKE.ssc_data_set_string(ssc_data_ptr, name, value);
      }

      public void SetNumber(String name, float value)
      {
          sscapiPINVOKE.ssc_data_set_number(ssc_data_ptr, name, value);
      }

      public float GetNumber(String name)
      {
          float result = 0;
          float[] arr = new float[1];

          int res = sscapiPINVOKE.ssc_data_get_number(ssc_data_ptr, name, arr);
          if (res == 0)
          {
              result = 0;
          }
          else
          {
              result = arr[0];
          }
          return result;
      }


      public float[] GetArray(String name)
      {
          int len;
          IntPtr res = sscapiPINVOKE.ssc_data_get_array(ssc_data_ptr, name,out len);
          float[] arr = new float[len];
          Marshal.Copy(res, arr, 0, len);
          return arr;
      }
  
      public bool Exec()
      {
          return (sscapiPINVOKE.ssc_module_exec(ssc_module_ptr, ssc_data_ptr) != 0);
      }

      public bool ModuleInfo(int index)
      {
          IntPtr ip  = sscapiPINVOKE.ssc_module_entry(index);
          ssc_entry_ptr = new HandleRef(null,ip);
          return (ip != IntPtr.Zero);
      }
  
      public String ModuleName()
      {
          IntPtr moduleName = sscapiPINVOKE.ssc_entry_name(ssc_entry_ptr);
          return Marshal.PtrToStringAnsi(moduleName);
      }
  
      public String ModuleDescription()
      {
          IntPtr moduleDescription = sscapiPINVOKE.ssc_entry_description(ssc_entry_ptr);
          return Marshal.PtrToStringAnsi(moduleDescription);
      }

      public int Module_Version()
      {
          return sscapiPINVOKE.ssc_entry_version(ssc_entry_ptr);
      }
    }
}
