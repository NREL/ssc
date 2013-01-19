using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace CS_SSC_API
{

    class sscapiPINVOKE
    {


        static sscapiPINVOKE()
        {
        }


        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_version")]
        public static extern int ssc_version();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_build_info")]
        public static extern IntPtr ssc_build_info();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_INVALID_get")]
        public static extern int SSC_INVALID_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_STRING_get")]
        public static extern int SSC_STRING_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_NUMBER_get")]
        public static extern int SSC_NUMBER_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_ARRAY_get")]
        public static extern int SSC_ARRAY_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_MATRIX_get")]
        public static extern int SSC_MATRIX_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_TABLE_get")]
        public static extern int SSC_TABLE_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_create")]
        public static extern IntPtr ssc_data_create();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_free")]
        public static extern void ssc_data_free(HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
        public static extern void ssc_data_clear(HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
        public static extern void ssc_data_unassign(HandleRef cxtData, string variableName);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
        public static extern int ssc_data_query(HandleRef cxtData, string variableName);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
        public static extern string ssc_data_first(HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
        public static extern string ssc_data_next(HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
        public static extern void ssc_data_set_string(HandleRef cxtData, string name, string value);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
        public static extern void ssc_data_set_number(HandleRef cxtData, string name, float value);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
        public static extern void ssc_data_set_array(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
        public static extern void ssc_data_set_matrix(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[][] matrix, int nRows, int nCols);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
        public static extern void ssc_data_set_table(HandleRef cxtData, string name, HandleRef cxtTable);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
        public static extern string ssc_data_get_string(HandleRef cxtData, string name);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
        public static extern int ssc_data_get_number(HandleRef cxt_dat, string name, out float number);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
        public static extern IntPtr ssc_data_get_array(HandleRef cxt_data, string name, out int len);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
        public static extern IntPtr ssc_data_get_matrix(HandleRef cxtData, string name, out int nRows, out int nCols);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
        public static extern IntPtr ssc_data_get_table(HandleRef cxtData, string name);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
        public static extern IntPtr ssc_module_entry(int moduleIndex);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
        public static extern IntPtr ssc_entry_name(HandleRef cxtEntry);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
        public static extern IntPtr ssc_entry_description(HandleRef cxtEntry);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
        public static extern int ssc_entry_version(HandleRef cxtEntry);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_create")]
        public static extern IntPtr ssc_module_create(string moduleName);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
        public static extern void ssc_module_free(HandleRef cxtModule);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_INPUT_get")]
        public static extern int SSC_INPUT_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_OUTPUT_get")]
        public static extern int SSC_OUTPUT_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_INOUT_get")]
        public static extern int SSC_INOUT_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
        public static extern IntPtr ssc_module_var_info(HandleRef cxtInfo, int jarg2);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
        public static extern int ssc_info_var_type(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
        public static extern int ssc_info_data_type(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
        public static extern string ssc_info_name(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
        public static extern string ssc_info_label(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
        public static extern string ssc_info_units(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
        public static extern string ssc_info_meta(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
        public static extern string ssc_info_group(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
        public static extern string ssc_info_required(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
        public static extern string ssc_info_constraints(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
        public static extern string ssc_info_uihint(HandleRef cxtInfo);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
        public static extern int ssc_module_exec_simple(string moduleName, HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
        public static extern string ssc_module_exec_simple_nothread(string moduleName, HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_LOG_get")]
        public static extern int SSC_LOG_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_UPDATE_get")]
        public static extern int SSC_UPDATE_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_EXECUTE_get")]
        public static extern int SSC_EXECUTE_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
        public static extern int ssc_module_exec(HandleRef cxtModule, HandleRef cxtData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
        public static extern int ssc_module_exec_with_handler(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_extproc_output")]
        public static extern void ssc_module_extproc_output(HandleRef cxtModule, string outputLine);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_NOTICE_get")]
        public static extern int SSC_NOTICE_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_WARNING_get")]
        public static extern int SSC_WARNING_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_ERROR_get")]
        public static extern int SSC_ERROR_get();

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
        public static extern string ssc_module_log(HandleRef cxtModule, int index, HandleRef messageType, [Out, MarshalAs(UnmanagedType.LPArray)]float[] time);

        [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__ssc_segfault")]
        public static extern void __ssc_segfault();
    }
    
    
    
    
    
    
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
          float value=0;

          int res = sscapiPINVOKE.ssc_data_get_number(ssc_data_ptr, name, out value);
          if (res == 0)
          {
              result = float.NaN;
          }
          else
          {
              result = value;
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
