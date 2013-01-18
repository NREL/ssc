using System;
using System.Runtime.InteropServices;

class sscapiPINVOKE {


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
  public static extern void ssc_data_free(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
  public static extern void ssc_data_clear(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
  public static extern void ssc_data_unassign(HandleRef jarg1, string jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
  public static extern int ssc_data_query(HandleRef jarg1, string jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
  public static extern string ssc_data_first(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
  public static extern string ssc_data_next(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
  public static extern void ssc_data_set_string(HandleRef jarg1, string jarg2, string jarg3);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
  public static extern void ssc_data_set_number(HandleRef jarg1, string jarg2, float jarg3);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
  public static extern void ssc_data_set_array(HandleRef jarg1, string jarg2, HandleRef jarg3, int jarg4);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
  public static extern void ssc_data_set_matrix(HandleRef jarg1, string jarg2, HandleRef jarg3, int jarg4, int jarg5);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
  public static extern void ssc_data_set_table(HandleRef jarg1, string jarg2, HandleRef jarg3);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
  public static extern string ssc_data_get_string(HandleRef jarg1, string jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
  public static extern int ssc_data_get_number(HandleRef cxt_dat, string name, HandleRef number);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
  public static extern IntPtr ssc_data_get_array(HandleRef cxt_data, string name, out int len);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
  public static extern IntPtr ssc_data_get_matrix(HandleRef jarg1, string jarg2, HandleRef jarg3, HandleRef jarg4);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
  public static extern IntPtr ssc_data_get_table(HandleRef jarg1, string jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
  public static extern IntPtr ssc_module_entry(int jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
  public static extern IntPtr ssc_entry_name(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
  public static extern IntPtr ssc_entry_description(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
  public static extern int ssc_entry_version(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention=CallingConvention.Cdecl,EntryPoint = "ssc_module_create")]
  public static extern IntPtr ssc_module_create(string jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
  public static extern void ssc_module_free(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_INPUT_get")]
  public static extern int SSC_INPUT_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_OUTPUT_get")]
  public static extern int SSC_OUTPUT_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_INOUT_get")]
  public static extern int SSC_INOUT_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
  public static extern IntPtr ssc_module_var_info(HandleRef jarg1, int jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
  public static extern int ssc_info_var_type(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
  public static extern int ssc_info_data_type(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
  public static extern string ssc_info_name(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
  public static extern string ssc_info_label(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
  public static extern string ssc_info_units(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
  public static extern string ssc_info_meta(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
  public static extern string ssc_info_group(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
  public static extern string ssc_info_required(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
  public static extern string ssc_info_constraints(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
  public static extern string ssc_info_uihint(HandleRef jarg1);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
  public static extern int ssc_module_exec_simple(string jarg1, HandleRef jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
  public static extern string ssc_module_exec_simple_nothread(string jarg1, HandleRef jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_LOG_get")]
  public static extern int SSC_LOG_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_UPDATE_get")]
  public static extern int SSC_UPDATE_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_EXECUTE_get")]
  public static extern int SSC_EXECUTE_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
  public static extern int ssc_module_exec(HandleRef jarg1, HandleRef jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
  public static extern int ssc_module_exec_with_handler(HandleRef jarg1, HandleRef jarg2, HandleRef jarg3, HandleRef jarg4);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_extproc_output")]
  public static extern void ssc_module_extproc_output(HandleRef jarg1, string jarg2);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_NOTICE_get")]
  public static extern int SSC_NOTICE_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_WARNING_get")]
  public static extern int SSC_WARNING_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SSC_ERROR_get")]
  public static extern int SSC_ERROR_get();

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
  public static extern string ssc_module_log(HandleRef jarg1, int jarg2, HandleRef jarg3, HandleRef jarg4);

  [DllImport("ssc.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__ssc_segfault")]
  public static extern void __ssc_segfault();
}
