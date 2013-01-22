// SAM Simulation Core (SSC) MATLAB API
// Copyright (c) 2012 National Renewable Energy Laboratory
// author: Steven H. Janzou

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

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_version")]
        public static extern int ssc_version32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_version")]
        public static extern int ssc_version64();
        public static int ssc_version()
        {
            return (System.IntPtr.Size == 8) ? ssc_version64() : ssc_version32();
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_build_info")]
        public static extern IntPtr ssc_build_info32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_build_info")]
        public static extern IntPtr ssc_build_info64();
        public static IntPtr ssc_build_info()
        {
            return (System.IntPtr.Size == 8) ? ssc_build_info64() : ssc_build_info32();
        }

        public static int SSC_INVALID_get()
        {
            return 0;
        }

        public static int SSC_STRING_get()
        {
            return 1;
        }

        public static int SSC_NUMBER_get()
        {
            return 2;
        }

        public static int SSC_ARRAY_get()
        {
            return 3;
        }

        public static int SSC_MATRIX_get()
        {
            return 4;
        }

        public static int SSC_TABLE_get()
        {
            return 5;
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_create")]
        public static extern IntPtr ssc_data_create32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_create")]
        public static extern IntPtr ssc_data_create64();
        public static IntPtr ssc_data_create()
        {
            return (System.IntPtr.Size == 8) ? ssc_data_create64() : ssc_data_create32();
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_free")]
        public static extern void ssc_data_free32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_free")]
        public static extern void ssc_data_free64(HandleRef cxtData);
        public static void ssc_data_free(HandleRef cxtData)
        {
            if (System.IntPtr.Size == 8) 
            {
                ssc_data_free64(cxtData);
            }
            else
            {
                ssc_data_free32(cxtData);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
        public static extern void ssc_data_clear32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
        public static extern void ssc_data_clear64(HandleRef cxtData);
        public static void ssc_data_clear(HandleRef cxtData)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_clear64(cxtData);
            }
            else
            {
                ssc_data_clear32(cxtData);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
        public static extern void ssc_data_unassign32(HandleRef cxtData, string variableName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
        public static extern void ssc_data_unassign64(HandleRef cxtData, string variableName);
        public static void ssc_data_unassign(HandleRef cxtData, string variableName)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_unassign64(cxtData, variableName);
            }
            else
            {
                ssc_data_unassign32(cxtData, variableName);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
        public static extern int ssc_data_query32(HandleRef cxtData, string variableName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
        public static extern int ssc_data_query64(HandleRef cxtData, string variableName);
        public static int ssc_data_query(HandleRef cxtData, string variableName)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_query64(cxtData, variableName) : ssc_data_query32(cxtData, variableName);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
        public static extern IntPtr ssc_data_first32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
        public static extern IntPtr ssc_data_first64(HandleRef cxtData);
        public static IntPtr ssc_data_first(HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_first64(cxtData) : ssc_data_first32(cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
        public static extern IntPtr ssc_data_next32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
        public static extern IntPtr ssc_data_next64(HandleRef cxtData);
        public static IntPtr ssc_data_next(HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_next64(cxtData) : ssc_data_next32(cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
        public static extern void ssc_data_set_string32(HandleRef cxtData, string name, string value);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
        public static extern void ssc_data_set_string64(HandleRef cxtData, string name, string value);
        public static void ssc_data_set_string(HandleRef cxtData, string name, string value)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_string64(cxtData, name, value);
            }
            else
            {
                ssc_data_set_string32(cxtData, name, value);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
        public static extern void ssc_data_set_number32(HandleRef cxtData, string name, float value);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
        public static extern void ssc_data_set_number64(HandleRef cxtData, string name, float value);
        public static void ssc_data_set_number(HandleRef cxtData, string name, float value)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_number64(cxtData, name, value);
            }
            else
            {
                ssc_data_set_number32(cxtData, name, value);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
        public static extern void ssc_data_set_array32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
        public static extern void ssc_data_set_array64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);
        public static void ssc_data_set_array(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_array64(cxtData, name, array, length);
            }
            else
            {
                ssc_data_set_array32(cxtData, name, array, length);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
        public static extern void ssc_data_set_matrix32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
        public static extern void ssc_data_set_matrix64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);
        public static void ssc_data_set_matrix(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_matrix64(cxtData, name, matrix, nRows, nCols);
            }
            else
            {
                ssc_data_set_matrix32(cxtData, name, matrix, nRows, nCols);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
        public static extern void ssc_data_set_table32(HandleRef cxtData, string name, HandleRef cxtTable);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
        public static extern void ssc_data_set_table64(HandleRef cxtData, string name, HandleRef cxtTable);
        public static void ssc_data_set_table(HandleRef cxtData, string name, HandleRef cxtTable)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_table64(cxtData, name, cxtTable);
            }
            else
            {
                ssc_data_set_table32(cxtData, name, cxtTable);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
        public static extern IntPtr ssc_data_get_string32(HandleRef cxtData, string name);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
        public static extern IntPtr ssc_data_get_string64(HandleRef cxtData, string name);
        public static IntPtr ssc_data_get_string(HandleRef cxtData, string name)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_string64(cxtData, name) : ssc_data_get_string32(cxtData, name);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
        public static extern int ssc_data_get_number32(HandleRef cxtData, string name, out float number);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
        public static extern int ssc_data_get_number64(HandleRef cxtData, string name, out float number);
        public static int ssc_data_get_number(HandleRef cxtData, string name, out float number)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_number64(cxtData, name, out number) : ssc_data_get_number32(cxtData, name, out number);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
        public static extern IntPtr ssc_data_get_array32(HandleRef cxtData, string name, out int len);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
        public static extern IntPtr ssc_data_get_array64(HandleRef cxtData, string name, out int len);
        public static IntPtr ssc_data_get_array(HandleRef cxtData, string name, out int len)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_array64(cxtData, name, out len) : ssc_data_get_array32(cxtData, name, out len);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
        public static extern IntPtr ssc_data_get_matrix32(HandleRef cxtData, string name, out int nRows, out int nCols);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
        public static extern IntPtr ssc_data_get_matrix64(HandleRef cxtData, string name, out int nRows, out int nCols);
        public static IntPtr ssc_data_get_matrix(HandleRef cxtData, string name, out int nRows, out int nCols)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_matrix64(cxtData, name, out nRows, out nCols) : ssc_data_get_matrix32(cxtData, name, out nRows, out nCols);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
        public static extern IntPtr ssc_data_get_table32(HandleRef cxtData, string name);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
        public static extern IntPtr ssc_data_get_table64(HandleRef cxtData, string name);
        public static IntPtr ssc_data_get_table(HandleRef cxtData, string name)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_table64(cxtData, name) : ssc_data_get_table32(cxtData, name);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
        public static extern IntPtr ssc_module_entry32(int moduleIndex);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
        public static extern IntPtr ssc_module_entry64(int moduleIndex);
        public static IntPtr ssc_module_entry(int moduleIndex)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_entry64(moduleIndex) : ssc_module_entry32(moduleIndex);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
        public static extern IntPtr ssc_entry_name32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
        public static extern IntPtr ssc_entry_name64(HandleRef cxtEntry);
        public static IntPtr ssc_entry_name(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_name64(cxtEntry) : ssc_entry_name32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
        public static extern IntPtr ssc_entry_description32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
        public static extern IntPtr ssc_entry_description64(HandleRef cxtEntry);
        public static IntPtr ssc_entry_description(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_description64(cxtEntry) : ssc_entry_description32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
        public static extern int ssc_entry_version32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
        public static extern int ssc_entry_version64(HandleRef cxtEntry);
        public static int ssc_entry_version(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_version64(cxtEntry) : ssc_entry_version32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_create")]
        public static extern IntPtr ssc_module_create32(string moduleName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_create")]
        public static extern IntPtr ssc_module_create64(string moduleName);
        public static IntPtr ssc_module_create(string moduleName)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_create64(moduleName) : ssc_module_create32(moduleName);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
        public static extern void ssc_module_free32(HandleRef cxtModule);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
        public static extern void ssc_module_free64(HandleRef cxtModule);
        public static void ssc_module_free(HandleRef cxtModule)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_module_free64(cxtModule);
            }
            else
            {
                ssc_module_free32(cxtModule);
            }
        }

        public static int SSC_INPUT_get()
        {
            return 1;
        }

        public static int SSC_OUTPUT_get()
        {
            return 2;
        }

        public static int SSC_INOUT_get()
        {
            return 3;
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
        public static extern IntPtr ssc_module_var_info32(HandleRef cxtModule, int index);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
        public static extern IntPtr ssc_module_var_info64(HandleRef cxtModule, int index);
        public static IntPtr ssc_module_var_info(HandleRef cxtModule, int index)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_var_info64(cxtModule, index) : ssc_module_var_info32(cxtModule, index);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
        public static extern int ssc_info_var_type32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
        public static extern int ssc_info_var_type64(HandleRef cxtInfo);
        public static int ssc_info_var_type(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_var_type64(cxtInfo) : ssc_info_var_type32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
        public static extern int ssc_info_data_type32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
        public static extern int ssc_info_data_type64(HandleRef cxtInfo);
        public static int ssc_info_data_type(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_data_type64(cxtInfo) : ssc_info_data_type32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
        public static extern IntPtr ssc_info_name32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
        public static extern IntPtr ssc_info_name64(HandleRef cxtInfo);
        public static IntPtr ssc_info_name(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_name64(cxtInfo) : ssc_info_name32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
        public static extern IntPtr ssc_info_label32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
        public static extern IntPtr ssc_info_label64(HandleRef cxtInfo);
        public static IntPtr ssc_info_label(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_label64(cxtInfo) : ssc_info_label32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
        public static extern IntPtr ssc_info_units32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
        public static extern IntPtr ssc_info_units64(HandleRef cxtInfo);
        public static IntPtr ssc_info_units(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_units64(cxtInfo) : ssc_info_units32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
        public static extern IntPtr ssc_info_meta32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
        public static extern IntPtr ssc_info_meta64(HandleRef cxtInfo);
        public static IntPtr ssc_info_meta(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_meta64(cxtInfo) : ssc_info_meta32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
        public static extern IntPtr ssc_info_group32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
        public static extern IntPtr ssc_info_group64(HandleRef cxtInfo);
        public static IntPtr ssc_info_group(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_group64(cxtInfo) : ssc_info_group32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
        public static extern IntPtr ssc_info_required32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
        public static extern IntPtr ssc_info_required64(HandleRef cxtInfo);
        public static IntPtr ssc_info_required(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_required64(cxtInfo) : ssc_info_required32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
        public static extern IntPtr ssc_info_constraints32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
        public static extern IntPtr ssc_info_constraints64(HandleRef cxtInfo);
        public static IntPtr ssc_info_constraints(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_constraints64(cxtInfo) : ssc_info_constraints32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
        public static extern IntPtr ssc_info_uihint32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
        public static extern IntPtr ssc_info_uihint64(HandleRef cxtInfo);
        public static IntPtr ssc_info_uihint(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_uihint64(cxtInfo) : ssc_info_units32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
        public static extern int ssc_module_exec_simple32(string moduleName, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
        public static extern int ssc_module_exec_simple64(string moduleName, HandleRef cxtData);
        public static int ssc_module_exec_simple(string moduleName, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple64(moduleName, cxtData) : ssc_module_exec_simple32(moduleName, cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
        public static extern IntPtr ssc_module_exec_simple_nothread32(string moduleName, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
        public static extern IntPtr ssc_module_exec_simple_nothread64(string moduleName, HandleRef cxtData);
        public static IntPtr ssc_module_exec_simple_nothread(string moduleName, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple_nothread64(moduleName, cxtData) : ssc_module_exec_simple_nothread32(moduleName, cxtData);
        }

        public static int SSC_LOG_get()
        {
            return 0;
        }

        public static int SSC_UPDATE_get()
        {
            return 1;
        }

/* removed in ssc rev 578
        public static int SSC_EXECUTE_get()
        {
            return 2;
        }
 */

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
        public static extern int ssc_module_exec32(HandleRef cxtModule, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
        public static extern int ssc_module_exec64(HandleRef cxtModule, HandleRef cxtData);
        public static int ssc_module_exec(HandleRef cxtModule, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec64(cxtModule, cxtData) : ssc_module_exec32(cxtModule, cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
        public static extern int ssc_module_exec_with_handler32(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
        public static extern int ssc_module_exec_with_handler64(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);
        public static int ssc_module_exec_with_handler(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_with_handler64(cxtModule, cxtData, cxtHandler, cxtUserData) : ssc_module_exec_with_handler32(cxtModule, cxtData, cxtHandler, cxtUserData);
        }

 /* removed in ssc rev 578 
        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_extproc_output")]
        public static extern void ssc_module_extproc_output32(HandleRef cxtModule, string outputLine);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_extproc_output")]
        public static extern void ssc_module_extproc_output64(HandleRef cxtModule, string outputLine);
        public static void ssc_module_extproc_output(HandleRef cxtModule, string outputLine)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_module_extproc_output64(cxtModule, outputLine);
            }
            else
            {
                ssc_module_extproc_output32(cxtModule, outputLine);
            }
        }
  */

        public static int SSC_NOTICE_get()
        {
            return 1;
        }

        public static int SSC_WARNING_get()
        {
            return 2;
        }

        public static int SSC_ERROR_get()
        {
            return 3;
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
        public static extern IntPtr ssc_module_log32(HandleRef cxtModule, int index, out int messageType, out float time);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
        public static extern IntPtr ssc_module_log64(HandleRef cxtModule, int index, out int messageType, out float time);
        public static IntPtr ssc_module_log(HandleRef cxtModule, int index, out int messageType, out float time)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_log64(cxtModule, index, out messageType, out time) : ssc_module_log32(cxtModule, index, out messageType, out time);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__ssc_segfault")]
        public static extern void __ssc_segfault32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__ssc_segfault")]
        public static extern void __ssc_segfault64();
        public static void __ssc_segfault()
        {
            if (System.IntPtr.Size == 8)
            {
                __ssc_segfault64();
            }
            else
            {
                __ssc_segfault32();
            }
        }
    }
    
    
    
    
    
    
    public class SSC 
    {
        private HandleRef ssc_data_ptr;
        private HandleRef ssc_module_ptr;
//        private HandleRef ssc_info_ptr;
        private HandleRef ssc_entry_ptr;

      public SSC()
      {
          ssc_data_ptr = new HandleRef(this, sscapiPINVOKE.ssc_data_create());
      }

      public SSC( String module_name )
      {
        ssc_data_ptr = new HandleRef(this, sscapiPINVOKE.ssc_data_create());
        ssc_module_ptr = new HandleRef(this, sscapiPINVOKE.ssc_module_create(module_name));
      }

      ~SSC()
      {
          Clear();
      }

      public void Clear()
      {
          if (ssc_data_ptr.Handle != IntPtr.Zero)
          {
              sscapiPINVOKE.ssc_data_free(ssc_data_ptr);
          }
          if (ssc_module_ptr.Handle != IntPtr.Zero)
          {
              sscapiPINVOKE.ssc_module_free(ssc_module_ptr);
          }
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

      public void SetArray(String name, float[] arr)
      {
          int len = arr.Length;
          sscapiPINVOKE.ssc_data_set_array(ssc_data_ptr, name, arr, len);
      }

      public float[,] GetMatrix(String name)
      {
          int nRows, nCols;
          IntPtr res = sscapiPINVOKE.ssc_data_get_matrix(ssc_data_ptr, name, out nRows, out nCols);
          float[] sscMat = new float[nRows*nCols];
          Marshal.Copy(res, sscMat, 0, nRows * nCols);
          float[,] mat = new float[nRows, nCols];
          for (int i = 0; i < nRows; i++)
          {
              for (int j = 0; j < nCols; j++)
              {
                  mat[i , j] = sscMat[i*nCols+j];
              }
          }
          return mat;
      }

      public void SetMatrix(String name, float[,] matrix)
      {
          int nRows = matrix.GetLength(0);
          int nCols = matrix.GetLength(1);
          sscapiPINVOKE.ssc_data_set_matrix(ssc_data_ptr, name, matrix, nRows, nCols);
      }
       

        
      public bool Exec()
      {
          return (sscapiPINVOKE.ssc_module_exec(ssc_module_ptr, ssc_data_ptr) != 0);
      }

      public bool ModuleInfo(int index)
      {
          IntPtr ip  = sscapiPINVOKE.ssc_module_entry(index);
          ssc_entry_ptr = new HandleRef(this,ip);
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

      public int ModuleVersion()
      {
          return sscapiPINVOKE.ssc_entry_version(ssc_entry_ptr);
      }

      public int NumberVariables(String moduleName)
      {
          int numVariables = 0;

          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, numVariables));
          while (varInfo.Handle != IntPtr.Zero)
          {
              numVariables++;
              varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, numVariables));
          }
          return numVariables;
      }


      public String VariableType(int variableIndex)
      {
          String variableTypeString = "No variable type information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              int varType = sscapiPINVOKE.ssc_info_var_type(varInfo);
              if (varType == sscapiPINVOKE.SSC_INPUT_get())
              {
                  variableTypeString = "INPUT";
              }
              else if (varType == sscapiPINVOKE.SSC_OUTPUT_get())
              {
                  variableTypeString = "OUTPUT";
              }
              else if (varType == sscapiPINVOKE.SSC_INOUT_get())
              {
                  variableTypeString = "INOUT";
              }
          }
          return variableTypeString;
      }

      public String VariableData(int variableIndex)
      {
          String variableDataTypeString = "No variable data information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              int varDataType = sscapiPINVOKE.ssc_info_data_type(varInfo);
              if (varDataType == sscapiPINVOKE.SSC_STRING_get())
              {
                  variableDataTypeString = "STRING";
              }
              else if (varDataType == sscapiPINVOKE.SSC_NUMBER_get())
              {
                  variableDataTypeString = "NUMBER";
              }
              else if (varDataType == sscapiPINVOKE.SSC_ARRAY_get())
              {
                  variableDataTypeString = "ARRAY";
              }
              else if (varDataType == sscapiPINVOKE.SSC_MATRIX_get())
              {
                  variableDataTypeString = "MATRIX";
              }
              else if (varDataType == sscapiPINVOKE.SSC_TABLE_get())
              {
                  variableDataTypeString = "TABLE";
              }
          }
          return variableDataTypeString;
      }

  
      public String VariableName(int variableIndex)
      {
          String variableNameString = "No variable name information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableNameString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_name(varInfo));
          }
          return variableNameString;
      }

      public String VariableLabel(int variableIndex)
      {
          String variableLabelString = "No variable label information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableLabelString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_label(varInfo));
          }
          return variableLabelString;
      }

      public String VariableUnits(int variableIndex)
      {
          String variableUnitsString = "No variable units information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableUnitsString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_units(varInfo));
          }
          return variableUnitsString;
      }

      public String VariableMeta(int variableIndex)
      {
          String variableMetaString = "No variable meta information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableMetaString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_meta(varInfo));
          }
          return variableMetaString;
      }

      public String VariableGroup(int variableIndex)
      {
          String variableGroupString = "No variable group information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableGroupString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_group(varInfo));
          }
          return variableGroupString;
      }

      public String VariableRequired(int variableIndex)
      {
          String variableRequiredString = "No variable required information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableRequiredString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_required(varInfo));
          }
          return variableRequiredString;
      }

      public String VariableConstraints(int variableIndex)
      {
          String variableConstraintsString = "No variable constraints information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableConstraintsString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_constraints(varInfo));
          }
          return variableConstraintsString;
      }

      public String VariableUIHint(int variableIndex)
      {
          String variableUIHintString = "No variable ui hints information found.";
          HandleRef varInfo = new HandleRef(this, sscapiPINVOKE.ssc_module_var_info(ssc_module_ptr, variableIndex));
          if (varInfo.Handle != IntPtr.Zero)
          {
              variableUIHintString = Marshal.PtrToStringAnsi(sscapiPINVOKE.ssc_info_uihint(varInfo));
          }
          return variableUIHintString;
      }
    
    
    }
}
