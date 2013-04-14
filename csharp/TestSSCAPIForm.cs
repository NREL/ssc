// SAM Simulation Core (SSC) C# Example
// Copyright (c) 2012 National Renewable Energy Laboratory
// author: Steven H. Janzou and Aron P. Dobos

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace TestApplication
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

   
        private void btn4Version_Click(object sender, EventArgs e)
        {
            // uses main ssc class - version and build info
            txtData.Clear();
            txtData.AppendText("ssc version = " + SSC.API.Version() + "\n");
            txtData.AppendText("ssc build info = " + SSC.API.BuildInfo() + "\n");
        }

        private void btn4ModuleList_Click(object sender, EventArgs e)
        {
            SSC.Entry sscEntry = new SSC.Entry();
            txtData.Clear();
            int moduleIndex = 0;
            while (sscEntry.Get())
            {
                String module_name = sscEntry.Name();
                String description = sscEntry.Description();
                int version = sscEntry.Version();
                txtData.AppendText("\nModule: " + module_name + ", version: " + version + "\n");
                txtData.AppendText("    " + description + "\n");
                moduleIndex++;
            }        
        }

        private void btn4ArrayTest_Click(object sender, EventArgs e)
        {
            SSC.Data sscData = new SSC.Data();
            txtData.Clear();
            float[] arr = new float[10];
            for (int i = 0; i < arr.Length; i++)
            {
                arr[i] = i / 10.0f;
            }
            sscData.SetArray("TestArray", arr);

            float[] retArray = sscData.GetArray("TestArray");

            txtData.AppendText("Testing SetArray and GetArray\n");
            for (int i = 0; i < retArray.Length; i++)
            {
                txtData.AppendText("\treturned array element: " + i + " = " + retArray[i] + "\n");
            }

        }

        private void btn4TestMatrices_Click(object sender, EventArgs e)
        {
            SSC.Data sscData = new SSC.Data();
            txtData.Clear();
            float[,] matrix = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
            sscData.SetMatrix("TestMatrix", matrix);

            float[,] retMatrix = sscData.GetMatrix("TestMatrix");

            txtData.AppendText("Testing SetMatrix and GetMatrix\n");
            for (int i = 0; i < retMatrix.GetLength(0); i++)
            {
                for (int j = 0; j < retMatrix.GetLength(1); j++)
                {
                    txtData.AppendText("\treturned matrix element: (" + i + "," + j + ") = " + retMatrix[i, j] + "\n");
                }
            }
        }

        private void btn4PVWatts_Click(object sender, EventArgs e)
        {
            txtData.Clear();

            SSC.Data data = new SSC.Data();
            data.SetString("file_name", "abilene.tm2");
            data.SetNumber("system_size", 4.0f);
            data.SetNumber("derate", 0.77f);
            data.SetNumber("track_mode", 0);
            data.SetNumber("tilt", 20);
            data.SetNumber("azimuth", 180);

            SSC.Module mod = new SSC.Module("pvwattsv1");
            if (mod.Exec(data))
            {
                float tot = data.GetNumber("ac_annual");
                float[] ac = data.GetArray("ac_monthly");
                for (int i = 0; i < ac.Count(); i++)
                    txtData.AppendText("[" + i + "]: " + ac[i] + " kWh\n");
                txtData.AppendText("AC total: " + tot + "\n");
                txtData.AppendText("PVWatts test OK\n");
            }
            else
            {
                int idx = 0;
                String msg;
                int type;
                float time;
                while (mod.Log(idx, out msg, out type, out time))
                {
                    String stype = "NOTICE";
                    if (type == SSC.API.WARNING) stype = "WARNING";
                    else if (type == SSC.API.ERROR) stype = "ERROR";
                    txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
                    idx++;
                }
                txtData.AppendText("PVWatts example failed\n");
            }
        }

        private void btn4PVWattsFunc_Click(object sender, EventArgs e)
        {
            SSC.Module sscModule = new SSC.Module("pvwattsfunc");
            SSC.Data sscData = new SSC.Data();
            txtData.Clear();
            sscData.SetNumber("year", 1970); // general year (tiny effect in sun position)
            sscData.SetNumber("month", 1); // 1-12
            sscData.SetNumber("day", 1); //1-number of days in month
            sscData.SetNumber("hour", 9); // 0-23
            sscData.SetNumber("minute", 30); // minute of the hour (typically 30 min for midpoint calculation)
            sscData.SetNumber("lat", 33.4f); // latitude, degrees
            sscData.SetNumber("lon", -112); // longitude, degrees
            sscData.SetNumber("tz", -7); // timezone from gmt, hours
            sscData.SetNumber("time_step", 1); // time step, hours

            // solar and weather data
            sscData.SetNumber("beam", 824); // beam (DNI) irradiance, W/m2
            sscData.SetNumber("diffuse", 29); // diffuse (DHI) horizontal irradiance, W/m2
            sscData.SetNumber("tamb", 9.4f); // ambient temp, degree C
            sscData.SetNumber("wspd", 2.1f); // wind speed, m/s
            sscData.SetNumber("snow", 0); // snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)

            // system specifications
            sscData.SetNumber("system_size", 4); // system DC nameplate rating (kW)
            sscData.SetNumber("derate", 0.77f); // derate factor
            sscData.SetNumber("track_mode", 0); // tracking mode 0=fixed, 1=1axis, 2=2axis
            sscData.SetNumber("azimuth", 180); // azimuth angle 0=north, 90=east, 180=south, 270=west
            sscData.SetNumber("tilt", 20); // tilt angle from horizontal 0=flat, 90=vertical


            // previous timestep values of cell temperature and POA
            sscData.SetNumber("tcell", 6.94f); // calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
            sscData.SetNumber("poa", 84.5f); // plane of array irradiance (W/m2) from previous time step

            if (sscModule.Exec(sscData))
            {
                float poa = sscData.GetNumber("poa");
                float tcell = sscData.GetNumber("tcell");
                float dc = sscData.GetNumber("dc");
                float ac = sscData.GetNumber("ac");
                txtData.AppendText("poa: " + poa + " W/m2\n");
                txtData.AppendText("tcell: " + tcell + " C\n");
                txtData.AppendText("dc: " + dc + " W\n");
                txtData.AppendText("ac: " + ac + " W\n");
            }
            //System.GC.Collect(); // call to immediately free underlying pointers 

        }

        private void btn4ModulesAndVariables_Click(object sender, EventArgs e)
        {
            SSC.Entry sscEntry = new SSC.Entry();
            txtData.Clear();
            int moduleIndex = 0;
            while (sscEntry.Get())
            {
                String moduleName = sscEntry.Name();
                String description = sscEntry.Description();
                int version = sscEntry.Version();
                txtData.AppendText("\nModule: " + moduleName + ", version: " + version + "\n");
                txtData.AppendText(" " + description + "\n");
                moduleIndex++;

                SSC.Module sscModule = new SSC.Module(moduleName);
                SSC.Info sscInfo = new SSC.Info(sscModule);

                while (sscInfo.Get())
                {
                    txtData.AppendText("\t" + sscInfo.VarType() + ": \"" + sscInfo.Name() + "\" " + " [" + sscInfo.DataType() + "] " + sscInfo.Label() + " (" + sscInfo.Units() + ")\n");
                }
            }
        }

        private void btnPVSamV1_Click(object sender, EventArgs e)
        {
            txtData.Clear();

            // array parameters
            float ac_derate = 0.99f;
            float num_modules = 80;

            // module parameters
            float cec_area = 1.244f;
            float cec_alpha_sc = 2.651e-003f;
            float cec_beta_oc = -1.423e-001f;
            float cec_gamma_r = -4.070e-001f;
            float cec_i_mp_ref = 5.25f;
            float cec_i_sc_ref = 5.75f;
            float cec_n_s = 72;
            float cec_t_noct = 49.2f;
            float cec_v_mp_ref = 41;
            float cec_v_oc_ref = 47.7f;
            float cec_standoff = 6;
            float cec_height = 0;
            float cec_r_s = 0.105f;
            float cec_r_sh_ref = 160.48f;
            float cec_i_o_ref = 1.919e-010f;
            float cec_i_l_ref = 5.754f;
            float cec_adjust = 20.8f;
            float cec_a_ref = 1.9816f;

            // inverter parameters
            float inv_snl_c0 = -6.57929e-006f;
            float inv_snl_c1 = 4.72925e-005f;
            float inv_snl_c2 = 0.00202195f;
            float inv_snl_c3 = 0.000285321f;
            float inv_snl_paco = 4000;
            float inv_snl_pdco = 4186;
            float inv_snl_pnt = 0.17f;
            float inv_snl_pso = 19.7391f;
            float inv_snl_vdco = 310.67f;
            float inv_snl_vdcmax = 0;
            float inv_snl_vmin = 250;
            float inv_snl_vmax = 480;

            float tilt = 30;
            float azimuth = 180;
            float track_mode = 0;
            float soiling = 0.95f;
            float dc_derate = 0.95558f;

            // shading derate table
            float [,] shading_mxh ={ { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.475f,0.95f,1.0f,1.0f,0.7875f,0.2375f,0.25f,0.3625f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.4875f,1.0f,1.0f,1.0f,0.925f,0.6375f,0.6625f,0.225f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.15f,0.925f,1.0f,1.0f,1.0f,1.0f,1.0f,0.75f,0.2f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.45f,0.9125f,1.0f,1.0f,1.0f,1.0f,1.0f,0.625f,0.375f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.075f,0.05f,0.7875f,1.0f,1.0f,1.0f,1.0f,1.0f,1.0f,0.625f,0.4875f,0.025f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.15f,0.075f,0.9f,1.0f,1.0f,1.0f,1.0f,1.0f,1.0f,0.675f,0.5f,0.05f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.1f,0.0625f,0.8375f,1.0f,1.0f,1.0f,1.0f,1.0f,1.0f,0.6375f,0.4875f,0.025f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.6625f,0.9625f,1.0f,1.0f,1.0f,1.0f,1.0f,0.6125f,0.4f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.2f,0.9125f,1.0f,1.0f,1.0f,1.0f,1.0f,0.7375f,0.2125f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0625f,0.7f,1.0f,1.0f,1.0f,0.9375f,0.8f,0.7f,0.1875f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.45f,0.95f,1.0f,1.0f,0.8125f,0.3625f,0.3625f,0.375f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f },
	              { 0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0125f,0.525f,0.95f,1.0f,0.9875f,0.75f,0.175f,0.2125f,0.275f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f } };

            float[] soling_arr = { soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling };


            double mod_power = cec_v_mp_ref * cec_i_mp_ref;
            double dnum_series = 0.5 * (inv_snl_vmin + inv_snl_vmax) / cec_v_mp_ref;

            if (inv_snl_vdcmax > 0)
	            while ((dnum_series > 0) && ((dnum_series * cec_v_oc_ref) > inv_snl_vdcmax))
		            dnum_series -= 1;

            if (dnum_series < 1)
	            dnum_series = 1;

            int num_series = (int) Math.Truncate( dnum_series );

            double dnum_parallel = num_modules / num_series;
            if (dnum_parallel < 1)
	            dnum_parallel = 1;

            double dnum_inverters = Math.Ceiling(num_series * dnum_parallel * mod_power / inv_snl_paco);
            if (dnum_inverters < 1)
	            dnum_inverters = 1;

            int num_parallel = (int) Math.Truncate(dnum_parallel);
            int num_inverters = (int) Math.Truncate(dnum_inverters);


            txtData.AppendText("Modules per string = " + num_series + "\n");
            txtData.AppendText("Strings in parallel = " + num_parallel + "\n");
            txtData.AppendText("Number of inverters = " + num_inverters + "\n");



            SSC.Data data = new SSC.Data();
            data.SetString("weather_file", "abilene.tm2");

            data.SetNumber( "ac_derate", ac_derate );
            data.SetNumber("modules_per_string", num_series);
            data.SetNumber("strings_in_parallel", num_parallel);
            data.SetNumber("inverter_count", num_inverters);
            data.SetNumber("modules_per_string", 8);
            data.SetNumber("strings_in_parallel", 10);
            data.SetNumber("inverter_count", 5);
            data.SetNumber("subarray1_tilt", tilt);
            data.SetNumber( "subarray1_azimuth", azimuth );
            data.SetNumber( "subarray1_track_mode", track_mode );
            data.SetMatrix( "subarray1_shading_mxh", shading_mxh );
            data.SetArray( "subarray1_soiling", soling_arr );
            data.SetNumber( "subarray1_derate", dc_derate );

            // set up values for other sub arrays - not used (currently)
            data.SetNumber( "subarray2_tilt", 0 );
            data.SetNumber( "subarray3_tilt", 0 );
            data.SetNumber( "subarray4_tilt", 0 );

            data.SetNumber( "module_model", 1 );

            data.SetNumber( "cec_area", cec_area );
            data.SetNumber( "cec_a_ref", cec_a_ref );
            data.SetNumber( "cec_adjust", cec_adjust );
            data.SetNumber( "cec_alpha_sc", cec_alpha_sc );
            data.SetNumber( "cec_beta_oc", cec_beta_oc );
            data.SetNumber( "cec_gamma_r", cec_gamma_r );
            data.SetNumber( "cec_i_l_ref", cec_i_l_ref );
            data.SetNumber( "cec_i_mp_ref", cec_i_mp_ref );
            data.SetNumber( "cec_i_o_ref", cec_i_o_ref );
            data.SetNumber( "cec_i_sc_ref", cec_i_sc_ref );
            data.SetNumber( "cec_n_s", cec_n_s );
            data.SetNumber( "cec_r_s", cec_r_s );
            data.SetNumber( "cec_r_sh_ref", cec_r_sh_ref );
            data.SetNumber( "cec_t_noct", cec_t_noct );
            data.SetNumber( "cec_v_mp_ref", cec_v_mp_ref );
            data.SetNumber( "cec_v_oc_ref", cec_v_oc_ref );
            data.SetNumber( "cec_temp_corr_mode", 0 );
            data.SetNumber( "cec_standoff", cec_standoff );
            data.SetNumber( "cec_height", cec_height );

            data.SetNumber( "inverter_model", 1 );

            data.SetNumber( "inv_snl_c0", inv_snl_c0 );
            data.SetNumber( "inv_snl_c1", inv_snl_c1 );
            data.SetNumber( "inv_snl_c2", inv_snl_c2 );
            data.SetNumber( "inv_snl_c3", inv_snl_c3 );
            data.SetNumber( "inv_snl_paco", inv_snl_paco );
            data.SetNumber( "inv_snl_pdco", inv_snl_pdco );
            data.SetNumber( "inv_snl_pnt", inv_snl_pnt );
            data.SetNumber( "inv_snl_pso", inv_snl_pso );
            data.SetNumber( "inv_snl_vdco", inv_snl_vdco );
            data.SetNumber( "inv_snl_vdcmax", inv_snl_vdcmax );


            // all variables have been set up for pvsamv1
            // run the model

            SSC.Module module = new SSC.Module("pvsamv1");

            if (module.Exec(data))
            {
	            float [] ac_hourly = data.GetArray("hourly_ac_net");
	            float [] ac_monthly = data.GetArray("monthly_ac_net");
	            float ac_annual = data.GetNumber("annual_ac_net");
                for (int i=0; i< ac_monthly.Length; i++)
                    txtData.AppendText("ac_monthly[" + i + "] (kWh) = " + ac_monthly[i] + "\n");
                txtData.AppendText("ac_annual (kWh) = " + ac_annual + "\n");
            }
            else
            {
                int idx = 0;
                String msg;
                int type;
                float time;
                while (module.Log(idx, out msg, out type, out time))
                {
                    String stype = "NOTICE";
                    if (type == SSC.API.WARNING) stype = "WARNING";
                    else if (type == SSC.API.ERROR) stype = "ERROR";
                    txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
                    idx++;
                }
                txtData.AppendText("PVSamV1 example failed\n");
             }

        }

        private void button1_Click(object sender, EventArgs e)
        {


 
            txtData.Clear();
            SSC.Data data = new SSC.Data();
            data.SetString( "weather_file", "C:/Projects/SAM/trunk/SAMwx/deploy/exelib/climate_files/AZ Phoenix.tm2" );
            data.SetNumber( "use_wf_albedo", 1f );
            data.SetNumber( "albedo", 0.2f );
            data.SetNumber( "irrad_mode", 0f );
            data.SetNumber( "sky_model", 2f );
            data.SetNumber( "ac_derate", 0.99f );
            data.SetNumber( "modules_per_string", 9f );
            data.SetNumber( "strings_in_parallel", 2f );
            data.SetNumber( "inverter_count", 1f );
            data.SetNumber( "enable_mismatch_vmax_calc", 0f );
            data.SetNumber( "subarray1_tilt", 20f );
            data.SetNumber( "subarray1_tilt_eq_lat", 0f );
            data.SetNumber( "subarray1_azimuth", 180f );
            data.SetNumber( "subarray1_track_mode", 0f );
            data.SetNumber( "subarray1_rotlim", 45f );
            data.SetNumber( "subarray1_enable_backtracking", 0f );
            data.SetNumber( "subarray1_btwidth", 2f );
            data.SetNumber( "subarray1_btspacing", 1f );
            data.SetArray( "subarray1_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
            data.SetNumber( "subarray1_derate", 0.955598f );
            data.SetNumber( "subarray2_enable", 0f );
            data.SetNumber( "subarray2_nstrings", 0f );
            data.SetNumber( "subarray2_tilt", 20f );
            data.SetNumber( "subarray2_tilt_eq_lat", 0f );
            data.SetNumber( "subarray2_azimuth", 180f );
            data.SetNumber( "subarray2_track_mode", 0f );
            data.SetNumber( "subarray2_rotlim", 45f );
            data.SetNumber( "subarray2_enable_backtracking", 0f );
            data.SetNumber( "subarray2_btwidth", 2f );
            data.SetNumber( "subarray2_btspacing", 1f );
            data.SetArray( "subarray2_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
            data.SetNumber( "subarray2_derate", 0.955598f );
            data.SetNumber( "subarray3_enable", 0f );
            data.SetNumber( "subarray3_nstrings", 0f );
            data.SetNumber( "subarray3_tilt", 20f );
            data.SetNumber( "subarray3_tilt_eq_lat", 0f );
            data.SetNumber( "subarray3_azimuth", 180f );
            data.SetNumber( "subarray3_track_mode", 0f );
            data.SetNumber( "subarray3_rotlim", 45f );
            data.SetNumber( "subarray3_enable_backtracking", 0f );
            data.SetNumber( "subarray3_btwidth", 2f );
            data.SetNumber( "subarray3_btspacing", 1f );
            data.SetArray( "subarray3_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
            data.SetNumber( "subarray3_derate", 0.955598f );
            data.SetNumber( "subarray4_enable", 0f );
            data.SetNumber( "subarray4_nstrings", 0f );
            data.SetNumber( "subarray4_tilt", 20f );
            data.SetNumber( "subarray4_tilt_eq_lat", 0f );
            data.SetNumber( "subarray4_azimuth", 180f );
            data.SetNumber( "subarray4_track_mode", 0f );
            data.SetNumber( "subarray4_rotlim", 45f );
            data.SetNumber( "subarray4_enable_backtracking", 0f );
            data.SetNumber( "subarray4_btwidth", 2f );
            data.SetNumber( "subarray4_btspacing", 1f );
            data.SetArray( "subarray4_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
            data.SetNumber( "subarray4_derate", 0.955598f );
            data.SetNumber( "module_model", 1f );
            data.SetNumber( "spe_area", 0.74074f );
            data.SetNumber( "spe_rad0", 200f );
            data.SetNumber( "spe_rad1", 400f );
            data.SetNumber( "spe_rad2", 600f );
            data.SetNumber( "spe_rad3", 800f );
            data.SetNumber( "spe_rad4", 1000f );
            data.SetNumber( "spe_eff0", 13.5f );
            data.SetNumber( "spe_eff1", 13.5f );
            data.SetNumber( "spe_eff2", 13.5f );
            data.SetNumber( "spe_eff3", 13.5f );
            data.SetNumber( "spe_eff4", 13.5f );
            data.SetNumber( "spe_reference", 4f );
            data.SetNumber( "spe_module_structure", 0f );
            data.SetNumber( "spe_a", -3.56f );
            data.SetNumber( "spe_b", -0.075f );
            data.SetNumber( "spe_dT", 3f );
            data.SetNumber( "spe_temp_coeff", -0.5f );
            data.SetNumber( "spe_fd", 1f );
            data.SetNumber( "cec_area", 1.244f );
            data.SetNumber( "cec_a_ref", 1.9816f );
            data.SetNumber( "cec_adjust", 20.8f );
            data.SetNumber( "cec_alpha_sc", 0.002651f );
            data.SetNumber( "cec_beta_oc", -0.14234f );
            data.SetNumber( "cec_gamma_r", -0.407f );
            data.SetNumber( "cec_i_l_ref", 5.754f );
            data.SetNumber( "cec_i_mp_ref", 5.25f );
            data.SetNumber( "cec_i_o_ref", 1.919e-010f );
            data.SetNumber( "cec_i_sc_ref", 5.75f );
            data.SetNumber( "cec_n_s", 72f );
            data.SetNumber( "cec_r_s", 0.105f );
            data.SetNumber( "cec_r_sh_ref", 160.48f );
            data.SetNumber( "cec_t_noct", 49.2f );
            data.SetNumber( "cec_v_mp_ref", 41f );
            data.SetNumber( "cec_v_oc_ref", 47.7f );
            data.SetNumber( "cec_temp_corr_mode", 0f );
            data.SetNumber( "cec_standoff", 6f );
            data.SetNumber( "cec_height", 0f );
            data.SetNumber( "cec_mounting_config", 0f );
            data.SetNumber( "cec_heat_transfer", 0f );
            data.SetNumber( "cec_mounting_orientation", 0f );
            data.SetNumber( "cec_gap_spacing", 0.05f );
            data.SetNumber( "cec_module_width", 1f );
            data.SetNumber( "cec_module_length", 1.244f );
            data.SetNumber( "cec_array_rows", 1f );
            data.SetNumber( "cec_array_cols", 10f );
            data.SetNumber( "cec_backside_temp", 20f );
            data.SetNumber( "6par_celltech", 1f );
            data.SetNumber( "6par_vmp", 30f );
            data.SetNumber( "6par_imp", 6f );
            data.SetNumber( "6par_voc", 37f );
            data.SetNumber( "6par_isc", 7f );
            data.SetNumber( "6par_bvoc", -0.11f );
            data.SetNumber( "6par_aisc", 0.004f );
            data.SetNumber( "6par_gpmp", -0.41f );
            data.SetNumber( "6par_nser", 60f );
            data.SetNumber( "6par_area", 1.3f );
            data.SetNumber( "6par_tnoct", 46f );
            data.SetNumber( "6par_standoff", 6f );
            data.SetNumber( "6par_mounting", 0f );
            data.SetNumber( "snl_module_structure", 0f );
            data.SetNumber( "snl_a", -3.62f );
            data.SetNumber( "snl_b", -0.075f );
            data.SetNumber( "snl_dtc", 3f );
            data.SetNumber( "snl_ref_a", -3.62f );
            data.SetNumber( "snl_ref_b", -0.075f );
            data.SetNumber( "snl_ref_dT", 3f );
            data.SetNumber( "snl_fd", 1f );
            data.SetNumber( "snl_a0", 0.94045f );
            data.SetNumber( "snl_a1", 0.052641f );
            data.SetNumber( "snl_a2", -0.0093897f );
            data.SetNumber( "snl_a3", 0.00072623f );
            data.SetNumber( "snl_a4", -1.9938e-005f );
            data.SetNumber( "snl_aimp", -0.00038f );
            data.SetNumber( "snl_aisc", 0.00061f );
            data.SetNumber( "snl_area", 1.244f );
            data.SetNumber( "snl_b0", 1f );
            data.SetNumber( "snl_b1", -0.002438f );
            data.SetNumber( "snl_b2", 0.0003103f );
            data.SetNumber( "snl_b3", -1.246e-005f );
            data.SetNumber( "snl_b4", 2.112e-007f );
            data.SetNumber( "snl_b5", -1.359e-009f );
            data.SetNumber( "snl_bvmpo", -0.139f );
            data.SetNumber( "snl_bvoco", -0.136f );
            data.SetNumber( "snl_c0", 1.0039f );
            data.SetNumber( "snl_c1", -0.0039f );
            data.SetNumber( "snl_c2", 0.291066f );
            data.SetNumber( "snl_c3", -4.73546f );
            data.SetNumber( "snl_c4", 0.9942f );
            data.SetNumber( "snl_c5", 0.0058f );
            data.SetNumber( "snl_c6", 1.0723f );
            data.SetNumber( "snl_c7", -0.0723f );
            data.SetNumber( "snl_impo", 5.25f );
            data.SetNumber( "snl_isco", 5.75f );
            data.SetNumber( "snl_ixo", 5.65f );
            data.SetNumber( "snl_ixxo", 3.85f );
            data.SetNumber( "snl_mbvmp", 0f );
            data.SetNumber( "snl_mbvoc", 0f );
            data.SetNumber( "snl_n", 1.221f );
            data.SetNumber( "snl_series_cells", 72f );
            data.SetNumber( "snl_vmpo", 40f );
            data.SetNumber( "snl_voco", 47.7f );
            data.SetNumber( "inverter_model", 1f );
            data.SetNumber( "inv_spe_efficiency", 95f );
            data.SetNumber( "inv_spe_power_ac", 4000f );
            data.SetNumber( "inv_snl_c0", -6.57929e-006f );
            data.SetNumber( "inv_snl_c1", 4.72925e-005f );
            data.SetNumber( "inv_snl_c2", 0.00202195f );
            data.SetNumber( "inv_snl_c3", 0.000285321f );
            data.SetNumber( "inv_snl_paco", 4000f );
            data.SetNumber( "inv_snl_pdco", 4186f );
            data.SetNumber( "inv_snl_pnt", 0.17f );
            data.SetNumber( "inv_snl_pso", 19.7391f );
            data.SetNumber( "inv_snl_vdco", 310.67f );
            data.SetNumber( "inv_snl_vdcmax", 0f );
            data.SetNumber( "self_shading_enabled", 0f );
            data.SetNumber( "self_shading_length", 1.84844f );
            data.SetNumber( "self_shading_width", 0.673f );
            data.SetNumber( "self_shading_mod_orient", 1f );
            data.SetNumber( "self_shading_str_orient", 0f );
            data.SetNumber( "self_shading_ncellx", 6f );
            data.SetNumber( "self_shading_ncelly", 12f );
            data.SetNumber( "self_shading_ndiode", 3f );
            data.SetNumber( "self_shading_nmodx", 2f );
            data.SetNumber( "self_shading_nstrx", 1f );
            data.SetNumber( "self_shading_nmody", 3f );
            data.SetNumber( "self_shading_nrows", 3f );
            data.SetNumber( "self_shading_rowspace", 5f );

            float [] ac_hourly = new float[]{0};
            SSC.Module module = new SSC.Module("pvsamv1");
            if (module.Exec(data))
            {
 	            ac_hourly = data.GetArray("hourly_ac_net");
	            float [] ac_monthly = data.GetArray("monthly_ac_net");
	            float ac_annual = data.GetNumber("annual_ac_net");
                for (int i=0; i< ac_monthly.Length; i++)
                    txtData.AppendText("ac_monthly[" + i + "] (kWh) = " + ac_monthly[i] + "\n");
                txtData.AppendText("ac_annual (kWh) = " + ac_annual + "\n");
            }
            else
            {
                int idx = 0;
                String msg;
                int type;
                float time;
                while (module.Log(idx, out msg, out type, out time))
                {
                    String stype = "NOTICE";
                    if (type == SSC.API.WARNING) stype = "WARNING";
                    else if (type == SSC.API.ERROR) stype = "ERROR";
                    txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
                    idx++;
                }
                txtData.AppendText("pvsamv1 example failed\n");
            }


            data.SetNumber( "analysis_years", 25f );
            data.SetArray( "energy_availability", new float[]{ 100f } );
            data.SetArray( "energy_degradation", new float[]{ 0.5f } );
            data.SetMatrix( "energy_curtailment", new float[,]
            { { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f }, 
            { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f } } );
            data.SetNumber( "system_use_lifetime_output", 0f );
            data.SetArray( "energy_net_hourly", ac_hourly );

            float[] net_hourly= new float[]{0};
            module = new SSC.Module("annualoutput");
            if (module.Exec(data))
            {
             	net_hourly = data.GetArray("hourly_e_net_delivered");
            }
            else
            {
	            int idx = 0;
	            String msg;
	            int type;
	            float time;
	            while (module.Log(idx, out msg, out type, out time))
	            {
		            String stype = "NOTICE";
		            if (type == SSC.API.WARNING) stype = "WARNING";
		            else if (type == SSC.API.ERROR) stype = "ERROR";
		            txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
		            idx++;
	            }
	            txtData.AppendText("annualoutput example failed\n");
            }
            data.SetNumber( "analysis_years", 25f );
            data.SetArray( "e_with_system", net_hourly);
            data.SetArray( "system_availability", new float[]{ 100f } );
            data.SetArray( "system_degradation", new float[]{ 0.5f } );
            data.SetArray( "load_escalation", new float[]{ 2.5f } );
            data.SetArray( "rate_escalation", new float[]{ 2.5f } );
            data.SetNumber( "ur_sell_eq_buy", 1f );
            data.SetNumber( "ur_monthly_fixed_charge", 0f );
            data.SetNumber( "ur_flat_buy_rate", 0.12f );
            data.SetNumber( "ur_flat_sell_rate", 0f );
            data.SetNumber( "ur_tou_enable", 0f );
            data.SetNumber( "ur_tou_p1_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p1_sell_rate", 0f );
            data.SetNumber( "ur_tou_p2_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p2_sell_rate", 0f );
            data.SetNumber( "ur_tou_p3_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p3_sell_rate", 0f );
            data.SetNumber( "ur_tou_p4_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p4_sell_rate", 0f );
            data.SetNumber( "ur_tou_p5_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p5_sell_rate", 0f );
            data.SetNumber( "ur_tou_p6_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p6_sell_rate", 0f );
            data.SetNumber( "ur_tou_p7_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p7_sell_rate", 0f );
            data.SetNumber( "ur_tou_p8_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p8_sell_rate", 0f );
            data.SetNumber( "ur_tou_p9_buy_rate", 0.12f );
            data.SetNumber( "ur_tou_p9_sell_rate", 0f );
            data.SetString( "ur_tou_sched_weekday", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
            data.SetString( "ur_tou_sched_weekend", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
            data.SetNumber( "ur_dc_enable", 0f );
            data.SetNumber( "ur_dc_fixed_m1", 0f );
            data.SetNumber( "ur_dc_fixed_m2", 0f );
            data.SetNumber( "ur_dc_fixed_m3", 0f );
            data.SetNumber( "ur_dc_fixed_m4", 0f );
            data.SetNumber( "ur_dc_fixed_m5", 0f );
            data.SetNumber( "ur_dc_fixed_m6", 0f );
            data.SetNumber( "ur_dc_fixed_m7", 0f );
            data.SetNumber( "ur_dc_fixed_m8", 0f );
            data.SetNumber( "ur_dc_fixed_m9", 0f );
            data.SetNumber( "ur_dc_fixed_m10", 0f );
            data.SetNumber( "ur_dc_fixed_m11", 0f );
            data.SetNumber( "ur_dc_fixed_m12", 0f );
            data.SetNumber( "ur_dc_p1", 0f );
            data.SetNumber( "ur_dc_p2", 0f );
            data.SetNumber( "ur_dc_p3", 0f );
            data.SetNumber( "ur_dc_p4", 0f );
            data.SetNumber( "ur_dc_p5", 0f );
            data.SetNumber( "ur_dc_p6", 0f );
            data.SetNumber( "ur_dc_p7", 0f );
            data.SetNumber( "ur_dc_p8", 0f );
            data.SetNumber( "ur_dc_p9", 0f );
            data.SetString( "ur_dc_sched_weekday", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
            data.SetString( "ur_dc_sched_weekend", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
            data.SetNumber( "ur_tr_enable", 0f );
            data.SetNumber( "ur_tr_sell_mode", 1f );
            data.SetNumber( "ur_tr_sell_rate", 0f );
            data.SetNumber( "ur_tr_s1_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s1_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s1_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s1_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s1_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s1_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s1_rate1", 0f );
            data.SetNumber( "ur_tr_s1_rate2", 0f );
            data.SetNumber( "ur_tr_s1_rate3", 0f );
            data.SetNumber( "ur_tr_s1_rate4", 0f );
            data.SetNumber( "ur_tr_s1_rate5", 0f );
            data.SetNumber( "ur_tr_s1_rate6", 0f );
            data.SetNumber( "ur_tr_s2_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s2_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s2_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s2_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s2_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s2_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s2_rate1", 0f );
            data.SetNumber( "ur_tr_s2_rate2", 0f );
            data.SetNumber( "ur_tr_s2_rate3", 0f );
            data.SetNumber( "ur_tr_s2_rate4", 0f );
            data.SetNumber( "ur_tr_s2_rate5", 0f );
            data.SetNumber( "ur_tr_s2_rate6", 0f );
            data.SetNumber( "ur_tr_s3_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s3_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s3_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s3_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s3_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s3_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s3_rate1", 0f );
            data.SetNumber( "ur_tr_s3_rate2", 0f );
            data.SetNumber( "ur_tr_s3_rate3", 0f );
            data.SetNumber( "ur_tr_s3_rate4", 0f );
            data.SetNumber( "ur_tr_s3_rate5", 0f );
            data.SetNumber( "ur_tr_s3_rate6", 0f );
            data.SetNumber( "ur_tr_s4_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s4_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s4_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s4_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s4_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s4_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s4_rate1", 0f );
            data.SetNumber( "ur_tr_s4_rate2", 0f );
            data.SetNumber( "ur_tr_s4_rate3", 0f );
            data.SetNumber( "ur_tr_s4_rate4", 0f );
            data.SetNumber( "ur_tr_s4_rate5", 0f );
            data.SetNumber( "ur_tr_s4_rate6", 0f );
            data.SetNumber( "ur_tr_s5_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s5_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s5_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s5_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s5_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s5_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s5_rate1", 0f );
            data.SetNumber( "ur_tr_s5_rate2", 0f );
            data.SetNumber( "ur_tr_s5_rate3", 0f );
            data.SetNumber( "ur_tr_s5_rate4", 0f );
            data.SetNumber( "ur_tr_s5_rate5", 0f );
            data.SetNumber( "ur_tr_s5_rate6", 0f );
            data.SetNumber( "ur_tr_s6_energy_ub1", 1e+09f );
            data.SetNumber( "ur_tr_s6_energy_ub2", 1e+09f );
            data.SetNumber( "ur_tr_s6_energy_ub3", 1e+09f );
            data.SetNumber( "ur_tr_s6_energy_ub4", 1e+09f );
            data.SetNumber( "ur_tr_s6_energy_ub5", 1e+09f );
            data.SetNumber( "ur_tr_s6_energy_ub6", 1e+09f );
            data.SetNumber( "ur_tr_s6_rate1", 0f );
            data.SetNumber( "ur_tr_s6_rate2", 0f );
            data.SetNumber( "ur_tr_s6_rate3", 0f );
            data.SetNumber( "ur_tr_s6_rate4", 0f );
            data.SetNumber( "ur_tr_s6_rate5", 0f );
            data.SetNumber( "ur_tr_s6_rate6", 0f );
            data.SetNumber( "ur_tr_sched_m1", 0f );
            data.SetNumber( "ur_tr_sched_m2", 0f );
            data.SetNumber( "ur_tr_sched_m3", 0f );
            data.SetNumber( "ur_tr_sched_m4", 0f );
            data.SetNumber( "ur_tr_sched_m5", 0f );
            data.SetNumber( "ur_tr_sched_m6", 0f );
            data.SetNumber( "ur_tr_sched_m7", 0f );
            data.SetNumber( "ur_tr_sched_m8", 0f );
            data.SetNumber( "ur_tr_sched_m9", 0f );
            data.SetNumber( "ur_tr_sched_m10", 0f );
            data.SetNumber( "ur_tr_sched_m11", 0f );
            data.SetNumber( "ur_tr_sched_m12", 0f );

            module = new SSC.Module("utilityrate");
            if (module.Exec(data))
            {
             // TODO: place output variables here
            }
            else
            {
	            int idx = 0;
	            String msg;
	            int type;
	            float time;
	            while (module.Log(idx, out msg, out type, out time))
	            {
		            String stype = "NOTICE";
		            if (type == SSC.API.WARNING) stype = "WARNING";
		            else if (type == SSC.API.ERROR) stype = "ERROR";
		            txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
		            idx++;
	            }
	            txtData.AppendText("utilityrate example failed\n");
            }
            data.SetNumber( "analysis_years", 25f );
            data.SetNumber( "federal_tax_rate", 28f );
            data.SetNumber( "state_tax_rate", 7f );
            data.SetNumber( "property_tax_rate", 0f );
            data.SetNumber( "prop_tax_cost_assessed_percent", 100f );
            data.SetNumber( "prop_tax_assessed_decline", 0f );
            data.SetNumber( "sales_tax_rate", 5f );
            data.SetNumber( "real_discount_rate", 8f );
            data.SetNumber( "inflation_rate", 2.5f );
            data.SetNumber( "insurance_rate", 0f );
            data.SetNumber( "system_capacity", 3.8745f );
            data.SetNumber( "system_heat_rate", 0f );
            data.SetNumber( "loan_term", 25f );
            data.SetNumber( "loan_rate", 7.5f );
            data.SetNumber( "loan_debt", 100f );
            data.SetArray( "om_fixed", new float[]{ 0f } );
            data.SetNumber( "om_fixed_escal", 0f );
            data.SetArray( "om_production", new float[]{ 0f } );
            data.SetNumber( "om_production_escal", 0f );
            data.SetArray( "om_capacity", new float[]{ 20f } );
            data.SetNumber( "om_capacity_escal", 0f );
            data.SetArray( "om_fuel_cost", new float[]{ 0f } );
            data.SetNumber( "om_fuel_cost_escal", 0f );
            data.SetNumber( "annual_fuel_usage", 0f );
            data.SetNumber( "itc_fed_amount", 0f );
            data.SetNumber( "itc_fed_amount_deprbas_fed", 0f );
            data.SetNumber( "itc_fed_amount_deprbas_sta", 0f );
            data.SetNumber( "itc_sta_amount", 0f );
            data.SetNumber( "itc_sta_amount_deprbas_fed", 0f );
            data.SetNumber( "itc_sta_amount_deprbas_sta", 0f );
            data.SetNumber( "itc_fed_percent", 30f );
            data.SetNumber( "itc_fed_percent_maxvalue", 1e+09f );
            data.SetNumber( "itc_fed_percent_deprbas_fed", 0f );
            data.SetNumber( "itc_fed_percent_deprbas_sta", 0f );
            data.SetNumber( "itc_sta_percent", 0f );
            data.SetNumber( "itc_sta_percent_maxvalue", 1e+09f );
            data.SetNumber( "itc_sta_percent_deprbas_fed", 0f );
            data.SetNumber( "itc_sta_percent_deprbas_sta", 0f );
            data.SetArray( "ptc_fed_amount", new float[]{ 0f } );
            data.SetNumber( "ptc_fed_term", 10f );
            data.SetNumber( "ptc_fed_escal", 2.5f );
            data.SetArray( "ptc_sta_amount", new float[]{ 0f } );
            data.SetNumber( "ptc_sta_term", 10f );
            data.SetNumber( "ptc_sta_escal", 2.5f );
            data.SetNumber( "ibi_fed_amount", 0f );
            data.SetNumber( "ibi_fed_amount_tax_fed", 1f );
            data.SetNumber( "ibi_fed_amount_tax_sta", 1f );
            data.SetNumber( "ibi_fed_amount_deprbas_fed", 0f );
            data.SetNumber( "ibi_fed_amount_deprbas_sta", 0f );
            data.SetNumber( "ibi_sta_amount", 0f );
            data.SetNumber( "ibi_sta_amount_tax_fed", 1f );
            data.SetNumber( "ibi_sta_amount_tax_sta", 1f );
            data.SetNumber( "ibi_sta_amount_deprbas_fed", 0f );
            data.SetNumber( "ibi_sta_amount_deprbas_sta", 0f );
            data.SetNumber( "ibi_uti_amount", 0f );
            data.SetNumber( "ibi_uti_amount_tax_fed", 1f );
            data.SetNumber( "ibi_uti_amount_tax_sta", 1f );
            data.SetNumber( "ibi_uti_amount_deprbas_fed", 0f );
            data.SetNumber( "ibi_uti_amount_deprbas_sta", 0f );
            data.SetNumber( "ibi_oth_amount", 0f );
            data.SetNumber( "ibi_oth_amount_tax_fed", 1f );
            data.SetNumber( "ibi_oth_amount_tax_sta", 1f );
            data.SetNumber( "ibi_oth_amount_deprbas_fed", 0f );
            data.SetNumber( "ibi_oth_amount_deprbas_sta", 0f );
            data.SetNumber( "ibi_fed_percent", 0f );
            data.SetNumber( "ibi_fed_percent_maxvalue", 1e+09f );
            data.SetNumber( "ibi_fed_percent_tax_fed", 1f );
            data.SetNumber( "ibi_fed_percent_tax_sta", 1f );
            data.SetNumber( "ibi_fed_percent_deprbas_fed", 0f );
            data.SetNumber( "ibi_fed_percent_deprbas_sta", 0f );
            data.SetNumber( "ibi_sta_percent", 0f );
            data.SetNumber( "ibi_sta_percent_maxvalue", 1e+09f );
            data.SetNumber( "ibi_sta_percent_tax_fed", 1f );
            data.SetNumber( "ibi_sta_percent_tax_sta", 1f );
            data.SetNumber( "ibi_sta_percent_deprbas_fed", 0f );
            data.SetNumber( "ibi_sta_percent_deprbas_sta", 0f );
            data.SetNumber( "ibi_uti_percent", 0f );
            data.SetNumber( "ibi_uti_percent_maxvalue", 1e+09f );
            data.SetNumber( "ibi_uti_percent_tax_fed", 1f );
            data.SetNumber( "ibi_uti_percent_tax_sta", 1f );
            data.SetNumber( "ibi_uti_percent_deprbas_fed", 0f );
            data.SetNumber( "ibi_uti_percent_deprbas_sta", 0f );
            data.SetNumber( "ibi_oth_percent", 0f );
            data.SetNumber( "ibi_oth_percent_maxvalue", 1e+09f );
            data.SetNumber( "ibi_oth_percent_tax_fed", 1f );
            data.SetNumber( "ibi_oth_percent_tax_sta", 1f );
            data.SetNumber( "ibi_oth_percent_deprbas_fed", 0f );
            data.SetNumber( "ibi_oth_percent_deprbas_sta", 0f );
            data.SetNumber( "cbi_fed_amount", 0f );
            data.SetNumber( "cbi_fed_maxvalue", 1e+09f );
            data.SetNumber( "cbi_fed_tax_fed", 1f );
            data.SetNumber( "cbi_fed_tax_sta", 1f );
            data.SetNumber( "cbi_fed_deprbas_fed", 0f );
            data.SetNumber( "cbi_fed_deprbas_sta", 0f );
            data.SetNumber( "cbi_sta_amount", 0f );
            data.SetNumber( "cbi_sta_maxvalue", 1e+09f );
            data.SetNumber( "cbi_sta_tax_fed", 1f );
            data.SetNumber( "cbi_sta_tax_sta", 1f );
            data.SetNumber( "cbi_sta_deprbas_fed", 0f );
            data.SetNumber( "cbi_sta_deprbas_sta", 0f );
            data.SetNumber( "cbi_uti_amount", 0f );
            data.SetNumber( "cbi_uti_maxvalue", 1e+09f );
            data.SetNumber( "cbi_uti_tax_fed", 1f );
            data.SetNumber( "cbi_uti_tax_sta", 1f );
            data.SetNumber( "cbi_uti_deprbas_fed", 0f );
            data.SetNumber( "cbi_uti_deprbas_sta", 0f );
            data.SetNumber( "cbi_oth_amount", 0f );
            data.SetNumber( "cbi_oth_maxvalue", 1e+09f );
            data.SetNumber( "cbi_oth_tax_fed", 1f );
            data.SetNumber( "cbi_oth_tax_sta", 1f );
            data.SetNumber( "cbi_oth_deprbas_fed", 0f );
            data.SetNumber( "cbi_oth_deprbas_sta", 0f );
            data.SetArray( "pbi_fed_amount", new float[]{ 0f } );
            data.SetNumber( "pbi_fed_term", 0f );
            data.SetNumber( "pbi_fed_escal", 0f );
            data.SetNumber( "pbi_fed_tax_fed", 1f );
            data.SetNumber( "pbi_fed_tax_sta", 1f );
            data.SetArray( "pbi_sta_amount", new float[]{ 0f } );
            data.SetNumber( "pbi_sta_term", 0f );
            data.SetNumber( "pbi_sta_escal", 0f );
            data.SetNumber( "pbi_sta_tax_fed", 1f );
            data.SetNumber( "pbi_sta_tax_sta", 1f );
            data.SetArray( "pbi_uti_amount", new float[]{ 0f } );
            data.SetNumber( "pbi_uti_term", 0f );
            data.SetNumber( "pbi_uti_escal", 0f );
            data.SetNumber( "pbi_uti_tax_fed", 1f );
            data.SetNumber( "pbi_uti_tax_sta", 1f );
            data.SetArray( "pbi_oth_amount", new float[]{ 0f } );
            data.SetNumber( "pbi_oth_term", 0f );
            data.SetNumber( "pbi_oth_escal", 0f );
            data.SetNumber( "pbi_oth_tax_fed", 1f );
            data.SetNumber( "pbi_oth_tax_sta", 1f );
            data.SetNumber( "market", 0f );
            data.SetNumber( "mortgage", 0f );
            data.SetNumber( "total_installed_cost", 22194.2f );
            data.SetNumber( "salvage_percentage", 0f );
            data.SetArray( "energy_value", new float[]{ 822.827f, 839.18f, 855.859f, 872.869f, 890.214f, 907.908f, 925.954f, 944.356f, 963.123f, 982.266f, 1001.79f, 1021.7f, 1042.01f, 1062.72f, 1083.83f, 1105.38f, 1127.35f, 1149.75f, 1172.6f, 1195.9f, 1219.67f, 1243.91f, 1268.64f, 1293.85f, 1319.56f } );
            data.SetArray( "energy_net", new float[]{ 6856.65f, 6822.37f, 6788.25f, 6754.31f, 6720.54f, 6686.94f, 6653.5f, 6620.24f, 6587.14f, 6554.2f, 6521.43f, 6488.82f, 6456.38f, 6424.1f, 6391.98f, 6360.02f, 6328.22f, 6296.57f, 6265.09f, 6233.77f, 6202.6f, 6171.58f, 6140.73f, 6110.02f, 6079.47f } );

            module = new SSC.Module("cashloan");
            if (module.Exec(data))
            {
 	            float lcoe_real = data.GetNumber("lcoe_real");
	            float lcoe_nom = data.GetNumber("lcoe_nom");
	            float npv = data.GetNumber("npv");
	            txtData.AppendText("LCOE real (cents/kWh) = " + lcoe_real  + "\n");
	            txtData.AppendText("LCOE nominal (cents/kWh) = " + lcoe_nom + "\n");
	            txtData.AppendText("NPV = $" + npv + "\n");
            
            }
            else
            {
	            int idx = 0;
	            String msg;
	            int type;
	            float time;
	            while (module.Log(idx, out msg, out type, out time))
	            {
		            String stype = "NOTICE";
		            if (type == SSC.API.WARNING) stype = "WARNING";
		            else if (type == SSC.API.ERROR) stype = "ERROR";
		            txtData.AppendText("[ " + stype + " at time:" + time + " ]: " + msg + "\n");
		            idx++;
	            }
	            txtData.AppendText("cashloan example failed\n");
            }



            // end button1_click
        }
    }
}
