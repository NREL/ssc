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
	            float ac_hourly = data.GetNumber("hourly_ac_net");
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
    }
}
