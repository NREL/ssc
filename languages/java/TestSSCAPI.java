
import java.lang.reflect.Field;
import java.util.Arrays;

public class TestSSCAPI {

/**
* Adds the specified path to the java library path
*
* @param pathToAdd the path to add
* @throws Exception
*/
    public static void addLibraryPath(String pathToAdd) throws Exception{
        final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
        usrPathsField.setAccessible(true);

        //get array of paths
        final String[] paths = (String[])usrPathsField.get(null);

        //check if the path to add is already present
        for(String path : paths) {
            if(path.equals(pathToAdd)) {
                return;
            }
        }

        //add the new path
        final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
        newPaths[newPaths.length-1] = pathToAdd;
        usrPathsField.set(null, newPaths);
    }


    public static void TestArrays()
    {
        SSC.Data sscData = new SSC.Data();
        float[] arr = new float[10];
        for (int i = 0; i < arr.length; i++)
        {
            arr[i] = i / 10.0f;
        }
        sscData.setArray("TestArray", arr);

        float[] retArray = sscData.getArray("TestArray");

        System.out.println("\nTesting SetArray and GetArray");
        for (int i = 0; i < retArray.length; i++)
        {
            System.out.println("\treturned array element: " + i + " = " + retArray[i]);
        }

    }

    public static void TestMatrices()
    {
        SSC.Data sscData = new SSC.Data();
        float[][] matrix = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
        sscData.setMatrix("TestMatrix", matrix);

        float[][] retMatrix = sscData.getMatrix("TestMatrix");

        System.out.println("\nTesting SetMatrix and GetMatrix");
        for (int r = 0; r < retMatrix.length; r++)
        {
            for (int c = 0; c < retMatrix[r].length; c++)
            {
                System.out.println("\treturned matrix element: (" + r + "," + c + ") = " + retMatrix[r][c]);
            }
        }
    }

    public static void PVWatts()
    {
        System.out.println("\nPVWatts example");
        SSC.Data data = new SSC.Data();
        data.setString("file_name", "abilene.tm2");
        data.setNumber("system_size", 4.0f);
        data.setNumber("derate", 0.77f);
        data.setNumber("track_mode", 0);
        data.setNumber("tilt", 20);
        data.setNumber("azimuth", 180);

        SSC.Module mod = new SSC.Module("pvwattsv1");
        if (mod.exec(data))
        {
            float tot = data.getNumber("ac_annual");
            float[] ac = data.getArray("ac_monthly");
            for (int i = 0; i < ac.length; i++)
                System.out.println("[" + i + "]: " + ac[i] + " kWh");
            System.out.println("AC total: " + tot);
            System.out.println("PVWatts test OK\n");
        }
        else
        {
            int idx = 0;
            String msg="";
            int[] type={0};
            float[] time={0};
            while (mod.log(idx, msg, type, time))
            {
                String stype = "NOTICE";
                if (type[0] == SSC.API.WARNING) stype = "WARNING";
                else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                System.out.println("[ " + stype + " at time:" + time[0] + " ]: " + msg);
                idx++;
            }
            System.out.println("PVWatts example failed");
        }
    }

    public static void PVWattsFunc()
    {
        System.out.println("\nPVWatts func example");
        SSC.Module sscModule = new SSC.Module("pvwattsfunc");
        SSC.Data sscData = new SSC.Data();
        sscData.setNumber("year", 1970); // general year (tiny effect in sun position)
        sscData.setNumber("month", 1); // 1-12
        sscData.setNumber("day", 1); //1-number of days in month
        sscData.setNumber("hour", 9); // 0-23
        sscData.setNumber("minute", 30); // minute of the hour (typically 30 min for midpoint calculation)
        sscData.setNumber("lat", 33.4f); // latitude, degrees
        sscData.setNumber("lon", -112); // longitude, degrees
        sscData.setNumber("tz", -7); // timezone from gmt, hours
        sscData.setNumber("time_step", 1); // time step, hours

        // solar and weather data
        sscData.setNumber("beam", 824); // beam (DNI) irradiance, W/m2
        sscData.setNumber("diffuse", 29); // diffuse (DHI) horizontal irradiance, W/m2
        sscData.setNumber("tamb", 9.4f); // ambient temp, degree C
        sscData.setNumber("wspd", 2.1f); // wind speed, m/s
        sscData.setNumber("snow", 0); // snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)

        // system specifications
        sscData.setNumber("system_size", 4); // system DC nameplate rating (kW)
        sscData.setNumber("derate", 0.77f); // derate factor
        sscData.setNumber("track_mode", 0); // tracking mode 0=fixed, 1=1axis, 2=2axis
        sscData.setNumber("azimuth", 180); // azimuth angle 0=north, 90=east, 180=south, 270=west
        sscData.setNumber("tilt", 20); // tilt angle from horizontal 0=flat, 90=vertical


        // previous timestep values of cell temperature and POA
        sscData.setNumber("tcell", 6.94f); // calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
        sscData.setNumber("poa", 84.5f); // plane of array irradiance (W/m2) from previous time step

        if (sscModule.exec(sscData))
        {
            float poa = sscData.getNumber("poa");
            float tcell = sscData.getNumber("tcell");
            float dc = sscData.getNumber("dc");
            float ac = sscData.getNumber("ac");
            System.out.println("poa: " + poa + " W/m2");
            System.out.println("tcell: " + tcell + " C");
            System.out.println("dc: " + dc + " W");
            System.out.println("ac: " + ac + " W");
        }
    }

    public static void PVSamV1_shading()
    {
        System.out.println("\nPVSamV1 with shading matrix");

        SSC.Data data = new SSC.Data();

        // pvsamv1 input variables
        data.setString( "weather_file", "AZ Phoenix.tm2" );
        data.setNumber( "use_wf_albedo", 1f );
        data.setNumber( "albedo", 0.2f );
        data.setNumber( "irrad_mode", 0f );
        data.setNumber( "sky_model", 2f );
        data.setNumber( "ac_derate", 0.99f );
        data.setNumber( "modules_per_string", 9f );
        data.setNumber( "strings_in_parallel", 2f );
        data.setNumber( "inverter_count", 1f );
        data.setNumber( "enable_mismatch_vmax_calc", 0f );
        data.setNumber( "subarray1_tilt", 20f );
        data.setNumber( "subarray1_tilt_eq_lat", 0f );
        data.setNumber( "subarray1_azimuth", 180f );
        data.setNumber( "subarray1_track_mode", 0f );
        data.setNumber( "subarray1_rotlim", 45f );
        data.setNumber( "subarray1_enable_backtracking", 0f );
        data.setNumber( "subarray1_btwidth", 2f );
        data.setNumber( "subarray1_btspacing", 1f );
        data.setMatrix( "subarray1_shading_mxh", new float[][]
        { { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.7f, 0.7f, 0.7f, 0.7f, 0.7f, 0.7f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.7f, 0.7f, 0.7f, 0.7f, 0.7f, 0.7f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f }, 
        { 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 1f, 0.5f, 0.5f } } );
        data.setArray( "subarray1_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray1_derate", 0.955598f );
        data.setNumber( "subarray2_enable", 0f );
        data.setNumber( "subarray2_nstrings", 0f );
        data.setNumber( "subarray2_tilt", 20f );
        data.setNumber( "subarray2_tilt_eq_lat", 0f );
        data.setNumber( "subarray2_azimuth", 180f );
        data.setNumber( "subarray2_track_mode", 0f );
        data.setNumber( "subarray2_rotlim", 45f );
        data.setNumber( "subarray2_enable_backtracking", 0f );
        data.setNumber( "subarray2_btwidth", 2f );
        data.setNumber( "subarray2_btspacing", 1f );
        data.setArray( "subarray2_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray2_derate", 0.955598f );
        data.setNumber( "subarray3_enable", 0f );
        data.setNumber( "subarray3_nstrings", 0f );
        data.setNumber( "subarray3_tilt", 20f );
        data.setNumber( "subarray3_tilt_eq_lat", 0f );
        data.setNumber( "subarray3_azimuth", 180f );
        data.setNumber( "subarray3_track_mode", 0f );
        data.setNumber( "subarray3_rotlim", 45f );
        data.setNumber( "subarray3_enable_backtracking", 0f );
        data.setNumber( "subarray3_btwidth", 2f );
        data.setNumber( "subarray3_btspacing", 1f );
        data.setArray( "subarray3_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray3_derate", 0.955598f );
        data.setNumber( "subarray4_enable", 0f );
        data.setNumber( "subarray4_nstrings", 0f );
        data.setNumber( "subarray4_tilt", 20f );
        data.setNumber( "subarray4_tilt_eq_lat", 0f );
        data.setNumber( "subarray4_azimuth", 180f );
        data.setNumber( "subarray4_track_mode", 0f );
        data.setNumber( "subarray4_rotlim", 45f );
        data.setNumber( "subarray4_enable_backtracking", 0f );
        data.setNumber( "subarray4_btwidth", 2f );
        data.setNumber( "subarray4_btspacing", 1f );
        data.setArray( "subarray4_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray4_derate", 0.955598f );
        data.setNumber( "module_model", 1f );
        data.setNumber( "spe_area", 0.74074f );
        data.setNumber( "spe_rad0", 200f );
        data.setNumber( "spe_rad1", 400f );
        data.setNumber( "spe_rad2", 600f );
        data.setNumber( "spe_rad3", 800f );
        data.setNumber( "spe_rad4", 1000f );
        data.setNumber( "spe_eff0", 13.5f );
        data.setNumber( "spe_eff1", 13.5f );
        data.setNumber( "spe_eff2", 13.5f );
        data.setNumber( "spe_eff3", 13.5f );
        data.setNumber( "spe_eff4", 13.5f );
        data.setNumber( "spe_reference", 4f );
        data.setNumber( "spe_module_structure", 0f );
        data.setNumber( "spe_a", -3.56f );
        data.setNumber( "spe_b", -0.075f );
        data.setNumber( "spe_dT", 3f );
        data.setNumber( "spe_temp_coeff", -0.5f );
        data.setNumber( "spe_fd", 1f );
        data.setNumber( "cec_area", 1.244f );
        data.setNumber( "cec_a_ref", 1.9816f );
        data.setNumber( "cec_adjust", 20.8f );
        data.setNumber( "cec_alpha_sc", 0.002651f );
        data.setNumber( "cec_beta_oc", -0.14234f );
        data.setNumber( "cec_gamma_r", -0.407f );
        data.setNumber( "cec_i_l_ref", 5.754f );
        data.setNumber( "cec_i_mp_ref", 5.25f );
        data.setNumber( "cec_i_o_ref", 1.919e-010f );
        data.setNumber( "cec_i_sc_ref", 5.75f );
        data.setNumber( "cec_n_s", 72f );
        data.setNumber( "cec_r_s", 0.105f );
        data.setNumber( "cec_r_sh_ref", 160.48f );
        data.setNumber( "cec_t_noct", 49.2f );
        data.setNumber( "cec_v_mp_ref", 41f );
        data.setNumber( "cec_v_oc_ref", 47.7f );
        data.setNumber( "cec_temp_corr_mode", 0f );
        data.setNumber( "cec_standoff", 6f );
        data.setNumber( "cec_height", 0f );
        data.setNumber( "cec_mounting_config", 0f );
        data.setNumber( "cec_heat_transfer", 0f );
        data.setNumber( "cec_mounting_orientation", 0f );
        data.setNumber( "cec_gap_spacing", 0.05f );
        data.setNumber( "cec_module_width", 1f );
        data.setNumber( "cec_module_length", 1.244f );
        data.setNumber( "cec_array_rows", 1f );
        data.setNumber( "cec_array_cols", 10f );
        data.setNumber( "cec_backside_temp", 20f );
        data.setNumber( "6par_celltech", 1f );
        data.setNumber( "6par_vmp", 30f );
        data.setNumber( "6par_imp", 6f );
        data.setNumber( "6par_voc", 37f );
        data.setNumber( "6par_isc", 7f );
        data.setNumber( "6par_bvoc", -0.11f );
        data.setNumber( "6par_aisc", 0.004f );
        data.setNumber( "6par_gpmp", -0.41f );
        data.setNumber( "6par_nser", 60f );
        data.setNumber( "6par_area", 1.3f );
        data.setNumber( "6par_tnoct", 46f );
        data.setNumber( "6par_standoff", 6f );
        data.setNumber( "6par_mounting", 0f );
        data.setNumber( "snl_module_structure", 0f );
        data.setNumber( "snl_a", -3.62f );
        data.setNumber( "snl_b", -0.075f );
        data.setNumber( "snl_dtc", 3f );
        data.setNumber( "snl_ref_a", -3.62f );
        data.setNumber( "snl_ref_b", -0.075f );
        data.setNumber( "snl_ref_dT", 3f );
        data.setNumber( "snl_fd", 1f );
        data.setNumber( "snl_a0", 0.94045f );
        data.setNumber( "snl_a1", 0.052641f );
        data.setNumber( "snl_a2", -0.0093897f );
        data.setNumber( "snl_a3", 0.00072623f );
        data.setNumber( "snl_a4", -1.9938e-005f );
        data.setNumber( "snl_aimp", -0.00038f );
        data.setNumber( "snl_aisc", 0.00061f );
        data.setNumber( "snl_area", 1.244f );
        data.setNumber( "snl_b0", 1f );
        data.setNumber( "snl_b1", -0.002438f );
        data.setNumber( "snl_b2", 0.0003103f );
        data.setNumber( "snl_b3", -1.246e-005f );
        data.setNumber( "snl_b4", 2.112e-007f );
        data.setNumber( "snl_b5", -1.359e-009f );
        data.setNumber( "snl_bvmpo", -0.139f );
        data.setNumber( "snl_bvoco", -0.136f );
        data.setNumber( "snl_c0", 1.0039f );
        data.setNumber( "snl_c1", -0.0039f );
        data.setNumber( "snl_c2", 0.291066f );
        data.setNumber( "snl_c3", -4.73546f );
        data.setNumber( "snl_c4", 0.9942f );
        data.setNumber( "snl_c5", 0.0058f );
        data.setNumber( "snl_c6", 1.0723f );
        data.setNumber( "snl_c7", -0.0723f );
        data.setNumber( "snl_impo", 5.25f );
        data.setNumber( "snl_isco", 5.75f );
        data.setNumber( "snl_ixo", 5.65f );
        data.setNumber( "snl_ixxo", 3.85f );
        data.setNumber( "snl_mbvmp", 0f );
        data.setNumber( "snl_mbvoc", 0f );
        data.setNumber( "snl_n", 1.221f );
        data.setNumber( "snl_series_cells", 72f );
        data.setNumber( "snl_vmpo", 40f );
        data.setNumber( "snl_voco", 47.7f );
        data.setNumber( "inverter_model", 1f );
        data.setNumber( "inv_spe_efficiency", 95f );
        data.setNumber( "inv_spe_power_ac", 4000f );
        data.setNumber( "inv_snl_c0", -6.57929e-006f );
        data.setNumber( "inv_snl_c1", 4.72925e-005f );
        data.setNumber( "inv_snl_c2", 0.00202195f );
        data.setNumber( "inv_snl_c3", 0.000285321f );
        data.setNumber( "inv_snl_paco", 4000f );
        data.setNumber( "inv_snl_pdco", 4186f );
        data.setNumber( "inv_snl_pnt", 0.17f );
        data.setNumber( "inv_snl_pso", 19.7391f );
        data.setNumber( "inv_snl_vdco", 310.67f );
        data.setNumber( "inv_snl_vdcmax", 0f );
        data.setNumber( "self_shading_enabled", 0f );
        data.setNumber( "self_shading_length", 1.84844f );
        data.setNumber( "self_shading_width", 0.673f );
        data.setNumber( "self_shading_mod_orient", 1f );
        data.setNumber( "self_shading_str_orient", 0f );
        data.setNumber( "self_shading_ncellx", 6f );
        data.setNumber( "self_shading_ncelly", 12f );
        data.setNumber( "self_shading_ndiode", 3f );
        data.setNumber( "self_shading_nmodx", 2f );
        data.setNumber( "self_shading_nstrx", 1f );
        data.setNumber( "self_shading_nmody", 3f );
        data.setNumber( "self_shading_nrows", 3f );
        data.setNumber( "self_shading_rowspace", 5f );

        SSC.Module module = new SSC.Module("pvsamv1");
        if (module.exec(data))
        {
                float[] ac_hourly = data.getArray("hourly_ac_net");
                float[] ac_monthly = data.getArray("monthly_ac_net");
                float ac_annual = data.getNumber("annual_ac_net");
                for (int i = 0; i < ac_monthly.length; i++)
                    System.out.println("ac_monthly[" + i + "] (kWh) = " + ac_monthly[i]);
                System.out.println("ac_annual (kWh) = " + ac_annual);
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("pvsamv1 example failed\n");
        }
    
    }
    
    
    public static void PVSamV1_Residential()
    {
        System.out.println("\nPVSamV1 with residential defaults");
        

        SSC.Data data = new SSC.Data();

        // pvsamv1 input variables
        data.setString( "weather_file", "AZ Phoenix.tm2" );
        data.setNumber( "use_wf_albedo", 1f );
        data.setNumber( "albedo", 0.2f );
        data.setNumber( "irrad_mode", 0f );
        data.setNumber( "sky_model", 2f );
        data.setNumber( "ac_derate", 0.99f );
        data.setNumber( "modules_per_string", 9f );
        data.setNumber( "strings_in_parallel", 2f );
        data.setNumber( "inverter_count", 1f );
        data.setNumber( "enable_mismatch_vmax_calc", 0f );
        data.setNumber( "subarray1_tilt", 20f );
        data.setNumber( "subarray1_tilt_eq_lat", 0f );
        data.setNumber( "subarray1_azimuth", 180f );
        data.setNumber( "subarray1_track_mode", 0f );
        data.setNumber( "subarray1_rotlim", 45f );
        data.setNumber( "subarray1_enable_backtracking", 0f );
        data.setNumber( "subarray1_btwidth", 2f );
        data.setNumber( "subarray1_btspacing", 1f );
        data.setArray( "subarray1_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray1_derate", 0.955598f );
        data.setNumber( "subarray2_enable", 0f );
        data.setNumber( "subarray2_nstrings", 0f );
        data.setNumber( "subarray2_tilt", 20f );
        data.setNumber( "subarray2_tilt_eq_lat", 0f );
        data.setNumber( "subarray2_azimuth", 180f );
        data.setNumber( "subarray2_track_mode", 0f );
        data.setNumber( "subarray2_rotlim", 45f );
        data.setNumber( "subarray2_enable_backtracking", 0f );
        data.setNumber( "subarray2_btwidth", 2f );
        data.setNumber( "subarray2_btspacing", 1f );
        data.setArray( "subarray2_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray2_derate", 0.955598f );
        data.setNumber( "subarray3_enable", 0f );
        data.setNumber( "subarray3_nstrings", 0f );
        data.setNumber( "subarray3_tilt", 20f );
        data.setNumber( "subarray3_tilt_eq_lat", 0f );
        data.setNumber( "subarray3_azimuth", 180f );
        data.setNumber( "subarray3_track_mode", 0f );
        data.setNumber( "subarray3_rotlim", 45f );
        data.setNumber( "subarray3_enable_backtracking", 0f );
        data.setNumber( "subarray3_btwidth", 2f );
        data.setNumber( "subarray3_btspacing", 1f );
        data.setArray( "subarray3_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray3_derate", 0.955598f );
        data.setNumber( "subarray4_enable", 0f );
        data.setNumber( "subarray4_nstrings", 0f );
        data.setNumber( "subarray4_tilt", 20f );
        data.setNumber( "subarray4_tilt_eq_lat", 0f );
        data.setNumber( "subarray4_azimuth", 180f );
        data.setNumber( "subarray4_track_mode", 0f );
        data.setNumber( "subarray4_rotlim", 45f );
        data.setNumber( "subarray4_enable_backtracking", 0f );
        data.setNumber( "subarray4_btwidth", 2f );
        data.setNumber( "subarray4_btspacing", 1f );
        data.setArray( "subarray4_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray4_derate", 0.955598f );
        data.setNumber( "module_model", 1f );
        data.setNumber( "spe_area", 0.74074f );
        data.setNumber( "spe_rad0", 200f );
        data.setNumber( "spe_rad1", 400f );
        data.setNumber( "spe_rad2", 600f );
        data.setNumber( "spe_rad3", 800f );
        data.setNumber( "spe_rad4", 1000f );
        data.setNumber( "spe_eff0", 13.5f );
        data.setNumber( "spe_eff1", 13.5f );
        data.setNumber( "spe_eff2", 13.5f );
        data.setNumber( "spe_eff3", 13.5f );
        data.setNumber( "spe_eff4", 13.5f );
        data.setNumber( "spe_reference", 4f );
        data.setNumber( "spe_module_structure", 0f );
        data.setNumber( "spe_a", -3.56f );
        data.setNumber( "spe_b", -0.075f );
        data.setNumber( "spe_dT", 3f );
        data.setNumber( "spe_temp_coeff", -0.5f );
        data.setNumber( "spe_fd", 1f );
        data.setNumber( "cec_area", 1.244f );
        data.setNumber( "cec_a_ref", 1.9816f );
        data.setNumber( "cec_adjust", 20.8f );
        data.setNumber( "cec_alpha_sc", 0.002651f );
        data.setNumber( "cec_beta_oc", -0.14234f );
        data.setNumber( "cec_gamma_r", -0.407f );
        data.setNumber( "cec_i_l_ref", 5.754f );
        data.setNumber( "cec_i_mp_ref", 5.25f );
        data.setNumber( "cec_i_o_ref", 1.919e-010f );
        data.setNumber( "cec_i_sc_ref", 5.75f );
        data.setNumber( "cec_n_s", 72f );
        data.setNumber( "cec_r_s", 0.105f );
        data.setNumber( "cec_r_sh_ref", 160.48f );
        data.setNumber( "cec_t_noct", 49.2f );
        data.setNumber( "cec_v_mp_ref", 41f );
        data.setNumber( "cec_v_oc_ref", 47.7f );
        data.setNumber( "cec_temp_corr_mode", 0f );
        data.setNumber( "cec_standoff", 6f );
        data.setNumber( "cec_height", 0f );
        data.setNumber( "cec_mounting_config", 0f );
        data.setNumber( "cec_heat_transfer", 0f );
        data.setNumber( "cec_mounting_orientation", 0f );
        data.setNumber( "cec_gap_spacing", 0.05f );
        data.setNumber( "cec_module_width", 1f );
        data.setNumber( "cec_module_length", 1.244f );
        data.setNumber( "cec_array_rows", 1f );
        data.setNumber( "cec_array_cols", 10f );
        data.setNumber( "cec_backside_temp", 20f );
        data.setNumber( "6par_celltech", 1f );
        data.setNumber( "6par_vmp", 30f );
        data.setNumber( "6par_imp", 6f );
        data.setNumber( "6par_voc", 37f );
        data.setNumber( "6par_isc", 7f );
        data.setNumber( "6par_bvoc", -0.11f );
        data.setNumber( "6par_aisc", 0.004f );
        data.setNumber( "6par_gpmp", -0.41f );
        data.setNumber( "6par_nser", 60f );
        data.setNumber( "6par_area", 1.3f );
        data.setNumber( "6par_tnoct", 46f );
        data.setNumber( "6par_standoff", 6f );
        data.setNumber( "6par_mounting", 0f );
        data.setNumber( "snl_module_structure", 0f );
        data.setNumber( "snl_a", -3.62f );
        data.setNumber( "snl_b", -0.075f );
        data.setNumber( "snl_dtc", 3f );
        data.setNumber( "snl_ref_a", -3.62f );
        data.setNumber( "snl_ref_b", -0.075f );
        data.setNumber( "snl_ref_dT", 3f );
        data.setNumber( "snl_fd", 1f );
        data.setNumber( "snl_a0", 0.94045f );
        data.setNumber( "snl_a1", 0.052641f );
        data.setNumber( "snl_a2", -0.0093897f );
        data.setNumber( "snl_a3", 0.00072623f );
        data.setNumber( "snl_a4", -1.9938e-005f );
        data.setNumber( "snl_aimp", -0.00038f );
        data.setNumber( "snl_aisc", 0.00061f );
        data.setNumber( "snl_area", 1.244f );
        data.setNumber( "snl_b0", 1f );
        data.setNumber( "snl_b1", -0.002438f );
        data.setNumber( "snl_b2", 0.0003103f );
        data.setNumber( "snl_b3", -1.246e-005f );
        data.setNumber( "snl_b4", 2.112e-007f );
        data.setNumber( "snl_b5", -1.359e-009f );
        data.setNumber( "snl_bvmpo", -0.139f );
        data.setNumber( "snl_bvoco", -0.136f );
        data.setNumber( "snl_c0", 1.0039f );
        data.setNumber( "snl_c1", -0.0039f );
        data.setNumber( "snl_c2", 0.291066f );
        data.setNumber( "snl_c3", -4.73546f );
        data.setNumber( "snl_c4", 0.9942f );
        data.setNumber( "snl_c5", 0.0058f );
        data.setNumber( "snl_c6", 1.0723f );
        data.setNumber( "snl_c7", -0.0723f );
        data.setNumber( "snl_impo", 5.25f );
        data.setNumber( "snl_isco", 5.75f );
        data.setNumber( "snl_ixo", 5.65f );
        data.setNumber( "snl_ixxo", 3.85f );
        data.setNumber( "snl_mbvmp", 0f );
        data.setNumber( "snl_mbvoc", 0f );
        data.setNumber( "snl_n", 1.221f );
        data.setNumber( "snl_series_cells", 72f );
        data.setNumber( "snl_vmpo", 40f );
        data.setNumber( "snl_voco", 47.7f );
        data.setNumber( "inverter_model", 1f );
        data.setNumber( "inv_spe_efficiency", 95f );
        data.setNumber( "inv_spe_power_ac", 4000f );
        data.setNumber( "inv_snl_c0", -6.57929e-006f );
        data.setNumber( "inv_snl_c1", 4.72925e-005f );
        data.setNumber( "inv_snl_c2", 0.00202195f );
        data.setNumber( "inv_snl_c3", 0.000285321f );
        data.setNumber( "inv_snl_paco", 4000f );
        data.setNumber( "inv_snl_pdco", 4186f );
        data.setNumber( "inv_snl_pnt", 0.17f );
        data.setNumber( "inv_snl_pso", 19.7391f );
        data.setNumber( "inv_snl_vdco", 310.67f );
        data.setNumber( "inv_snl_vdcmax", 0f );
        data.setNumber( "self_shading_enabled", 0f );
        data.setNumber( "self_shading_length", 1.84844f );
        data.setNumber( "self_shading_width", 0.673f );
        data.setNumber( "self_shading_mod_orient", 1f );
        data.setNumber( "self_shading_str_orient", 0f );
        data.setNumber( "self_shading_ncellx", 6f );
        data.setNumber( "self_shading_ncelly", 12f );
        data.setNumber( "self_shading_ndiode", 3f );
        data.setNumber( "self_shading_nmodx", 2f );
        data.setNumber( "self_shading_nstrx", 1f );
        data.setNumber( "self_shading_nmody", 3f );
        data.setNumber( "self_shading_nrows", 3f );
        data.setNumber( "self_shading_rowspace", 5f );

        float[] ac_hourly = {0};
        
        SSC.Module module = new SSC.Module("pvsamv1");
        if (module.exec(data))
        {
                ac_hourly = data.getArray("hourly_ac_net");
                float[] ac_monthly = data.getArray("monthly_ac_net");
                float ac_annual = data.getNumber("annual_ac_net");
                for (int i = 0; i < ac_monthly.length; i++)
                    System.out.println("ac_monthly[" + i + "] (kWh) = " + ac_monthly[i]);
                System.out.println("ac_annual (kWh) = " + ac_annual);
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("pvsamv1 example failed\n");
        }

        // annualoutput input variables
        data.setNumber( "analysis_years", 25f );
        data.setArray( "energy_availability", new float[]{ 100f } );
        data.setArray( "energy_degradation", new float[]{ 0.5f } );
        data.setMatrix( "energy_curtailment", new float[][]
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
        data.setNumber( "system_use_lifetime_output", 0f );
        data.setArray( "energy_net_hourly", ac_hourly);       
        
        
        float[] net_hourly = {0};
        module = new SSC.Module("annualoutput");
        if (module.exec(data))
        {
                net_hourly = data.getArray("hourly_e_net_delivered");
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("annualoutput example failed\n");
        }
        
        // utilityrate input variables
        data.setNumber( "analysis_years", 25f );
        data.setArray( "e_with_system", net_hourly);
        
        data.setArray( "system_availability", new float[]{ 100f } );
        data.setArray( "system_degradation", new float[]{ 0.5f } );
        data.setArray( "load_escalation", new float[]{ 2.5f } );
        data.setArray( "rate_escalation", new float[]{ 2.5f } );
        data.setNumber( "ur_sell_eq_buy", 1f );
        data.setNumber( "ur_monthly_fixed_charge", 0f );
        data.setNumber( "ur_flat_buy_rate", 0.12f );
        data.setNumber( "ur_flat_sell_rate", 0f );
        data.setNumber( "ur_tou_enable", 0f );
        data.setNumber( "ur_tou_p1_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p1_sell_rate", 0f );
        data.setNumber( "ur_tou_p2_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p2_sell_rate", 0f );
        data.setNumber( "ur_tou_p3_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p3_sell_rate", 0f );
        data.setNumber( "ur_tou_p4_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p4_sell_rate", 0f );
        data.setNumber( "ur_tou_p5_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p5_sell_rate", 0f );
        data.setNumber( "ur_tou_p6_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p6_sell_rate", 0f );
        data.setNumber( "ur_tou_p7_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p7_sell_rate", 0f );
        data.setNumber( "ur_tou_p8_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p8_sell_rate", 0f );
        data.setNumber( "ur_tou_p9_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p9_sell_rate", 0f );
        data.setString( "ur_tou_sched_weekday", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
        data.setString( "ur_tou_sched_weekend", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
        data.setNumber( "ur_dc_enable", 0f );
        data.setNumber( "ur_dc_fixed_m1", 0f );
        data.setNumber( "ur_dc_fixed_m2", 0f );
        data.setNumber( "ur_dc_fixed_m3", 0f );
        data.setNumber( "ur_dc_fixed_m4", 0f );
        data.setNumber( "ur_dc_fixed_m5", 0f );
        data.setNumber( "ur_dc_fixed_m6", 0f );
        data.setNumber( "ur_dc_fixed_m7", 0f );
        data.setNumber( "ur_dc_fixed_m8", 0f );
        data.setNumber( "ur_dc_fixed_m9", 0f );
        data.setNumber( "ur_dc_fixed_m10", 0f );
        data.setNumber( "ur_dc_fixed_m11", 0f );
        data.setNumber( "ur_dc_fixed_m12", 0f );
        data.setNumber( "ur_dc_p1", 0f );
        data.setNumber( "ur_dc_p2", 0f );
        data.setNumber( "ur_dc_p3", 0f );
        data.setNumber( "ur_dc_p4", 0f );
        data.setNumber( "ur_dc_p5", 0f );
        data.setNumber( "ur_dc_p6", 0f );
        data.setNumber( "ur_dc_p7", 0f );
        data.setNumber( "ur_dc_p8", 0f );
        data.setNumber( "ur_dc_p9", 0f );
        data.setString( "ur_dc_sched_weekday", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
        data.setString( "ur_dc_sched_weekend", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
        data.setNumber( "ur_tr_enable", 0f );
        data.setNumber( "ur_tr_sell_mode", 1f );
        data.setNumber( "ur_tr_sell_rate", 0f );
        data.setNumber( "ur_tr_s1_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s1_rate1", 0f );
        data.setNumber( "ur_tr_s1_rate2", 0f );
        data.setNumber( "ur_tr_s1_rate3", 0f );
        data.setNumber( "ur_tr_s1_rate4", 0f );
        data.setNumber( "ur_tr_s1_rate5", 0f );
        data.setNumber( "ur_tr_s1_rate6", 0f );
        data.setNumber( "ur_tr_s2_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s2_rate1", 0f );
        data.setNumber( "ur_tr_s2_rate2", 0f );
        data.setNumber( "ur_tr_s2_rate3", 0f );
        data.setNumber( "ur_tr_s2_rate4", 0f );
        data.setNumber( "ur_tr_s2_rate5", 0f );
        data.setNumber( "ur_tr_s2_rate6", 0f );
        data.setNumber( "ur_tr_s3_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s3_rate1", 0f );
        data.setNumber( "ur_tr_s3_rate2", 0f );
        data.setNumber( "ur_tr_s3_rate3", 0f );
        data.setNumber( "ur_tr_s3_rate4", 0f );
        data.setNumber( "ur_tr_s3_rate5", 0f );
        data.setNumber( "ur_tr_s3_rate6", 0f );
        data.setNumber( "ur_tr_s4_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s4_rate1", 0f );
        data.setNumber( "ur_tr_s4_rate2", 0f );
        data.setNumber( "ur_tr_s4_rate3", 0f );
        data.setNumber( "ur_tr_s4_rate4", 0f );
        data.setNumber( "ur_tr_s4_rate5", 0f );
        data.setNumber( "ur_tr_s4_rate6", 0f );
        data.setNumber( "ur_tr_s5_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s5_rate1", 0f );
        data.setNumber( "ur_tr_s5_rate2", 0f );
        data.setNumber( "ur_tr_s5_rate3", 0f );
        data.setNumber( "ur_tr_s5_rate4", 0f );
        data.setNumber( "ur_tr_s5_rate5", 0f );
        data.setNumber( "ur_tr_s5_rate6", 0f );
        data.setNumber( "ur_tr_s6_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s6_rate1", 0f );
        data.setNumber( "ur_tr_s6_rate2", 0f );
        data.setNumber( "ur_tr_s6_rate3", 0f );
        data.setNumber( "ur_tr_s6_rate4", 0f );
        data.setNumber( "ur_tr_s6_rate5", 0f );
        data.setNumber( "ur_tr_s6_rate6", 0f );
        data.setNumber( "ur_tr_sched_m1", 0f );
        data.setNumber( "ur_tr_sched_m2", 0f );
        data.setNumber( "ur_tr_sched_m3", 0f );
        data.setNumber( "ur_tr_sched_m4", 0f );
        data.setNumber( "ur_tr_sched_m5", 0f );
        data.setNumber( "ur_tr_sched_m6", 0f );
        data.setNumber( "ur_tr_sched_m7", 0f );
        data.setNumber( "ur_tr_sched_m8", 0f );
        data.setNumber( "ur_tr_sched_m9", 0f );
        data.setNumber( "ur_tr_sched_m10", 0f );
        data.setNumber( "ur_tr_sched_m11", 0f );
        data.setNumber( "ur_tr_sched_m12", 0f );

        module = new SSC.Module("utilityrate");
        if (module.exec(data))
        {
         // TODO: place output variables here
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("utilityrate example failed\n");
        }
        
        // cashloan input variables
        data.setNumber( "analysis_years", 25f );
        data.setNumber( "federal_tax_rate", 28f );
        data.setNumber( "state_tax_rate", 7f );
        data.setNumber( "property_tax_rate", 0f );
        data.setNumber( "prop_tax_cost_assessed_percent", 100f );
        data.setNumber( "prop_tax_assessed_decline", 0f );
        data.setNumber( "sales_tax_rate", 5f );
        data.setNumber( "real_discount_rate", 8f );
        data.setNumber( "inflation_rate", 2.5f );
        data.setNumber( "insurance_rate", 0f );
        data.setNumber( "system_capacity", 3.8745f );
        data.setNumber( "system_heat_rate", 0f );
        data.setNumber( "loan_term", 25f );
        data.setNumber( "loan_rate", 7.5f );
        data.setNumber( "loan_debt", 100f );
        data.setArray( "om_fixed", new float[]{ 0f } );
        data.setNumber( "om_fixed_escal", 0f );
        data.setArray( "om_production", new float[]{ 0f } );
        data.setNumber( "om_production_escal", 0f );
        data.setArray( "om_capacity", new float[]{ 20f } );
        data.setNumber( "om_capacity_escal", 0f );
        data.setArray( "om_fuel_cost", new float[]{ 0f } );
        data.setNumber( "om_fuel_cost_escal", 0f );
        data.setNumber( "annual_fuel_usage", 0f );
        data.setNumber( "itc_fed_amount", 0f );
        data.setNumber( "itc_fed_amount_deprbas_fed", 0f );
        data.setNumber( "itc_fed_amount_deprbas_sta", 0f );
        data.setNumber( "itc_sta_amount", 0f );
        data.setNumber( "itc_sta_amount_deprbas_fed", 0f );
        data.setNumber( "itc_sta_amount_deprbas_sta", 0f );
        data.setNumber( "itc_fed_percent", 30f );
        data.setNumber( "itc_fed_percent_maxvalue", 1e+038f );
        data.setNumber( "itc_fed_percent_deprbas_fed", 0f );
        data.setNumber( "itc_fed_percent_deprbas_sta", 0f );
        data.setNumber( "itc_sta_percent", 0f );
        data.setNumber( "itc_sta_percent_maxvalue", 1e+038f );
        data.setNumber( "itc_sta_percent_deprbas_fed", 0f );
        data.setNumber( "itc_sta_percent_deprbas_sta", 0f );
        data.setArray( "ptc_fed_amount", new float[]{ 0f } );
        data.setNumber( "ptc_fed_term", 10f );
        data.setNumber( "ptc_fed_escal", 2.5f );
        data.setArray( "ptc_sta_amount", new float[]{ 0f } );
        data.setNumber( "ptc_sta_term", 10f );
        data.setNumber( "ptc_sta_escal", 2.5f );
        data.setNumber( "ibi_fed_amount", 0f );
        data.setNumber( "ibi_fed_amount_tax_fed", 1f );
        data.setNumber( "ibi_fed_amount_tax_sta", 1f );
        data.setNumber( "ibi_fed_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_fed_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_sta_amount", 0f );
        data.setNumber( "ibi_sta_amount_tax_fed", 1f );
        data.setNumber( "ibi_sta_amount_tax_sta", 1f );
        data.setNumber( "ibi_sta_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_sta_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_uti_amount", 0f );
        data.setNumber( "ibi_uti_amount_tax_fed", 1f );
        data.setNumber( "ibi_uti_amount_tax_sta", 1f );
        data.setNumber( "ibi_uti_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_uti_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_oth_amount", 0f );
        data.setNumber( "ibi_oth_amount_tax_fed", 1f );
        data.setNumber( "ibi_oth_amount_tax_sta", 1f );
        data.setNumber( "ibi_oth_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_oth_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_fed_percent", 0f );
        data.setNumber( "ibi_fed_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_fed_percent_tax_fed", 1f );
        data.setNumber( "ibi_fed_percent_tax_sta", 1f );
        data.setNumber( "ibi_fed_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_fed_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_sta_percent", 0f );
        data.setNumber( "ibi_sta_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_sta_percent_tax_fed", 1f );
        data.setNumber( "ibi_sta_percent_tax_sta", 1f );
        data.setNumber( "ibi_sta_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_sta_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_uti_percent", 0f );
        data.setNumber( "ibi_uti_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_uti_percent_tax_fed", 1f );
        data.setNumber( "ibi_uti_percent_tax_sta", 1f );
        data.setNumber( "ibi_uti_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_uti_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_oth_percent", 0f );
        data.setNumber( "ibi_oth_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_oth_percent_tax_fed", 1f );
        data.setNumber( "ibi_oth_percent_tax_sta", 1f );
        data.setNumber( "ibi_oth_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_oth_percent_deprbas_sta", 0f );
        data.setNumber( "cbi_fed_amount", 0f );
        data.setNumber( "cbi_fed_maxvalue", 1e+038f );
        data.setNumber( "cbi_fed_tax_fed", 1f );
        data.setNumber( "cbi_fed_tax_sta", 1f );
        data.setNumber( "cbi_fed_deprbas_fed", 0f );
        data.setNumber( "cbi_fed_deprbas_sta", 0f );
        data.setNumber( "cbi_sta_amount", 0f );
        data.setNumber( "cbi_sta_maxvalue", 1e+038f );
        data.setNumber( "cbi_sta_tax_fed", 1f );
        data.setNumber( "cbi_sta_tax_sta", 1f );
        data.setNumber( "cbi_sta_deprbas_fed", 0f );
        data.setNumber( "cbi_sta_deprbas_sta", 0f );
        data.setNumber( "cbi_uti_amount", 0f );
        data.setNumber( "cbi_uti_maxvalue", 1e+038f );
        data.setNumber( "cbi_uti_tax_fed", 1f );
        data.setNumber( "cbi_uti_tax_sta", 1f );
        data.setNumber( "cbi_uti_deprbas_fed", 0f );
        data.setNumber( "cbi_uti_deprbas_sta", 0f );
        data.setNumber( "cbi_oth_amount", 0f );
        data.setNumber( "cbi_oth_maxvalue", 1e+038f );
        data.setNumber( "cbi_oth_tax_fed", 1f );
        data.setNumber( "cbi_oth_tax_sta", 1f );
        data.setNumber( "cbi_oth_deprbas_fed", 0f );
        data.setNumber( "cbi_oth_deprbas_sta", 0f );
        data.setArray( "pbi_fed_amount", new float[]{ 0f } );
        data.setNumber( "pbi_fed_term", 0f );
        data.setNumber( "pbi_fed_escal", 0f );
        data.setNumber( "pbi_fed_tax_fed", 1f );
        data.setNumber( "pbi_fed_tax_sta", 1f );
        data.setArray( "pbi_sta_amount", new float[]{ 0f } );
        data.setNumber( "pbi_sta_term", 0f );
        data.setNumber( "pbi_sta_escal", 0f );
        data.setNumber( "pbi_sta_tax_fed", 1f );
        data.setNumber( "pbi_sta_tax_sta", 1f );
        data.setArray( "pbi_uti_amount", new float[]{ 0f } );
        data.setNumber( "pbi_uti_term", 0f );
        data.setNumber( "pbi_uti_escal", 0f );
        data.setNumber( "pbi_uti_tax_fed", 1f );
        data.setNumber( "pbi_uti_tax_sta", 1f );
        data.setArray( "pbi_oth_amount", new float[]{ 0f } );
        data.setNumber( "pbi_oth_term", 0f );
        data.setNumber( "pbi_oth_escal", 0f );
        data.setNumber( "pbi_oth_tax_fed", 1f );
        data.setNumber( "pbi_oth_tax_sta", 1f );
        data.setNumber( "market", 0f );
        data.setNumber( "mortgage", 0f );
        data.setNumber( "total_installed_cost", 22194.2f );
        data.setNumber( "salvage_percentage", 0f );

        module = new SSC.Module("cashloan");
        if (module.exec(data))
        {
                float lcoe_real = data.getNumber("lcoe_real");
                float lcoe_nom = data.getNumber("lcoe_nom");
                float npv = data.getNumber("npv");
                System.out.println("LCOE real (cents/kWh) = " + lcoe_real );
                System.out.println("LCOE nominal (cents/kWh) = " + lcoe_nom);
                System.out.println("NPV = $" + npv );
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("cashloan example failed\n");
        }
       
    }
    

    public static void PVSamV1_Commercial()
    {
        System.out.println("\nPVSamV1 with commercial defaults");
        

        SSC.Data data = new SSC.Data();

        // pvsamv1 input variables
        data.setString( "weather_file", "AZ Phoenix.tm2" );
        data.setNumber( "use_wf_albedo", 1f );
        data.setNumber( "albedo", 0.2f );
        data.setNumber( "irrad_mode", 0f );
        data.setNumber( "sky_model", 2f );
        data.setNumber( "ac_derate", 0.99f );
        data.setNumber( "modules_per_string", 8f );
        data.setNumber( "strings_in_parallel", 116f );
        data.setNumber( "inverter_count", 6f );
        data.setNumber( "enable_mismatch_vmax_calc", 0f );
        data.setNumber( "subarray1_tilt", 33f );
        data.setNumber( "subarray1_tilt_eq_lat", 0f );
        data.setNumber( "subarray1_azimuth", 180f );
        data.setNumber( "subarray1_track_mode", 0f );
        data.setNumber( "subarray1_rotlim", 45f );
        data.setNumber( "subarray1_enable_backtracking", 0f );
        data.setNumber( "subarray1_btwidth", 2f );
        data.setNumber( "subarray1_btspacing", 1f );
        data.setArray( "subarray1_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray1_derate", 0.955598f );
        data.setNumber( "subarray2_enable", 0f );
        data.setNumber( "subarray2_nstrings", 0f );
        data.setNumber( "subarray2_tilt", 20f );
        data.setNumber( "subarray2_tilt_eq_lat", 0f );
        data.setNumber( "subarray2_azimuth", 180f );
        data.setNumber( "subarray2_track_mode", 0f );
        data.setNumber( "subarray2_rotlim", 45f );
        data.setNumber( "subarray2_enable_backtracking", 0f );
        data.setNumber( "subarray2_btwidth", 2f );
        data.setNumber( "subarray2_btspacing", 1f );
        data.setArray( "subarray2_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray2_derate", 0.955598f );
        data.setNumber( "subarray3_enable", 0f );
        data.setNumber( "subarray3_nstrings", 0f );
        data.setNumber( "subarray3_tilt", 20f );
        data.setNumber( "subarray3_tilt_eq_lat", 0f );
        data.setNumber( "subarray3_azimuth", 180f );
        data.setNumber( "subarray3_track_mode", 0f );
        data.setNumber( "subarray3_rotlim", 45f );
        data.setNumber( "subarray3_enable_backtracking", 0f );
        data.setNumber( "subarray3_btwidth", 2f );
        data.setNumber( "subarray3_btspacing", 1f );
        data.setArray( "subarray3_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray3_derate", 0.955598f );
        data.setNumber( "subarray4_enable", 0f );
        data.setNumber( "subarray4_nstrings", 0f );
        data.setNumber( "subarray4_tilt", 20f );
        data.setNumber( "subarray4_tilt_eq_lat", 0f );
        data.setNumber( "subarray4_azimuth", 180f );
        data.setNumber( "subarray4_track_mode", 0f );
        data.setNumber( "subarray4_rotlim", 45f );
        data.setNumber( "subarray4_enable_backtracking", 0f );
        data.setNumber( "subarray4_btwidth", 2f );
        data.setNumber( "subarray4_btspacing", 1f );
        data.setArray( "subarray4_soiling", new float[]{ 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f, 0.95f } );
        data.setNumber( "subarray4_derate", 0.955598f );
        data.setNumber( "module_model", 1f );
        data.setNumber( "spe_area", 0.74074f );
        data.setNumber( "spe_rad0", 200f );
        data.setNumber( "spe_rad1", 400f );
        data.setNumber( "spe_rad2", 600f );
        data.setNumber( "spe_rad3", 800f );
        data.setNumber( "spe_rad4", 1000f );
        data.setNumber( "spe_eff0", 13.5f );
        data.setNumber( "spe_eff1", 13.5f );
        data.setNumber( "spe_eff2", 13.5f );
        data.setNumber( "spe_eff3", 13.5f );
        data.setNumber( "spe_eff4", 13.5f );
        data.setNumber( "spe_reference", 4f );
        data.setNumber( "spe_module_structure", 0f );
        data.setNumber( "spe_a", -3.56f );
        data.setNumber( "spe_b", -0.075f );
        data.setNumber( "spe_dT", 3f );
        data.setNumber( "spe_temp_coeff", -0.5f );
        data.setNumber( "spe_fd", 1f );
        data.setNumber( "cec_area", 1.244f );
        data.setNumber( "cec_a_ref", 1.9816f );
        data.setNumber( "cec_adjust", 20.8f );
        data.setNumber( "cec_alpha_sc", 0.002651f );
        data.setNumber( "cec_beta_oc", -0.14234f );
        data.setNumber( "cec_gamma_r", -0.407f );
        data.setNumber( "cec_i_l_ref", 5.754f );
        data.setNumber( "cec_i_mp_ref", 5.25f );
        data.setNumber( "cec_i_o_ref", 1.919e-010f );
        data.setNumber( "cec_i_sc_ref", 5.75f );
        data.setNumber( "cec_n_s", 72f );
        data.setNumber( "cec_r_s", 0.105f );
        data.setNumber( "cec_r_sh_ref", 160.48f );
        data.setNumber( "cec_t_noct", 49.2f );
        data.setNumber( "cec_v_mp_ref", 41f );
        data.setNumber( "cec_v_oc_ref", 47.7f );
        data.setNumber( "cec_temp_corr_mode", 0f );
        data.setNumber( "cec_standoff", 6f );
        data.setNumber( "cec_height", 0f );
        data.setNumber( "cec_mounting_config", 0f );
        data.setNumber( "cec_heat_transfer", 0f );
        data.setNumber( "cec_mounting_orientation", 0f );
        data.setNumber( "cec_gap_spacing", 0.05f );
        data.setNumber( "cec_module_width", 1f );
        data.setNumber( "cec_module_length", 1.244f );
        data.setNumber( "cec_array_rows", 1f );
        data.setNumber( "cec_array_cols", 10f );
        data.setNumber( "cec_backside_temp", 20f );
        data.setNumber( "6par_celltech", 1f );
        data.setNumber( "6par_vmp", 30f );
        data.setNumber( "6par_imp", 6f );
        data.setNumber( "6par_voc", 37f );
        data.setNumber( "6par_isc", 7f );
        data.setNumber( "6par_bvoc", -0.11f );
        data.setNumber( "6par_aisc", 0.004f );
        data.setNumber( "6par_gpmp", -0.41f );
        data.setNumber( "6par_nser", 60f );
        data.setNumber( "6par_area", 1.3f );
        data.setNumber( "6par_tnoct", 46f );
        data.setNumber( "6par_standoff", 6f );
        data.setNumber( "6par_mounting", 0f );
        data.setNumber( "snl_module_structure", 0f );
        data.setNumber( "snl_a", -3.62f );
        data.setNumber( "snl_b", -0.075f );
        data.setNumber( "snl_dtc", 3f );
        data.setNumber( "snl_ref_a", -3.62f );
        data.setNumber( "snl_ref_b", -0.075f );
        data.setNumber( "snl_ref_dT", 3f );
        data.setNumber( "snl_fd", 1f );
        data.setNumber( "snl_a0", 0.94045f );
        data.setNumber( "snl_a1", 0.052641f );
        data.setNumber( "snl_a2", -0.0093897f );
        data.setNumber( "snl_a3", 0.00072623f );
        data.setNumber( "snl_a4", -1.9938e-005f );
        data.setNumber( "snl_aimp", -0.00038f );
        data.setNumber( "snl_aisc", 0.00061f );
        data.setNumber( "snl_area", 1.244f );
        data.setNumber( "snl_b0", 1f );
        data.setNumber( "snl_b1", -0.002438f );
        data.setNumber( "snl_b2", 0.0003103f );
        data.setNumber( "snl_b3", -1.246e-005f );
        data.setNumber( "snl_b4", 2.112e-007f );
        data.setNumber( "snl_b5", -1.359e-009f );
        data.setNumber( "snl_bvmpo", -0.139f );
        data.setNumber( "snl_bvoco", -0.136f );
        data.setNumber( "snl_c0", 1.0039f );
        data.setNumber( "snl_c1", -0.0039f );
        data.setNumber( "snl_c2", 0.291066f );
        data.setNumber( "snl_c3", -4.73546f );
        data.setNumber( "snl_c4", 0.9942f );
        data.setNumber( "snl_c5", 0.0058f );
        data.setNumber( "snl_c6", 1.0723f );
        data.setNumber( "snl_c7", -0.0723f );
        data.setNumber( "snl_impo", 5.25f );
        data.setNumber( "snl_isco", 5.75f );
        data.setNumber( "snl_ixo", 5.65f );
        data.setNumber( "snl_ixxo", 3.85f );
        data.setNumber( "snl_mbvmp", 0f );
        data.setNumber( "snl_mbvoc", 0f );
        data.setNumber( "snl_n", 1.221f );
        data.setNumber( "snl_series_cells", 72f );
        data.setNumber( "snl_vmpo", 40f );
        data.setNumber( "snl_voco", 47.7f );
        data.setNumber( "inverter_model", 1f );
        data.setNumber( "inv_spe_efficiency", 95f );
        data.setNumber( "inv_spe_power_ac", 4000f );
        data.setNumber( "inv_snl_c0", -3.55551e-008f );
        data.setNumber( "inv_snl_c1", 8.6927e-005f );
        data.setNumber( "inv_snl_c2", 0.000564748f );
        data.setNumber( "inv_snl_c3", -0.00369635f );
        data.setNumber( "inv_snl_paco", 36000f );
        data.setNumber( "inv_snl_pdco", 37453.9f );
        data.setNumber( "inv_snl_pnt", 0.6f );
        data.setNumber( "inv_snl_pso", 194.963f );
        data.setNumber( "inv_snl_vdco", 309.917f );
        data.setNumber( "inv_snl_vdcmax", 600f );
        data.setNumber( "self_shading_enabled", 0f );
        data.setNumber( "self_shading_length", 1.84844f );
        data.setNumber( "self_shading_width", 0.673f );
        data.setNumber( "self_shading_mod_orient", 1f );
        data.setNumber( "self_shading_str_orient", 0f );
        data.setNumber( "self_shading_ncellx", 6f );
        data.setNumber( "self_shading_ncelly", 12f );
        data.setNumber( "self_shading_ndiode", 3f );
        data.setNumber( "self_shading_nmodx", 116f );
        data.setNumber( "self_shading_nstrx", 1f );
        data.setNumber( "self_shading_nmody", 3f );
        data.setNumber( "self_shading_nrows", 2f );
        data.setNumber( "self_shading_rowspace", 5f );

        float[] ac_hourly = {0};
        
        SSC.Module module = new SSC.Module("pvsamv1");
        if (module.exec(data))
        {
                ac_hourly = data.getArray("hourly_ac_net");
                float[] ac_monthly = data.getArray("monthly_ac_net");
                float ac_annual = data.getNumber("annual_ac_net");
                for (int i = 0; i < ac_monthly.length; i++)
                    System.out.println("ac_monthly[" + i + "] (kWh) = " + ac_monthly[i]);
                System.out.println("ac_annual (kWh) = " + ac_annual);
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("pvsamv1 example failed\n");
        }

        // annualoutput input variables
        data.setNumber( "analysis_years", 25f );
        data.setArray( "energy_availability", new float[]{ 100f } );
        data.setArray( "energy_degradation", new float[]{ 0.5f } );
        data.setMatrix( "energy_curtailment", new float[][]
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
        data.setNumber( "system_use_lifetime_output", 0f );
        data.setArray( "energy_net_hourly", ac_hourly);       
        
        
        float[] net_hourly = {0};
        module = new SSC.Module("annualoutput");
        if (module.exec(data))
        {
                net_hourly = data.getArray("hourly_e_net_delivered");
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("annualoutput example failed\n");
        }
        
        // utilityrate input variables
        data.setNumber( "analysis_years", 25f );
        data.setArray( "e_with_system", net_hourly);
        data.setArray( "system_availability", new float[]{ 100f } );
        data.setArray( "system_degradation", new float[]{ 0.5f } );
        data.setArray( "load_escalation", new float[]{ 2.5f } );
        data.setArray( "rate_escalation", new float[]{ 2.5f } );
        data.setNumber( "ur_sell_eq_buy", 1f );
        data.setNumber( "ur_monthly_fixed_charge", 0f );
        data.setNumber( "ur_flat_buy_rate", 0.12f );
        data.setNumber( "ur_flat_sell_rate", 0f );
        data.setNumber( "ur_tou_enable", 0f );
        data.setNumber( "ur_tou_p1_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p1_sell_rate", 0f );
        data.setNumber( "ur_tou_p2_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p2_sell_rate", 0f );
        data.setNumber( "ur_tou_p3_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p3_sell_rate", 0f );
        data.setNumber( "ur_tou_p4_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p4_sell_rate", 0f );
        data.setNumber( "ur_tou_p5_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p5_sell_rate", 0f );
        data.setNumber( "ur_tou_p6_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p6_sell_rate", 0f );
        data.setNumber( "ur_tou_p7_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p7_sell_rate", 0f );
        data.setNumber( "ur_tou_p8_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p8_sell_rate", 0f );
        data.setNumber( "ur_tou_p9_buy_rate", 0.12f );
        data.setNumber( "ur_tou_p9_sell_rate", 0f );
        data.setString( "ur_tou_sched_weekday", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
        data.setString( "ur_tou_sched_weekend", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" );
        data.setNumber( "ur_dc_enable", 0f );
        data.setNumber( "ur_dc_fixed_m1", 0f );
        data.setNumber( "ur_dc_fixed_m2", 0f );
        data.setNumber( "ur_dc_fixed_m3", 0f );
        data.setNumber( "ur_dc_fixed_m4", 0f );
        data.setNumber( "ur_dc_fixed_m5", 0f );
        data.setNumber( "ur_dc_fixed_m6", 0f );
        data.setNumber( "ur_dc_fixed_m7", 0f );
        data.setNumber( "ur_dc_fixed_m8", 0f );
        data.setNumber( "ur_dc_fixed_m9", 0f );
        data.setNumber( "ur_dc_fixed_m10", 0f );
        data.setNumber( "ur_dc_fixed_m11", 0f );
        data.setNumber( "ur_dc_fixed_m12", 0f );
        data.setNumber( "ur_dc_p1", 0f );
        data.setNumber( "ur_dc_p2", 0f );
        data.setNumber( "ur_dc_p3", 0f );
        data.setNumber( "ur_dc_p4", 0f );
        data.setNumber( "ur_dc_p5", 0f );
        data.setNumber( "ur_dc_p6", 0f );
        data.setNumber( "ur_dc_p7", 0f );
        data.setNumber( "ur_dc_p8", 0f );
        data.setNumber( "ur_dc_p9", 0f );
        data.setString( "ur_dc_sched_weekday", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
        data.setString( "ur_dc_sched_weekend", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444" );
        data.setNumber( "ur_tr_enable", 0f );
        data.setNumber( "ur_tr_sell_mode", 1f );
        data.setNumber( "ur_tr_sell_rate", 0f );
        data.setNumber( "ur_tr_s1_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s1_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s1_rate1", 0f );
        data.setNumber( "ur_tr_s1_rate2", 0f );
        data.setNumber( "ur_tr_s1_rate3", 0f );
        data.setNumber( "ur_tr_s1_rate4", 0f );
        data.setNumber( "ur_tr_s1_rate5", 0f );
        data.setNumber( "ur_tr_s1_rate6", 0f );
        data.setNumber( "ur_tr_s2_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s2_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s2_rate1", 0f );
        data.setNumber( "ur_tr_s2_rate2", 0f );
        data.setNumber( "ur_tr_s2_rate3", 0f );
        data.setNumber( "ur_tr_s2_rate4", 0f );
        data.setNumber( "ur_tr_s2_rate5", 0f );
        data.setNumber( "ur_tr_s2_rate6", 0f );
        data.setNumber( "ur_tr_s3_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s3_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s3_rate1", 0f );
        data.setNumber( "ur_tr_s3_rate2", 0f );
        data.setNumber( "ur_tr_s3_rate3", 0f );
        data.setNumber( "ur_tr_s3_rate4", 0f );
        data.setNumber( "ur_tr_s3_rate5", 0f );
        data.setNumber( "ur_tr_s3_rate6", 0f );
        data.setNumber( "ur_tr_s4_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s4_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s4_rate1", 0f );
        data.setNumber( "ur_tr_s4_rate2", 0f );
        data.setNumber( "ur_tr_s4_rate3", 0f );
        data.setNumber( "ur_tr_s4_rate4", 0f );
        data.setNumber( "ur_tr_s4_rate5", 0f );
        data.setNumber( "ur_tr_s4_rate6", 0f );
        data.setNumber( "ur_tr_s5_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s5_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s5_rate1", 0f );
        data.setNumber( "ur_tr_s5_rate2", 0f );
        data.setNumber( "ur_tr_s5_rate3", 0f );
        data.setNumber( "ur_tr_s5_rate4", 0f );
        data.setNumber( "ur_tr_s5_rate5", 0f );
        data.setNumber( "ur_tr_s5_rate6", 0f );
        data.setNumber( "ur_tr_s6_energy_ub1", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub2", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub3", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub4", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub5", 1e+038f );
        data.setNumber( "ur_tr_s6_energy_ub6", 1e+038f );
        data.setNumber( "ur_tr_s6_rate1", 0f );
        data.setNumber( "ur_tr_s6_rate2", 0f );
        data.setNumber( "ur_tr_s6_rate3", 0f );
        data.setNumber( "ur_tr_s6_rate4", 0f );
        data.setNumber( "ur_tr_s6_rate5", 0f );
        data.setNumber( "ur_tr_s6_rate6", 0f );
        data.setNumber( "ur_tr_sched_m1", 0f );
        data.setNumber( "ur_tr_sched_m2", 0f );
        data.setNumber( "ur_tr_sched_m3", 0f );
        data.setNumber( "ur_tr_sched_m4", 0f );
        data.setNumber( "ur_tr_sched_m5", 0f );
        data.setNumber( "ur_tr_sched_m6", 0f );
        data.setNumber( "ur_tr_sched_m7", 0f );
        data.setNumber( "ur_tr_sched_m8", 0f );
        data.setNumber( "ur_tr_sched_m9", 0f );
        data.setNumber( "ur_tr_sched_m10", 0f );
        data.setNumber( "ur_tr_sched_m11", 0f );
        data.setNumber( "ur_tr_sched_m12", 0f );

        module = new SSC.Module("utilityrate");
        if (module.exec(data))
        {
         // TODO: place output variables here
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("utilityrate example failed\n");
        }
        
        // cashloan input variables
        data.setNumber( "analysis_years", 25f );
        data.setNumber( "federal_tax_rate", 28f );
        data.setNumber( "state_tax_rate", 7f );
        data.setNumber( "property_tax_rate", 2f );
        data.setNumber( "prop_tax_cost_assessed_percent", 100f );
        data.setNumber( "prop_tax_assessed_decline", 0f );
        data.setNumber( "sales_tax_rate", 5f );
        data.setNumber( "real_discount_rate", 5.2f );
        data.setNumber( "inflation_rate", 2.5f );
        data.setNumber( "insurance_rate", 0.5f );
        data.setNumber( "system_capacity", 199.752f );
        data.setNumber( "system_heat_rate", 0f );
        data.setNumber( "loan_term", 25f );
        data.setNumber( "loan_rate", 7.5f );
        data.setNumber( "loan_debt", 100f );
        data.setArray( "om_fixed", new float[]{ 0f } );
        data.setNumber( "om_fixed_escal", 0f );
        data.setArray( "om_production", new float[]{ 0f } );
        data.setNumber( "om_production_escal", 0f );
        data.setArray( "om_capacity", new float[]{ 20f } );
        data.setNumber( "om_capacity_escal", 0f );
        data.setArray( "om_fuel_cost", new float[]{ 0f } );
        data.setNumber( "om_fuel_cost_escal", 0f );
        data.setNumber( "annual_fuel_usage", 0f );
        data.setNumber( "depr_fed_type", 1f );
        data.setNumber( "depr_fed_sl_years", 7f );
        data.setArray( "depr_fed_custom", new float[]{ 0f } );
        data.setNumber( "depr_sta_type", 1f );
        data.setNumber( "depr_sta_sl_years", 7f );
        data.setArray( "depr_sta_custom", new float[]{ 0f } );
        data.setNumber( "itc_fed_amount", 0f );
        data.setNumber( "itc_fed_amount_deprbas_fed", 1f );
        data.setNumber( "itc_fed_amount_deprbas_sta", 1f );
        data.setNumber( "itc_sta_amount", 0f );
        data.setNumber( "itc_sta_amount_deprbas_fed", 0f );
        data.setNumber( "itc_sta_amount_deprbas_sta", 0f );
        data.setNumber( "itc_fed_percent", 30f );
        data.setNumber( "itc_fed_percent_maxvalue", 1e+038f );
        data.setNumber( "itc_fed_percent_deprbas_fed", 1f );
        data.setNumber( "itc_fed_percent_deprbas_sta", 1f );
        data.setNumber( "itc_sta_percent", 0f );
        data.setNumber( "itc_sta_percent_maxvalue", 1e+038f );
        data.setNumber( "itc_sta_percent_deprbas_fed", 0f );
        data.setNumber( "itc_sta_percent_deprbas_sta", 0f );
        data.setArray( "ptc_fed_amount", new float[]{ 0f } );
        data.setNumber( "ptc_fed_term", 10f );
        data.setNumber( "ptc_fed_escal", 2.5f );
        data.setArray( "ptc_sta_amount", new float[]{ 0f } );
        data.setNumber( "ptc_sta_term", 10f );
        data.setNumber( "ptc_sta_escal", 2.5f );
        data.setNumber( "ibi_fed_amount", 0f );
        data.setNumber( "ibi_fed_amount_tax_fed", 1f );
        data.setNumber( "ibi_fed_amount_tax_sta", 1f );
        data.setNumber( "ibi_fed_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_fed_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_sta_amount", 0f );
        data.setNumber( "ibi_sta_amount_tax_fed", 1f );
        data.setNumber( "ibi_sta_amount_tax_sta", 1f );
        data.setNumber( "ibi_sta_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_sta_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_uti_amount", 0f );
        data.setNumber( "ibi_uti_amount_tax_fed", 1f );
        data.setNumber( "ibi_uti_amount_tax_sta", 1f );
        data.setNumber( "ibi_uti_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_uti_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_oth_amount", 0f );
        data.setNumber( "ibi_oth_amount_tax_fed", 1f );
        data.setNumber( "ibi_oth_amount_tax_sta", 1f );
        data.setNumber( "ibi_oth_amount_deprbas_fed", 0f );
        data.setNumber( "ibi_oth_amount_deprbas_sta", 0f );
        data.setNumber( "ibi_fed_percent", 0f );
        data.setNumber( "ibi_fed_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_fed_percent_tax_fed", 1f );
        data.setNumber( "ibi_fed_percent_tax_sta", 1f );
        data.setNumber( "ibi_fed_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_fed_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_sta_percent", 0f );
        data.setNumber( "ibi_sta_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_sta_percent_tax_fed", 1f );
        data.setNumber( "ibi_sta_percent_tax_sta", 1f );
        data.setNumber( "ibi_sta_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_sta_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_uti_percent", 0f );
        data.setNumber( "ibi_uti_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_uti_percent_tax_fed", 1f );
        data.setNumber( "ibi_uti_percent_tax_sta", 1f );
        data.setNumber( "ibi_uti_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_uti_percent_deprbas_sta", 0f );
        data.setNumber( "ibi_oth_percent", 0f );
        data.setNumber( "ibi_oth_percent_maxvalue", 1e+038f );
        data.setNumber( "ibi_oth_percent_tax_fed", 1f );
        data.setNumber( "ibi_oth_percent_tax_sta", 1f );
        data.setNumber( "ibi_oth_percent_deprbas_fed", 0f );
        data.setNumber( "ibi_oth_percent_deprbas_sta", 0f );
        data.setNumber( "cbi_fed_amount", 0f );
        data.setNumber( "cbi_fed_maxvalue", 1e+038f );
        data.setNumber( "cbi_fed_tax_fed", 1f );
        data.setNumber( "cbi_fed_tax_sta", 1f );
        data.setNumber( "cbi_fed_deprbas_fed", 0f );
        data.setNumber( "cbi_fed_deprbas_sta", 0f );
        data.setNumber( "cbi_sta_amount", 0f );
        data.setNumber( "cbi_sta_maxvalue", 1e+038f );
        data.setNumber( "cbi_sta_tax_fed", 1f );
        data.setNumber( "cbi_sta_tax_sta", 1f );
        data.setNumber( "cbi_sta_deprbas_fed", 0f );
        data.setNumber( "cbi_sta_deprbas_sta", 0f );
        data.setNumber( "cbi_uti_amount", 0f );
        data.setNumber( "cbi_uti_maxvalue", 1e+038f );
        data.setNumber( "cbi_uti_tax_fed", 1f );
        data.setNumber( "cbi_uti_tax_sta", 1f );
        data.setNumber( "cbi_uti_deprbas_fed", 0f );
        data.setNumber( "cbi_uti_deprbas_sta", 0f );
        data.setNumber( "cbi_oth_amount", 0f );
        data.setNumber( "cbi_oth_maxvalue", 1e+038f );
        data.setNumber( "cbi_oth_tax_fed", 1f );
        data.setNumber( "cbi_oth_tax_sta", 1f );
        data.setNumber( "cbi_oth_deprbas_fed", 0f );
        data.setNumber( "cbi_oth_deprbas_sta", 0f );
        data.setArray( "pbi_fed_amount", new float[]{ 0f } );
        data.setNumber( "pbi_fed_term", 10f );
        data.setNumber( "pbi_fed_escal", 0f );
        data.setNumber( "pbi_fed_tax_fed", 1f );
        data.setNumber( "pbi_fed_tax_sta", 1f );
        data.setArray( "pbi_sta_amount", new float[]{ 0f } );
        data.setNumber( "pbi_sta_term", 10f );
        data.setNumber( "pbi_sta_escal", 0f );
        data.setNumber( "pbi_sta_tax_fed", 1f );
        data.setNumber( "pbi_sta_tax_sta", 1f );
        data.setArray( "pbi_uti_amount", new float[]{ 0f } );
        data.setNumber( "pbi_uti_term", 10f );
        data.setNumber( "pbi_uti_escal", 0f );
        data.setNumber( "pbi_uti_tax_fed", 1f );
        data.setNumber( "pbi_uti_tax_sta", 1f );
        data.setArray( "pbi_oth_amount", new float[]{ 0f } );
        data.setNumber( "pbi_oth_term", 10f );
        data.setNumber( "pbi_oth_escal", 0f );
        data.setNumber( "pbi_oth_tax_fed", 1f );
        data.setNumber( "pbi_oth_tax_sta", 1f );
        data.setNumber( "market", 1f );
        data.setNumber( "total_installed_cost", 922575f );
        data.setNumber( "salvage_percentage", 0f );

        module = new SSC.Module("cashloan");
        if (module.exec(data))
        {
                float lcoe_real = data.getNumber("lcoe_real");
                float lcoe_nom = data.getNumber("lcoe_nom");
                float npv = data.getNumber("npv");
                System.out.println("LCOE real (cents/kWh) = " + lcoe_real );
                System.out.println("LCOE nominal (cents/kWh) = " + lcoe_nom);
                System.out.println("NPV = $" + npv );
        }
        else
        {
                int idx = 0;
                String msg="";
                int[] type={0};
                float[] time={0};
                while (module.log(idx, msg, type, time))
                {
                        String stype = "NOTICE";
                        if (type[0] == SSC.API.WARNING) stype = "WARNING";
                        else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                        System.out.println("[ " + stype + " at time:" + time + " ]: " + msg );
                        idx++;
                }
                System.out.println("cashloan example failed\n");
        }
       
    }
    
    
    public static void ModulesAndVariables()
    {
        SSC.Entry sscEntry = new SSC.Entry();
        int moduleIndex = 0;
        while (sscEntry.get())
        {
            String moduleName = sscEntry.name();
            String description = sscEntry.description();
            int version = sscEntry.version();
            System.out.println("\nModule: " + moduleName + ", version: " + version);
            System.out.println(" " + description + "\n");
            moduleIndex++;

            SSC.Module sscModule = new SSC.Module(moduleName);
            SSC.Info sscInfo = new SSC.Info(sscModule);

            while (sscInfo.get())
            {
                System.out.println("\t" + sscInfo.varType() + ": \"" + sscInfo.name() + "\" " + " [" + sscInfo.dataType() + "] " + sscInfo.label() + " (" + sscInfo.units() + ")");
            }
        }
    }


    public static void Version()
    {
        System.out.println("\nVersion begin");
        SSC.API sscObj = new SSC.API();
        System.out.println("ssc version = " + sscObj.Version());
        System.out.println("ssc build info = " + sscObj.BuildInfo());
        System.out.println("Version end");
    }



    public static void ModuleList()
    {
        System.out.println("\nModule list begin");
        SSC.Entry sscEntry = new SSC.Entry();
        while( sscEntry.get())
        {
            String module_name = sscEntry.name();
            String description = sscEntry.description();
            int version = sscEntry.version();
            System.out.println( "Module: " + module_name + ", version: " + version);
            System.out.println( "    " + description );
        }
        System.out.println("Module list end");
    }
    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
        Version();
        ModuleList();
        ModulesAndVariables();
        TestArrays();
        TestMatrices();
        PVWattsFunc();
        PVWatts();
        PVSamV1_shading();
        PVSamV1_Residential();
        PVSamV1_Commercial();
    }
}
