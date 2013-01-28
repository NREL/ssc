using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
//using namespace SSC;

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
    }
}
