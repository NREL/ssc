#ifndef __LIB_PV_IO_MANAGER_H__
#define __LIB_PV_IO_MANAGER_H__

#include <map>
#include <memory>
#include <math.h>
#include <string>

#include "lib_irradproc.h"
#include "lib_util.h"

#include "../ssc/common.h"
#include "../ssc/core.h"

/// Structure containing data relevent at the SimulationManager level
struct Simulation_IO;

/// Structure contain data relevent to the Irradiance model
struct Irradiance_IO;

/// Structure containing subarray-level IO information
struct Subarray_IO;

/**
* \class PVIOManager
*
* This class contains the input and output data needed by all of the submodels in the detailed PV model
* It is intended to be passed as a pointer and modified with the goal of encapsulating all input that comes
* in at the user level and all output that needs to ultimately be passed back to the user.
*
*
* \note The PVIOManager contains structures that contain specific information about each system component
*/
class PVIOManager
{
public:
	/// Create a PVIOManager object by parsing the compute model
	PVIOManager(compute_module* cm, std::string cmName);

	/// Return pointer to compute module
	compute_module * getComputeModule() const;

	/// Return Simulation specific information
	Simulation_IO * getSimulationIO() const;

	/// Return Irradiance specific information
	Irradiance_IO * getIrradianceIO() const;

	/// Return Subarray specific information for the given subarray
	Subarray_IO * getSubarrayIO(size_t subarray) const;


public:
	/** These structures contain specific IO data for each part of the model
	* They are owned exclusively by the PVIOManager
	*/
	std::unique_ptr<Simulation_IO> m_SimulationIO;
	std::unique_ptr<Irradiance_IO> m_IrradianceIO;
	std::vector<std::unique_ptr<Subarray_IO>> m_SubarraysIO;

private:

	/// A pointer to the underlying compute module object which we don't manage
	compute_module * m_computeModule;

	/// The compute module name
	std::string m_computeModuleName;
};

struct Subarray_IO
{
	Subarray_IO(compute_module* cm, std::string cmName, size_t subarrayNumber)
	{
		std::string prefix = "subarray" + util::to_string(static_cast<int>(subarrayNumber)) + "_";

		enable = true;
		if (subarrayNumber > 1)
			enable = cm->as_boolean(prefix + "enable");

		if (enable)
		{
			tilt = fabs(cm->as_double(prefix + "tilt"));
			azimuth = cm->as_double(prefix + "azimuth");
			trackMode = cm->as_integer(prefix + "track_mode");
			trackerRotationLimit = cm->as_double(prefix + "rotlim");
			tiltEqualLatitude = cm->as_boolean(prefix + "tilt_eq_lat");
			groundCoverageRatio = cm->as_double(prefix + "gcr");
			monthlyTilt = cm->as_vector_double(prefix + "monthly_tilt");
			backtrackingEnabled = cm->as_boolean(prefix + "backtrack");
			derate = (1 - cm->as_double(prefix + "mismatch_loss") / 100) *
				(1 - cm->as_double(prefix + "diodeconn_loss") / 100) *
				(1 - cm->as_double(prefix + "dcwiring_loss") / 100) *
				(1 - cm->as_double(prefix + "tracking_loss") / 100) *
				(1 - cm->as_double(prefix + "nameplate_loss") / 100) *
				(1 - cm->as_double("dcoptimizer_loss") / 100);

			if (groundCoverageRatio < 0.01)
				throw compute_module::exec_error(cmName, "array ground coverage ratio must obey 0.01 < gcr");
		}
	}

	/**
	* Subarray Inputs
	*/
	bool enable;
	size_t nStrings;
	std::vector<double> monthlySoiling;
	double derate;
	double dcLoss;
	double groundCoverageRatio;
	double tilt;
	double azimuth;
	int trackMode;
	double trackerRotationLimit;
	bool tiltEqualLatitude;
	std::vector<double> monthlyTilt;
	bool backtrackingEnabled;
	int shadeMode;

	/**
	*  Structure to represent the plane of array calculations returned by irradiance processor
	*/
	struct {
		double ibeam;
		double iskydiff;
		double ignddiff;
		double ipoa;
		int sunup;
		double aoi;
		double stilt;
		double sazi;
		double nonlinear_dc_shading_derate;
		bool usePOAFromWF;
		int poaShadWarningCount;
		poaDecompReq poaAll;
	} poa;

	/**
	* Structure to represent module level information, calculated by module model
	*/
	struct {
		double dcpwr;
		double dcv;
		double voc;
		double isc;
		double dceff;
		double tcell;
	} module;

};

/**
* \struct Irradiance_IO
*
* This structure contains the input and output data needed by the IrradianceModel
* It is contained within the IOManager.
*
* \note The data contained in Irradiance_IO is primarily the data read in from the weather file
*		In general, the data is independent of a subarray, with the exception of plane-of-array (POA)
*		irradiance data, which may be specified in the weatherfile on a subarray basis.
*
*/
struct Irradiance_IO
{
	/// Construct the Irradiance_IO structure from the compute module input.  This sets up all inputs for the IrradianceModel
	Irradiance_IO(compute_module* cm, std::string cmName);

	/// Allocate the Irradiance_IO outputs
	void AllocateOutputs(compute_module* cm);

	/// Assign outputs from member data after the IrradianceModel has run 
	void AssignOutputs(compute_module* cm);

	// Constants
	static const int irradiationMax = 1500;						  /// The maximum irradiation (W/m2) allowed
	static const int irradprocNoInterpolateSunriseSunset = -1;    /// Interpolate the sunrise/sunset

																  // Irradiance Data Inputs
	std::unique_ptr<weather_data_provider> weatherDataProvider;   /// A class which encapsulates the weather data regardless of input method
	weather_record weatherRecord;								  /// Describes the weather data
	weather_header weatherHeader;								  /// Describes the weather data header
	double tsShiftHours;										  /// Sun position time offset
	bool instantaneous;											  /// Describes whether the weather data is instantaneous (or not)
	size_t numberOfYears;										  /// The number of years in the simulation
	size_t numberOfWeatherFileRecords;							  /// The number of records in the weather file
	size_t stepsPerHour;										  /// The number of steps per hour
	size_t numberOfSubarrays;									  /// The number of subarrays (needed if reading POA data from weather file)
	double dtHour;											      /// The timestep in hours
	int radiationMode;											  /// Specify which components of radiance should be used: 0=B&D, 1=G&B, 2=G&D, 3=POA-Ref, 4=POA-Pyra
	int skyModel;												  /// Specify which sky diffuse model should be used: 0=isotropic, 1=hdkr, 2=perez
	bool useWeatherFileAlbedo;									  /// Specify whether to use the weather file albedo
	std::vector<double> userSpecifiedMonthlyAlbedo;				  /// User can provide monthly ground albedo values (0-1)

	// Irradiance data Outputs (p_ is just a convention to organize all pointer outputs)
	ssc_number_t * p_weatherFileGHI;			/// The Global Horizonal Irradiance from the weather file
	ssc_number_t * p_weatherFileDNI;			/// The Direct Normal (Beam) Irradiance from the weather file
	ssc_number_t * p_weatherFileDHI;			/// The Direct Normal (Beam) Irradiance from the weather file
	std::vector<ssc_number_t *> p_weatherFilePOA; /// The Plane of Array Irradiance from the weather file
	ssc_number_t * p_sunPositionTime;			/// <UNSURE>
	ssc_number_t * p_weatherFileWindSpeed;		/// The Wind Speed from the weather file
	ssc_number_t * p_weatherFileAmbientTemp;	/// The ambient temperature from the weather file
	ssc_number_t * p_weatherFileAlbedo;			/// The ground albedo from the weather file
	ssc_number_t * p_weatherFileSnowDepth;		/// The snow depth from the weather file
	ssc_number_t * p_IrradianceCalculated[3];	/// The calculated components of the irradiance
	ssc_number_t * p_sunZenithAngle;			/// The calculate sun zenith angle
	ssc_number_t * p_sunAltitudeAngle;			/// The calculated sun altitude angle
	ssc_number_t * p_sunAzimuthAngle;			/// The calculated sun azimuth angle
	ssc_number_t * p_absoluteAirmass;			/// The calculated absolute airmass
	ssc_number_t * p_sunUpOverHorizon;			/// The calculation of whether the sun is up over the horizon
};

struct Simulation_IO
{
	Simulation_IO(compute_module* cm, Irradiance_IO & IrradianceIO);

	size_t numberOfYears;
	size_t numberOfWeatherFileRecords;
	size_t numberOfSteps;
	size_t stepsPerHour;
	double dtHour;
	bool useLifetimeOutput = 0;
};


#endif
