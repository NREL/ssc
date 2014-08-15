
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#include "STObject.h"
#include "SolarField.h"
#include "Heliostat.h"
#include "Receiver.h"
//#include "procs.h"

#ifdef _USE_SOLTRACE

ST_OpticalProperties::ST_OpticalProperties()
{
	int i;
	for (i=0;i<4;i++) Grating[i] = 0;
	
	OpticSurfNumber = 1;
	ApertureStopOrGratingType = 0;
	DiffractionOrder = 0;
	Reflectivity = 0;
	Transmissivity = 0;
	RMSSlopeError = 0;
	RMSSpecError = 0;
	DistributionType = 'g';
}

ST_OpticalProperties &ST_OpticalProperties::operator=(const ST_OpticalProperties &rhs)
{
	DistributionType = rhs.DistributionType;
	OpticSurfNumber = rhs.OpticSurfNumber;
	ApertureStopOrGratingType = rhs.ApertureStopOrGratingType;
	DiffractionOrder = rhs.DiffractionOrder;
	Reflectivity = rhs.Reflectivity;
	Transmissivity = rhs.Transmissivity;
	RMSSlopeError = rhs.RMSSlopeError;
	RMSSpecError = rhs.RMSSpecError;
	
	for (int i=0;i<4;i++)
	{
		//RefractiveIndex[i] = rhs.RefractiveIndex[i];
		Grating[i] = rhs.Grating[i];
	}

	return *this;
}

void ST_OpticalProperties::Write(FILE *fdat){
	if(! fdat) return;
	fprintf(fdat, 	
		"OPTICAL\t%c\t"
		"%d\t%d\t%d\t"
		"%lg\t%lg\t%lg\t%lg\t"
		"%lg\t%lg\t"
		"%lg\t%lg\t%lg\t%lg\n",

		DistributionType,
		ApertureStopOrGratingType, OpticSurfNumber, DiffractionOrder,
		Reflectivity, Transmissivity, RMSSlopeError, RMSSpecError,
		0., 0.,
		Grating[0], Grating[1], Grating[2], Grating[3] );

}

void ST_OpticalPropertySet::Write(FILE *fdat)
{
	if(! fdat) return;
	fprintf(fdat, "OPTICAL PAIR\t%s\n", Name.c_str() );
	Front.Write(fdat);
	Back.Write(fdat);
}

ST_Element::ST_Element()
{
	Enabled = true;
	ZRot = 0.;
	ShapeIndex = ' ';
	Ap_A=Ap_B=Ap_C=Ap_D=Ap_E=Ap_F=Ap_G=Ap_H=0.;
	Su_A=Su_B=Su_C=Su_D=Su_E=Su_F=Su_G=Su_H=0.;
	ApertureArea = 0.;
	SurfaceIndex = ' ';
	Comment = "";
	InteractionType = 0;
	
	ZAperture = 0;

	Optics = NULL;	
}

void ST_Element::Write(FILE *fdat){
	if(! fdat) return;
	fprintf(fdat, 
	"%d\t"
	"%lg\t%lg\t%lg\t"
	"%lg\t%lg\t%lg\t"
	"%lg\t"
	"%c\t"
	"%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t"
	"%c\t"
	"%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t%lg\t"
	"%s\t"
	"%s\t%d\t"
	"%s\n",

	Enabled?1:0,
	Origin[0], Origin[1], Origin[2],	 //Origin
	AimPoint[0], AimPoint[1], AimPoint[2],
	ZRot,
	ShapeIndex,
		Ap_A, Ap_B, Ap_C, Ap_D, Ap_E, Ap_F, Ap_G, Ap_H,
	SurfaceIndex,
		Su_A, Su_B, Su_C, Su_D, Su_E, Su_F, Su_G, Su_H,
	"", //Surface geometry file
	OpticName.c_str(), InteractionType,
	Comment.c_str() );


}

void ST_Element::UpdateRotationMatrix(){
	
	/*double CosRefZ[3],Euler[3],CosLoc[3],CosRefX[3],CosRefY[3],/*RRefToLoc[3][3],RLocToRef[3][3];*/
		
	double Alpha,Beta,Gamma,
		CosAlpha,CosBeta,CosGamma,
		SinAlpha,SinBeta,SinGamma;
		

	double dx = AimPoint[0] - Origin[0];
	double dy = AimPoint[1] - Origin[1];
	double dz = AimPoint[2] - Origin[2];
	double dtot = sqrt(dx*dx + dy*dy + dz*dz);

	dx /= dtot;
	dy /= dtot;
	dz /= dtot;

	/*CosRefZ[0] = dx;
	CosRefZ[1] = dy;
	CosRefZ[2] = dz;*/

	Euler[0]  = atan2(dx,dz);
	Euler[1] = asin(dy);
	Euler[2] = ZRot*(acos(-1.0)/180.0);

	Alpha = Euler[0];
	Beta  = Euler[1];
	Gamma = Euler[2];
	CosAlpha = cos(Alpha);
	CosBeta  = cos(Beta);
	CosGamma = cos(Gamma);
	SinAlpha = sin(Alpha);
	SinBeta  = sin(Beta);
	SinGamma = sin(Gamma);

	//{Fill in elements of the transformation matrix as per Spencer and Murty paper
	// page 673 equation (2)}
	RRefToLoc[0][0] = CosAlpha*CosGamma + SinAlpha*SinBeta*SinGamma;
	RRefToLoc[0][1] = -CosBeta*SinGamma;
	RRefToLoc[0][2] = -SinAlpha*CosGamma + CosAlpha*SinBeta*SinGamma;
	RRefToLoc[1][0] = CosAlpha*SinGamma - SinAlpha*SinBeta*CosGamma;
	RRefToLoc[1][1] = CosBeta*CosGamma;
	RRefToLoc[1][2] = -SinAlpha*SinGamma - CosAlpha*SinBeta*CosGamma;
	RRefToLoc[2][0] = SinAlpha*CosBeta;
	RRefToLoc[2][1] = SinBeta;
	RRefToLoc[2][2] = CosAlpha*CosBeta;

	//Transpose the matrix to change the direction of functionality
	st_matrix_transpose(RRefToLoc, RLocToRef);

}


ST_Sun::ST_Sun()
{
	Reset();
}

void ST_Sun::Reset()
{
	
	ShapeIndex = ' ';
	Sigma = 0;
	
}

void ST_Sun::Write(FILE *fdat)
{
	if(! fdat) return;
	fprintf(fdat, "SUN\tPTSRC\t%d\tSHAPE\t%c\tSIGMA\t%lg\tHALFWIDTH\t%lg\n", PointSource?1:0, ShapeIndex, Sigma, Sigma);
	fprintf(fdat, "XYZ\t%lg\t%lg\t%lg\tUSELDH\t%d\tLDH\t%lg\t%lg\t%lg\n", Origin[0], Origin[1], Origin[2], 0, 0., 0., 0.);
	if( ShapeIndex == 'd' ){
		int np = SunShapeAngle.size();
		fprintf(fdat, "USER SHAPE DATA\t%d\n", np);
		for (int i=0;i<np;i++)
			fprintf(fdat, "%lg\t%lg\n", SunShapeAngle.at(i), SunShapeIntensity.at(i) );
	}
}

ST_RayData::ST_RayData()
{
	m_dataCount = 0;
	m_dataCapacity = 0;
}

ST_RayData::~ST_RayData()
{
	Clear();
}

ST_RayData::ray_t *ST_RayData::Append( double pos[3],
				 double cos[3],
				 int element,
				 int stage,
				 unsigned int raynum )
{
	if (m_dataCount == m_dataCapacity)
	{
		// need to allocate more blocks
		block_t *b = new block_t;
		b->count = 0;
		m_blockList.push_back( b );
		m_dataCapacity += block_size;
	}

	ray_t *r = Index( m_dataCount, true );
	if( r != 0 )
	{
		::memcpy( &r->pos, pos, sizeof(double)*3 );
		::memcpy( &r->cos, cos, sizeof(double)*3 );
		r->element = element;
		r->stage = stage;
		r->raynum = raynum;
		m_dataCount++;
		return r;
	}
	else return 0;
}

bool ST_RayData::Overwrite( unsigned int idx,
				double pos[3],
				double cos[3],
				int element,
				int stage,
				unsigned int raynum)
{
	ray_t *r = Index( idx, true );
	if ( r != 0 )
	{
		::memcpy( r->pos, pos, sizeof(double)*3 );
		::memcpy( r->cos, cos, sizeof(double)*3 );
		r->element = element;
		r->stage = stage;
		r->raynum = raynum;
		return true;
	}
	else
		return false;
}

bool ST_RayData::Query( unsigned int idx,
				double pos[3],
				double cos[3],
				int *element,
				int *stage,
				unsigned int *raynum)
{
	ray_t *r = Index( idx, false );
	if ( r != 0 )
	{
		if (pos!=0) ::memcpy( pos, r->pos, sizeof(double)*3 );
		if (cos!=0) ::memcpy( cos, r->cos, sizeof(double)*3 );
		if (element) *element = r->element;
		if (stage) *stage = r->stage;
		if (raynum) *raynum = r->raynum;
		return true;
	}
	else
		return false;

}

void ST_RayData::Merge( ST_RayData &src )
{
	std::vector<block_t*> list, partial_blocks;
	size_t i;

	list.reserve( m_blockList.size() + src.m_blockList.size() );

	for (i=0;i<m_blockList.size();i++)
	{
		if (m_blockList[i]->count == block_size)
			list.push_back( m_blockList[i] );
		else
			partial_blocks.push_back( m_blockList[i] );
	}

	for (i=0;i<src.m_blockList.size();i++)
	{
		if (src.m_blockList[i]->count == block_size)
			list.push_back( src.m_blockList[i] );
		else
			partial_blocks.push_back( src.m_blockList[i] );
	}

	src.m_blockList.clear();
	src.m_dataCount = 0;
	src.m_dataCapacity = 0;

	m_blockList = list;
	m_dataCapacity = m_dataCount = m_blockList.size() * block_size;

	// append all the data in the partial blocks

	for (i=0;i<partial_blocks.size();i++)
	{
		block_t *b = partial_blocks[i];
		for (size_t j=0;j<b->count;j++)
		{
			ray_t &r = b->data[j];
			Append( r.pos, r.cos, r.element, r.stage, r.raynum );
		}

		delete b;
	}
	partial_blocks.clear();
}

void ST_RayData::Clear()
{
	for (size_t i=0;i<m_blockList.size();i++)
		delete m_blockList[i];
	m_blockList.clear();
	m_dataCount = 0;
	m_dataCapacity = 0;
}

st_uint_t ST_RayData::Count()
{
	return m_dataCount;
}

ST_RayData::ray_t *ST_RayData::Index(st_uint_t i, bool write_access)
{
	if (i >= m_dataCapacity)
		return 0;

	size_t block_num = i / block_size;
	size_t block_idx = i % block_size;

	if (block_num >= m_blockList.size()
		 || block_idx >= block_size )
		return 0;

	// update block.count to highest accessed index
	block_t *b = m_blockList[block_num];

	if (write_access && block_idx >= b->count)
		b->count = block_idx+1;

	if (!write_access && block_idx >= b->count)
		return 0;

	return &(b->data[block_idx]);
}

void ST_RayData::Print()
{
	printf("[ blocks: %d count: %u capacity: %u ]\n",
		m_blockList.size(),
		(unsigned int)m_dataCount,
		(unsigned int)m_dataCapacity );

	size_t n = Count();
	for (size_t i=0;i<n;i++)
	{
		double pos[3],cos[3];
		int elm, stage;
		unsigned int ray;
		if (Query(i, pos, cos, &elm, &stage, &ray))
		{
			printf("   [%u] = { [%lg,%lg,%lg][%lg,%lg,%lg] %d %d %u }\n", i,
				pos[0], pos[1], pos[2],
				cos[0], cos[1], cos[2],
				elm, stage, ray);
		}
	}

	printf("\n");
}

ST_Stage::ST_Stage()
{
	st_uint_t i,j;
	for (i=0;i<3;i++) Origin[i]=AimPoint[i]=Euler[i]=0;
	for (i=0;i<3;i++) for (j=0;j<3;j++) RRefToLoc[i][j]=RLocToRef[i][j]=0;
	
	ZRot = 0;
	MultiHitsPerRay = true;
	Virtual = false;
	TraceThrough = false;
	Name = "";
}

ST_Stage::~ST_Stage()
{
	for (st_uint_t i=0;i<ElementList.size();i++)
		delete ElementList[i];
	ElementList.clear();
}

void ST_Stage::Write(FILE *fdat){
	if(! fdat) return;
	fprintf(fdat, 
		"STAGE\tXYZ\t%lg\t%lg\t%lg\tAIM\t%lg\t%lg\t%lg\tZROT\t%lg\tVIRTUAL\t%d\t"
		"MULTIHIT\t%d\tELEMENTS\t%d\tTRACETHROUGH\t%d\n",
		Origin[0], Origin[1], Origin[2],
		AimPoint[0], AimPoint[1], AimPoint[2],
		ZRot,
		Virtual?1:0,
		MultiHitsPerRay?1:0,
		(int)ElementList.size(),
		TraceThrough?1:0);

	fprintf(fdat, "%s\n", Name.c_str());
	
	for (int i=0;i<(int)ElementList.size();i++)
		ElementList.at(i)->Write(fdat);
	
}

ST_System::ST_System()
{
	SunRayCount = 0;
	Pi = acos(-1);
	sim_raycount=1000;
	sim_raymax=100000;
	sim_errors_sunshape=true;
	sim_errors_optical=true;
}

ST_System::~ST_System()
{
	for (st_uint_t i=0;i<StageList.size();i++)
		delete StageList[i];
	StageList.clear();
}

void ST_System::Write(FILE *fdat){
	if(! fdat) return;
	
	Sun.Write(fdat);

	fprintf(fdat, "OPTICS LIST COUNT\t%d\n", (int)OpticsList.size() );
	for (int i=0;i<(int)OpticsList.size();i++)
		OpticsList.at(i)->Write( fdat );

	fprintf(fdat, "STAGE LIST COUNT\t%d\n", (int)StageList.size() );
	for (int i=0;i<(int)StageList.size();i++)
		StageList[i]->Write( fdat );
}

void ST_System::ClearAll()
{
	for (st_uint_t i=0;i<OpticsList.size();i++)
		delete OpticsList[i];
	OpticsList.clear();

	for (st_uint_t i=0;i<StageList.size();i++)
		delete StageList[i];
	StageList.clear();
}

bool ST_System::CreateSTSystem(var_set &variables, SolarField &SF, Hvector &helios){
	/* 
	Take the geometry specified in the SolarField SF and the heliostats listed in helios and create a 
	SolTrace simulation object.
	*/

	double pi = acos(-1.), r2d = 180./pi;

	//Resize the stage list. There will be 2 stages -- the heliostat field and the reciever
	if(StageList.size() != 0) ClearAll();
	for(int i=0; i<2; i++){
		StageList.push_back( new ST_Stage() );
	}
	
	/*--- Configure sun shape ---*/
	int sun_type;
	double sigma;
	char shape = 'i';	//invalid
	to_integer(variables["ambient"][0]["sun_type"].value, &sun_type );
	to_double(variables["ambient"][0]["sun_rad_limit"].value, &sigma);
	Sun.PointSource = sun_type == 0;
	if(sun_type == 2){ shape = 'p'; }	//Pillbox sun
	else if(sun_type == 4){	shape = 'g'; }		//Gaussian sun
	else if(sun_type == 1){	//Limb-darkened sun
		/* Create a table based on the limb-darkened profile and set as a user sun */
		shape = 'd';
		int np = 26;
		double
			R = 4.65e-3,	//maximum subtended angle
			dr = R/double(np-1),
			*angle = new double[np],
			*intens = new double[np];
		for(int i=0; i<np; i++){
			angle[i] = dr*double(i);
			intens[i] = 1.0 - 0.5138*pow(angle[i]/R, 4);
			angle[i] *= 1000.;	//mrad
		}
		intens[np-1] = 0.;
		
		//Fill into the sun object
		Sun.SunShapeIntensity.resize(np);
		Sun.SunShapeAngle.resize(np);
		for(int i=0; i<np; i++){
			Sun.SunShapeIntensity.at(i) = intens[i];
			Sun.SunShapeAngle.at(i) = angle[i];
		}
		
		delete [] angle;
		delete [] intens;
	}
	else if(sun_type == 5){		//Buie sun
		shape = 'd';
		double
			kappa, gamma, theta, chi;
		//calculate coefficients
		chi = variables["ambient"][0]["sun_csr"].value_double(); 
		kappa = 0.9*log(13.5 * chi)*pow(chi, -0.3);
		gamma = 2.2*log(0.52 * chi)*pow(chi, 0.43) - 0.1;

		int np = 50;
		double
			*angle = new double[np],
			*intens = new double[np];

		for(int i=0; i<np; i++){
			theta = (double)i*25./(double)np;
			angle[i] = theta;
			if(theta > 4.65){
				intens[i] = exp(kappa)*pow(theta, gamma);
			}
			else
			{
				intens[i] = cos(0.326 * theta)/cos(0.308 * theta);
			}
		}
		//Fill into the sun object
		Sun.SunShapeIntensity.resize(np);
		Sun.SunShapeAngle.resize(np);
		for(int i=0; i<np; i++){
			Sun.SunShapeIntensity.at(i) = intens[i];
			Sun.SunShapeAngle.at(i) = angle[i];
		}
		
		delete [] angle;
		delete [] intens;

	}
	else if(sun_type == 0){}	//Point sun
	else if(sun_type == 3){	//User sun
		shape = 'd';
		matrix_t<double> *sundat = SF.getAmbientObject()->getUserSun();
		int np = sundat->nrows();
		double
			*angle = new double[np],
			*intens = new double[np];
		for(int i=0; i<np; i++){
			angle[i] = sundat->at(i,0);
			intens[i] = sundat->at(i,1);
			//angle[i] *= 1000.; //mrad
		}

		//Fill into the sun object
		Sun.SunShapeIntensity.resize(np);
		Sun.SunShapeAngle.resize(np);
		for(int i=0; i<np; i++){
			Sun.SunShapeIntensity.at(i) = intens[i];
			Sun.SunShapeAngle.at(i) = angle[i];
		}
		
		delete [] angle;
		delete [] intens;
	}
	else{ return false; }
	//set other sun parameters
	Sun.ShapeIndex = shape;
	Sun.Sigma = sigma;
	
	/*--- Set the sun position ---*/
	Vect *svect = SF.getAmbientObject()->getSunVector();
	Sun.Origin[0] = svect->i*1.e4;
	Sun.Origin[1] = svect->j*1.e4;
	Sun.Origin[2] = svect->k*1.e4;
	
	/*
	--- Set up optical property set ---
	
	The optical property set describes the behavior of a surface (front and back sides) optically.
	Reflective properties, transmissivity, slope error and specularity, and error type are specified. 
	Several irrelevant properties must also be set, including refraction and grating properties.
	
	Create an optical property set for each heliostat template
	
	*/
	
	int nhtemp;
	if(OpticsList.size() > 0){
		return false;	//Error, should have been cleared earlier
	}
	else{	
		nhtemp = SF.getHeliostatTemplates()->size();
		for(int i=0; i<nhtemp; i++){
			OpticsList.push_back( new ST_OpticalPropertySet() );
		}
	}

	//int *optic = new int[nhtemp];
	//double grating[] = {0.,0.,0.};
	
	//map the optics pointer to the heliostat template name
	unordered_map<std::string, ST_OpticalPropertySet*> optics_map;

	int ii=0;
	for(htemp_map::iterator it = SF.getHeliostatTemplates()->begin(); it != SF.getHeliostatTemplates()->end(); it++){
		Heliostat *H = it->second;
		OpticsList.at(ii)->Name = (*H->getHeliostatName()).c_str();
		optics_map[ OpticsList.at(ii)->Name ] = OpticsList.at(ii);
		
		/* 
		The optical error in SolTrace is described in spherical coordinates, so the total error 
		budget should represent the weighted average of the X and Y components. To average, divide
		the final terms by sqrt(2). If both X and Y errors are the same, then the result will be
		sigma_spherical = sigma_x = sigma_y. Otherwise, the value will fall between sigma_x and 
		sigma_y.
		*/
		
		double refl = H->getTotalReflectivity();	//reflectivity * soiling
		//Note that the reflected energy is also reduced by the fraction of inactive heliostat aperture. Since
		//we input the actual heliostat dimensions into soltrace, apply this derate on the reflectivity.
		refl *= H->getReflectiveAreaDerate();
		//calculate total error about the normal vector. Dimensionality effects in X and Y are combined through convolution.
		double err[2], errnorm=0., errsurf;
		H->getErrorAngular(err);
		errnorm = (pow(err[0], 2) + pow(err[1], 2))/2.;
		H->getErrorSurface(err);
		errnorm += (pow(err[0], 2) + pow(err[1], 2))/2.;
		errnorm = sqrt(errnorm)*1000.; //mrad - normal vector error
		H->getErrorReflected(err);
		errsurf = sqrt( (pow(err[0], 2) + pow(err[1], 2))/2. )*1000.;	//mrad - reflected vector error
				
		/* 
		st_optic(st_context_t pcxt, 
					st_uint_t idx, 
					int fb,			// 1=front,2=back 
					char dist, 
					int optnum, 
					int apgr, 
					int order,
					double rreal, 
					double rimag,
					double ref, 
					double tra,
					double gratingab12[3],
					double rmsslope, 
					double rmsspec,
					int userefltable, 
					int npoints,
					double *angles, 
					double *refls );
		*/
		
		//Add the front
		OpticsList.at(ii)->Front.DistributionType = 'g';
		OpticsList.at(ii)->Front.OpticSurfNumber = 0;
		OpticsList.at(ii)->Front.ApertureStopOrGratingType = 0;
		OpticsList.at(ii)->Front.DiffractionOrder = 0;
		OpticsList.at(ii)->Front.Reflectivity = refl;
		OpticsList.at(ii)->Front.Transmissivity = 0.;
		for(int j=0; j<4; j++) OpticsList.at(ii)->Front.Grating[j] = 0.;
		OpticsList.at(ii)->Front.RMSSlopeError = errnorm;
		OpticsList.at(ii)->Front.RMSSpecError = errsurf;
		//st_optic(cxt, optic[ii], 1, 'g', 0, 0, 0, 0., 0., refl, 0., grating, errnorm, errsurf, 0, 0, NULL, NULL);
		//add the back
		OpticsList.at(ii)->Back.DistributionType = 'g';
		OpticsList.at(ii)->Back.OpticSurfNumber = 0;
		OpticsList.at(ii)->Back.ApertureStopOrGratingType = 0;
		OpticsList.at(ii)->Back.DiffractionOrder = 0;
		OpticsList.at(ii)->Back.Reflectivity = 0.;
		OpticsList.at(ii)->Back.Transmissivity = 0.;
		for(int j=0; j<4; j++) OpticsList.at(ii)->Back.Grating[j] = 0.;
		OpticsList.at(ii)->Back.RMSSlopeError = 100.0;
		OpticsList.at(ii)->Back.RMSSpecError = 0.;
		
		ii++;
	}
	
	/*
	--- Set the heliostat stage ---

	this contains all heliostats regardless of differeing geometry or optical properties
	*/

	ST_Stage *h_stage = StageList.at(0);
	//global origin
	h_stage->Origin[0] = 0.;
	h_stage->Origin[1] = 0.;
	h_stage->Origin[2] = 0.;
	//global aim, leave as 0,0,1
	h_stage->AimPoint[0] = 0.;
	h_stage->AimPoint[1] = 0.;
	h_stage->AimPoint[2] = 1.;
	//no z rotation
	h_stage->ZRot = 0.;
	//{virtual stage, multiple hits per ray, trace through} UI checkboxes
	h_stage->MultiHitsPerRay = true;
	h_stage->Virtual = false;
	h_stage->TraceThrough = false;
	//name
	h_stage->Name = "Heliostat field";
		
	/*
	--- Add elements to the stage ---
	
	"apersurfdialog.cpp"

	static AperSurfDialog::TypeInfo aperture_types[] = {
	{ 'c', 1, "D", "circle", "Circular" },
	{ 'h', 1, "D", "hexagon", "Hexagonal" },
	{ 't', 1, "D", "triangle", "Triangular" },
	{ 'r', 2, "W H", "rectangle", "Rectangular" },
	{ 'l', 3, "X1 X2 L", "troughsection", "Single Axis Curvature Section" },
	{ 'a', 3, "R1 R2 Theta", "annulus", "Annular" },
	{ 'i', 6, "X1 Y1 X2 Y2 X3 Y3", "irreg_tri", "Irregular Triangle" },
	{ 'q', 8, "X1 Y1 X2 Y2 X3 Y3 X4 Y4", "irreg_quad", "Irregular Quadrilateral" },
	{ 0, 0, NULL, NULL } };

	static AperSurfDialog::TypeInfo surface_types[] = {
	{ 'm', -1, "", "zernike", "Zernike Series File" },
	{ 'v', -1, "", "vshot", "VSHOT Data File" },
	{ 'r', -1, "", "polynomial", "Rotationally Symmetric Polynomial File" },
	{ 'i', -1, "", "cubicspline", "Rotationally Symmetric Cubic Spline File" },
	{ 'p', 2, "Cx Cy", "parabola", "Parabolic" },
	{ 's', 1, "C", "sphere", "Spherical" },
	{ 'o', 2, "C Kappa", "hyperhemi", "Hyperboloids and Hemiellipsoids" },
	{ 'f', 0, "", "flat", "Flat" },
	{ 'c', 1, "Theta", "cone", "Conical" },
	{ 't', 1, "1/R", "cylinder", "Cylindrical" },
	{ 0, 0, NULL, NULL } };
	
	*/

	int nh = helios.size();
	
	if(h_stage->ElementList.size() != 0){
		return false;		//error
	}

	try
	{
		h_stage->ElementList.reserve(nh * (helios.front()->IsFacetDetail() ? helios.front()->getNumCantX()*helios.front()->getNumCantY() : 1));
	}
	catch(...){
		//memory allocation error
		return false;
	}

	/*for(int i=0; i<nh; i++){
		h_stage->ElementList.push_back( new ST_Element() );
	}*/
	
	//keep track of the heliostat area
	double Ahtot=0.;
	for(int i=0; i<nh; i++){
		
		Heliostat *H = helios.at(i);
		Ahtot += H->getArea();

		matrix_t<Reflector> *panels = H->getPanels();
		bool isdetail = H->IsFacetDetail();
		int 
			ncantx = isdetail ? panels->ncols() : 1,
			ncanty = isdetail ? panels->nrows() : 1,
			npanels = ncantx * ncanty;

		//Get values that apply to the whole heliostat
		bool enabled = H->getInLayout();
		
		Point *P; 
		Vect *V;
		P = H->getLocation();
		V = H->getTrackVector();
		
		double zrot = r2d*Toolbox::ZRotationTransform(*V);

		char shape = H->IsRound() ? 'c' : 'r';

		string opticname = (*H->getHeliostatName()).c_str();

		for(int j=0; j<ncantx; j++){
			for(int k=0; k<ncanty; k++){

				h_stage->ElementList.push_back( new ST_Element() );
				
				ST_Element *element = h_stage->ElementList.back();

				element->Enabled = enabled;
				
				if(isdetail){
					//Calculate unique positions and aim vectors for each facet
					PointVect *F = panels->at(k, j).getOrientation();
					Point Floc = *F->point();
					Vect Faim = *F->vect();
					Toolbox::unitvect(Faim);

					//Rotate to match heliostat rotation
					Toolbox::rotation( H->getZenithTrack(), 0, Floc);
					Toolbox::rotation( H->getZenithTrack(), 0, Faim);
					Toolbox::rotation( H->getAzimuthTrack(), 2, Floc);
					Toolbox::rotation( H->getAzimuthTrack(), 2, Faim);

					element->Origin[0] = P->x + Floc.x;
					element->Origin[1] = P->y + Floc.y;
					element->Origin[2] = P->z + Floc.z;

					element->AimPoint[0] = element->Origin[0] + Faim.i*1000.;
					element->AimPoint[1] = element->Origin[1] + Faim.j*1000.;
					element->AimPoint[2] = element->Origin[2] + Faim.k*1000.;

				}
				else{
					element->Origin[0] = P->x;
					element->Origin[1] = P->y;
					element->Origin[2] = P->z;
				
					element->AimPoint[0] = P->x + V->i*1000.;
					element->AimPoint[1] = P->y + V->j*1000.;
					element->AimPoint[2] = P->z + V->k*1000.;
				}

				//element->ZRot = 0.;
				element->ZRot = zrot;
		
				element->ShapeIndex = shape;
				
				//Set up the surface description
				if(H->IsRound()){
					element->Ap_A = H->getWidth();
				}
				else{
					if(isdetail){
						//Image size is for each individual facet.
						element->Ap_A = panels->at(k, j).getWidth();
						element->Ap_B = panels->at(k, j).getHeight();
					}
					else{
						element->Ap_A = H->getWidth();
						element->Ap_B = H->getHeight();
					}
				}
				
		
				//Model surface as either flat or parabolic focus in X and/or Y
				//double spar[] ={0., 0., 0., 0., 0., 0., 0., 0.};
				if(H->getFocusMethod() == 0){	//Flat
					element->SurfaceIndex = 'f';
				}
				else{	//Not flat
					//coefs are 1/2*f where f is focal length in x or y
					element->Su_A = 0.5/H->getFocalX();
					element->Su_B = 0.5/H->getFocalY();
					element->SurfaceIndex = 'p';
				}
				element->InteractionType = 2;	//1 = refract, 2 = reflect
				element->OpticName = opticname;
				element->Optics = optics_map[ opticname ];
			}
		}
	}
		
	/*--- Set the receiever stages ---*/
	ST_Stage *r_stage = StageList.at(1);
	//Global origin
	r_stage->Origin[0] = 0.;
	r_stage->Origin[1] = 0.;
	r_stage->Origin[2] = 0.;
	//Aim point
	r_stage->AimPoint[0] = 0.;
	r_stage->AimPoint[1] = 0.;
	r_stage->AimPoint[2] = 1.;
	//No z rotation
	r_stage->ZRot = 0.;
	//{virtual stage, multiple hits per ray, trace through} UI checkboxes
	r_stage->Virtual = false;
	r_stage->MultiHitsPerRay = true;
	r_stage->TraceThrough = false;
	//Name
	r_stage->Name = "Receiver";

	vector<Receiver*> *recs = SF.getReceivers();
	int nrecs = recs->size();
	unordered_map<int, Receiver*> rstage_map;	//map between element number and pointer to the receiver
	
	if(r_stage->ElementList.size() > 0){
		return false;	//Error
	}
	for(int i=0; i<nrecs; i++){
		r_stage->ElementList.push_back( new ST_Element() );
	}
	
	for(int i=0; i<nrecs; i++){
		//Get the receiver
		Receiver *rec = recs->at(i);
		if(! rec->isReceiverEnabled() ) continue;
		rstage_map[i] = rec;	//keep track of the element number 
		//Get the receiver geometry type
		int recgeom = rec->getReceiverGeomType();
		
		//append an optics set, required for the receiver
		OpticsList.push_back( new ST_OpticalPropertySet() );
		ST_OpticalPropertySet *copt = OpticsList.at(OpticsList.size()-1);

		switch (recgeom)
		{
		case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
		{
			//Add optics stage
			copt->Name = (*rec->getReceiverName()).c_str();
			
			//set the optical properties. This should be a diffuse surface, make it a pillbox distribution w/ equal angular reflection probability.
			copt->Front.DistributionType = 'g';
			copt->Front.OpticSurfNumber = 0;
			copt->Front.ApertureStopOrGratingType = 0;
			copt->Front.Reflectivity = 1.-rec->getAbsorptance();
			copt->Front.RMSSlopeError = 100.;
			copt->Front.RMSSpecError = 100.;
			//back
			copt->Back.DistributionType = 'g';
			copt->Back.OpticSurfNumber = 0;
			copt->Back.ApertureStopOrGratingType = 0;
			copt->Back.Reflectivity = 1.-rec->getAbsorptance();
			copt->Back.RMSSlopeError = 100.;
			copt->Back.RMSSpecError = 100.;

			//displace by radius, inside is front, x1 and x2 = 0 for closed cylinder ONLY
			//Add a closed cylindrical receiver to the stage 
			double diam = rec->getReceiverWidth();
			Point pos;
			Vect aim;

			ST_Element *element = r_stage->ElementList.at(i);
			element->Enabled = true;
			pos.x = rec->getOffsetX();
			pos.y = rec->getOffsetY() - diam/2.;
			pos.z = rec->getOffsetZ() + rec->getOpticalHeight();
			element->Origin[0] = pos.x;
			element->Origin[1] = pos.y;
			element->Origin[2] = pos.z;
			
			//calculate the aim point. we need to rotate the receiver from a horizontal position into a vertical
			//position. The aim vector defines the Z axis with respect to the SolTrace receiver coordinates, and
			//in SolTrace, the cylindrical cross section lies in the X-Z plane.
			double 
				az = rec->getReceiverAzimuth(),
				el = rec->getReceiverElevation();
			aim.i = cos(el)*sin(az);
			aim.j = cos(el)*cos(az);
			aim.k = sin(el);
			element->AimPoint[0] = pos.x + aim.i*1000.;
			element->AimPoint[1] = pos.y + aim.j*1000.;
			element->AimPoint[2] = pos.z + aim.k*1000.;
			
			element->ZRot = 0.;
			/* in the special case of a closed cylinder, use parameters X1=0, X2=0, L = rec height */
			element->Ap_C = rec->getReceiverHeight();
			
			element->ShapeIndex = 'l';		//single axis curvature section
			element->SurfaceIndex = 't';
			element->Su_A = 2./diam;
			element->InteractionType = 2;
			element->OpticName = (*rec->getReceiverName()).c_str();
			element->Optics = copt;
			break;
		}
		case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
			/* 
			X1 is opposite of X2, Length L. X1 and X2 for aperture defines the span of the circle 
			defined in the surface shape. Receivers with span angles greater than 180 must be 
			constructed by parts.
			*/
			break;
		case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
			break;
		case Receiver::REC_GEOM_TYPE::PLANE_RECT:
		{
			double
				width = rec->getReceiverWidth(),
				height = rec->getReceiverHeight();
			//For the elliptical cavity, SolTrace can only handle circular apertures. Check to make sure and return an error if necessary.
			bool is_ellipse = recgeom == 4;
			if(is_ellipse && fabs(width - height) > 1.e-4){
				//PopMessage(wxT("SolTrace is not currently able to model elliptical apertures. Consider setting the aperture width equal to the aperture height."), wxT("Error"), wxICON_ERROR|wxOK);
				//Release context
				//st_free_context(cxt);
				return false;
			}
			
			copt->Name = (*rec->getReceiverName()).c_str();
			//set the optical properties. This should be a diffuse surface, make it a pillbox distribution w/ equal angular reflection probability.
			copt->Front.DistributionType = 'g';
			copt->Front.OpticSurfNumber = 0;
			copt->Front.ApertureStopOrGratingType = 0;
			copt->Front.Reflectivity = 1.-rec->getAbsorptance();
			copt->Front.RMSSlopeError = Pi/4.;
			copt->Front.RMSSpecError = Pi/4.;
			
			copt->Back.DistributionType = 'g';
			copt->Back.OpticSurfNumber = 0;
			copt->Back.ApertureStopOrGratingType = 0;
			copt->Back.Reflectivity = 1.-rec->getAbsorptance();
			copt->Back.RMSSlopeError = Pi/4.;
			copt->Back.RMSSpecError = Pi/4.;
			
			//Add a flat aperture to the stage
			Point pos;
			Vect aim;
			ST_Element *element = r_stage->ElementList.at(i);
			element->Enabled = true;
			pos.x = rec->getOffsetX();
			pos.y = rec->getOffsetY();
			pos.z = rec->getOffsetZ() + rec->getOpticalHeight();
			element->Origin[0] = pos.x;
			element->Origin[1] = pos.y;
			element->Origin[2] = pos.z;
			
			//Calculate the receiver aperture aim point
			double 
				az = rec->getReceiverAzimuth(),
				el = rec->getReceiverElevation();
			aim.Set(cos(el)*sin(az), cos(el)*cos(az), sin(el));
			element->AimPoint[0] = pos.x + aim.i*1000.;
			element->AimPoint[1] = pos.y + aim.j*1000.;
			element->AimPoint[2] = pos.z + aim.k*1000.;
			
			element->ZRot = 0.;
			
			//Set up the aperture arguments array
			element->Ap_A = width;
			element->Ap_B = is_ellipse ? 0. : height;
			//aperture shape 'c' circular or 'r' rectangular
			element->ShapeIndex = is_ellipse ? 'c' : 'r';
			element->SurfaceIndex = 'f';
			element->InteractionType = 2;
			element->OpticName = (*rec->getReceiverName()).c_str();
			element->Optics = copt;
			break;
		}
		case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
		case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
		case Receiver::REC_GEOM_TYPE::POLYGON_OPEN:
		case Receiver::REC_GEOM_TYPE::POLYGON_CAV:
		default:
			throw spexception("Unsupported receiver type in SolTrace geometry generation algorithm.");
			break;
		}			

	}

	//Simulation options
	int maxrays, minrays, seed;
	to_integer(variables["fluxsim"][0]["min_rays"].value, &minrays);
	to_integer(variables["fluxsim"][0]["max_rays"].value, &maxrays);
	to_integer(variables["fluxsim"][0]["seed"].value, &seed);
	sim_errors_sunshape = lower_case(variables["fluxsim"][0]["is_sunshape_err"].value) == "true";
	sim_errors_optical = lower_case(variables["fluxsim"][0]["is_optical_err"].value) == "true";
	
	sim_raycount = minrays;
	sim_raymax = maxrays;
	
	return true;

}

void ST_System::LoadIntoContext(ST_System *System, st_context_t spcxt){
	/* 
	Use the defined 'System' to generate the required SolTrace API context.
	*/

	//sun shape
	st_sun(spcxt, System->Sun.PointSource ? 1 : 0, System->Sun.ShapeIndex, System->Sun.Sigma);
	if(System->Sun.ShapeIndex == 'd'){
		//Add user defined angles
		int np = System->Sun.SunShapeAngle.size();
		double
			*angle = new double[np],
			*intens = new double[np];

		for(int i=0; i<np; i++){
			angle[i] = System->Sun.SunShapeAngle.at(i);
			intens[i] = System->Sun.SunShapeIntensity.at(i);
		}
		st_sun_userdata(spcxt, np, angle, intens);
		delete [] angle;
		delete [] intens;
	}
	//Sun position
	st_sun_xyz(spcxt, System->Sun.Origin[0], System->Sun.Origin[1], System->Sun.Origin[2]);

	st_clear_optics(spcxt);
	//Add all of the optics
	for (unsigned int nopt=0;nopt<System->OpticsList.size();nopt++)
	{
		int idx = st_add_optic(spcxt, System->OpticsList[nopt]->Name.c_str());
		
		ST_OpticalProperties *f = &System->OpticsList[nopt]->Front;
		st_optic(spcxt, idx, 1, f->DistributionType,
			f->OpticSurfNumber, f->ApertureStopOrGratingType, f->DiffractionOrder,
			0., 0.,
			f->Reflectivity, f->Transmissivity,
			f->Grating, f->RMSSlopeError, f->RMSSpecError, 0, 0, NULL, NULL );

		f = &System->OpticsList[nopt]->Back;
		st_optic(spcxt, idx, 2, f->DistributionType,
			f->OpticSurfNumber, f->ApertureStopOrGratingType, f->DiffractionOrder,
			0., 0.,
			f->Reflectivity, f->Transmissivity,
			f->Grating, f->RMSSlopeError, f->RMSSpecError, 0, 0, NULL, NULL );
	}

	//Add all of the elements and stages
	//st_clear_stages(spcxt);
	st_add_stages(spcxt, System->StageList.size());

	for (unsigned int ns=0;ns<System->StageList.size();ns++)
	{
		ST_Stage *stage = System->StageList[ns];

		st_stage_xyz(spcxt, ns, stage->Origin[0], stage->Origin[1], stage->Origin[2] );
		st_stage_aim(spcxt, ns, stage->AimPoint[0], stage->AimPoint[1], stage->AimPoint[2] );
		st_stage_zrot(spcxt, ns, stage->ZRot );
		st_stage_flags( spcxt, ns, stage->Virtual?1:0, stage->MultiHitsPerRay?1:0, stage->TraceThrough?1:0);

		st_clear_elements(spcxt, ns);
		st_add_elements( spcxt, ns, stage->ElementList.size() );

		for (unsigned int idx=0;idx<stage->ElementList.size();idx++)
		{
			ST_Element *e = stage->ElementList[idx];

			st_element_enabled( spcxt, ns, idx, e->Enabled?1:0 );
			st_element_xyz( spcxt, ns, idx, e->Origin[0], e->Origin[1], e->Origin[2] );
			st_element_aim( spcxt, ns, idx, e->AimPoint[0], e->AimPoint[1], e->AimPoint[2] );
			st_element_zrot( spcxt, ns, idx, e->ZRot );
			st_element_aperture( spcxt, ns, idx, e->ShapeIndex );
			double apar[] = {e->Ap_A, e->Ap_B, e->Ap_C, e->Ap_D, e->Ap_E, e->Ap_F, e->Ap_G, e->Ap_H};
			st_element_aperture_params( spcxt, ns, idx, apar);
			st_element_surface( spcxt, ns, idx, e->SurfaceIndex );
			//No surface file yet..
			double spar[] = {e->Su_A, e->Su_B, e->Su_C, e->Su_D, e->Su_E, e->Su_F, e->Su_G, e->Su_H};
			st_element_surface_params( spcxt, ns, idx, spar );

			st_element_interaction( spcxt, ns, idx, e->InteractionType );

			st_element_optic( spcxt, ns, idx, e->OpticName.c_str() );
		}

	}



}
#endif