#include "tckernel.h"


tcKernel::tcKernel(tcstypeprovider *prov)
	: tcskernel(prov), m_start(0), m_end(0), m_step(0)
{
	m_storeArrMatData = false;
	m_storeAllParameters = false;
}

tcKernel::~tcKernel()
{

}

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif


bool tcKernel::converged( double time )
{
	if (m_step != 0.0 )
	{
		int istep = (int) (time-m_start)/m_step;
		int nstep = (int) (m_end-m_start)/m_step;
		int nnsteps = nstep/200;
		if ( nnsteps == 0 ) nnsteps = 1;		
		if (istep % nnsteps == 0)
		{
			double percent = 100 * (((double)istep) / ((double)nstep) );
			//double elapsed = m_watch.Time() * 0.001;
			update( "calculating", percent, (float)istep );
			//if ( !m_progressDialog->Update( (int) percent, wxString::Format("%.1lf %% complete, %.2lf seconds elapsed, hour %.1lf", percent, elapsed, time/3600 )) )
		}
	}

	std::string buf;
	char ibuf[128];
	size_t j,k;
	for ( size_t i=0;i<m_results.size(); i++ )
	{
		tcsvalue &v = m_results[i].u->values[ m_results[i].idx ];
		switch( m_results[i].type )
		{
		case TCS_NUMBER:
			m_results[i].values[ m_dataIndex ].dval = v.data.value;
			break;
		case TCS_STRING:
			m_results[i].values[ m_dataIndex ].sval = v.data.cstr;
			break;
		case TCS_ARRAY:
			if ( m_storeArrMatData )
			{
				buf = "[ ";
				for (j=0;j<v.data.array.length;j++)
				{
					mysnprintf(ibuf, 126, "%lg%c", v.data.array.values[j],
						j < v.data.array.length-1 ? ',' : ' ');
					buf += ibuf;
				}
				buf += "]";
				m_results[i].values[ m_dataIndex ].sval = buf;
			}
			break;
		case TCS_MATRIX:
			if ( m_storeArrMatData )
			{
				mysnprintf( ibuf, 126, "{ %dx%d ", v.data.matrix.nrows, v.data.matrix.ncols );
				buf = ibuf;
				for (j=0;j<v.data.matrix.nrows;j++)
				{
					buf += " [";
					for (k=0;k<v.data.matrix.ncols;k++)
					{
						mysnprintf(ibuf, 126, "%lg%c", TCS_MATRIX_INDEX(&v, j, k),
							k < v.data.matrix.ncols-1 ? ',' : ' ');
						buf += ibuf;
					}
					buf += "]";
				}
				buf += " }";
				m_results[i].values[ m_dataIndex ].sval = buf;		
			}
			break;
		}
	}

	m_dataIndex++;

	return true;
}

int tcKernel::simulate( double start, double end, double step )
{

	// find all output variables and add to results vector
	m_start = start;
	m_end = end;
	m_step = step;
	m_dataIndex = 0;

	if ( end <= start || step <= 0 )
		return -77;

	int nsteps = (int)( (end-start)/step ) + 1;

	size_t ndatasets = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx=0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			if (is_ssc_array_output(vars[idx].name) || m_storeAllParameters)
				ndatasets++;
			idx++;
		}
	}

	if ( ndatasets < 1 )
		return -88;

	m_results.resize( ndatasets );

	size_t idataset = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx = 0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			if (is_ssc_array_output(vars[idx].name) || m_storeAllParameters )
			{
				dataset &d = m_results[ idataset++ ];
				char buf[32];
				sprintf(buf, "%d", i);
				d.u = &m_units[i];
				d.uidx = i;
				d.idx = idx;
				d.group = "Unit " + std::string(buf) + " (" + std::string(m_units[i].type->name) + ")";//: " + m_units[i].name;
				d.name = vars[idx].name;
				d.units = vars[idx].units;
				d.type = vars[idx].data_type;
				d.values.resize( nsteps, dataitem(0.0) );
			}
			idx++;
		}
	}
	
	//wxGetApp().Yield( true );

	//m_watch.Start();
	int code = tcskernel::simulate( start, end, step );
	//if (time_sec) *time_sec = ((double)m_watch.Time())*0.001;
	return code;
}

tcKernel::dataset *tcKernel::get_results(int idx)
{
	if (idx >= (int) m_results.size()) return 0;
	else return &m_results[idx];
}

void tcKernel::set_unit_value_ssc_string( int id, const char *name )
{
	set_unit_value( id, name, as_string(name) );
}

void tcKernel::set_unit_value_ssc_double( int id, const char *name )
{
	set_unit_value( id, name, as_double(name) );
}

void tcKernel::set_unit_value_ssc_double( int id, const char *name, double x )
{
	set_unit_value( id, name, x );
}


void tcKernel::set_unit_value_ssc_array( int id, const char *name )
{
	size_t len;
	ssc_number_t * p = as_array(name, &len);
	double *pt = new double[len];
	for ( size_t i=0;i<len;i++ ) pt[i] = (double) p[i];
	set_unit_value(id, name, pt, len); 
	delete [] pt;
	return;
}

void tcKernel::set_unit_value_ssc_matrix( int id, const char *name )
{
	size_t nr,nc;
	ssc_number_t *p = as_matrix(name, &nr, &nc);
	double *pt = new double[nr*nc];
	for ( size_t i=0;i<nr*nc;i++ ) pt[i] = (double) p[i];
	set_unit_value(id, name, pt, nr, nc); 
	delete [] pt;
	return;
}

bool tcKernel::set_output_array(const char *output_name, size_t len, double scaling)
{
	return set_output_array(output_name, output_name, len, scaling);
}

bool tcKernel::set_output_array(const char *ssc_output_name, const char *tcs_output_name, size_t len, double scaling)
{
	int idx=0;
	ssc_number_t *output_array = allocate( ssc_output_name, len );
	while( tcKernel::dataset *d = get_results(idx++) )
	{
		if ( (d->type == TCS_NUMBER) && (d->name == tcs_output_name) && (d->values.size() == len ) )
		{
			for (size_t i=0;i<len;i++)
				output_array[i] = (ssc_number_t) d->values[i].dval * scaling;
			return true;
		}
	}

	return false;
}

bool tcKernel::set_all_output_arrays()
{
	int idx=0;
	while( tcKernel::dataset *d = get_results(idx++) )
	{	// if the TCS value is a TCS_NUMBER (so that we can put the value into a one-dimensional array - single value for 8760 hours)
		// and
		// if there is an SSC_OUTPUT with the same name
		if ( (d->type == TCS_NUMBER) && ( is_ssc_array_output(d->name) ) )
		{
			ssc_number_t *output_array = allocate( d->name, d->values.size() );
			for (size_t i=0; i<d->values.size(); i++)
				output_array[i] = (ssc_number_t) d->values[i].dval;
		}
	}

	return true;
}
