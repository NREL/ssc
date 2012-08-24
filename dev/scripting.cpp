#include <wx/wx.h>
#include <wx/imaglist.h>
#include <wx/splitter.h>
#include <wx/filename.h>
#include <wx/statline.h>
#include <wx/html/htmlwin.h>
#include <wx/aui/aui.h>
#include <wx/tokenzr.h>
#include <wx/busyinfo.h>

#include <cml/codeedit.h>
#include <cml/mtrand.h>
#include <cml/util.h>
#include <cml/wpplotdata.h>
#include <cml/wpplotdataarray.h>
#include <cml/wpbarplot.h>
#include <cml/wplineplot.h>
#include <cml/wpscatterplot.h>
#include <cml/wpplotsurface2d.h>

#include <cml/dview/wxdvplotctrl.h>
#include <cml/dview/wxdvarraydataset.h>

#include <lk_lex.h>
#include <lk_parse.h>
#include <lk_eval.h>
#include <lk_invoke.h>
#include <lk_stdlib.h>

#include "sscdev.h"
#include "dataview.h"
#include "scripting.h"

void Output( const wxString &text )
{
	app_frame->Log( text, false );
}


void Output( const char *fmt, ... )
{
	static char buf[2048];
	va_list ap;
	va_start(ap, fmt);
#if defined(_MSC_VER)||defined(_WIN32)
	_vsnprintf(buf, 2046, fmt, ap);
#else
	vsnprintf(buf, 2046, fmt, ap);
#endif
	va_end(ap);
	Output( wxString(buf) );	
}

void ClearOutput()
{
	app_frame->ClearLog();
}


class PlotWin;

static int _iplot = 1;
static PlotWin *_curplot = 0;

class PlotWin : public wxFrame
{
	WPPlotSurface2D *m_plot;
public:
	PlotWin( wxWindow *parent )
		: wxFrame( 0, wxID_ANY, 
			wxString::Format("plot %d", _iplot++),
			wxDefaultPosition, wxSize(500,400) )
	{
		m_plot = new WPPlotSurface2D( this );
		m_plot->SetEnableDefaultRightClickMenu();
		m_plot->SetBackgroundColour(*wxWHITE);
		_curplot = this;
		Show();
	}

	virtual ~PlotWin()
	{
		if ( _curplot == this )
			_curplot = 0;
	}

	enum { BAR, LINE, SCATTER };
	void Add( double *x, double *y, int len, int thick, wxColour &col, int type,
		const wxString &xlab, const wxString &ylab, const wxString &series,
		int xap, int yap)
	{
		if (len <= 0 ) return;
		WPPlotDataArray *arr = new WPPlotDataArray;
		arr->alloc( len );
		for (int i=0;i<len;i++)
			arr->append( PointF( x[i], y[i] ) );

		WPPlottable2D *p = 0;

		switch (type )
		{
		case BAR:
			p = new WPBarPlot;
			((WPBarPlot*)p)->FillColour = col;
			((WPBarPlot*)p)->BarWidth = thick;
			break;
		case LINE:
			p = new WPLinePlot;
			((WPLinePlot*)p)->LineColour =  col;
			((WPLinePlot*)p)->LineThickness = thick;
			break;
		case SCATTER:
			p = new WPScatterPlot;
			((WPScatterPlot*)p)->Colour = col;
			((WPScatterPlot*)p)->PointSize = thick;
			break;
		}

		if (!p)
		{
			delete arr;
			return;
		}
		arr->SetXLabel( xlab );
		arr->SetYLabel( ylab );
		p->SetData( arr );
		p->SetLabel( series );
		m_plot->Add( p, (WPPlotSurface2D::AxisPosition) xap, (WPPlotSurface2D::AxisPosition) yap );
		m_plot->Refresh();
	}

	static PlotWin *Current() { return _curplot; }
	static void NewPlot( ) { _curplot = 0; }
	static WPPlotSurface2D *Surface()
	{
		if ( Current() ) return Current()->m_plot;
		else return 0;
	}
};


void fcall_newplot( lk::invoke_t &cxt )
{
	LK_DOC("newplot", "Switches to a new plotting window on the next call to plot.", "([boolean:remove all]):none");
	PlotWin::NewPlot(  );

	if ( cxt.arg_count() == 1 && cxt.arg(0).as_boolean() )
	{
		wxWindowList wl = ::wxTopLevelWindows;
		for (size_t i=0;i<wl.size();i++)
			if ( PlotWin *p = dynamic_cast<PlotWin*>( wl[i] ))
				p->Destroy();
	}
}

void fcall_plot( lk::invoke_t &cxt )
{
	LK_DOC("plot", "Creates an XY line, bar, or scatter plot. Options include thick, type, color, xap, yap, xlabel, ylabel, series.", "(array:x, array:y, table:options):void");

	PlotWin *plot = PlotWin::Current();
	if ( plot == 0 )
		plot = new PlotWin( 0 );

	lk::vardata_t &a0 = cxt.arg(0).deref();
	lk::vardata_t &a1 = cxt.arg(1).deref();

	if ( a0.type() == lk::vardata_t::VECTOR
		&& a1.type() == lk::vardata_t::VECTOR
		&& a0.length() == a1.length()
		&& a0.length() > 0 )
	{
		int thick = 1;
		int type = PlotWin::LINE;
		wxColour col = *wxBLUE;
		wxString xlab = "x";
		wxString ylab = "y";
		wxString series = wxEmptyString;
		int xap = WPPlotSurface2D::XAXIS_BOTTOM;
		int yap = WPPlotSurface2D::YAXIS_LEFT;

		if (cxt.arg_count() > 2 && cxt.arg(2).deref().type() == lk::vardata_t::HASH )
		{
			lk::vardata_t &t = cxt.arg(2).deref();
			if ( lk::vardata_t *arg = t.lookup("thick") )
			{
				thick = arg->as_integer();
				if (thick < 1) thick = 1;
			}

			if ( lk::vardata_t *arg = t.lookup("type") )
			{
				wxString stype = arg->as_string().c_str();
				stype.Lower();
				if (stype == "bar") type = PlotWin::BAR;
				else if (stype == "scatter") type = PlotWin::SCATTER;
			}
			
			if (lk::vardata_t *arg = t.lookup("color") )
			{
				if ( arg->type() == lk::vardata_t::VECTOR
					&& arg->length() == 3 )
				{
					int r = arg->index(0)->as_integer();
					int g = arg->index(1)->as_integer();
					int b = arg->index(2)->as_integer();

					col = wxColour(r,g,b);
				}
				else
					col = wxColour( wxString(arg->as_string().c_str()));
			}

			if (lk::vardata_t *arg = t.lookup("xap"))
			{
				if (arg->as_string() == "top") xap = WPPlotSurface2D::XAXIS_TOP;
			}

			if ( lk::vardata_t *arg = t.lookup("yap"))
			{
				if (arg->as_string() == "right") yap = WPPlotSurface2D::YAXIS_RIGHT;
			}

			if ( lk::vardata_t *arg = t.lookup("series"))
				series = arg->as_string().c_str();

			if ( lk::vardata_t *arg = t.lookup("xlabel"))
				xlab = arg->as_string().c_str();

			if ( lk::vardata_t *arg = t.lookup("ylabel"))
				ylab = arg->as_string().c_str();
		}
		
		int len = cxt.arg(0).length();
		double *x = new double[len];
		double *y = new double[len];

		for (int i=0;i<len;i++)
		{
			x[i] = a0.index(i)->as_number();
			y[i] = a1.index(i)->as_number();
		}

		plot->Add( x, y, len, thick, col, type, xlab, ylab, series, xap, yap );

		delete [] x;
		delete [] y;
	}
}

void fcall_plotopt( lk::invoke_t &cxt )
{
	LK_DOC("plotopt", "Modifies the current plot properties like title, coarse, fine, legend, legendpos, wpos, wsize", "(table:options):void");
	WPPlotSurface2D *plot = PlotWin::Surface();
	if (!plot) return;

	bool mod = false;
	if ( lk::vardata_t *arg = cxt.arg(0).lookup("title") )
	{
		plot->SetTitle( arg->as_string() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("coarse") )
	{
		plot->SetShowCoarseGrid( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("fine") )
	{
		plot->SetShowFineGrid( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("legend") )
	{
		plot->SetShowLegend( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("legendpos") )
	{
		double xper = 90, yper = 10;
		if (arg->type() == lk::vardata_t::VECTOR && arg->length() == 2 )
		{
			xper = arg->index(0)->as_number();
			yper = arg->index(1)->as_number();
			plot->SetLegendXYPercent( PointF(xper,yper) );
			mod = true;
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("window") )
	{
		if (PlotWin::Current()
			&& arg->type() == lk::vardata_t::VECTOR 
			&& arg->length() == 4 )
		{
			int x = arg->index(0)->as_integer();
			int y = arg->index(1)->as_integer();
			int w = arg->index(2)->as_integer();
			int h = arg->index(3)->as_integer();
			if ( x >= 0 && y >= 0 )
				PlotWin::Current()->SetPosition( wxPoint(x, y) );

			if ( w > 0 && h > 0 )
				PlotWin::Current()->SetClientSize( wxSize(w,h) );

		}
	}

	if (mod)
		plot->Refresh();
}

void fcall_plotpng( lk::invoke_t &cxt )
{
	LK_DOC( "plotpng", "Export the current plot as rendered on the screen to a PNG image file.", "(string:file name):boolean" );
	WPPlotSurface2D *plot = PlotWin::Surface();
	if (!plot) return;
	cxt.result().assign( plot->ExportPng( cxt.arg(0).as_string() ) );
}

void fcall_axis( lk::invoke_t &cxt )
{
	LK_DOC("axis", "Modifies axis properties (label, showlabel, min, max, ticklabels) on the current plot.", "(string:axis name 'x1' 'y1' 'x2' 'y2', table:options):void");
	lk_string axname = cxt.arg(0).as_string();
	WPPlotSurface2D *plot = PlotWin::Surface();
	if (!plot) return;
	WPAxis *axis = 0;
	if (axname == "x1") axis = plot->GetXAxis1();
	if (axname == "x2") axis = plot->GetXAxis2();
	if (axname == "y1") axis = plot->GetYAxis1();
	if (axname == "y2") axis = plot->GetYAxis2();
	if (!axis) return;

	if (cxt.arg_count() < 2 || cxt.arg(1).type() != lk::vardata_t::HASH ) return;
	bool mod = false;

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("label") )
	{
		axis->Label = arg->as_string();
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("min") )
	{
		double min, max;
		axis->GetWorld(&min, &max);
		min = arg->as_number();
		axis->SetWorld(min, max);
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("max") )
	{
		double min, max;
		axis->GetWorld(&min,&max);
		max = arg->as_number();
		axis->SetWorld(min,max);
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("ticklabels") )
	{
		axis->HideTickText = !arg->as_boolean();
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("showlabel") )
	{
		axis->HideLabel = !arg->as_boolean();
		mod = true;
	}

	if (mod) plot->Refresh();
}

void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC("out", "Output data to the console.", "(...):none");
	
	for (size_t i=0;i<cxt.arg_count();i++)
		Output( cxt.arg(i).as_string() );
}

void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output data to the console with a newline.", "(...):none");
	
	for (size_t i=0;i<cxt.arg_count();i++)
		Output( cxt.arg(i).as_string()  + "\n" ); 
}

void fcall_in(  lk::invoke_t &cxt )
{
	LK_DOC("in", "Input text from the user.", "(none):string");
	cxt.result().assign( wxGetTextFromUser("Standard Input:") );	
}

void fcall_rand( lk::invoke_t &cxt )
{
	LK_DOC("rand", "Generate a random number between 0 and 1.", "(none):number");
static MTRand rng;
	cxt.result().assign( rng.rand() );
}

void fcall_httpget( lk::invoke_t &cxt )
{
	LK_DOC("http_get", "Issue an HTTP GET request and return the text.", "(string:url):string");
	cxt.result().assign( WebHttpGet( cxt.arg(0).as_string() ) );
}

void fcall_httpdownload( lk::invoke_t &cxt )
{
	LK_DOC("http_download", "Download a file using HTTP", "(string:url, string:local file):boolean");
	cxt.result().assign( WebHttpDownload( cxt.arg(0).as_string(), cxt.arg(1).as_string(), "application/binary", false ) );
}

void fcall_decompress( lk::invoke_t &cxt )
{
	LK_DOC("decompress", "Decompress a local archive file.", "(string:archive, string:target):boolean");
	cxt.result().assign( DecompressFile( cxt.arg(0).as_string(), cxt.arg(1).as_string() ) );
}

lk::fcall_t* retool_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_in,
		fcall_out,
		fcall_outln,
		fcall_httpget,
		fcall_httpdownload,
		fcall_decompress,
		fcall_newplot,
		fcall_plot,
		fcall_plotopt,
		fcall_plotpng,
		fcall_axis,
		fcall_rand,
		0 };
		
	return (lk::fcall_t*)vec;
}

static bool sscvar_to_lkvar( lk::vardata_t &out, var_data *vv)
{
	if (!vv) return false;

	switch( vv->type )
	{
	case SSC_NUMBER:
		out.assign( (double) vv->num );
		break;
	case SSC_STRING:
		out.assign( vv->str.c_str() );
		break;
	case SSC_ARRAY:
		out.empty_vector();
		out.vec()->reserve( (size_t) vv->num.length() );
		for (int i=0;i<vv->num.length();i++)
			out.vec_append( vv->num[i] );
		break;
	case SSC_MATRIX:
		out.empty_vector();
		out.vec()->reserve( vv->num.nrows() );
		for (int i=0;i<vv->num.nrows();i++)
		{
			out.vec()->push_back( lk::vardata_t() );
			out.vec()->at(i).empty_vector();
			out.vec()->at(i).vec()->reserve( vv->num.ncols() );
			for (int j=0;j<vv->num.ncols();j++)
				out.vec()->at(i).vec_append( vv->num.at(i,j) );
		}
		break;
	case SSC_TABLE:
		{
			out.empty_hash();
			const char *key = vv->table.first();
			while (key != 0)
			{
				var_data *x = vv->table.lookup( key );
				lk::vardata_t &xvd = out.hash_item( lk_string(key) );
				sscvar_to_lkvar( xvd, x );
				key = vv->table.next();
			}
		}
		break;
	}

	return true;
}

static bool lkvar_to_sscvar( var_data *vv, lk::vardata_t &val )
{	
	if (!vv) return false;

	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		vv->type = SSC_NUMBER;
		vv->num = (ssc_number_t)val.as_number();
		break;
	case lk::vardata_t::STRING:
		vv->type = SSC_STRING;
		vv->str = std::string((const char*) val.as_string().c_str());
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				vv->type = SSC_ARRAY;
				vv->num.resize( dim1 );
				for ( size_t i=0;i<dim1;i++)
					vv->num[i] = (ssc_number_t)val.index(i)->as_number();
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				vv->type = SSC_MATRIX;
				vv->num.resize( dim1, dim2 );
				for ( size_t i=0;i<dim1;i++)
				{
					for ( size_t j=0;j<dim2;j++ )
					{
						double x = 0;
						if ( val.index(i)->type() == lk::vardata_t::VECTOR
							&& j < val.index(i)->length() )
							x = (ssc_number_t)val.index(i)->index(j)->as_number();

						vv->num.at(i,j) = x;
					}
				}
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			vv->type = SSC_TABLE;
			vv->table.clear();

			lk::varhash_t &hash = *val.hash();
			for ( lk::varhash_t::iterator it = hash.begin();
				it != hash.end();
				++it )
			{
				var_data *item = vv->table.assign( std::string( (const char*)(*it).first.c_str() ), var_data() );
				lkvar_to_sscvar( item, *(*it).second );
			}
		}
		break;
	}

	return true;
}

void fcall_var( lk::invoke_t &cxt )
{
	LK_DOC2( "var", "Sets or gets a variable value in the SSC data set.", 
		"Set a variable value.", "(string:name, variant:value):none", 
		"Get a variable value", "(string:name):variant" );
	
	var_table *vt = app_frame->GetVarTable();

	wxString name = cxt.arg(0).as_string();
	if (cxt.arg_count() == 1)
	{
		ssc_number_t val, *p;
		int i, j;
		var_data *vv = vt->lookup( name.c_str() );
		if (vv)	sscvar_to_lkvar( cxt.result(), vv );
	}
	else if (cxt.arg_count() == 2)
	{
		lk::vardata_t &val = cxt.arg(1).deref();		
		var_data *vv = vt->assign( name.c_str(), var_data() ); // create empty variable
		if (vv)
		{
			lkvar_to_sscvar( vv, val );		
			app_frame->GetDataView()->UpdateView();
		}
	}
}


void fcall_clear( lk::invoke_t &cxt )
{
	LK_DOC( "clear", "Deletes variables from the SSC data set.  If no variable name(s) are specified, all are deleted.", "([string or array:variable name(s) to delete]):none");
	
	if (cxt.arg_count() > 0)
	{
		if (cxt.arg(0).type() == lk::vardata_t::VECTOR)
		{
			size_t len = cxt.arg(0).length();
			for (size_t i=0;i<len;i++)
				app_frame->GetVarTable()->unassign( 
					(const char*) cxt.arg(0).index(i)->as_string().c_str() );
		}
		else
			app_frame->GetVarTable()->unassign( (const char*) cxt.arg(0).as_string().c_str() );
	}
	else
		app_frame->GetVarTable()->clear();

	app_frame->GetDataView()->UpdateView();
}

void fcall_save( lk::invoke_t &cxt )
{
	LK_DOC( "save", "Save the current variable data set to disk in the SSCdev binary data (*.bdat) format.", "(string:filename):boolean" );
	cxt.result().assign( app_frame->WriteBdatToDisk( cxt.arg(0).as_string() ) );
}

void fcall_load( lk::invoke_t &cxt )
{
	LK_DOC( "load", "Load a variable data set from an SSCdev binary data (*.bdat) file.", "(string:filename):boolean" );
	cxt.result().assign( app_frame->LoadBdat( cxt.arg(0).as_string() ) );
}

void fcall_run( lk::invoke_t &cxt )
{
	LK_DOC( "run", 
		"Starts the computation sequence defined.  If no parameter is given, it runs the currently defined list of compute modules. "
		"Passing a comma-separated list of compute module names changes the list.", 
		"([string:compute modules list]):none");

	if (cxt.arg_count() > 0)
	{
		wxArrayString list = Split( cxt.arg(0).as_string(), "," );
		app_frame->ClearCMs();
		for (size_t i=0;i<list.Count();i++)
			app_frame->AddCM( list[i] );
	}

	app_frame->Start();
}

void fcall_tsview( lk::invoke_t &cxt )
{
	LK_DOC( "tsview", "Show a timeseries viewer for the variables given in the comma-separated list, or plots the name-data pairs sent as arguments.  Variable must have 8760 values.", "(string:comma-separated variable name list -or- string:name1, array:values1,...):none");
	
	wxFrame *frm = new wxFrame(app_frame, -1, "Timeseries Viewer", wxDefaultPosition, wxSize(900,600));
	wxDVPlotCtrl *dv = new wxDVPlotCtrl( frm );
	var_table *vt = app_frame->GetVarTable();	
	int iadded = 0;	
	Vector<double> da(8760);

	if (cxt.arg_count() == 1)
	{
		wxArrayString selections = Split(cxt.arg(0).as_string(), ",");

		for (size_t i=0;i<selections.Count();i++)
		{
			var_data *v = vt->lookup( (const char*) selections[i].c_str() );
			if ( v != 0
				&& v->type == SSC_ARRAY
				&& v->num.length() == 8760)
			{
				for (int k=0;k<8760;k++)
					da[k] = v->num[k];

				dv->AddDataSet(  new wxDVArrayDataSet( selections[i], da ) );
				iadded++;
			}
		}
	}
	else
	{
		for (size_t i=1;i<cxt.arg_count();i+=2)
		{
			wxString name = cxt.arg(i-1).as_string();
			wxString units;

			wxString::size_type lpos = name.Find('(');
			wxString::size_type rpos = name.Find(')');
			if (lpos != wxString::npos
				&& rpos != wxString::npos
				&& rpos > lpos)
			{
				units = name.Mid( lpos+1, rpos-lpos-1 );
				name.Truncate(lpos);
				name.Trim();
			}

			if (cxt.arg(i).type() == lk::vardata_t::VECTOR
				&& cxt.arg(i).length() == 8760)
			{
				for (int k=0;k<8760;k++)
					da[k] = cxt.arg(i).index(k)->as_number();
				
				dv->AddDataSet(  new wxDVArrayDataSet( name, units, 1.0, da ) );
				iadded++;
			}
		}
	}
			
	if (iadded == 0)
	{
		frm->Destroy();
		cxt.result().assign( 0.0 );
	}
	else
	{
		dv->SelectDataOnBlankTabs();
		frm->Show();
		cxt.result().assign( (double)iadded );
	}
}

lk::fcall_t* ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_var,
		fcall_clear,
		fcall_run,
		fcall_save,
		fcall_load,
		fcall_tsview,
		0 };
		
	return (lk::fcall_t*)vec;
}


enum { ID_CODEEDITOR = wxID_HIGHEST+1, ID_RUN };

static bool eval_callback( int line, void *cbdata )
{
	wxGetApp().Yield(true);
	return !((EditorWindow*)cbdata)->IsStopFlagSet();
}


EditorWindow::EditorWindow( wxWindow *parent )
		: wxPanel( parent )
{
	m_stopScriptFlag = false;	
	m_scriptRunning = false;
		
	SetBackgroundColour( *wxWHITE );		
		
	m_env = new lk::env_t;
		
	m_env->register_funcs( retool_funcs() );	
	m_env->register_funcs( ssc_funcs(), this );	
	m_env->register_funcs( lk::stdlib_basic() );
	m_env->register_funcs( lk::stdlib_string() );
	m_env->register_funcs( lk::stdlib_math() );
	m_env->register_funcs( lk::stdlib_wxui() );

	StringMap tips;
	std::vector<lk_string> list = m_env->list_funcs();
	wxString funclist;
	for (size_t i=0;i<list.size();i++)
	{
		lk::doc_t d;
		if (lk::doc_t::info( m_env->lookup_func( list[i] ), d ))
		{
			wxString data = ::LimitColumnWidth( d.func_name + d.sig1 + "\n\n" + d.desc1, 90 );
			if (d.has_2) data += ::LimitColumnWidth( "\n\n" + d.func_name + d.sig2 + "\n\n" + d.desc2, 90 );
			if (d.has_3) data += ::LimitColumnWidth( "\n\n" + d.func_name + d.sig3 + "\n\n" + d.desc3, 90 );

			tips[ d.func_name ] = data;			
			funclist += d.func_name + " ";
		}
	}
	
		
	wxBoxSizer *szdoc = new wxBoxSizer( wxVERTICAL );
	szdoc->Add( new wxButton( this, wxID_NEW, "New" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_SAVEAS, "Save as" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_FIND, "Find" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_FORWARD, "Find next" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, ID_RUN, "Run" ), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( m_stopButton = new wxButton( this, wxID_STOP, "Stop" ), 0, wxALL|wxEXPAND, 2 );	
	m_stopButton->SetForegroundColour( *wxRED );
	m_stopButton->Hide();
					
	m_editor = new CodeEdit(this, ID_CODEEDITOR );
	m_editor->ApplyLKStyling();
	m_editor->EnableCallTips(true);
	m_editor->SetCallTipData('(',')', false, tips);
	m_editor->StyleSetForeground( wxSTC_C_WORD2, wxColour(0,128,192) );
	m_editor->SetKeyWords( 1, funclist );

	wxBoxSizer *szedit = new wxBoxSizer( wxVERTICAL );
	szedit->Add( m_editor, 1, wxALL|wxEXPAND, 1 );
	szedit->Add( m_statusLabel = new wxStaticText( this, wxID_ANY, wxEmptyString ), 0, wxALL|wxEXPAND, 1 );

	wxBoxSizer *szmain = new wxBoxSizer( wxHORIZONTAL );
	szmain->Add( szdoc, 0, wxALL|wxEXPAND, 0 );
	szmain->Add( new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL ), 0, wxALL|wxEXPAND, 1 );
	szmain->Add( szedit, 1, wxALL|wxEXPAND, 0 );
	SetSizer( szmain );
		
	m_editor->SetFocus();
}

EditorWindow::~EditorWindow()
{
	delete m_env;
}

void EditorWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_NEW:
		CloseDoc();
		break;
	case wxID_OPEN:
		Open();
		break;
	case wxID_SAVE:
		Save();
		break;
	case wxID_SAVEAS:
		SaveAs();
		break;
	case wxID_UNDO: m_editor->Undo(); break;
	case wxID_REDO: m_editor->Redo(); break;
	case wxID_CUT: m_editor->Cut(); break;
	case wxID_COPY: m_editor->Copy(); break;
	case wxID_PASTE: m_editor->Paste(); break;
	case wxID_SELECTALL: m_editor->SelectAll(); break;
	case wxID_FIND: m_editor->ShowFindDialog(); break;
	case wxID_FORWARD: m_editor->FindNext(); break;
	case wxID_HELP:
		{
			wxFrame *frm = new wxFrame( this, wxID_ANY, "Scripting Reference", wxDefaultPosition, wxSize(800, 700) );
			frm->SetIcon(wxIcon("appicon"));
			wxHtmlWindow *html = new wxHtmlWindow( frm, wxID_ANY, wxDefaultPosition, wxDefaultSize );

			wxString data;
			data += lk::html_doc( "SSC Variables and Control", ssc_funcs() );
			data += lk::html_doc( "I/O and Plotting Functions", retool_funcs() );
			data += lk::html_doc( "Standard Operations", lk::stdlib_basic() );
			data += lk::html_doc( "String Functions", lk::stdlib_string() );
			data += lk::html_doc( "Math Functions", lk::stdlib_math() );
			data += lk::html_doc( "User Interface Functions", lk::stdlib_wxui() );		
			html->SetPage( data );
			frm->Show();
		}
		break;
	case ID_RUN:
		Exec();
		break;
	case wxID_STOP:
		m_stopScriptFlag = true;
		break;
	}
}
	
void EditorWindow::Open()
{
	CloseDoc();
	wxFileDialog dlg(this, "Open", wxEmptyString, wxEmptyString,
							"LK Script Files (*.lk)|*.lk",
							wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_CHANGE_DIR);

	if (dlg.ShowModal() == wxID_OK)
	{									
		if (!Load( dlg.GetPath() ))
			wxMessageBox("Could not load file:\n\n" + dlg.GetPath());
		else
			app_frame->AddRecent( dlg.GetPath() );
	}
}

bool EditorWindow::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool EditorWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save as...",
		wxPathOnly(m_fileName),
		wxFileNameFromPath(m_fileName),
		"LK Script Files (*.lk)|*.lk", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
		return Write( dlg.GetPath() );
	else
		return false;
}

	
bool EditorWindow::CloseDoc()
{
	if (m_scriptRunning)
	{
		if (wxYES==wxMessageBox("A script is running. Cancel it?", "Query", wxYES_NO))
			m_stopScriptFlag = true;

		return false;
	}

	if (m_editor->GetModify())
	{
		Raise();
		wxString id = m_fileName.IsEmpty() ? "untitled" : m_fileName;
		int result = wxMessageBox("Script modified. Save it?\n\n" + id, "Query", wxYES_NO|wxCANCEL);
		if ( result == wxCANCEL 
			|| (result == wxYES && !Save()) )
			return false;
	}

	m_editor->SetText( wxEmptyString );
	m_editor->EmptyUndoBuffer();
	m_editor->SetSavePoint();
	m_fileName.Clear();
	m_statusLabel->SetLabel( m_fileName );
	return true;
}

bool EditorWindow::Write( const wxString &file )
{
	wxBusyInfo info( "Saving script file...");
	wxMilliSleep( 100 );

	if ( ((wxStyledTextCtrl*)m_editor)->SaveFile( file ) )
	{
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
	else return false;
}

bool EditorWindow::Load( const wxString &file )
{
	FILE *fp = fopen(file.c_str(), "r");
	if ( fp )
	{
		wxString str;
		char buf[1024];
		while(fgets( buf, 1023, fp ) != 0)
			str += buf;

		fclose(fp);
		m_editor->SetText(str);
		m_editor->EmptyUndoBuffer();
		m_editor->SetSavePoint();
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
	else return false;
}
	
	
void EditorWindow::Exec()
{
	if (m_scriptRunning)
	{
		wxMessageBox("A script is already running.");
		return;
	}
		
	m_env->clear_objs();
	m_env->clear_vars();

	m_scriptRunning = true;
	m_stopScriptFlag = false;

	m_stopButton->Show();

	Layout();

	wxString script = m_editor->GetText();
	if (!m_fileName.IsEmpty())
	{
		wxString fn = m_fileName + "~";
		FILE *fp = fopen( (const char*)fn.c_str(), "w" );
		if (fp)
		{
			for (size_t i=0;i<script.Len();i++)
				fputc( (char)script[i], fp );
			fclose(fp);
		}
	}
		
	ClearOutput();
	Output("Start: " + wxNow()  + "\n");

	lk::input_string p( script );
	lk::parser parse( p );
	
	lk::node_t *tree = parse.script();
				
	wxYield();

	if ( parse.error_count() != 0 
		|| parse.token() != lk::lexer::END)
	{
		Output("parsing did not reach end of input\n");
	}
	else
	{
		m_env->clear_vars();
		m_env->clear_objs();

		lk::vardata_t result;
		unsigned int ctl_id = lk::CTL_NONE;
		wxStopWatch sw;
		std::vector<lk_string> errors;
		if ( lk::eval( tree, m_env, errors, result, 0, ctl_id, eval_callback, this ) )
		{
			long time = sw.Time();
			Output("elapsed time: %ld msec\n", time);

			/*
			lk_string key;
			lk::vardata_t *value;
			bool has_more = env.first( key, value );
			while( has_more )
			{
				applog("env{%s}=%s\n", key, value->as_string().c_str());
				has_more = env.next( key, value );
			}
			*/

		}
		else
		{
			Output("eval fail\n");
			for (size_t i=0;i<errors.size();i++)
				Output( wxString(errors[i].c_str()) + "\n");

		}
	}
			
	int i=0;
	while ( i < parse.error_count() )
		Output( parse.error(i++) );

	if (tree) delete tree;
		
	m_stopButton->Hide();
		
	Layout();
		
	m_env->clear_objs();
	m_env->clear_vars();

	m_scriptRunning = false;
	m_stopScriptFlag = false;

}

BEGIN_EVENT_TABLE( EditorWindow, wxPanel )
	EVT_BUTTON( wxID_NEW, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_OPEN, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_SAVE, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_SAVEAS, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, EditorWindow::OnCommand )

	EVT_BUTTON( wxID_FIND, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_FORWARD, EditorWindow::OnCommand )

	
	EVT_BUTTON( ID_RUN, EditorWindow::OnCommand )

	EVT_BUTTON( wxID_STOP, EditorWindow::OnCommand )
	EVT_BUTTON( ID_RUN, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, EditorWindow::OnCommand )

END_EVENT_TABLE()


