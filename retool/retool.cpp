#include <wx/wx.h>
#include <wx/statline.h>
#include <wx/html/htmlwin.h>
#include <wx/tokenzr.h>

#include <cml/codeedit.h>
#include <cml/mtrand.h>
#include <cml/util.h>
#include <cml/wpplotdata.h>
#include <cml/wpplotdataarray.h>
#include <cml/wpbarplot.h>
#include <cml/wplineplot.h>
#include <cml/wpscatterplot.h>
#include <cml/wpplotsurface2d.h>
#include <cml/alglib/statistics.h>

#include <lk_absyn.h>
#include <lk_env.h>
#include <lk_eval.h>
#include <lk_lex.h>
#include <lk_parse.h>
#include <lk_stdlib.h>

#include <wxdvplotctrl.h>


#include "sscapi.h"

class OutputWindow;
static OutputWindow *__g_outputWindow = 0;
static bool __g_scriptRunning = false;


class OutputWindow : public wxFrame
{
private:
	wxTextCtrl *m_text;
public:
	OutputWindow() : wxFrame( 0, wxID_ANY, "Output Window", wxDefaultPosition, wxSize(900, 200) )
	{
		SetIcon( wxICON( appicon ) );
		m_text = new wxTextCtrl( this,  wxID_ANY, "Ready\n", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
	}
	virtual ~OutputWindow() { __g_outputWindow = 0; }
	void CloseEventHandler( wxCloseEvent &evt ) { Hide(); }
	void Append( const wxString &text ) { m_text->AppendText( text ); }
	void Clear() { m_text->Clear(); }
	DECLARE_EVENT_TABLE()
};
BEGIN_EVENT_TABLE(OutputWindow, wxFrame)
	EVT_CLOSE( OutputWindow::CloseEventHandler )
END_EVENT_TABLE()

void ShowOutputWindow()
{
	if (__g_outputWindow == 0)
		__g_outputWindow = new OutputWindow();

	if ( !__g_outputWindow->IsShown())
	{
		__g_outputWindow->Show();
		__g_outputWindow->Raise();
	}
}

void HideOutputWindow()
{
	if (__g_outputWindow) __g_outputWindow->Hide();
}

void Output( const wxString &text )
{
	if (__g_outputWindow) __g_outputWindow->Append( text );
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
	if (__g_outputWindow) __g_outputWindow->Clear();
}

void DestroyOutputWindow()
{
	if (__g_outputWindow) __g_outputWindow->Destroy();
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
			((WPLinePlot*)p)->LinePen.SetColour( col );
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
	cxt.result().assign( WebHttpDownload( cxt.arg(0).as_string(), cxt.arg(1).as_string() ) );
}

void fcall_decompress( lk::invoke_t &cxt )
{
	LK_DOC("decompress", "Decompress a local archive file.", "(string:archive, string:target):boolean");
	cxt.result().assign( DecompressFile( cxt.arg(0).as_string(), cxt.arg(1).as_string() ) );
}


void fcall_pearson( lk::invoke_t &cxt )
{
	LK_DOC("pearson", "Calculate the Pearson linear rank correlation coefficient of two arrays.", "(array:x, array:y):real");

	if ( cxt.arg_count() != 1
		|| cxt.arg(0).type() != lk::vardata_t::VECTOR
		|| cxt.arg(1).type() != lk::vardata_t::VECTOR
		|| cxt.arg(0).length() < 2
		|| cxt.arg(1).length() != cxt.arg(0).length() )
	{
		cxt.error( "pearson must be supplied with 2 arrays of the same length" );
		return;
	}
	
	int len = cxt.arg(0).length();

	double *x = new double[len];
	double *y = new double[len];

	for (int i=0;i<len;i++)
	{
		x[i] = cxt.arg(0).index(i)->as_number();
		y[i] = cxt.arg(0).index(i)->as_number();
	}

	alglib::real_1d_array xx, yy;
	xx.setcontent(len,x);
	yy.setcontent(len,y);
	cxt.result().assign( alglib::pearsoncorr2( xx, yy ) );

	delete [] x;
	delete [] y;
}

lk::fcall_t* retool_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_in,
		fcall_out,
		fcall_outln,
		fcall_newplot,
		fcall_plot,
		fcall_plotopt,
		fcall_plotpng,
		fcall_axis,
		fcall_pearson,
		fcall_rand,
		fcall_httpget,
		fcall_httpdownload,
		fcall_decompress,
		0 };
		
	return (lk::fcall_t*)vec;
}

// forward decl.
void fcall_sscvar( lk::invoke_t & );
void fcall_sscclear( lk::invoke_t & );
void fcall_sscrun( lk::invoke_t & );

lk::fcall_t* ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_sscvar,
		fcall_sscclear,
		fcall_sscrun,
		0 };
		
	return (lk::fcall_t*)vec;
}


enum { ID_CODEEDITOR = wxID_HIGHEST+1, ID_RUN, ID_OUTPUT_WINDOW };

static int __ndoc = 0;

//forward
bool eval_callback( int, void* );

class EditorWindow : public wxFrame
{
private:
	static int __s_numEditorWindows;
	lk::env_t *m_env;
	ssc_data_t m_sscData;
	CodeEdit *m_editor;
	wxStaticText *m_statusLabel;
	wxString m_fileName;
	wxButton *m_stopButton;
	wxGauge *m_progressBar;
	wxTextCtrl *m_txtProgress;
	bool m_stopScriptFlag;
public:
	EditorWindow()
		: wxFrame( 0, wxID_ANY, wxString::Format("untitled %d",++__ndoc), wxDefaultPosition, wxSize(800,800) )
	{
		__s_numEditorWindows++;
		m_stopScriptFlag = false;
		SetIcon( wxICON( appicon ) );

		m_sscData = ::ssc_data_create();
		
		SetBackgroundColour( *wxWHITE );
		
		m_progressBar = new wxGauge( this, wxID_ANY, 100, wxDefaultPosition, wxDefaultSize, wxGA_SMOOTH );
		m_txtProgress = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(270, 21), wxTE_READONLY );
		m_txtProgress->SetBackgroundColour(*wxBLACK);
		m_txtProgress->SetForegroundColour(*wxGREEN);

		wxBoxSizer *sztools = new wxBoxSizer( wxHORIZONTAL );
		sztools->Add( m_stopButton = new wxButton( this, wxID_STOP, "Stop" ), 0, wxALL|wxEXPAND, 2 );
		sztools->Add( new wxButton( this, ID_RUN, "Run" ), 0, wxALL|wxEXPAND, 2  );
		sztools->Add( m_progressBar, 0, wxALL|wxEXPAND, 2  );
		sztools->Add( m_txtProgress, 0, wxALL|wxEXPAND, 2  );
		sztools->Add( m_statusLabel = new wxStaticText(this,wxID_ANY,wxEmptyString), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );

		m_stopButton->SetForegroundColour( *wxRED );

		m_stopButton->Hide();
		m_progressBar->Hide();
		m_txtProgress->Hide();
			
		wxMenu *file_menu = new wxMenu;
		file_menu->Append( wxID_NEW, "New\tCtrl-N" );
		file_menu->AppendSeparator();
		file_menu->Append( wxID_OPEN, "Open\tCtrl-O");
		file_menu->Append( wxID_SAVE, "Save\tCtrl-S" );
		file_menu->Append( wxID_SAVEAS, "Save as...");
		file_menu->AppendSeparator();
		file_menu->Append( wxID_CLOSE, "Close\tCtrl-W");
		file_menu->AppendSeparator();
		file_menu->Append( wxID_EXIT, "Exit" );

		wxMenu *edit_menu = new wxMenu;
		edit_menu->Append( wxID_UNDO, "Undo\tCtrl-Z" );
		edit_menu->Append( wxID_REDO, "Redo\tCtrl-Y" );
		edit_menu->AppendSeparator();
		edit_menu->Append( wxID_CUT, "Cut\tCtrl-X" );
		edit_menu->Append( wxID_COPY, "Copy\tCtrl-C" );
		edit_menu->Append( wxID_PASTE, "Paste\tCtrl-V" );
		edit_menu->AppendSeparator();
		edit_menu->Append( wxID_SELECTALL, "Select all\tCtrl-A" );
		edit_menu->AppendSeparator();
		edit_menu->Append( wxID_FIND, "Find...\tCtrl-F" );
		edit_menu->Append( wxID_FORWARD, "Find next\tCtrl-G" );

		wxMenu *act_menu = new wxMenu;
		act_menu->Append( ID_RUN, "Run\tF5" );
		act_menu->AppendSeparator();
		act_menu->Append( ID_OUTPUT_WINDOW, "Show output window\tF6" );
		
		wxMenu *help_menu = new wxMenu;
		help_menu->Append( wxID_HELP, "Script reference\tF1");

		wxMenuBar *menu_bar = new wxMenuBar;
		menu_bar->Append( file_menu, "File" );
		menu_bar->Append( edit_menu, "Edit" );
		menu_bar->Append( act_menu, "Actions" );
		menu_bar->Append( help_menu, "Help" );
		
		SetMenuBar( menu_bar );
		
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

		
		m_editor = new CodeEdit(this, ID_CODEEDITOR );
		m_editor->ApplyLKStyling();
		m_editor->EnableCallTips(true);
		m_editor->SetCallTipData('(',')', false, tips);
		m_editor->StyleSetForeground( wxSTC_C_WORD2, wxColour(0,128,192) );
		m_editor->SetKeyWords( 1, funclist );

		wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
		szmain->Add( sztools, 0, wxALL|wxEXPAND, 0 );
		szmain->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0 );
		szmain->Add( m_editor, 1, wxALL|wxEXPAND, 0 );
		SetSizer( szmain );
		
		m_editor->SetFocus();
	}

	virtual ~EditorWindow()
	{
		delete m_env;
		ssc_data_free( m_sscData );
	}

	ssc_data_t GetSSCData() { return m_sscData; }

	wxString GetFileName() { return m_fileName; }

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case wxID_NEW:
			(new EditorWindow())->Show();
			break;
		case wxID_OPEN:
			{
				wxFileDialog dlg(this, "Open", wxEmptyString, wxEmptyString,
									  "LK Script Files (*.lk)|*.lk",
									  wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_CHANGE_DIR);

				if (dlg.ShowModal() == wxID_OK)
				{
					wxWindowList wl = ::wxTopLevelWindows;
					for (size_t i=0;i<wl.size();i++)
					{
						if (EditorWindow *ew = dynamic_cast<EditorWindow*>( wl[i] ))
						{
							if ( dlg.GetPath() == ew->GetFileName() )
							{
								ew->Raise();
								return;
							}
						}
					}

					EditorWindow *target = this;
					if (m_editor->GetModify() || !m_fileName.IsEmpty())
					{
						target = new EditorWindow;
						target->Show();
					}
					
					if (!target->Load( dlg.GetPath() ))
						wxMessageBox("Could not load file:\n\n" + dlg.GetPath());
				}
			}
			break;
		case wxID_SAVE:
			Save();
			break;
		case wxID_SAVEAS:
			SaveAs();
			break;
		case wxID_CLOSE:
			Close();
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
				data += lk::html_doc( "REtool Functions", retool_funcs() );
				data += lk::html_doc( "SSC Functions", ssc_funcs() );
				data += lk::html_doc( "Basic Operations", lk::stdlib_basic() );
				data += lk::html_doc( "String Functions", lk::stdlib_string() );
				data += lk::html_doc( "Math Functions", lk::stdlib_math() );
				data += lk::html_doc( "User Interface Functions", lk::stdlib_wxui() );		
				html->SetPage( data );
				frm->Show();
			}
			break;
		case wxID_EXIT:
			{
				wxWindowList wl = ::wxTopLevelWindows;
				for (size_t i=0;i<wl.size();i++)
					if ( dynamic_cast<EditorWindow*>( wl[i] ) != 0 
						&& wl[i] != this )
						wl[i]->Close();

				Close();
			}
			break;
		case ID_RUN:
			Exec();
			break;
		case ID_OUTPUT_WINDOW:
			ShowOutputWindow();
			break;
		case wxID_STOP:
			m_stopScriptFlag = true;
			break;
		}
	}

	bool IsStopFlagSet() { return m_stopScriptFlag; }
	
	
	bool Save()
	{
		if ( m_fileName.IsEmpty() )
			return SaveAs();
		else
			return Write( m_fileName );
	}

	bool SaveAs()
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

	
	bool CloseDoc()
	{
		if (__g_scriptRunning)
		{
			if (wxYES==wxMessageBox("A script is running. Cancel it?", "Query", wxYES_NO))
				m_stopScriptFlag = true;

			return false;
		}

		if (m_editor->GetModify())
		{
			Raise();
			wxString id = m_fileName.IsEmpty() ? this->GetTitle() : m_fileName;
			int result = wxMessageBox("Script modified. Save it?\n\n" + id, "Query", wxYES_NO|wxCANCEL);
			if ( result == wxCANCEL 
				|| (result == wxYES && !Save()) )
				return false;
		}

		m_editor->SetText("");
		m_editor->EmptyUndoBuffer();
		m_editor->SetSavePoint();
		m_fileName = "";
		SetTitle( wxString::Format("untitled %d",++__ndoc) );
		return true;
	}

	bool Write( const wxString &file )
	{
		if ( ((wxStyledTextCtrl*)m_editor)->SaveFile( file ) )
		{
			m_fileName = file;
			SetTitle( wxFileNameFromPath(m_fileName) );
			m_statusLabel->SetLabel( m_fileName );
			return true;
		}
		else return false;
	}

	bool Load( const wxString &file )
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
			SetTitle( wxFileNameFromPath(m_fileName) );
			m_statusLabel->SetLabel( m_fileName );
			return true;
		}
		else return false;
	}
	
	
	void Exec()
	{
		if (__g_scriptRunning)
		{
			wxMessageBox("A script is already running.");
			return;
		}

		__g_scriptRunning = true;
		m_stopScriptFlag = false;

		m_stopButton->Show();
		m_progressBar->Show();
		m_txtProgress->Show();

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
		
		ShowOutputWindow();
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
		m_progressBar->Hide();
		m_txtProgress->Hide();
		
		Layout();

		__g_scriptRunning = false;
		m_stopScriptFlag = false;

	}

	void Progress(const wxString &text, float percent)
	{
		m_txtProgress->SetValue(text);
		m_txtProgress->Update();
		m_progressBar->SetValue( (int)percent );
		m_progressBar->Update();
	}


	void CloseEventHandler( wxCloseEvent &evt )
	{	
		if ( !CloseDoc() )
		{
			evt.Veto();
			return;
		}

		Destroy();
		if (--__s_numEditorWindows == 0)
		{
			DestroyOutputWindow();
			wxWindowList wl = ::wxTopLevelWindows;
			for (size_t i=0;i<wl.size();i++)
				if (dynamic_cast<PlotWin*>( wl[i] ) != 0)
					wl[i]->Close();
		}
	}



	DECLARE_EVENT_TABLE()
};
int EditorWindow::__s_numEditorWindows = 0;

BEGIN_EVENT_TABLE( EditorWindow, wxFrame )
	EVT_MENU( wxID_NEW, EditorWindow::OnCommand )
	EVT_MENU( wxID_OPEN, EditorWindow::OnCommand )
	EVT_MENU( wxID_SAVE, EditorWindow::OnCommand )
	EVT_MENU( wxID_SAVEAS, EditorWindow::OnCommand )
	EVT_MENU( wxID_HELP, EditorWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, EditorWindow::OnCommand )
	EVT_MENU( wxID_EXIT, EditorWindow::OnCommand )

	EVT_MENU( wxID_UNDO, EditorWindow::OnCommand )
	EVT_MENU( wxID_REDO, EditorWindow::OnCommand )
	EVT_MENU( wxID_CUT, EditorWindow::OnCommand )
	EVT_MENU( wxID_COPY, EditorWindow::OnCommand )
	EVT_MENU( wxID_PASTE, EditorWindow::OnCommand )
	EVT_MENU( wxID_SELECTALL, EditorWindow::OnCommand )
	EVT_MENU( wxID_FIND, EditorWindow::OnCommand )
	EVT_MENU( wxID_FORWARD, EditorWindow::OnCommand )

	
	EVT_MENU( ID_RUN, EditorWindow::OnCommand )
	EVT_MENU( ID_OUTPUT_WINDOW, EditorWindow::OnCommand )

	EVT_BUTTON( wxID_STOP, EditorWindow::OnCommand )
	EVT_BUTTON( ID_RUN, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, EditorWindow::OnCommand )

	EVT_CLOSE( EditorWindow::CloseEventHandler )
END_EVENT_TABLE()


class REToolApp : public wxApp
{
public:
	virtual bool OnInit()
	{
		::wxInitAllImageHandlers();
		(new EditorWindow())->Show();
		return true;
	}
};

IMPLEMENT_APP( REToolApp );



void fcall_sscvar( lk::invoke_t &cxt )
{
	LK_DOC2( "ssc_var", "Set or get a variable value in the SSC data object.", "Set a variable value.", "(string:name, variant:value):none", "Get a variable value", "(string:name):variant" );
	

	EditorWindow *pWnd = (EditorWindow*) cxt.user_data();
	if (!pWnd) return;

	ssc_data_t pData = pWnd->GetSSCData();

	wxString name = cxt.arg(0).as_string();
	if (cxt.arg_count() == 1)
	{
		ssc_number_t val, *p;
		int i, j, len, rows, cols;
		int type = ::ssc_data_query( pData, name );
		switch( type )
		{
		case SSC_NUMBER:
			ssc_data_get_number( pData, name, &val );
			cxt.result().assign( (double) val );
			break;
		case SSC_STRING:
			if (const char *s =  ssc_data_get_string( pData, name ))
				cxt.result().assign( s );
			break;
		case SSC_ARRAY:
			p = ssc_data_get_array( pData, name, &len );
			cxt.result().empty_vector();
			cxt.result().vec()->reserve( (size_t) len );
			for (i=0;i<len;i++)
				cxt.result().vec_append( p[i] );
			break;
		case SSC_MATRIX:
			p = ssc_data_get_matrix( pData, name, &rows, &cols );
			cxt.result().empty_vector();
			cxt.result().vec()->reserve( rows );
			for ( i=0;i<rows;i++)
			{
				cxt.result().vec()->push_back( lk::vardata_t() );
				cxt.result().vec()->at(i).empty_vector();
				cxt.result().vec()->at(i).vec()->reserve( cols );
				for (j=0;j<cols;j++)
				{
					val = p[ i*cols + j ];
					cxt.result().vec()->at(i).vec_append( val );
				}
			}
			break;			
		}
	}
	else if (cxt.arg_count() == 2)
	{
		lk::vardata_t &val = cxt.arg(1).deref();
		switch (val.type())
		{
		case lk::vardata_t::NUMBER:
			ssc_data_set_number( pData, name, (double) val.as_number() );
			break;
		case lk::vardata_t::STRING:
			ssc_data_set_string( pData, name, (const char*) val.as_string().c_str() );
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
					ssc_number_t *p = new ssc_number_t[ dim1 ];
					for ( size_t i=0;i<dim1;i++)
						p[i] = (ssc_number_t)val.index(i)->as_number();

					ssc_data_set_array( pData, name, p, dim1 );
					delete [] p;
				}
				else if ( dim1 > 0 && dim2 > 0 )
				{
					ssc_number_t *p = new ssc_number_t[ dim1 * dim2 ];
					for ( size_t i=0;i<dim1;i++)
					{
						for ( size_t j=0;j<dim2;j++ )
						{
							double x = 0;
							if ( val.index(i)->type() == lk::vardata_t::VECTOR
								&& j < val.index(i)->length() )
								x = (ssc_number_t)val.index(i)->index(j)->as_number();

							p[ dim2*i +j ] = x;
						}
					}
					ssc_data_set_matrix( pData, name, p, dim1, dim2 );
					delete [] p;
				}
			}
		}
	}
}

void fcall_sscclear( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_clear", "Clear the SSC data object.", "(none):none");
	
	EditorWindow *pWnd = (EditorWindow*) cxt.user_data();
	if (!pWnd) return;

	ssc_data_clear( pWnd->GetSSCData() );
}

ssc_bool_t my_handler( ssc_module_t p_mod, ssc_handler_t p_handler, int action, 
	float f0, float f1, const char *s0, const char *s1, void *user_data )
{
	EditorWindow *ew = (EditorWindow*) user_data;
	if (action == SSC_LOG)
	{
		// print log message to console
		wxString msg;
		switch( (int)f0 )
		{
		case SSC_NOTICE: msg << "Notice: " << s0 << " time " << f1; break;
		case SSC_WARNING: msg << "Warning: " << s0 << " time " << f1; break;
		case SSC_ERROR: msg << "Error: " << s0 << " time " << f1; break;
		default: msg << "Log notice uninterpretable: " << f0 << " time " << f1; break;
		}

		Output(msg);
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		ew->Progress( wxString::Format("(%.2f %%) %s",f0,s0), f0 );
		wxGetApp().Yield(true);
		return 1; // return 0 to abort simulation as needed.
	}
	else if (action == SSC_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO**
		return 0;
	}
	else
		return 0;
}

void fcall_sscrun( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_run", "Run one or more compute modules on the SSC data object.  Returns null on success, or an error message otherwise.", "(string:comma separated list of compute module names):variant" );
	
	EditorWindow *pWnd = (EditorWindow*) cxt.user_data();
	if (!pWnd) return;

	wxArrayString list = wxStringTokenize( cxt.arg(0).as_string(), ", " );
	for (size_t i=0;i<list.Count();i++)
	{
		ssc_module_t p_mod = ::ssc_module_create( (const char*) list[i].c_str() );
			
		if (p_mod == 0)
		{
			Output("create fail: " + list[i] );
			break;
		}

		wxStopWatch sw;
		sw.Start();			
		if (! ::ssc_module_exec_with_handler( p_mod, pWnd->GetSSCData(),
			my_handler,	pWnd) )
		{
			Output("exec fail: "+list[i]);
			::ssc_module_free( p_mod );
			break;
		}

		::ssc_module_free( p_mod );
	}
}

bool eval_callback( int line, void *cbdata )
{
	wxGetApp().Yield(true);
	return !((EditorWindow*)cbdata)->IsStopFlagSet();
}