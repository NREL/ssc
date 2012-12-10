#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/imaglist.h>

#include <wx/config.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/accel.h>
#include <wx/image.h>
#include <wx/fs_zip.h>
#include <wx/html/htmlwin.h>
#include <wx/snglinst.h>
#include <wx/progdlg.h>
#include <wx/busyinfo.h>
#include <wx/dir.h>
#include <wx/stdpaths.h>
#include <wx/generic/helpext.h>
#include <wx/clipbrd.h>
#include <wx/splitter.h>
#include <wx/snglinst.h>
#include <wx/statline.h>
#include <wx/filepicker.h>
#include <wx/grid.h>
#include <wx/notebook.h>

#include "sscdev.h"
#include "dataview.h"
#include "cmform.h"
#include "scripting.h"


/* exported application global variables */

int SC_major_ver = 2;
int SC_minor_ver = 0;
int SC_micro_ver = 1;

SCFrame *app_frame = NULL;
wxConfig *app_config = NULL;

void applog(const wxString &s)
{
	if (app_frame) app_frame->Log(s);
}

/* ************************************************************
   ************ SC Application (set up handlers/config) ******
   ************************************************************ */

IMPLEMENT_APP(SCApp)

bool SCApp::OnInit()
{
	SetAppName( "SSCdev" );
	
	// set the current working directory to locate .pdb on crash
	if ( argc > 0 )
		wxSetWorkingDirectory( wxPathOnly(argv[0]) );

	app_config = new wxConfig( "sscdev", "WXAPPS" );
	
	wxInitAllImageHandlers();

    wxFileSystem::AddHandler(new wxZipFSHandler);

	app_frame = new SCFrame;
	SetTopWindow(app_frame);

	if ( argc > 1 )
		app_frame->LoadBdat( argv[1] );

	bool first_load = true;
	wxString fl_key = wxString::Format("FirstLoad_%d",
		SC_major_ver*10000
		+SC_minor_ver*100
		+SC_micro_ver );

	app_config->Read(fl_key, &first_load, true);
	if (first_load)
	{
		// register the first load
		app_config->Write(fl_key, false);

		// on first load, maximize, and show help 'Getting Started'
		app_frame->SetPosition(wxPoint(10,10));
		app_frame->SetClientSize(700, 600);
	}

	return true;
}

int SCApp::OnExit()
{	
	if (app_config)
		delete app_config;
			
	return 0;
}

#include "splash.cpng"
static char *SC_about_text = "SSCdev - System Simulator Core\n\n"
"Developer interface for simulation, component development, validation, and basic visualization of SSC computation modules.";

class AboutDialog : public wxDialog
{
public:
	AboutDialog( wxWindow *parent )
		: wxDialog( parent, wxID_ANY, "About SSCdev", wxDefaultPosition, wxDefaultSize, wxBORDER_SIMPLE )
	{
		SetBackgroundColour( *wxWHITE );
		wxString version_str = wxString::Format("Version %d.%d.%d", SC_major_ver, SC_minor_ver, SC_micro_ver );
				
		wxTextCtrl *text = new wxTextCtrl(this, wxID_ANY, SC_about_text, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
		text->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "consolas"));
		text->SetForegroundColour( wxColour(90,90,90) );

		wxBoxSizer *sz_main = new wxBoxSizer( wxVERTICAL );
		
		sz_main->Add( new wxStaticBitmap( this, wxID_ANY, wxBITMAP_PNG_FROM_DATA(splash) ),
			0, wxALL|wxEXPAND, 4 );

		sz_main->Add( text, 0, wxALL|wxEXPAND, 4 );

		sz_main->Add( new wxStaticText( this, wxID_ANY, version_str ), 0, wxALL|wxEXPAND, 4 );
		sz_main->Add( CreateButtonSizer( wxOK ), 0, wxALL|wxEXPAND, 4 );

		SetSizer( sz_main );
		Fit();
	}
};

enum{   ID_START, ID_STOP, ID_SHOW_STATS,
		ID_LOAD_UNLOAD_DLL,
		ID_DLL_PATH,
		ID_CHOOSE_DLL,
		ID_OUTPUT, ID_MODULES,
		ID_ADD_VARIABLE,
		ID_RUN_SCRIPT,
		ID_LOAD_BDAT, ID_SAVE_BDAT,
					
		// up to 100 recent items can be accommodated
		ID_RECENT = 500,
		ID_RECENT_LAST = 600
};

BEGIN_EVENT_TABLE(SCFrame, wxFrame)
	EVT_TOOL( ID_ADD_VARIABLE,             SCFrame::OnCommand )

	EVT_TOOL( wxID_OPEN,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_SAVE,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_FIND,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_FORWARD,                 SCFrame::OnCommand )
	EVT_TOOL( ID_MODULES,            SCFrame::OnCommand )
	
	EVT_MENU(ID_RUN_SCRIPT, SCFrame::OnCommand)
	EVT_MENU(ID_LOAD_BDAT, SCFrame::OnCommand)
	EVT_MENU(ID_SAVE_BDAT, SCFrame::OnCommand)

	EVT_TOOL( wxID_EXIT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_ABOUT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_HELP,                  SCFrame::OnCommand )

	EVT_TOOL( ID_START, SCFrame::OnCommand )
	EVT_TOOL( ID_SHOW_STATS,              SCFrame::OnCommand )
	EVT_TOOL( ID_LOAD_UNLOAD_DLL,              SCFrame::OnCommand )
	EVT_TEXT_ENTER( ID_DLL_PATH,                SCFrame::OnCommand )
	EVT_MENU( ID_CHOOSE_DLL,            SCFrame::OnCommand )
	
	EVT_CLOSE( SCFrame::OnCloseFrame )
	
	// For recent file menu
	EVT_MENU_RANGE( ID_RECENT, ID_RECENT+MAX_RECENT, SCFrame::OnRecent)
	
END_EVENT_TABLE()

SCFrame::SCFrame()
   : wxFrame(NULL, wxID_ANY, "SSCdev", wxDefaultPosition, wxSize(800,600)),
   m_recentCount(0)
{
	m_varTable = new var_table;
	
	SetIcon( wxIcon("appicon") );
	SetBackgroundColour( *wxWHITE );
	wxStatusBar *sb = CreateStatusBar(2);
	int widths[] = {-1, -3};
	sb->SetStatusWidths( 2, widths );
		
	wxSplitterWindow *split_win = new wxSplitterWindow( this, wxID_ANY,
		wxPoint(0,0), wxSize(800,700), wxSP_LIVE_UPDATE|wxBORDER_NONE );

	m_notebook = new wxNotebook( split_win, wxID_ANY );

	m_dataView = new DataView(m_notebook);
	m_dataView->SetDataObject( m_varTable );

	m_scriptWindow = new EditorWindow(m_notebook);

	m_notebook->AddPage( m_dataView, "Variable Viewer", true );
	m_notebook->AddPage( m_scriptWindow, "Scripting", false );

	m_txtOutput = new wxTextCtrl(split_win, ID_OUTPUT, wxEmptyString, wxDefaultPosition, wxDefaultSize,
		wxTE_READONLY | wxTE_MULTILINE | wxHSCROLL | wxTE_DONTWRAP);
	m_txtOutput->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "consolas") );
	m_txtOutput->SetForegroundColour( *wxBLUE );
	
	split_win->SplitHorizontally( m_notebook, m_txtOutput, -180 );
	split_win->SetSashGravity( 1 );

	wxBoxSizer *sz_main = new wxBoxSizer(wxVERTICAL);
	sz_main->Add( split_win, 1, wxALL|wxEXPAND, 0 );
	SetSizer(sz_main );

	m_recentMenu = new wxMenu;	
	long ct = 0;
	if (app_config->Read("RecentCount", &ct))
		m_recentCount = (int)ct;

	if (m_recentCount > MAX_RECENT)
		m_recentCount = MAX_RECENT;

	for (int i=0;i<m_recentCount;i++)
	{
		wxString fn;
		if (app_config->Read(wxString::Format("RecentFile_%d", i), &fn))
		{
			fn.Replace("\\","/");
			m_recentFiles[i] = fn;
		}
	}

	app_config->Read("CurrentDirectory", &m_currentAppDir);

	wxString dll_path;
	app_config->Read("DllPath", &dll_path);
	if (wxFileExists(dll_path))
	{
		m_dllPath = dll_path;
		sscdll_load( dll_path.c_str() );
		m_lastLoadTime = wxNow();
	}

	UpdateRecentMenu();	

	m_fileMenu = new wxMenu;
	m_fileMenu->Append( wxID_NEW, "New\tCtrl-N" );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_OPEN, "Open\tCtrl-O" );
	m_fileMenu->Append( wxID_SAVE, "Save\tCtrl-S" );
	m_fileMenu->Append( wxID_SAVEAS, "Save As" );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( ID_LOAD_BDAT, "Load binary data file..\tF7" );
	m_fileMenu->Append( ID_SAVE_BDAT, "Save binary data file...\tF8" );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( ID_START, "Start simulation...\tF5" );
	m_fileMenu->Append( ID_RUN_SCRIPT, "Run script...\tF6");
	m_fileMenu->Append( ID_MODULES, "Modules...\tF2" );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( ID_LOAD_UNLOAD_DLL, "Load/unload SSC library\tF4");
	m_fileMenu->Append( ID_CHOOSE_DLL, "Choose SSC library...\tF9");
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_ANY, "Recent files", m_recentMenu );
	m_fileMenu->AppendSeparator();
	m_fileMenu->Append( wxID_EXIT, "Exit" );

	m_helpMenu = new wxMenu;
	m_helpMenu->Append( wxID_ABOUT, "About" );

	wxMenuBar *mb = new wxMenuBar;
	mb->Append( m_fileMenu, "File" );
	mb->Append( m_helpMenu, "Help" );
	SetMenuBar( mb );


	Show();
	SetClientSize( 750, 600);	
	
	// restore window position
	bool b_maximize = false;
	int f_x,f_y,f_width,f_height;
	app_config->Read("FrameX", &f_x, -1);
	app_config->Read("FrameY", &f_y, -1);
	app_config->Read("FrameWidth", &f_width, -1);
	app_config->Read("FrameHeight", &f_height, -1);
	app_config->Read("FrameMaximized", &b_maximize, false);

	if (b_maximize)
	{
		this->Maximize();
	}
	else
	{
		if (f_width > 100 && f_height > 100)
			this->SetClientSize(f_width, f_height);

		if (f_x > 0 && f_y > 0)
			this->SetPosition(wxPoint(f_x,f_y));
	}

	UpdateUI();
	
}

SCFrame::~SCFrame()
{
	delete m_varTable;
}

void SCFrame::UpdateUI()
{
	wxString status;

	if (sscdll_isloaded())
	{
		int ver = 0;
		char * build = "no info";
		try {
			ver = ssc_version();
			build = const_cast<char*>( ssc_build_info() );
		} catch (sscdll_error &e) {
			status = wxString(e.text.c_str()) + " ";
			ver = -999;
		}
		status += m_dllPath + " ( " + m_lastLoadTime + " ) Version " + wxString::Format("%d [%s]", ver, build);
	}
	else
		status = "SSC dynamic library not loaded.";

	SetStatusText( status, 1 );
}
	
void SCFrame::UpdateRecentMenu()
{
	int i;
	for (i=0;i<MAX_RECENT;i++)
	{
		if (m_recentMenu->FindItem(ID_RECENT+i) != NULL)
			m_recentMenu->Destroy(ID_RECENT+i);
	}

	for (i=0;i<m_recentCount;i++)
	{
		wxString name;
		name.Printf("%d  %s", i+1, m_recentFiles[i].c_str());
		m_recentMenu->Append(ID_RECENT+i, name);
	}

}

void SCFrame::OnRecent(wxCommandEvent &evt)
{
	int id = evt.GetId() - ID_RECENT;
	if (id < 0 || id >= MAX_RECENT)
		return;
	
	m_lastFile = m_recentFiles[id];
	if (m_lastFile != "")
	{
		if (m_lastFile.Right(4) == "bdat")
			LoadBdat(m_lastFile);
		else
		{
			m_notebook->SetSelection(1);
			if (m_scriptWindow->CloseDoc())
				m_scriptWindow->Load( m_lastFile );
		}

	}
}

wxArrayString SCFrame::GetRecentFiles()
{
	wxArrayString list;
	for (int i=0;i<m_recentCount;i++)
		list.Add( m_recentFiles[i] );
	return list;
}

void SCFrame::AddRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<m_recentCount;i++)
	{
		if (norm_fn == m_recentFiles[i])
		{
			index = i;
			break;
		}
	}

	if (index >= 0)
	{
		// bring this file to the front of the
		// recent file list

		for (i=index;i>0;i--)
			m_recentFiles[i] = m_recentFiles[i-1];
	}
	else // not found in recent list
	{
		// add this to the front of the recent list
		// and increment the recent count if its 
		// less than MAX_RECENT

		for (i=MAX_RECENT-1;i>0;i--)
			m_recentFiles[i] = m_recentFiles[i-1];

		if (m_recentCount < MAX_RECENT)
			m_recentCount++;
	}
	
	m_recentFiles[0] = norm_fn;
	UpdateRecentMenu();
}

void SCFrame::RemoveRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<m_recentCount;i++)
	{
		if (norm_fn == m_recentFiles[i])
		{
			index = i;
			break;
		}
	}

	if (index >= 0)
	{
		for (i=index;i<MAX_RECENT-1;i++)
			m_recentFiles[i] = m_recentFiles[i+1];

		m_recentCount--;
		UpdateRecentMenu();
	}
}


void SCFrame::OnCloseFrame( wxCloseEvent &evt )
{
	if (evt.CanVeto() && !CloseDocument())
	{
		evt.Veto();
		return;
	}	

	/* save window position */
	bool b_maximize = this->IsMaximized();
	int f_x,f_y,f_width,f_height;

	this->GetPosition(&f_x,&f_y);
	this->GetClientSize(&f_width, &f_height);
	
	app_config->Write("FrameX", f_x);
	app_config->Write("FrameY", f_y);
	app_config->Write("FrameWidth", f_width);
	app_config->Write("FrameHeight", f_height);
	app_config->Write("FrameMaximized", b_maximize);

	long ct = (long)m_recentCount;
	app_config->Write("RecentCount", ct);
	for (int i=0;i<m_recentCount;i++)
	{
		wxString key;
		key.Printf("RecentFile_%d", i);
		app_config->Write(key, m_recentFiles[i]);
	}
	app_config->Write("CurrentDirectory", m_currentAppDir);
	app_config->Write("DllPath", m_dllPath );

	sscdll_unload(); // make sure dll is unloaded;

	Destroy();
}

void SCFrame::SaveBdat()
{
	wxFileDialog dlg(this, "Save SSCdev State", wxPathOnly(m_lastFile),
		m_lastFile, "Binary Data File (*.bdat)|*.bdat", wxFD_SAVE);
	
	int ret = dlg.ShowModal();
	m_lastFile = dlg.GetPath();

	if (ret!=wxID_OK) return;

	if(!WriteBdatToDisk(m_lastFile))
		wxMessageBox("Error writing:\n\n" + m_lastFile );
	else
		AddRecent( m_lastFile );
}

bool SCFrame::CloseDocument()
{
	return (m_scriptWindow->CloseDoc());
}

void SCFrame::LoadUnloadLibrary()
{	
	if (!sscdll_isloaded())
	{
		m_lastLoadTime = wxNow();

		sscdll_load( m_dllPath.c_str() );

		m_currentAppDir = wxPathOnly(m_dllPath);
	}
	else
		sscdll_unload();

	UpdateUI();
}

void SCFrame::ChooseDynamicLibrary()
{
		
	wxFileDialog fd(this, "Choose SSC dynamic library", m_currentAppDir, "ssc32.dll", 
#ifdef __WXMSW__
		"DLL Files (*.dll)|*.dll"
#endif
#ifdef __WXMAC__
		"DYLIB Files *.dylib|*.dylib"
#endif
		, wxFD_OPEN);
	if (fd.ShowModal() != wxID_OK) return;
	wxString file = fd.GetPath();
	m_dllPath = file;
	m_currentAppDir = wxPathOnly(file);
	sscdll_load(file.c_str());
	m_lastLoadTime = wxNow();
	UpdateUI();	
}

void SCFrame::OnCommand(wxCommandEvent &evt)
{	
	switch(evt.GetId())
	{
	case ID_ADD_VARIABLE:
		m_dataView->AddVariable();
		break;
	case ID_START:		
		m_txtOutput->Clear();
		Start();
		break;
	case wxID_NEW:
		m_notebook->SetSelection( 1 );
		m_scriptWindow->CloseDoc();
		break;
	case wxID_OPEN:
		m_notebook->SetSelection( 1 );
		m_scriptWindow->Open();
		break;
	case wxID_SAVE:
		m_notebook->SetSelection( 1 );
		m_scriptWindow->Save();
		break;
	case wxID_SAVEAS:		
		m_notebook->SetSelection( 1 );
		m_scriptWindow->SaveAs();
		break;
	case ID_RUN_SCRIPT:
		m_scriptWindow->Exec();
		break;
	case ID_LOAD_BDAT:
		LoadBdat();
		break;
	case ID_SAVE_BDAT:
		SaveBdat();
		break;
	case wxID_FIND:
		break;
	case wxID_FORWARD:
		break;
	case wxID_EXIT:
		this->Close();
		break;
	case wxID_ABOUT:
		{		
			AboutDialog dlg(this);
			dlg.CenterOnParent();
			dlg.ShowModal();	
		}
		break;
	case ID_LOAD_UNLOAD_DLL:
		LoadUnloadLibrary();
		break;
	case ID_CHOOSE_DLL:
		ChooseDynamicLibrary();
		break;
	case ID_SHOW_STATS:
		m_dataView->ShowStats();
		break;
	case ID_DLL_PATH:
		{
			m_currentAppDir = wxPathOnly( m_dllPath );
			sscdll_load( m_dllPath.c_str());
			m_lastLoadTime = wxNow();
			UpdateUI();
		}
		break;
	case ID_MODULES:
		{
			CMForm dlg( this );
			dlg.CentreOnParent();
			dlg.SetCMList( m_cmList );
			if (dlg.ShowModal()==wxID_OK)
				m_cmList = dlg.GetCMList();

			UpdateUI();
		}
		break;
	}
}

void SCFrame::ClearCMs()
{
	m_cmList.clear();
	UpdateUI();
}

bool SCFrame::AddCM( const wxString &name )
{
	wxArrayString list = CMForm::GetAvailableCMs();
	if (list.Index( name ) != wxNOT_FOUND)
	{
		m_cmList.Add( name );
		UpdateUI();
		return true;
	}
	else
		return false;
}

void SCFrame::WriteVarTable( wxDataOutputStream &o, var_table &vt )
{
	o.Write16( 0xae ); // start identifier, versioner
	o.Write32( vt.size() );
	const char *key = vt.first();
	while (key != 0)
	{
		o.WriteString( key );
		var_data *v = vt.lookup( key );
		o.Write8( v->type );
		switch( v->type )
		{
		case SSC_STRING:
			o.WriteString( wxString( (const char*)v->str.c_str() ) ); break;
		case SSC_NUMBER:
			o.WriteDouble( v->num ); break;
		case SSC_ARRAY:
			o.Write32( v->num.length() );
			for (size_t i=0;i<v->num.length(); i++)
				o.WriteDouble( v->num[i] );
			break;
		case SSC_MATRIX:
			o.Write32( v->num.nrows() );
			o.Write32( v->num.ncols() );
			for (size_t r=0;r<v->num.nrows();r++)
				for (size_t c=0;c<v->num.ncols();c++)
					o.WriteDouble( v->num.at(r,c) );
			break;
		case SSC_TABLE:
			WriteVarTable(o, v->table );
			break;
		}

		key = vt.next();
	}
	o.Write16( 0xae ); // end identifier
}

bool SCFrame::ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first )
{
	if (clear_first)
		vt.clear();

	int code = o.Read16();
	int size = o.Read32();
	size_t len, nrows, ncols;
	for (int nn=0;nn<size;nn++)
	{
		var_data vv;

		wxString key = o.ReadString();
		vv.type = (unsigned char) o.Read8();
		switch( vv.type )
		{
		case SSC_STRING:
			vv.str = (const char*)o.ReadString().c_str(); break;
		case SSC_NUMBER:
			vv.num = (ssc_number_t)o.ReadDouble(); break;
		case SSC_ARRAY:
			len = (size_t)o.Read32();
			vv.num.resize( len );
			for (size_t i=0;i<len;i++)
				vv.num[i] = (ssc_number_t) o.ReadDouble();
			break;
		case SSC_MATRIX:
			nrows = (size_t)o.Read32();
			ncols = (size_t)o.Read32();
			vv.num.resize(nrows,ncols);
			for (size_t r=0;r<nrows;r++)
				for (size_t c=0;c<ncols;c++)
					vv.num.at(r,c) = (ssc_number_t)o.ReadDouble();
			break;
		case SSC_TABLE:
			if (!ReadVarTable( o, vv.table, true ))
				return false;
			break;
		}

		vt.assign( key.ToStdString(), vv );
	}

	return (o.Read16() == code);
}

bool SCFrame::LoadBdat( wxString fn )
{
	if (fn.IsEmpty())
	{
	
		wxFileDialog dlg(this, "Load SSCdev State",
			wxPathOnly(m_lastFile),
			m_lastFile,
			"Binary Data File (*.bdat)|*.bdat",
			wxFD_OPEN);

		if (dlg.ShowModal() == wxID_OK)
		{
			m_lastFile = dlg.GetPath();
			fn = m_lastFile;
		}
		else
			return false;
	}

	wxBusyInfo busy("Loading: " + fn);

	wxFileInputStream fp( fn );
	if (!fp.Ok()) return false;
	wxDataInputStream in( fp );
		
	m_cmList.clear();
	m_varTable->clear();
	UpdateUI();
	m_dataView->UpdateView();

	int code = in.Read16(); // start header code, versioner


	int n_cmmods = in.Read32();
	for (int i=0;i<n_cmmods;i++)
		m_cmList.Add( in.ReadString() );

	wxArrayString sel_vars;
	std::vector<int> cwl;

	int nn = in.Read32();
	for (int i=0;i<nn;i++)
		sel_vars.Add( in.ReadString() );

	nn = in.Read32();
	for (int i=0;i<nn;i++)
		cwl.push_back( in.Read32() );

	bool vtok = ReadVarTable( in, *m_varTable, true );

	m_dataView->UpdateView();	
	m_dataView->SetSelections( sel_vars );
	m_dataView->UpdateView();
	m_dataView->SetColumnWidths( cwl );

	UpdateUI();

	AddRecent(fn);

	return vtok && in.Read16() == code;	
}

bool SCFrame::WriteBdatToDisk(const wxString &fn)
{
	wxBusyInfo busy("Writing: " + fn);

	wxFileOutputStream fp( fn );
	if (!fp.Ok()) return false;
	wxDataOutputStream o( fp );
	o.Write16( 0xe3 );

	o.Write32( m_cmList.Count() );
	for (int i=0;i<m_cmList.Count();i++)
		o.WriteString( m_cmList[i] );

	wxArrayString selvars = m_dataView->GetSelections();
	o.Write32( selvars.Count() );
	for (size_t i=0;i<selvars.Count(); i++)
		o.WriteString( selvars[i] );

	std::vector<int> cwl = m_dataView->GetColumnWidths();
	o.Write32( cwl.size() );
	for (size_t i=0;i<cwl.size();i++)
		o.Write32( cwl[i] );

	WriteVarTable( o, *m_varTable );

	o.Write16( 0xe3 );

	UpdateUI();

	return true;
}

void SCFrame::Log(const wxString &text, bool wnl)
{
	if (wnl) m_txtOutput->AppendText(text + "\n");
	else m_txtOutput->AppendText(text);
}

void SCFrame::ClearLog()
{
	m_txtOutput->Clear();
}

class default_sync_proc : public util::sync_piped_process
{
private:
	ssc_handler_t m_handler;
public:
	default_sync_proc( ssc_handler_t ph ) : m_handler(ph) {  }

	virtual void on_stdout(const std::string &line_text)
	{
		::ssc_module_extproc_output( m_handler, line_text.c_str() );
	}
};

ssc_bool_t my_handler( ssc_module_t p_mod, ssc_handler_t p_handler, int action, 
	float f0, float f1, const char *s0, const char *s1, void *user_data )
{
	wxProgressDialog *dlg = (wxProgressDialog*) user_data;
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

		app_frame->Log(msg);
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		dlg->Update( (int) f0, s0 );
		wxGetApp().Yield(true);
		return 1; // return 0 to abort simulation as needed.
	}
	else if (action == SSC_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO**
		default_sync_proc exe( p_handler );
		return exe.spawn( s0, s1 ) == 0;
	}
	else
		return 0;
}

void SCFrame::Copy( ssc_data_t p_data, var_table *vt, bool clear_first)
{
	if (clear_first)
		::ssc_data_clear( p_data );

	const char *name = vt->first();
	while( name )
	{
		var_data *v = vt->lookup( name );

		if (v)
		{
			switch(v->type)
			{
			case SSC_STRING:
				::ssc_data_set_string( p_data, name, v->str.c_str() );
				break;
			case SSC_NUMBER:
				::ssc_data_set_number( p_data, name, (ssc_number_t)v->num );
				break;
			case SSC_ARRAY:
				::ssc_data_set_array( p_data, name, v->num.data(), v->num.length() );
				break;
			case SSC_MATRIX:
				::ssc_data_set_matrix( p_data, name, v->num.data(), v->num.nrows(), v->num.ncols() );
				break;
			}
		}

		name = vt->next();
	}
}

void SCFrame::Copy( var_table *vt,  ssc_data_t p_data, bool clear_first )
{	
	if (clear_first) vt->clear();

	const char *name = ::ssc_data_first( p_data );
	while (name)
	{
		int type = ::ssc_data_query( p_data, name );
		switch( type )
		{
		case SSC_STRING:
			{
				const char *s = ::ssc_data_get_string( p_data, name );
				if (s) vt->assign( name, var_data(  std::string(s) ) );
			}
			break;
		case SSC_NUMBER:
			{
				ssc_number_t val = 0.0;
				if ( ::ssc_data_get_number( p_data, name, &val ) )
					vt->assign( name, var_data( val ) );
			}
			break;
		case SSC_ARRAY:
			{
				int len = 0;
				const ssc_number_t *pvals = ::ssc_data_get_array( p_data, name, &len );
				if (pvals)
					vt->assign( name, var_data( pvals, len ) );
			}
			break;
		case SSC_MATRIX:
			{
				int nrows = 0, ncols = 0;
				const ssc_number_t *pmat = ::ssc_data_get_matrix( p_data, name, &nrows, &ncols );
				if (pmat)
					vt->assign( name, var_data( pmat, nrows, ncols ) );
			}
			break;
		}

		name = ::ssc_data_next( p_data );
	}
}

void SCFrame::Start()
{
	wxProgressDialog dlg("Simulation", "ready");
	dlg.Show();
	wxGetApp().Yield(true);
	
	try {

		ssc_data_t p_data = ::ssc_data_create();

		Copy( p_data, m_varTable, true );
		
		for (int i=0;i<m_cmList.Count();i++)
		{
			dlg.SetTitle("Status: " + m_cmList[i] );
			dlg.Update(0);
			wxGetApp().Yield(true);

			ssc_module_t p_mod = ::ssc_module_create( (const char*) m_cmList[i].c_str() );
			
			if (p_mod == 0)
			{
				Log("CREATE_FAIL: " + m_cmList[i] );
				break;
			}
			
			::wxSetWorkingDirectory( wxPathOnly(m_dllPath) );

			wxStopWatch sw;
			sw.Start();			
			if (! ::ssc_module_exec_with_handler( p_mod, p_data,
				my_handler,	&dlg) )
			{
				Log("EXEC_FAIL: "+m_cmList[i]);
				::ssc_module_free( p_mod );
				break;
			}
			//else
			//	Log("EXEC_SUCCESS: " + m_cmList[i] + " (" + wxString::Format("%.3lf", (double)sw.Time()/1000.0) + " sec)");

			::ssc_module_free( p_mod );
		}

		Copy( m_varTable, p_data, false );
		m_dataView->UpdateView();

		::ssc_data_free( p_data );
	 
	} catch(sscdll_error &e) {
		wxMessageBox("Library error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}
	
}
