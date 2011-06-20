#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>

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
#include <wx/aui/aui.h>
#include <wx/splitter.h>
#include <wx/snglinst.h>
#include <wx/statline.h>
#include <wx/filepicker.h>

#include <cml/util.h>
#include <cml/painter.h>
#include <cml/array.h>
#include <cml/afeditctrls.h>
#include <cml/afuiorgctrls.h>
#include <cml/afdialogs.h>


#include <cml/pixmaps/stock_save_24.xpm>
#include <cml/pixmaps/stock_save_as_24.xpm>
#include <cml/pixmaps/stock_open_24.xpm>
#include <cml/pixmaps/stock_text_indent_16.xpm>
#include <cml/pixmaps/stock_preferences_16.xpm>
#include <cml/pixmaps/stock_convert_16.xpm>
#include <cml/pixmaps/stock_convert_24.xpm>
#include <cml/pixmaps/stock_exec_24.xpm>
#include <cml/pixmaps/stock_help_16.xpm>
#include <cml/pixmaps/stock_redo_24.xpm>
#include <cml/pixmaps/stock_undo_rtl_24.xpm>
#include <cml/pixmaps/stock_jump_to_24.xpm>
#include <cml/pixmaps/stock_search_24.xpm>
#include <cml/pixmaps/stock_refresh_24.xpm>
#include <cml/pixmaps/stock_about_24.xpm>
#include <cml/pixmaps/stock_preferences_24.xpm>
#include <cml/pixmaps/stock_trash_24.xpm>
#include <cml/pixmaps/stock_media_play_24.xpm>
#include <cml/pixmaps/stock_media_record_24.xpm>
#include <cml/pixmaps/stock_connect_24.xpm>
#include <cml/pixmaps/stock_disconnect_24.xpm>

#include "sscdev.h"
#include "dataview.h"
#include "cmform.h"
#include "automation.h"
#include "splash.xpm"

/* exported application global variables */

int SC_major_ver = 0;
int SC_minor_ver = 1;
int SC_micro_ver = 1;

SCFrame *app_frame = NULL;
wxArrayString app_args;
wxConfig *app_config = NULL;


void applog(const wxString &s)
{
	if (app_frame) app_frame->Log(s);
}

/* ************************************************************
   ************ SC Application (set up handlers/config) ******
   ************************************************************ */

IMPLEMENT_APP(SCApp)

wxString SCApp::GetInstanceName()
{
	return m_inst_name;
}

SCApp::SCApp()
{
}

bool SCApp::OnInit()
{
	// segmentation faults handled in SCApp::OnFatalException
	// works on MSW to check during development
#ifdef __WXMSW__
	if (!wxIsDebuggerRunning())
		wxHandleFatalExceptions();
#endif

	// generate a unique instance name
	int counter = 1;
	do
	{
		m_inst_name = "sscdev-" + wxGetUserId() + wxString::Format("-inst%d", counter++);
		m_inst_checker = new wxSingleInstanceChecker( m_inst_name );
		if (!m_inst_checker->IsAnotherRunning())
			counter = 0;
		else
			delete m_inst_checker;
	}
	while ( counter > 0 );

	// now we've found a unique instance name
	// and the app's instance checker object has been created
	// we will use the instance name as the temporary work directory
	SetAppName( "SSCdev" );
	
	// accumulate all the command line args
	for (int i=0;i<argc;i++)
		app_args.Add(argv[i]);

	// set the current working directory to locate .pdb on crash
	wxSetWorkingDirectory( wxPathOnly(app_args[0]) );

	app_config = new wxConfig( "sscdev", "WXAPPS" );
	
	/* needed for the html help viewer */
	wxImage::AddHandler(new wxBMPHandler);
    wxImage::AddHandler(new wxPNGHandler);
    wxImage::AddHandler(new wxJPEGHandler);
	wxImage::AddHandler(new wxXPMHandler);

    wxFileSystem::AddHandler(new wxZipFSHandler);

	app_frame = new SCFrame;
	SetTopWindow(app_frame);

	if ((int)app_args.Count() > 1)
		app_frame->Load(app_args[1]);

	bool first_load = true;
	wxString fl_key = Format("FirstLoad_%d",
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

	if (m_inst_checker)
		delete m_inst_checker;
			
	return 0;
}

#ifdef __WXMSW__
/* simple class to get a stack trace */
#include <wx/stackwalk.h>

class TextMessageDialog : public wxDialog
{
private:
	wxTextCtrl *m_text;
public:
	TextMessageDialog(const wxString &text, const wxString &title="Notice") : wxDialog( NULL, -1, title, wxPoint(30,30), wxSize(500,400), wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE )
	{
		m_text = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_READONLY);
		m_text->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );
		m_text->ChangeValue(text);
		wxButton *btnclose = new wxButton(this, wxID_CANCEL, "Close");
		wxBoxSizer *btnsz = new wxBoxSizer(wxHORIZONTAL);
		btnsz->AddStretchSpacer(1);
		btnsz->Add(btnclose, 0, wxALL, 2);
		wxBoxSizer *vert = new wxBoxSizer(wxVERTICAL);
		vert->Add( m_text, 1, wxALL|wxEXPAND, 0 );
		vert->Add( btnsz, 0, wxALL|wxEXPAND, 2 );
		SetSizer(vert);
	}

	void SetText(const wxString &text) { m_text->ChangeValue(text);	}
};

void wxTextMessageDialog(const wxString &text, const wxString &title="Notice")
{
	TextMessageDialog dlg(text);
	dlg.ShowModal();	
}

class StackDump : public wxStackWalker
{
public:
    const wxString& GetStackTrace() const { return m_stackTrace; }
protected:
    virtual void OnStackFrame(const wxStackFrame& frame)
    {
		wxString line = wxString::Format("[%d] ", (int)frame.GetLevel());
        wxString name = frame.GetName();
        line += wxString::Format("%p %s", frame.GetAddress(), name.c_str());
        if ( frame.HasSourceLocation() ) line += " (" + frame.GetFileName() + "):" << frame.GetLine();
		m_stackTrace += line + "\n";
    }
private:
    wxString m_stackTrace;
};

#endif

void SCApp::OnFatalException()
{
	StackDump dump;
	dump.WalkFromException();

	wxString msgtext = TruncateLines( dump.GetStackTrace(), 9, "..." );

	wxString body;	
	body += "SYSTEM INFORMATION:\n\n";
	body += "User Name: " + wxGetUserName() + "\n";
	body += "Home Dir: " + wxGetHomeDir() + "\n";
	body += "Email Address: " + wxGetEmailAddress() + "\n";;
	body += "Full Host Name: " + wxGetFullHostName() + "\n";
	body += "OS: " + wxGetOsDescription() + "\n";
	body += "Little Endian?: " + wxString::Format("%s",wxIsPlatformLittleEndian()?"Yes":"No") + "\n";
	body += "64-bit?: " + wxString::Format("%s",wxIsPlatform64Bit()?"Yes":"No") + "\n";
	body += "Free Memory: " + wxString::Format("%lg kB",wxGetFreeMemory().ToDouble()/1024) + "\n";
	body += "\n";

	body += "\nCRASH TRACE [" + wxNow() + "]\n\n" + dump.GetStackTrace();

	wxTextMessageDialog(body);
}


/* ************************************************************
   ************ SC 'About' Dialog ****************************
   ************************************************************ */

enum { ID_ABOUT_CLOSE, ID_ABOUT_TEXT,ID_ABOUT_CRASH};

BEGIN_EVENT_TABLE(SCAbout, wxDialog)
	EVT_CLOSE( SCAbout::OnCloseFrame )
	EVT_BUTTON( ID_ABOUT_CLOSE,SCAbout::OnClose )
	EVT_BUTTON( ID_ABOUT_CRASH,SCAbout::OnCrash )
END_EVENT_TABLE()

static char *SC_about_text = "SSCdev - System Simulator Core\n\n"
"Developer interface for simulation, component development, validation, and basic visualization of SSC computation modules.";

SCAbout::SCAbout(wxWindow *parent)
: wxDialog(parent, -1, "SC", wxDefaultPosition, wxDefaultSize,wxBORDER_SIMPLE)
{
	wxBitmap pic;
	
	if (!pic.LoadFile( wxPathOnly(app_args[0]) + "/splash.bmp", wxBITMAP_TYPE_BMP ))
		pic = wxBitmap(splash_xpm);

	SetBackgroundColour( *wxWHITE );
	mBitmap = new wxStaticBitmap(this, -1, pic);
	mLblVersion = new wxStaticText(this, -1, "");
	mLblVersion->SetBackgroundColour( *wxWHITE );

	wxAcceleratorEntry entries[1];
	entries[0].Set(::wxACCEL_CTRL, WXK_F7, ID_ABOUT_CRASH);
	wxAcceleratorTable acceltab(1,entries);
	SetAcceleratorTable(acceltab);

	mLblVersion->SetLabel( wxString::Format(" Version %d.%d.%d", 
		SC_major_ver, SC_minor_ver, SC_micro_ver ));

	mBtnClose = new wxButton(this, ID_ABOUT_CLOSE, "Close");	

	mText = new wxTextCtrl(this, ID_ABOUT_TEXT, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
	mText->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	mText->SetForegroundColour( wxColour(90,90,90) );

	mText->SetSize(4,pic.GetHeight()+4,pic.GetWidth()-8,126);
	mText->SetValue(SC_about_text);
	mLblVersion->SetSize(3,pic.GetHeight()+134,pic.GetWidth()-135,17);
	mBtnClose->SetSize(pic.GetWidth()-74,pic.GetHeight()+134,70,21);

	
	/* hidden button that can be 'clicked' or 
	activated by the accelerator to cause a crash */
	wxButton *cb = new wxButton(this, ID_ABOUT_CRASH, "SegF");
	cb->SetSize( 10, 10,50,21);
	cb->Show(false);

#ifdef __WXMSW__
	this->SetClientSize( pic.GetWidth(), pic.GetHeight()+130+29 );
#else
	this->SetClientSize( pic.GetWidth()-1, pic.GetHeight()+130+29 );
#endif

	this->CenterOnScreen();
	SetEscapeId(ID_ABOUT_CLOSE);
}

void SCAbout::OnCloseFrame(wxCloseEvent &evt)
{
	EndModal(0);
}

void SCAbout::OnClose(wxCommandEvent &evt)
{
	EndModal(0);
}

void SCAbout::OnCrash(wxCommandEvent &evt)
{
	try {
		__ssc_segfault();
	}catch(sscdll_error e){
		wxMessageBox(e.func + ": " + e.text,"Error",wxICON_ERROR|wxOK);
	}
}


/* ************************************************************
   ************ SC  Parent window class  ******************
   ************************************************************ */

enum{   ID_START, ID_STOP, ID_SHOW_STATS,
		ID_LOAD_UNLOAD_DLL,
		ID_DLL_PATH,
		ID_CHOOSE_DLL,
		ID_OUTPUT,
		ID_ADD_VARIABLE,
					
		// up to 100 recent items can be accommodated
		ID_RECENT = 500,
		ID_RECENT_LAST = 600
};

BEGIN_EVENT_TABLE(SCFrame, wxFrame)
	EVT_TOOL( ID_ADD_VARIABLE,             SCFrame::OnCommand )

	EVT_TOOL( wxID_OPEN,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_SAVE,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_SAVEAS,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_PREFERENCES,            SCFrame::OnCommand )

	EVT_TOOL( wxID_EXIT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_ABOUT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_HELP,                  SCFrame::OnCommand )

	EVT_TOOL( ID_START, SCFrame::OnCommand )
	EVT_TOOL( ID_SHOW_STATS,              SCFrame::OnCommand )
	EVT_TOOL( ID_LOAD_UNLOAD_DLL,              SCFrame::OnCommand )
	EVT_TEXT_ENTER( ID_DLL_PATH,                SCFrame::OnCommand )
	EVT_BUTTON( ID_CHOOSE_DLL,            SCFrame::OnCommand )
	

    EVT_AUITOOLBAR_TOOL_DROPDOWN(wxID_OPEN, SCFrame::OnRecentDropDownButton)
	
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

	
	m_toolBar = new wxAuiToolBar(this);

	m_txtSelectedCMs = new wxTextCtrl( m_toolBar, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(270, 21), wxTE_READONLY );
	m_txtSelectedCMs->SetBackgroundColour( *wxWHITE );
	m_txtSelectedCMs->SetForegroundColour( *wxBLUE );

	m_gauProgress = new wxGauge( m_toolBar, wxID_ANY, 100, wxDefaultPosition, wxDefaultSize, wxGA_SMOOTH );
	m_txtProgress = new wxTextCtrl( m_toolBar, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(270, 21), wxTE_READONLY );
	m_txtProgress->SetBackgroundColour(*wxBLACK);
	m_txtProgress->SetForegroundColour(*wxGREEN);
	
	m_toolBar->AddTool( ID_LOAD_UNLOAD_DLL,"Load/unload ssc32.dll", wxBitmap(stock_connect_24_xpm),"Load/unload ssc32.dll");
	//m_toolBar->AddSeparator();
	m_toolBar->AddTool( ID_START ,"Start", wxBitmap(stock_media_play_24_xpm),"Start computations...");
	//m_toolBar->AddTool( ID_STOP, "Stop", wxBitmap(stock_media_record_24_xpm), "Stop computations");
	m_toolBar->AddSeparator();
	m_toolBar->AddTool( wxID_OPEN ,"Open", wxBitmap(stock_open_24_xpm),"Open input file");
    m_toolBar->SetToolDropDown(wxID_OPEN, true);
	m_toolBar->AddTool( wxID_SAVE, "Save", wxBitmap(stock_save_24_xpm),"Save input file");
	//m_toolBar->AddTool( wxID_SAVEAS, "Save as", wxBitmap(stock_save_as_24_xpm), "Save input file as...");
	m_toolBar->AddSeparator();
	m_toolBar->AddTool( wxID_PREFERENCES, "Compute Modules...", wxBitmap(stock_preferences_24_xpm), "Options...");
	m_toolBar->AddSeparator();
	m_toolBar->AddControl( m_txtSelectedCMs );
	m_toolBar->AddSeparator();
	m_toolBar->AddControl( m_txtProgress );
	m_toolBar->AddControl( m_gauProgress );
	m_toolBar->AddStretchSpacer();
	m_toolBar->AddTool( wxID_ABOUT, "About SSCdev", wxBitmap(stock_about_24_xpm), "About SSCdev...");
	m_toolBar->SetToolBitmapSize(wxSize(24,24));
	m_toolBar->Realize();

	m_txtDllPath = new wxTextCtrl( this, ID_DLL_PATH, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	m_btnChooseDll = new wxButton( this, ID_CHOOSE_DLL, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT );
	m_lblDllStatus = new AFLabel( this, -1, "Status" );

	wxBoxSizer *sz_dll_info = new wxBoxSizer(wxHORIZONTAL);
	sz_dll_info->Add( m_lblDllStatus, 2, wxALL|wxEXPAND, 0 );
	sz_dll_info->Add( m_txtDllPath, 1, wxALL|wxEXPAND, 0 );
	sz_dll_info->Add( m_btnChooseDll, 0, wxALL|wxEXPAND, 0 );

	wxSplitterWindow *split_win = new wxSplitterWindow( this, wxID_ANY,
		wxPoint(0,0), wxSize(800,700), wxSP_LIVE_UPDATE|wxBORDER_NONE );

	wxAuiNotebook *nb = new wxAuiNotebook( split_win, wxID_ANY, wxDefaultPosition, wxDefaultSize,
		wxAUI_NB_TOP | wxAUI_NB_TAB_SPLIT | wxAUI_NB_TAB_MOVE | wxAUI_NB_SCROLL_BUTTONS);

	m_dataView = new DataView(nb);
	m_dataView->SetDataObject( m_varTable );

	m_automForm = new AutomationForm(nb);

	nb->AddPage( m_dataView, "Variables & Output", true, wxBitmap(stock_preferences_24_xpm) );
	nb->AddPage( m_automForm, "Automation & Control", false,  wxBitmap(stock_convert_24_xpm) );

	
	m_txtOutput = new wxTextCtrl(split_win, ID_OUTPUT, wxEmptyString, wxDefaultPosition, wxDefaultSize,
		wxTE_READONLY | wxTE_MULTILINE | wxHSCROLL | wxTE_DONTWRAP);
	m_txtOutput->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier") );
	m_txtOutput->SetForegroundColour( *wxBLUE );
	

	split_win->SplitHorizontally( nb, m_txtOutput, -180 );
	split_win->SetSashGravity( 1 );


	wxBoxSizer *sz_main = new wxBoxSizer(wxVERTICAL);
	sz_main->Add( m_toolBar, 0, wxALL|wxEXPAND, 0 );
	sz_main->Add( sz_dll_info, 0, wxALL|wxEXPAND, 0 );
	sz_main->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0);
	sz_main->Add( split_win, 1, wxALL|wxEXPAND, 0 );

	SetSizer(sz_main );
	m_txtProgress->Hide();
	m_gauProgress->Hide();

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
		m_txtDllPath->ChangeValue( dll_path );
		m_loadedDllPath = dll_path;
		sscdll_load( dll_path.c_str() );
		m_lastLoadTime = wxNow();
	}

	UpdateRecentMenu();	

	

	wxAcceleratorEntry entries[7];
	entries[0].Set( wxACCEL_NORMAL, WXK_F1, ID_LOAD_UNLOAD_DLL );
	entries[1].Set( wxACCEL_CTRL,   's',  wxID_SAVE );
	entries[2].Set( wxACCEL_CTRL,   'o',  wxID_OPEN );
	entries[3].Set( wxACCEL_NORMAL, WXK_F2, wxID_PREFERENCES );
	entries[4].Set( wxACCEL_NORMAL, WXK_F3, ID_SHOW_STATS );
	entries[5].Set( wxACCEL_NORMAL, WXK_F4, ID_ADD_VARIABLE );
	entries[6].Set( wxACCEL_NORMAL, WXK_F5, ID_START );
	SetAcceleratorTable( wxAcceleratorTable(7,entries) );
	


	/************ SHOW THE APPLICATION *************/
	
	this->Show();
	this->SetClientSize( 750, 600);	


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
		try {
			ver = ssc_version();
		} catch (sscdll_error e) {
			status = e.text + " ";
			ver = -999;
		}
		status += m_loadedDllPath + " ( " + m_lastLoadTime + " ) Version " + wxString::Format("%d", ver);
	}
	else
		status = "ssc32.dll not loaded.";

	m_lblDllStatus->SetCaption( status );

	m_toolBar->SetToolBitmap( ID_LOAD_UNLOAD_DLL, sscdll_isloaded() ? wxBitmap(stock_disconnect_24_xpm) : wxBitmap(stock_connect_24_xpm) );
	m_toolBar->SetToolShortHelp( ID_LOAD_UNLOAD_DLL, sscdll_isloaded() ? "Unload ssc32.dll" : "Load ssc32.dll" );

	m_toolBar->EnableTool( ID_START, sscdll_isloaded() );
	m_toolBar->EnableTool( ID_STOP, sscdll_isloaded() );
	m_toolBar->Refresh();

	
	wxString cmtext;
	for (int i=0;i<m_cmList.count();i++)
		cmtext += m_cmList[i].cm_mod_name + "  ";
	m_txtSelectedCMs->ChangeValue( cmtext );
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
		Load(m_lastFile);
}

void SCFrame::OnRecentDropDownButton(wxAuiToolBarEvent& evt)
{
	if (evt.IsDropDownClicked())
	{
		wxAuiToolBar* tb = static_cast<wxAuiToolBar*>(evt.GetEventObject());
		if (m_recentCount < 1)
		{
			wxMessageBox("No recent files.");
			tb->SetToolSticky(evt.GetId(), false);
			return;
		}

		tb->SetToolSticky(evt.GetId(), true);
		// line up our menu with the button
		wxRect rect = tb->GetToolRect(evt.GetId());
		wxPoint pt = tb->ClientToScreen(rect.GetBottomLeft());
		pt = ScreenToClient(pt);
		PopupMenu(m_recentMenu, pt);
		// make sure the button is "un-stuck"
		tb->SetToolSticky(evt.GetId(), false);
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
	app_config->Write("DllPath", m_txtDllPath->GetValue());

	sscdll_unload(); // make sure dll is unloaded;

	Destroy();
}

void SCFrame::Open()
{
	wxFileDialog dlg(this, "Load SSCDEV State",
		wxPathOnly(m_lastFile),
		m_lastFile,
		"Data File (*.sscdat)|*.sscdat",
		wxFD_OPEN);

	if (dlg.ShowModal() == wxID_OK)
	{
		m_lastFile = dlg.GetPath();
		if (!Load(m_lastFile))
			wxMessageBox("Error loading:\n\n", m_lastFile);
	}

}


void SCFrame::Save()
{
	wxFileDialog dlg(this, "Save SSCDEV State", wxPathOnly(m_lastFile),
		m_lastFile, "Data File (*.sscdat)|*.sscdat", wxFD_SAVE);
	
	int ret = dlg.ShowModal();
	m_lastFile = dlg.GetPath();

	if (ret!=wxID_OK) return;

	if(!WriteToDisk(m_lastFile))
		wxMessageBox("Error writing:\n\n" + m_lastFile );
	else
		AddRecent( m_lastFile );
}

void SCFrame::SaveAs()
{
}

bool SCFrame::CloseDocument()
{
	return (m_automForm->CloseEditors());
}

void SCFrame::Exit()
{
	Close( false );
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
	case wxID_OPEN:
		Open();
		break;
	case wxID_SAVE:
		Save();
		break;
	/*case wxID_SAVEAS:
		break;*/
	case wxID_EXIT:
		Exit();
		break;
	case wxID_ABOUT:
		{		
			SCAbout dlg(this);
			dlg.ShowModal();	
		}
		break;
	case ID_LOAD_UNLOAD_DLL:
		{
			if (!sscdll_isloaded())
			{
				m_loadedDllPath = m_txtDllPath->GetValue();
				sscdll_load( m_loadedDllPath.c_str() );
				m_lastLoadTime = wxNow();
				m_currentAppDir = wxPathOnly(m_loadedDllPath);
			}
			else
				sscdll_unload();

			UpdateUI();
		}
		break;
	case ID_CHOOSE_DLL:
		{
			wxFileDialog fd(this, "Choose ssc32.dll", m_currentAppDir, "ssc32.dll", "DLL Files (*.dll)|*.dll", wxFD_OPEN);
			if (fd.ShowModal() != wxID_OK) return;
			wxString file = fd.GetPath();
			m_txtDllPath->ChangeValue(file);
			m_currentAppDir = wxPathOnly(file);
			m_loadedDllPath = file;
			sscdll_load(file.c_str());
			m_lastLoadTime = wxNow();
			UpdateUI();
		}
		break;
	case ID_SHOW_STATS:
		m_dataView->ShowStats();
		break;
	case ID_DLL_PATH:
		{
			wxString file = m_txtDllPath->GetValue();
			m_currentAppDir = wxPathOnly( file );
			m_loadedDllPath = file;
			sscdll_load(file.c_str());
			m_lastLoadTime = wxNow();
			UpdateUI();
		}
		break;
	case wxID_PREFERENCES:
		{
			CMFormDialog dlg(this, "Compute Module Browser");
			dlg.CentreOnParent();
			dlg.GetPanel()->SetCMList( m_cmList );
			if (dlg.ShowModal()==wxID_OK)
				m_cmList = dlg.GetPanel()->GetCMList();

			UpdateUI();
		}
		break;
	}
}

bool SCFrame::Load(const wxString &fn)
{
	wxBusyInfo busy("Loading: " + fn);
	FILE *fp = fopen( (const char*)fn.c_str(), "r" );
	if (!fp) return false;
		
	m_cmList.clear();
	m_varTable->clear();
	UpdateUI();
	m_dataView->UpdateView();

	wxString buf;
	AllocReadLine(fp, buf); // header&fileformatversion line
	
	int n_cmmods = 0;
	AllocReadLine(fp, buf); sscanf( (const char*)buf.c_str(), "cm_mods %d", &n_cmmods);
	for (int i=0;i<n_cmmods;i++)
	{
		cmModule cm;
		AllocReadLine(fp, cm.cm_mod_name);
		int n_params = 0;
		AllocReadLine(fp, buf); sscanf((const char*)buf.c_str(), "params %d", &n_params);
		for (int j=0;j<n_params;j++)
		{
			cmParam pa;
			AllocReadLine(fp, pa.name);
			AllocReadLine(fp, buf); pa.type = atoi(buf.c_str());
			AllocReadLine(fp, pa.str);
			AllocReadLine(fp, buf); pa.num = (float)atof(buf.c_str());

			cm.params.append( pa );
		}

		m_cmList.append( cm );
	}

	wxArrayString sel_vars;
	Array<int> cwl;

	AllocReadLine(fp, buf); // selected_variables:
	AllocReadLine(fp, buf); sel_vars = Split(buf,",");
	AllocReadLine(fp, buf); StringToIntArray( buf, ',', cwl );

	int data_size = 0;
	AllocReadLine(fp, buf); sscanf( buf.c_str(), "data_set_size: %d", &data_size );
	
	for (int i=0;i<data_size;i++)
	{
		wxString name;
		int type;
		wxString sval;
		AllocReadLine(fp, name);
		AllocReadLine(fp, buf); type = atoi(buf.c_str());
		AllocReadLine(fp, sval);

		for (size_t n=0;n<sval.Length();n++)
			if (((char)sval.at(n)) == (char)11)
				sval[n] = '\n';

		var_data val;
		var_data::parse( type, (const char*)sval.c_str(), val );

		m_varTable->assign( (const char*)name.c_str(), val );
	}
	
	m_dataView->UpdateView();	
	m_dataView->SetSelections( sel_vars );
	m_dataView->UpdateView();
	m_dataView->SetColumnWidths( cwl );

	UpdateUI();

	fclose(fp);
	AddRecent(fn);
	return true;	
}

bool SCFrame::WriteToDisk(const wxString &fn)
{
	wxBusyInfo busy("Writing: " + fn);
	FILE *fp = fopen( (const char*)fn.c_str(), "w" );
	if (!fp) return false;

	fprintf(fp, "sscdev state file version 1\n");
	fprintf(fp, "cm_mods %d\n", m_cmList.count());
	for (int i=0;i<m_cmList.count();i++)
	{
		fprintf(fp, "%s\n", (const char*)m_cmList[i].cm_mod_name.c_str());
		fprintf(fp, "params %d\n", m_cmList[i].params.count());
		for (int j=0;j<m_cmList[i].params.count();j++)
		{
			fprintf(fp, "%s\n", (const char*)m_cmList[i].params[j].name.c_str());
			fprintf(fp, "%d\n", m_cmList[i].params[j].type );
			fprintf(fp, "%s\n", (const char*)m_cmList[i].params[j].str.c_str());
			fprintf(fp, "%lg\n", (double)m_cmList[i].params[j].num );
		}
	}

	wxString selvars = Unsplit(m_dataView->GetSelections(), ",");
	fprintf(fp, "selected_variables:\n%s\n", (const char*)selvars.c_str());
	fprintf(fp, "%s\n", (const char*)IntArrayToString(m_dataView->GetColumnWidths(), ',').c_str());

	int nvars = (int) m_varTable->size();
	fprintf(fp, "data_set_size: %d\n", nvars );

	const char *name = m_varTable->first();
	while( name )
	{
		var_data *v = m_varTable->lookup( name );
		
		fprintf(fp, "%s\n", name );
		fprintf(fp, "%d\n", v->type);
		
		std::string nlfix = v->to_string();
		for (size_t n=0;n<nlfix.length();n++)
			if (nlfix[n] == '\n') nlfix[n] = (char)11;

		fprintf(fp, "%s\n", nlfix.c_str());

		name = m_varTable->next();
	}

	fclose(fp);

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

void SCFrame::Progress(const wxString &text, float percent)
{
	m_txtProgress->SetValue(text);
	m_txtProgress->Update();
	m_gauProgress->SetValue( (int)percent );
	m_gauProgress->Update();
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
	SCFrame *sc_frame = (SCFrame*) user_data;
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

		sc_frame->Log(msg);
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		sc_frame->Progress( wxString::Format("(%.2f %%) %s",f0,s0), f0 );
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
	m_txtProgress->Clear();
	m_txtProgress->Show();
	m_gauProgress->SetValue(0);
	m_gauProgress->Show();

	wxGetApp().Yield(true);

	try {

		ssc_data_t p_data = ::ssc_data_create();

		Copy( p_data, m_varTable, true );


		for (int i=0;i<m_cmList.count();i++)
		{
			m_txtProgress->SetValue( m_cmList[i].cm_mod_name );
			m_gauProgress->SetValue( 0 );
			wxGetApp().Yield(true);

			ssc_module_t p_mod = ::ssc_module_create( (const char*) m_cmList[i].cm_mod_name.c_str() );
			
			if (p_mod == 0)
			{
				Log("CREATE_FAIL: " + m_cmList[i].cm_mod_name );
				break;
			}
			
			for (int j=0;j<m_cmList[i].params.count();j++)
			{
				cmParam &pa = m_cmList[i].params[j];
				if (pa.type == SSC_STRING) ::ssc_module_parameter_string( p_mod, pa.name.c_str(), pa.str.c_str() );
				else ::ssc_module_parameter_number( p_mod, pa.name.c_str(), (ssc_number_t)pa.num );
			}

			wxStopWatch sw;
			sw.Start();			
			if (! ::ssc_module_exec_with_handler( p_mod, p_data,
				my_handler,	this) )
			{
				Log("EXEC_FAIL: "+m_cmList[i].cm_mod_name);
				::ssc_module_free( p_mod );
				break;
			}
			else
				Log("EXEC_SUCCESS: " + m_cmList[i].cm_mod_name + " (" + wxString::Format("%.3lf", (double)sw.Time()/1000.0) + " sec)");

			::ssc_module_free( p_mod );
		}

		Copy( m_varTable, p_data, false );
		m_dataView->UpdateView();

		::ssc_data_free( p_data );
	 
	} catch(sscdll_error e) {
		wxMessageBox("DLL error: " + e.func + ": " + e.text );
	}
	
	m_txtProgress->Hide();
	m_gauProgress->Hide();
}
