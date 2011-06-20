#include <wx/wx.h>
#include <wx/imaglist.h>
#include <wx/splitter.h>
#include <wx/filename.h>

#include "automation.h"

#include <cml/sl_exec.h>
#include <cml/sl_invoke.h>
#include <cml/sl_parse.h>
#include <cml/sl_stdlib.h>

#include <cml/pixmaps/stock_text_indent_16.xpm>
#include <cml/pixmaps/stock_new_16.xpm>
#include <cml/pixmaps/stock_open_16.xpm>
#include <cml/pixmaps/stock_save_16.xpm>
#include <cml/pixmaps/stock_save_as_16.xpm>
#include <cml/pixmaps/stock_undo_16.xpm>
#include <cml/pixmaps/stock_redo_16.xpm>
#include <cml/pixmaps/stock_cut_16.xpm>
#include <cml/pixmaps/stock_copy_16.xpm>
#include <cml/pixmaps/stock_paste_16.xpm>
#include <cml/pixmaps/stock_exec_16.xpm>
#include <cml/pixmaps/stock_jump_to_16.xpm>
#include <cml/pixmaps/stock_media_play_16.xpm>
#include <cml/pixmaps/stock_search_16.xpm>
#include <cml/pixmaps/stock_search_replace_16.xpm>

#include "sscdev.h"


/********* SSEditorForm *************/
#define MAX_INSERT_ITEMS 2000

enum { ID_EDITOR = 1111, 

ID_RUNSCRIPT, 
ID_NEW, ID_OPEN, ID_SAVE, ID_SAVE_AS,
ID_UNDO, ID_REDO, ID_COPY, ID_CUT, ID_PASTE, ID_FIND, ID_FIND_NEXT, ID_REPLACE,
ID_FUNC_LIST, ID_OUTPUT, ID_GENDOCS,

ID_FUNC_LIST_BASE = 2000};

BEGIN_EVENT_TABLE(SSEditorForm, wxPanel)
EVT_STC_MODIFIED( ID_EDITOR, SSEditorForm::OnEditorModified)
END_EVENT_TABLE()

SSEditorForm::SSEditorForm( wxWindow *parent, AutomationForm *af )
	: wxPanel(parent,-1)
{
	mAMForm = af;
	mEditor = new CodeEdit(this, ID_EDITOR);
	
	SLInvokeLibrary *flib = af->GetFuncLib();

	wxArrayString fcnlist = flib->GetFunctionNames();
	wxString fcns = Unsplit(fcnlist," ");
	
	mEditor->SetKeyWords( 0, SLParse::GetLanguageKeywords().Lower());
	mEditor->SetKeyWords( 1, fcns.Lower());

	StringMap tipmap;
	Array<SLInvokeTable*> tabarr = flib->GetAll();
	for (int i=0;i<tabarr.count();i++)
	{
		fcnlist = tabarr[i]->GetFunctionNames();
		for (int j=0;j<(int)fcnlist.Count();j++)
		{
			wxString d,s;
			tabarr[i]->QueryInfo(fcnlist[j],d,s);
			tipmap[ fcnlist[j].Lower() ] = fcnlist[j] + " " + s + "\n" + d;
		}
	}

	mEditor->EnableCallTips(true);
	mEditor->SetCallTipData( '(', ')', false, tipmap );

	mEditor->ApplyScriptStyling();


	mStatusBar = new wxStatusBar(this, wxID_ANY);

	wxBoxSizer *peszv = new wxBoxSizer(wxVERTICAL);
	peszv->Add(mEditor,1,wxALL|wxEXPAND,0);
	peszv->Add(mStatusBar,0,wxALL|wxEXPAND,0);

	this->SetSizer( peszv );

	
	wxAcceleratorEntry entries[12];
	entries[0].Set( wxACCEL_CMD, 's', ID_SAVE);
	entries[1].Set( wxACCEL_CMD, 'o',  ID_OPEN );
	entries[2].Set( wxACCEL_CMD, 'f',  ID_FIND );
	entries[3].Set( wxACCEL_NORMAL, WXK_F3,  ID_FIND_NEXT );
	entries[4].Set( wxACCEL_CMD, 'h',  ID_REPLACE );
	entries[5].Set( wxACCEL_CMD, 'r',  ID_RUNSCRIPT );
	entries[6].Set( wxACCEL_NORMAL, WXK_F5,  ID_RUNSCRIPT );
	entries[7].Set( wxACCEL_SHIFT, WXK_F11, ID_GENDOCS );

	SetAcceleratorTable( wxAcceleratorTable(8,entries) );

}

bool SSEditorForm::Load(const wxString &fn)
{
	if (mEditor->LoadFile(fn))
	{
		mFileName = fn;
		mStatusBar->SetStatusText( "Loaded: " + mFileName + " (" + wxNow() + ")" );
		return true;
	}
	else
		return false;
}

bool SSEditorForm::Save()
{
	if (mFileName == "")
		return SaveAs();
	else
		return WriteToDisk( mFileName );
}

bool SSEditorForm::SaveAs()
{
	wxFileDialog fdlg(this, "Save File As", "", mFileName, "Script Files (*.ss)|*.ss|All Files (*.*)|*.*", wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
	if (fdlg.ShowModal() == wxID_OK)
	{
		wxString fn = fdlg.GetPath();
		return WriteToDisk( fn, true );
	}
	else
		return false;
}

bool SSEditorForm::WriteToDisk(const wxString &fn, bool force)
{
	if (fn == "") return false;
	if (!IsDirty() && !force) return true;
	mEditor->SetFileName( fn );
	if (mEditor->SaveFile( true ))
	{
		mFileName = fn;
		mStatusBar->SetStatusText( "Saved: " + mFileName + " (" + wxNow() + ")");
		mAMForm->UpdateTitle(this);
		return true;
	}
	else
		return false;
}

wxString SSEditorForm::GetFileName()
{
	return mFileName;
}

bool SSEditorForm::IsDirty()
{
	return mEditor->Modified();
}


wxString SSEditorForm::GetTitle()
{
	wxString title = "untitled";
	if (!mFileName.IsEmpty())
		title = wxFileNameFromPath( mFileName );
	if (IsDirty())
		title += " *";
	return title;
}

wxBitmap SSEditorForm::GetBitmapIcon()
{
	return wxBitmap(::stock_text_indent_16_xpm);
}

void SSEditorForm::OnEditorModified(wxStyledTextEvent &evt)
{
	mAMForm->UpdateTitle(this);
}


bool SSEditorForm::CanClose()
{
	if (IsDirty())
	{
		int ret =wxMessageBox("The file '" + GetTitle() + "' has changed.\n\nDo you want to save changes?" , "Query", wxYES_NO|wxCANCEL|wxICON_EXCLAMATION);
		if (ret == wxCANCEL)
			return false;
		else if (ret == wxNO)
			return true;
		else
			return Save();
	}
	else
		return true;
}

/********* Automation window ***********/

enum { ID_NOTEBOOK = 8741, ID_DIRBROWSER};

BEGIN_EVENT_TABLE( AutomationForm, wxPanel )
EVT_TOOL(ID_NEW, AutomationForm::OnCommand)
EVT_TOOL(ID_OPEN, AutomationForm::OnCommand)
EVT_TOOL(ID_SAVE, AutomationForm::OnCommand)
EVT_TOOL(ID_SAVE_AS, AutomationForm::OnCommand)
EVT_TOOL(ID_UNDO, AutomationForm::OnCommand)
EVT_TOOL(ID_REDO, AutomationForm::OnCommand)
EVT_TOOL(ID_CUT, AutomationForm::OnCommand)
EVT_TOOL(ID_COPY, AutomationForm::OnCommand)
EVT_TOOL(ID_PASTE, AutomationForm::OnCommand)
EVT_TOOL(ID_FIND, AutomationForm::OnCommand)
EVT_TOOL(ID_FIND_NEXT, AutomationForm::OnCommand)
EVT_TOOL(ID_REPLACE, AutomationForm::OnCommand)
EVT_TOOL(ID_RUNSCRIPT, AutomationForm::OnCommand)
EVT_TOOL(ID_FUNC_LIST, AutomationForm::OnCommand)
EVT_TOOL(ID_GENDOCS, AutomationForm::OnCommand)
EVT_COMMAND( ID_DIRBROWSER, afEVT_FILE_SELECTED, AutomationForm::OnFileSelect )
EVT_COMMAND( ID_DIRBROWSER, afEVT_FILE_OPENAPP, AutomationForm::OnFileOpenApp )
EVT_AUINOTEBOOK_PAGE_CLOSE(ID_NOTEBOOK, AutomationForm::OnNotebookPageClose)
EVT_AUINOTEBOOK_PAGE_CHANGED(ID_NOTEBOOK, AutomationForm::OnNotebookPageChange)
EVT_MENU_RANGE( ID_FUNC_LIST_BASE, ID_FUNC_LIST_BASE+MAX_INSERT_ITEMS-1, AutomationForm::OnFuncInsert)
END_EVENT_TABLE()

AutomationForm::AutomationForm(wxWindow *parent)
  : wxPanel (parent, -1, wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN)
{

	ScriptRunning = false;
	AbortFlag = false;
	PauseFlag = false;

	mFuncLib = ::SLStdlibCreateLibrary();

	SLInvokeTable *sttab = new SLInvokeTable;
	
void AppendSSCInvokeFunctions(SLInvokeTable *tab); // decl from (invoke.cpp)
	AppendSSCInvokeFunctions(sttab);
	mFuncLib->Assign("SSCdev Functions", sttab);

    // create some toolbars
    ToolBar = new wxAuiToolBar(this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                                         wxAUI_TB_DEFAULT_STYLE);
    ToolBar->AddTool(ID_NEW, "New script", wxBitmap(stock_new_16_xpm), "New script");
	ToolBar->AddSeparator();
	ToolBar->AddTool(ID_OPEN, "Open script", wxBitmap(stock_open_16_xpm), "Open script");
	ToolBar->AddTool(ID_SAVE, "Save script", wxBitmap(stock_save_16_xpm), "Save script");
	ToolBar->AddTool(ID_SAVE_AS, "Save script as", wxBitmap(stock_save_as_16_xpm), "Save script as...");
	ToolBar->AddSeparator();
	
	ToolBar->AddTool(ID_UNDO, "Undo editing", wxBitmap(stock_undo_16_xpm), "Undo editing");
	ToolBar->AddTool(ID_REDO, "Redo editing", wxBitmap(stock_redo_16_xpm), "Redo editing");
	ToolBar->AddSeparator();
	
	ToolBar->AddTool(ID_CUT, "Cut", wxBitmap(stock_cut_16_xpm), "Cut selection");
	ToolBar->AddTool(ID_COPY, "Copy", wxBitmap(stock_copy_16_xpm), "Copy selection");
	ToolBar->AddTool(ID_PASTE, "Paste", wxBitmap(stock_paste_16_xpm), "Paste");
	ToolBar->AddSeparator();
	
	ToolBar->AddTool(ID_FIND, "Find", wxBitmap(stock_search_16_xpm), "Find...");
	ToolBar->AddTool(ID_REPLACE, "Replace", wxBitmap(stock_search_replace_16_xpm), "Replace...");
	ToolBar->AddSeparator();

	ToolBar->AddTool(ID_RUNSCRIPT,"Run script", wxBitmap(stock_media_play_16_xpm),"Execute Script");
	ToolBar->AddSeparator();
	ToolBar->AddTool(ID_FUNC_LIST,"Functions", wxBitmap(stock_jump_to_16_xpm),"Browse Function Library");
	
	ToolBar->SetToolBitmapSize(wxSize(16,16));
	ToolBar->Realize();

	Notebook = new wxAuiNotebook(this, ID_NOTEBOOK, wxDefaultPosition, wxDefaultSize, 
		wxAUI_NB_BOTTOM|wxAUI_NB_SCROLL_BUTTONS|wxAUI_NB_TAB_MOVE|wxAUI_NB_TAB_SPLIT|
		wxAUI_NB_WINDOWLIST_BUTTON|wxAUI_NB_CLOSE_ON_ACTIVE_TAB|wxBORDER_NONE);
	
	wxBoxSizer *sz = new wxBoxSizer(wxVERTICAL);
	sz->Add( ToolBar, 0, wxALL|wxEXPAND );
	sz->Add( Notebook, 1, wxALL|wxEXPAND );
	SetSizer(sz);
}

AutomationForm::~AutomationForm()
{
	delete mFuncLib;
}

int AutomationForm::GetTabIdx(SSEditorForm *ef)
{
	for (int i=0;i<Notebook->GetPageCount();i++)
		if (Notebook->GetPage(i) == ef)
			return i;
	return -1;
}

Array<SSEditorForm*> AutomationForm::GetEditorFormList()
{
	Array<SSEditorForm*> list;
	for (int i=0;i<Notebook->GetPageCount();i++)
		list.append( (SSEditorForm*) Notebook->GetPage(i) );

	return list;
}

bool AutomationForm::CloseEditors()
{
	Array<SSEditorForm*> list = GetEditorFormList();

	for (int i=0;i<list.count();i++)
	{
		int page = GetTabIdx(list[i]);
		Notebook->SetSelection(page);
		if (list[i]->CanClose())
			Notebook->DeletePage( page );
	}

	return (Notebook->GetPageCount() == 0);
}

void AutomationForm::OnFileSelect(wxCommandEvent &evt)
{
	Load(evt.GetString());
}

void AutomationForm::OnFileOpenApp(wxCommandEvent &evt)
{
}


bool AutomationForm::LocateLine(const wxString &file, long line)
{
	return false;
}

void AutomationForm::UpdateTitle(SSEditorForm *ef)
{
	Notebook->SetPageText( GetTabIdx(ef), ef->GetTitle() );
}

bool AutomationForm::Load(const wxString &file)
{
	if (!file.IsEmpty())
	{
		Array<SSEditorForm*> EditorList = GetEditorFormList();
		wxFileName fn2( file );
		for (int i=0;i<EditorList.count();i++)
		{
			wxFileName fn1( EditorList[i]->GetFileName() );
			if (fn1 == fn2)
			{
				Notebook->SetSelection(i);
				return true;
			}
		}
	}

	SSEditorForm *ef = new SSEditorForm(Notebook, this);
	
	if (!file.IsEmpty() && !ef->Load(file)) 
	{
		ef->Destroy();
		return false;
	}

	Notebook->AddPage( ef, "editor", true/*, ef->GetBitmapIcon()*/ );
	UpdateTitle(ef);
	return true;
}

void AutomationForm::New()
{
	Load();
}

void AutomationForm::SaveAs()
{
	SSEditorForm *ef = Active();
	if (ef) ef->SaveAs();
}

void AutomationForm::Save()
{
	SSEditorForm *ef = Active();
	if (ef) ef->Save();
}

void AutomationForm::RunCurrent()
{
	SSEditorForm *ef = Active();
	if (ef) RunScript(ef->GetEditor()->GetText(), ef->GetFileName());
}

void AutomationForm::Find()
{
	SSEditorForm *ef = Active();
	if (ef) ef->GetEditor()->ShowFindDialog();
}

void AutomationForm::FindNext()
{
	SSEditorForm *ef = Active();
	if ( ef && ef->GetEditor()->FindNext() < 0)
		wxMessageBox("Error: '" + ef->GetEditor()->GetFindString() + "' could not found.","Notice");
}

void AutomationForm::Replace()
{
	SSEditorForm *ef = Active();
	if (ef) ef->GetEditor()->ShowReplaceDialog();
}


void AutomationForm::OnCommand(wxCommandEvent &evt)
{
	SSEditorForm *ef = Active();
	
	switch(evt.GetId())
	{
	case ID_NEW:
		New();
		break;
	case ID_OPEN:
		{
			wxFileDialog fdlg(this, "Open Files", "", "", "All Scripts (*.ss)|*.ss", wxFD_OPEN|wxFD_MULTIPLE );

			if (fdlg.ShowModal() == wxID_OK)
			{
				wxArrayString list;
				fdlg.GetPaths( list );
				for (int i=0;i<(int)list.Count();i++)
					Load( list[i] );
			}
		}
		break;
	case ID_SAVE:
		if (ef) ef->Save();
		break;
	case ID_SAVE_AS:
		if (ef) ef->SaveAs();
		break;
	case ID_UNDO:
		if (ef) ef->GetEditor()->Undo();
		break;
	case ID_REDO: 
		if (ef) ef->GetEditor()->Redo();
		break;
	case ID_CUT:
		if (ef) ef->GetEditor()->Cut();
		break;
	case ID_COPY:
		if (ef) ef->GetEditor()->Copy();
		break;
	case ID_PASTE:
		if (ef) ef->GetEditor()->Paste();
		break;
	case ID_FIND:
		if (ef) ef->GetEditor()->ShowFindDialog();
		break;
	case ID_FIND_NEXT:
		FindNext();
		break;
	case ID_REPLACE:
		if (ef) ef->GetEditor()->ShowReplaceDialog();
		break;
	case ID_RUNSCRIPT:
		if (ef) RunScript(ef->GetEditor()->GetText(), ef->GetFileName());
		break;
	case ID_FUNC_LIST:		
		{
			wxMenu popup;
			int insert_idx = ID_FUNC_LIST_BASE;

			wxArrayString libs = mFuncLib->List();

			for (int i=0;i<(int)libs.Count();i++)
			{
				SLInvokeTable *tab = mFuncLib->Query( libs[i] );
				if (!tab)
					continue;

				wxMenu *cur_menu = new wxMenu;
				wxArrayString flist = tab->GetFunctionNames();
				for (int j=0;j<(int)flist.Count();j++)
				{
					mFuncInsertList.Add( flist[j] );
					cur_menu->Append( insert_idx++, flist[j]);
				}
				popup.AppendSubMenu(cur_menu, libs[i] );
			}

			this->PopupMenu( &popup );
		}
		break;
	case ID_GENDOCS:
		wxMessageBox("Generating documentation in " + ::wxGetHomeDir());
		::SLGenerateHTMLDocs( mFuncLib, ::wxGetHomeDir());
		break;
	}
}

SSEditorForm *AutomationForm::Active()
{
	int page = Notebook->GetSelection();
	SSEditorForm *ef = NULL;
	if (page >= 0 && page < Notebook->GetPageCount())
		ef = (SSEditorForm*) Notebook->GetPage(page);
	return ef;
}

void AutomationForm::OnFuncInsert(wxCommandEvent &evt)
{
	SSEditorForm *ef = Active();
	if (ef)
		ef->GetEditor()->ReplaceSelection( mFuncInsertList[ evt.GetId() - ID_FUNC_LIST_BASE ] );
}



static void _exec_notify(void *data, int line, int msec, SLEngine *exec)
{
#define NOTIFY_COUNTER_VAL 40
	static int notify_counter = NOTIFY_COUNTER_VAL;
	
	if (data == NULL || exec == NULL)
	{
		notify_counter = NOTIFY_COUNTER_VAL;
		return;
	}

	if (--notify_counter)
		return;

	AutomationForm *swin = (AutomationForm*)data;
	/*swin->SetStatus( wxString::Format("Running [line %d, elapsed %.2lf seconds, %d alloc]...", 
		line,
		((double) msec)/1000,
		_slvariant_alloc_count));
	*/

	wxGetApp().Yield(true);

	if (swin->PauseFlag)
	{
		wxMessageBox("Script paused.  Press OK to continue.");
		swin->PauseFlag = false;
	}

	if (swin->AbortFlag)
		exec->Abort();
	
	notify_counter = NOTIFY_COUNTER_VAL;
}

static void _slengine_in( wxString &str, const wxString &prompt, void *ptr )
{
	str = wxGetTextFromUser(prompt, "Input", wxEmptyString, app_frame);
}

static void _slengine_out( const wxString &str, void *data )
{
	app_frame->Log( str,false );
}

static bool _useimporter( const wxString &name, wxString &script, void *Data )
{
	// then if not found, check as a file name
	FILE *fp = fopen(name.c_str(), "r");
	if (!fp)
		return false;

	wxString buf;
	char c;
	while ( (c=fgetc(fp)) != EOF )
		if (c != '\r')
			buf += c;

	script = buf;
	fclose(fp);
	return true;
}

class ScriptControlDialog : public wxDialog
{
public:
	ScriptControlDialog(wxWindow *parent)
		: wxDialog(parent, wxID_ANY, wxString("Script Control"),
			wxDefaultPosition, wxDefaultSize)
	{
		new wxButton(this, wxID_ABORT, "Abort", wxPoint(5,5), wxSize(80,30));
		SetClientSize(90,40);
		AbortFlag = NULL;
	}
	
	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == ::wxID_ABORT && AbortFlag)
			*AbortFlag = true;
	}

	bool *AbortFlag;

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(ScriptControlDialog, wxDialog)
	EVT_BUTTON( wxID_ABORT, ScriptControlDialog::OnCommand)
END_EVENT_TABLE()

void AutomationForm::RunScript(const wxString &input, const wxString &filename)
{
	if (ScriptRunning) return;

	SLParse parser;
	parser.SetInput( input );
	wxArrayString msgs;
	SLSyntax::Script *tree = parser.Parse(&msgs);

	app_frame->ClearLog();

	wxString cwd = wxGetCwd();

	wxString msg = "Failure.\n\n";
	if (tree)
	{
		msg = "Parse Success.\n\n";
		//SLTreePrinter printer;
		//printer.PrintScript( tree );

		SLSetInCallback( _slengine_in, NULL );
		SLSetOutCallback( _slengine_out, this );
		
		// execute
		SLEngine engine;

		Array<SLInvokeTable*> ftabs = mFuncLib->GetAll();
		for (int i=0;i<ftabs.count();i++)
			engine.AddFunctionTable( ftabs[i] );

		ScriptControlDialog dlg(this);
		dlg.AbortFlag = &AbortFlag;
		dlg.CentreOnParent();
		dlg.Show();

		wxWindowDisabler windis(&dlg);
		ScriptRunning = true;
		AbortFlag = false;
		PauseFlag = false;
		wxGetApp().Yield(true);
		
		wxStopWatch sw;
		sw.Start();
		_exec_notify(NULL,0,0,NULL);
		
		bool ok = engine.Exec(filename, tree, _exec_notify, this, _useimporter, NULL, &msgs );
		double runtime_secs = ((double)sw.Time())/1000.0;
		
		app_frame->Log(wxString::Format("\n--------------------------------------------------\n"
			"Execute %s. (%.2lf sec)\n",  ok?"Success":"Failure", runtime_secs));

		if (!ok)
			for (int i=0;i<msgs.Count();i++)
				app_frame->Log(msgs[i]);
		
		delete tree;

		SLSetInCallback( NULL, NULL );
		SLSetOutCallback( NULL, NULL );
				
		ScriptRunning = false;
	}
	else
	{
		app_frame->Log( Unsplit(msgs,"\n") );
	}

	wxSetWorkingDirectory(cwd);
	//app_frame->SwitchScreen(6);
}

void AutomationForm::OnNotebookPageClose(wxAuiNotebookEvent& evt)
{
	SSEditorForm *ef = Active();

	if (ef && ef->CanClose())
	{
		int idx = Notebook->GetSelection();
		Notebook->DeletePage( idx );
	}
	
	evt.Veto();
}

void AutomationForm::OnNotebookPageChange(wxAuiNotebookEvent& evt)
{
	/* nothing to do here yet */
}
