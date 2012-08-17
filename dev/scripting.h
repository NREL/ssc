#ifndef __automation_h
#define __automation_h

#include <wx/wx.h>
#include <cml/codeedit.h>
#include "dllinvoke.h"

#include <lk_absyn.h>
#include <lk_env.h>

class EditorWindow : public wxPanel
{
private:
	lk::env_t *m_env;
	CodeEdit *m_editor;
	wxStaticText *m_statusLabel;
	wxString m_fileName;
	wxButton *m_stopButton;
	bool m_stopScriptFlag;
	bool m_scriptRunning;
public:
	EditorWindow( wxWindow *parent );
	virtual ~EditorWindow();
	
	wxString GetFileName() { return m_fileName; }
	void OnCommand( wxCommandEvent &evt );
	bool IsStopFlagSet() { return m_stopScriptFlag; }	
	bool IsScriptRunning() { return m_scriptRunning; }
	bool Save();
	bool SaveAs();
	bool CloseDoc();
	bool Write( const wxString &file );
	bool Load( const wxString &file );	
	void Exec();

	DECLARE_EVENT_TABLE()
};


#endif

