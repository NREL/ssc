#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/mdi.h>
#include <wx/config.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/scrolwin.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>
#include <wx/filename.h>
#include <wx/tokenzr.h>
#include <wx/statline.h>
#include <wx/tglbtn.h>
#include <wx/splitter.h>

#ifdef __WXMSW__
#include <cml/xlautomation.h>
#endif

#include "dataview.h"
#include "editvariableform.h"

class DataView::Table : public wxGridTableBase
{
public:

	Table()	{ }
	virtual ~Table() { m_vt_ref = 0; }

	void Detach()
	{
		m_vt_ref = 0;
	}
	
	virtual int GetNumberRows()
	{
		int max0 = 0;
		for (int i=0;i<m_items.Count();i++)
		{
			if (!m_vt_ref) continue;
			var_data *v = m_vt_ref->lookup( (const char*)m_items[i].c_str() );
			if (!v) continue;

			int len = 0;
			if ( v->type == SSC_STRING ) len = 1;
			else len = v->num.ncells();

			if (len > max0) max0 = len;
		}
		return max0;
	}
	virtual int GetNumberCols()
	{
		return m_items.Count();
	}

	virtual bool IsEmptyCell(int row, int col)
	{
		if ( col < 0 || col >= m_items.Count() || row < 0 ) return true;
		
		if (!m_vt_ref) return true;
		var_data *v = m_vt_ref->lookup( (const char*)m_items[col].c_str() );
		if (!v) return true;

		if ( v->type == SSC_STRING && row >= 1 ) return true;

		if ( row >= v->num.ncells() ) return true;

		return false;
	}

	virtual wxString GetValue( int row, int col )
	{
		if (m_vt_ref && col >= 0 && col < m_items.Count())
		{
			var_data *v = m_vt_ref->lookup( (const char*)m_items[col].c_str() );
			if (!v) return "<lookup error>";

			if (v->type == SSC_STRING && row == 0) return wxString(v->str.c_str());
			else if (v->type == SSC_NUMBER && row == 0) return wxString::Format("%lg", (double) v->num);
			else if (v->type == SSC_ARRAY && row < v->num.length()) return wxString::Format("%lg", (double)v->num[row]);
			else if (v->type == SSC_MATRIX && row < v->num.ncells()) return wxString::Format("%lg", (double)v->num.data()[row] );
		}
		
		return wxEmptyString;
	}

	virtual void SetValue( int row, int col, const wxString &)
	{
		if (m_readonly || !m_vt_ref) return;
	}

	virtual wxString GetColLabelValue(int col)
	{
		if (col >= 0 && col < m_items.Count())
		{

			if (!m_vt_ref) return m_items[col];
			else {
				wxString label = m_items[col];
				var_data *v = m_vt_ref->lookup( (const char*)m_items[col].c_str() );
				if (v) label += " " + wxString(v->type_name());
				return label;
			}
		}
		else
			return "<unknown>";
	}
	
	void SetData( const wxArrayString &items, var_table *vt, bool ro)
	{
		m_items = items;
		m_vt_ref = vt;
		m_readonly = ro;
	}

private:
	bool m_readonly;
	wxArrayString m_items;
	var_table *m_vt_ref;
};



enum { ID_COPY_CLIPBOARD = 2315,
	   ID_DELETE_VARIABLE,
	   ID_DELETE_ALL_VARIABLES,
	   ID_UNSELECT_ALL,
	   ID_LIST,
	   ID_ADD_VARIABLE,
	   ID_EDIT_VARIABLE,
	   ID_GRID };

BEGIN_EVENT_TABLE( DataView, wxPanel )
	EVT_BUTTON( ID_COPY_CLIPBOARD, DataView::OnCommand )
	EVT_BUTTON( ID_UNSELECT_ALL, DataView::OnCommand )
	EVT_BUTTON( ID_ADD_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_EDIT_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_ALL_VARIABLES, DataView::OnCommand )
	EVT_CHECKLISTBOX( ID_LIST, DataView::OnVarListCheck )
	EVT_LISTBOX_DCLICK( ID_LIST, DataView::OnVarListDClick )
END_EVENT_TABLE()


DataView::DataView( wxWindow *parent ) 
	: wxPanel( parent ),
	m_vt(0),
	m_root_item(0),
	m_grid_table(0)
{
	wxBoxSizer *tb_sizer = new wxBoxSizer(wxHORIZONTAL);
	tb_sizer->Add( new wxButton(this, ID_ADD_VARIABLE, "Add variable..."), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_EDIT_VARIABLE, "Edit variable..."), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_VARIABLE, "Delete variable"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_ALL_VARIABLES, "Delete all variables"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton( this, ID_COPY_CLIPBOARD, "Copy to clipboard"), 0, wxEXPAND|wxALL, 2);
	tb_sizer->AddStretchSpacer(1);

	wxSplitterWindow *splitwin = new wxSplitterWindow(this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE ); 
	splitwin->SetMinimumPaneSize(210);

	wxPanel *left_panel = new wxPanel(splitwin);

	m_varlist = new wxCheckListBox( left_panel, ID_LIST );

	wxBoxSizer *left_tool_sizer = new wxBoxSizer(wxHORIZONTAL);
	// can add widgets with parent 'left_panel' into this sizer (empty for now)

	wxBoxSizer *left_sizer = new wxBoxSizer(wxVERTICAL);
	left_sizer->Add( m_varlist, 1, wxALL|wxEXPAND, 0);
	left_sizer->Add( left_tool_sizer, 0, wxALL|wxEXPAND, 0);

	left_panel->SetSizer( left_sizer );


	m_grid = new WFGridCtrl(splitwin, ID_GRID);
	m_grid->EnableEditing(false);
	m_grid->DisableDragCell();
	//mGrid->DisableDragColSize();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetDefaultCellAlignment( wxALIGN_RIGHT, wxALIGN_CENTER );
	m_grid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );

	splitwin->SplitVertically(left_panel, m_grid, 210);


	wxBoxSizer *szv_main = new wxBoxSizer(wxVERTICAL);
	szv_main->Add( tb_sizer, 0, wxALL|wxEXPAND, 1 );
	szv_main->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND);
	szv_main->Add( splitwin, 1, wxALL|wxEXPAND, 1 );

	SetSizer( szv_main );

}	

wxArrayString DataView::GetSelections()
{
	return m_selections;
}

void DataView::SetSelections(const wxArrayString &sel)
{
	m_selections = sel;
	int i=0;
	while (i < m_selections.Count())
	{
		if ( m_names.Index( m_selections[i] ) == wxNOT_FOUND )
			m_selections.RemoveAt(i);
		else
		{
			i++;
		}
	}

	for (unsigned int idx=0;idx<m_names.Count();idx++)
		m_varlist->Check( idx, (m_selections.Index( m_names[idx] ) >= 0) );	
}

void DataView::UpdateView()
{
	wxArrayString sel_list = m_selections;

	m_names.Clear();

	m_varlist->Clear();

	if (m_vt != NULL)
	{
		const char *name = m_vt->first();
		while (name)
		{
			int idx = m_varlist->Append(  name );
			m_varlist->Check( idx, false );
			m_names.Add( name );

			name = m_vt->next();
		}
	}
	
	SetSelections( sel_list );
	UpdateGrid();
}
	
void DataView::UpdateGrid()
{
	m_grid->Freeze();
	
	if (m_grid_table) m_grid_table->Detach();

	m_grid_table = new DataView::Table;
	m_grid_table->SetData( m_selections, m_vt, true );
	m_grid_table->SetAttrProvider( new WFGridCellAttrProvider );
	m_grid->SetTable( m_grid_table, true );
	m_grid->SetRowLabelSize(60);
	
	m_grid->SetColLabelSize( wxGRID_AUTOSIZE );
	m_grid->Thaw();


	m_grid->Layout();
	m_grid->GetParent()->Layout();
	m_grid->ForceRefresh();
}

void DataView::OnCommand(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_ADD_VARIABLE:
		{
			wxString name = wxGetTextFromUser("Enter variable name:");
			if (name.IsEmpty()) return;

			if (m_vt)
			{
				m_vt->assign( (const char*)name.c_str(), var_data("'empty'") );
				m_selections.Add( name );
				UpdateView();
			}
		}
		break;
	case ID_EDIT_VARIABLE:
		EditVariable( m_varlist->GetStringSelection() );
		break;
	case ID_DELETE_VARIABLE:
		DeleteVariable( m_varlist->GetStringSelection() );
		break;
	case ID_DELETE_ALL_VARIABLES:
		{
			if (m_vt && wxYES == wxMessageBox("Really delete all variables?", "Query", wxYES_NO))
			{
				m_vt->clear();
				UpdateView();
			}
		}
		break;
	case ID_COPY_CLIPBOARD:
	case ID_UNSELECT_ALL:
		break;
	}
}

void DataView::OnVarListCheck(wxCommandEvent &evt)
{
	int idx = evt.GetSelection();
	if (idx >= 0  && idx < m_names.Count())
	{
		wxString var = m_names[idx];
		
		if (m_varlist->IsChecked(idx) && m_selections.Index(var) == wxNOT_FOUND)
			m_selections.Add( var );

		if (!m_varlist->IsChecked(idx) && m_selections.Index(var) != wxNOT_FOUND)
			m_selections.Remove( var );
	}

	UpdateGrid();
}

void DataView::OnVarListDClick(wxCommandEvent &evt)
{
	EditVariable( m_varlist->GetStringSelection() );
}

void DataView::EditVariable( const wxString &name )
{
	if (name.IsEmpty()) return;

	if (m_vt)
	{
		var_data *v = m_vt->lookup( (const char*)name.c_str() );
		if (!v)
		{
			wxMessageBox("Could not locate variable: " + name);
			return;
		}

		EditVariableFormDialog dlg(this, "Edit Variable: " + name);
		dlg.SetVarData( *v );
		if (dlg.ShowModal() == wxID_OK)
		{
			dlg.GetVarData( *v );
			UpdateView();
		}
	}

}

void DataView::DeleteVariable( const wxString &name )
{
	if (m_vt)
	{
		m_vt->unassign( (const char*)name.c_str() );
		UpdateView();
	}
}
