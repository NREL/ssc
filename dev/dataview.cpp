#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/mdi.h>
#include <wx/config.h>
#include <wx/busyinfo.h>
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
#include <cml/wpplotdataarray.h>
#include <cml/wpplotsurface2d.h>
#include <cml/wpbarplot.h>
#include <cml/wplinearaxis.h>
#include <cml/wplineplot.h>

#include <cml/dview/wxdvplotctrl.h>
#include <cml/dview/wxdvarraydataset.h>

#include "dataview.h"
#include "editvariableform.h"
#include "statform.h"


class DataView::Table : public wxGridTableBase
{
public:

	Table()
	{
		m_vt_ref = 0;
		m_attr = new wxGridCellAttr;
		m_attr->SetBackgroundColour( wxColour( 240,240,240 ) );
		m_attr->SetTextColour( "navy" );
		m_attr->SetFont( wxFont(9, wxMODERN, wxNORMAL, wxNORMAL) );
	}

	virtual ~Table()
	{		
		m_attr->DecRef();
		m_vt_ref = 0;
	}

	

	
    virtual wxGridCellAttr *GetAttr(int row, int col,
                                    wxGridCellAttr::wxAttrKind  kind)
	{
		if (col >= 0 && col < m_items.Count())
		{
			if (!m_vt_ref) return NULL;
			var_data *v = m_vt_ref->lookup( (const char*)m_items[col].c_str() );
			if (!v) return NULL;

			if (v->type != SSC_MATRIX) return NULL;

			m_attr->IncRef();
			return m_attr;
		}
		else
			return NULL;
	}

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

			int len = 1;
			if (v->type == SSC_ARRAY) len = v->num.length();
			else if (v->type == SSC_MATRIX) len = v->num.nrows();
			else if (v->type == SSC_TABLE) len = v->table.size();

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

		if ( v->type == SSC_ARRAY && row >= v->num.length() ) return true;
		
		if ( v->type == SSC_MATRIX && row >= v->num.nrows() ) return true;

		if ( v->type == SSC_TABLE && row >= v->table.size() ) return true;

		return false;
	}

	virtual wxString GetValue( int row, int col )
	{
		if (m_vt_ref && col >= 0 && col < m_items.Count())
		{
			var_data *v = m_vt_ref->lookup( (const char*)m_items[col].c_str() );
			if (!v) return "<lookup error>";

			if (v->type == SSC_STRING && row == 0) return wxString(v->str.c_str());
			else if (v->type == SSC_NUMBER && row == 0) return wxString::Format("%lf", (double) v->num);
			else if (v->type == SSC_ARRAY && row < v->num.length()) return wxString::Format("%lf", (double)v->num[row]);
			else if (v->type == SSC_MATRIX && row < v->num.nrows())
			{
				wxString ret;
				for (int j=0;j<v->num.ncols();j++)
				{
					ret += wxString::Format("%*lf", 13, (double)v->num.at(row, j));
				}

				return ret;
			}
			else if (v->type == SSC_TABLE && row < v->table.size())
			{
				int k = 0;
				const char *key = v->table.first();
				while (key != 0)
				{
					if (k++ == row) break;
					key = v->table.next();
				}
				return ".{'" + wxString(key) + "'}";
			}
		}
		
		return wxEmptyString;
	}

	virtual wxString GetColLabelValue(int col)
	{
		if (col >= 0 && col < m_items.Count())
		{

			if (!m_vt_ref) return m_items[col];
			else return m_items[col];
		}
		else
			return "<unknown>";
	}
	
	void SetData( const wxArrayString &items, var_table *vt, bool ro)
	{
		m_items = items;
		m_vt_ref = vt;
	}

	virtual void SetValue(int,int,const wxString &)
	{
		/* nothing to do */
	}

private:
    wxGridCellAttr *m_attr;
	var_table *m_vt_ref;
	wxArrayString m_items;
};



enum { ID_COPY_CLIPBOARD = 2315,
	   ID_LIST,
	   ID_SHOW_STATS,
	   ID_ADD_VARIABLE,
	   ID_EDIT_VARIABLE,
	   ID_DELETE_VARIABLE,
	   ID_DELETE_ALL_VARIABLES,
	   ID_SELECT_ALL,
	   ID_UNSELECT_ALL,
	   ID_DELETE_CHECKED,
	   ID_DELETE_UNCHECKED,
	   ID_DVIEW,
	   ID_POPUP_EDIT,
	   ID_POPUP_DELETE,
	   ID_POPUP_STATS,
	   ID_POPUP_PLOT_BAR,
	   ID_POPUP_PLOT_LINE,
	   ID_GRID };

BEGIN_EVENT_TABLE( DataView, wxPanel )
	EVT_BUTTON( ID_COPY_CLIPBOARD, DataView::OnCommand )
	EVT_BUTTON( ID_UNSELECT_ALL, DataView::OnCommand )
	EVT_BUTTON( ID_ADD_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_EDIT_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_VARIABLE, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_ALL_VARIABLES, DataView::OnCommand )
	EVT_BUTTON( ID_SELECT_ALL, DataView::OnCommand )
	EVT_BUTTON( ID_UNSELECT_ALL, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_CHECKED, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_UNCHECKED, DataView::OnCommand )
	EVT_BUTTON( ID_SHOW_STATS, DataView::OnCommand )
	EVT_BUTTON( ID_DVIEW, DataView::OnCommand )
	EVT_CHECKLISTBOX( ID_LIST, DataView::OnVarListCheck )
	EVT_LISTBOX_DCLICK( ID_LIST, DataView::OnVarListDClick )
	EVT_GRID_CMD_LABEL_RIGHT_CLICK( ID_GRID, DataView::OnGridLabelRightClick )
	EVT_GRID_CMD_LABEL_LEFT_DCLICK( ID_GRID, DataView::OnGridLabelDoubleClick )
	
	EVT_MENU( ID_POPUP_EDIT, DataView::OnPopup )
	EVT_MENU( ID_POPUP_DELETE, DataView::OnPopup )
	EVT_MENU( ID_POPUP_STATS, DataView::OnPopup )
	EVT_MENU( ID_POPUP_PLOT_BAR, DataView::OnPopup )
	EVT_MENU( ID_POPUP_PLOT_LINE, DataView::OnPopup )

END_EVENT_TABLE()


DataView::DataView( wxWindow *parent ) 
	: wxPanel( parent ),
	m_vt(0),
	m_root_item(0),
	m_grid_table(0)
{
	SetBackgroundColour( *wxWHITE );

	wxBoxSizer *tb_sizer = new wxBoxSizer(wxVERTICAL);
	tb_sizer->Add( new wxButton(this, ID_ADD_VARIABLE, "Add..."), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_EDIT_VARIABLE, "Edit..."), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_VARIABLE, "Delete"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_CHECKED, "Del checked"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_UNCHECKED, "Del unchecked"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_DELETE_ALL_VARIABLES, "Del all"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_SELECT_ALL, "Select all"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton(this, ID_UNSELECT_ALL, "Unselect all"), 0, wxALL|wxEXPAND, 2);
	tb_sizer->Add( new wxButton( this, ID_COPY_CLIPBOARD, "Copy to clipboard"), 0, wxEXPAND|wxALL, 2);
	tb_sizer->Add( new wxButton( this, ID_SHOW_STATS, "Show stats..."), 0, wxEXPAND|wxALL, 2);
	tb_sizer->Add( new wxButton( this, ID_DVIEW, "Timeseries graph..."), 0, wxEXPAND|wxALL, 2);
	tb_sizer->AddStretchSpacer(1);

	wxSplitterWindow *splitwin = new wxSplitterWindow(this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE ); 
	splitwin->SetMinimumPaneSize(210);

	wxPanel *left_panel = new wxPanel(splitwin);

	m_varlist = new wxCheckListBox( left_panel, ID_LIST );
	m_varlist->SetFont( wxFont(9, wxMODERN, wxNORMAL, wxNORMAL) );

	wxBoxSizer *left_tool_sizer = new wxBoxSizer(wxHORIZONTAL);
	// can add widgets with parent 'left_panel' into this sizer (empty for now)

	wxBoxSizer *left_sizer = new wxBoxSizer(wxVERTICAL);
	left_sizer->Add( m_varlist, 1, wxALL|wxEXPAND, 0);
	left_sizer->Add( left_tool_sizer, 0, wxALL|wxEXPAND, 0);

	left_panel->SetSizer( left_sizer );


	m_grid = new WFGridCtrl(splitwin, ID_GRID);
	m_grid->SetFont( wxFont(8, wxMODERN, wxNORMAL, wxNORMAL) );
	m_grid->EnableEditing(false);
	m_grid->EnableCopyPaste(false);
	m_grid->DisableDragCell();
	//mGrid->DisableDragColSize();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetDefaultCellAlignment( wxALIGN_RIGHT, wxALIGN_CENTER );
	m_grid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );

	splitwin->SplitVertically(left_panel, m_grid, 290);


	wxBoxSizer *szv_main = new wxBoxSizer(wxHORIZONTAL);
	szv_main->Add( tb_sizer, 0, wxALL|wxEXPAND, 1 );
	szv_main->Add( new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL ), 0, wxALL|wxEXPAND, 1);
	szv_main->Add( splitwin, 1, wxALL|wxEXPAND, 1 );

	SetSizer( szv_main );

}	

Array<int> DataView::GetColumnWidths()
{
	Array<int> list;
	for (int i=0;i<m_grid->GetNumberCols();i++)
		list.append( m_grid->GetColumnWidth( i ) );
	return list;
}

void DataView::SetColumnWidths( const Array<int> &cwl )
{
	for (int i=0;i<cwl.count() && i<m_grid->GetNumberCols();i++)
		m_grid->SetColumnWidth( i, cwl[i] );
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
		int padto = 0;
		const char *name = m_vt->first();
		while (name)
		{
			int len = strlen(name);
			if (len > padto) padto = len;
			name = m_vt->next();
		}

		padto += 2;


		wxArrayString labels;
		name = m_vt->first();
		while (name)
		{
			m_names.Add( name );
			wxString label = name;

			if (var_data *v = m_vt->lookup(name))
			{
				for (int j=0;j< padto-strlen(name);j++)
					label += ' ';

				label += wxString(v->type_name());
				if (v->type == SSC_NUMBER)
					label += " " + FloatToStr( (ssc_number_t) v->num );				
				else if (v->type == SSC_STRING)
					label += " " + wxString(v->str.c_str());
				else if (v->type == SSC_ARRAY)
					label += wxString::Format( " [%d]", v->num.length() );
				else if (v->type == SSC_MATRIX)
					label += wxString::Format(" [%d,%d]", v->num.nrows(), v->num.ncols() );
				else if (v->type == SSC_TABLE)
					label += wxString::Format(" { %d }", (int) v->table.size() );
			}

			labels.Add( label );

			name = m_vt->next();
		}

		SortByLabels(m_names, labels );
		for (int i=0;i<m_names.Count();i++)
		{
			int idx = m_varlist->Append( labels[i]);
			m_varlist->Check( idx, false );
		}
	}
	
	SetSelections( sel_list );
	UpdateGrid();
}
	
void DataView::UpdateGrid()
{
//	Array<int> cwl = GetColumnWidths();
	m_grid->Freeze();
	
	if (m_grid_table) m_grid_table->Detach();

	m_grid_table = new DataView::Table;
	m_grid_table->SetData( m_selections, m_vt, true );
	m_grid->SetTable( m_grid_table, true );
	m_grid->SetRowLabelSize(60);
	m_grid->SetColLabelSize( wxGRID_AUTOSIZE );
	m_grid->Thaw();
	
	m_grid->Layout();
	m_grid->GetParent()->Layout();
//	SetColumnWidths(cwl);
	m_grid->ForceRefresh();
	m_grid->AutoSizeColumns();

}

void DataView::OnCommand(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_SHOW_STATS:
		ShowStats();
		break;
	case ID_DVIEW:
		{
			wxDialog dlg(this, -1, "Timeseries Viewer", wxDefaultPosition, wxSize(900,600), wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE);
			wxDVPlotCtrl *dv = new wxDVPlotCtrl( &dlg );
			wxBoxSizer *sz = new wxBoxSizer(wxVERTICAL);
			sz->Add( dv, 1, wxALL|wxEXPAND, 0 );
			sz->Add( dlg.CreateButtonSizer(wxOK), 0, wxALL|wxEXPAND, 0 );
			dlg.SetSizer(sz);
			
			Vector<double> da(8760);
			int iadded = 0;
			for (size_t i=0;i<m_selections.Count();i++)
			{
				var_data *v = m_vt->lookup( (const char*) m_selections[i].c_str() );
				if ( v != 0 
					&& v->type == SSC_ARRAY
					&& v->num.length() == 8760)
				{
					for (int k=0;k<8760;k++)
						da[k] = v->num[k];

					dv->AddDataSet(  new wxDVArrayDataSet( m_selections[i], da ) );
					iadded++;
				}
			}
			
			if (iadded == 0)
				wxMessageBox("Please check one or more array variables with 8760 values to show in the timeseries viewer.");
			else
			{
				dv->SelectDataOnBlankTabs();
				dlg.ShowModal();
			}

		}
		break;
	case ID_SELECT_ALL:
		{
			m_selections.Clear();
			const char *name = m_vt->first();
			while (name)
			{
				m_selections.Add( name );
				name = m_vt->next();
			}
			UpdateView();
		}
		break;
	case ID_UNSELECT_ALL:
		m_selections.Clear();
		UpdateView();
		break;
	case ID_DELETE_CHECKED:
		{
			wxArrayString list = m_selections;
			for (int i=0;i<list.Count();i++)
				DeleteVariable(list[i]);
		}
		break;
	case ID_DELETE_UNCHECKED:
		{
			wxArrayString list;
				
			const char *name = m_vt->first();
			while (name)
			{
				list.Add( name );
				name = m_vt->next();
			}

			for (int i=0;i<m_selections.Count();i++)
				list.Remove( m_selections[i] );
			
			for (int i=0;i<list.Count();i++)
				DeleteVariable(list[i]);
		}
		break;	
	case ID_ADD_VARIABLE:
		AddVariable();
		break;
	case ID_EDIT_VARIABLE:
		EditVariable();
		break;
	case ID_DELETE_VARIABLE:
		DeleteVariable();
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
		{
			wxBusyInfo busy("Copying to clipboard...");
			m_grid->Copy(true, true);
		}
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

void DataView::AddVariable()
{	
	wxString name = wxGetTextFromUser("Enter variable name:");
	if (name.IsEmpty()) return;
			
	if (m_vt)
	{
		if (m_vt->lookup( (const char*)name.c_str() ))
			if (wxNO==wxMessageBox("That var exists. overwrite with a new one?", "Q", wxYES_NO))
				return;

		m_vt->assign( (const char*)name.c_str(), var_data( (ssc_number_t)0.0 ) );
		if (m_selections.Index( name ) == wxNOT_FOUND)
			m_selections.Add( name );
		UpdateView();

		EditVariable( name );
	}
}

void DataView::OnVarListDClick(wxCommandEvent &evt)
{
	EditVariable();
}

wxString DataView::GetSelection()
{
	int n = m_varlist->GetSelection();
	if (n >= 0 && n < m_names.Count())
		return m_names[n];
	else
		return wxEmptyString;
}

void DataView::EditVariable( wxString name )
{
	if (name.IsEmpty()) name = GetSelection();
	if (name.IsEmpty()) return;
	if (!m_vt) return;

	var_data *v = m_vt->lookup( (const char*)name.c_str() );
	if (!v)
	{
		wxMessageBox("Could not locate variable: " + name);
		return;
	}

	if (v->type == SSC_TABLE)
	{
		wxDialog dlg( this, wxID_ANY, "Edit table: " + name, 
			wxDefaultPosition, wxSize(850,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
		DataView *dv = new DataView( &dlg );
		wxSizer *sz = new wxBoxSizer(wxVERTICAL);
		sz->Add(dv, 1, wxALL|wxEXPAND, 0 );
		sz->Add( new wxButton( &dlg, wxID_OK, "Close" ),0,wxALL,3 );
		dv->SetDataObject( &v->table );
		dlg.SetSizer( sz );
		dlg.ShowModal();
		UpdateView();
	}
	else
	{
		EditVariableFormDialog dlg(this, "Edit Variable: " + name);
		dlg.SetVarData( *v );
		if (dlg.ShowModal() == wxID_OK)
		{
			dlg.GetVarData( *v );
			UpdateView();
		}
	}
}

void DataView::DeleteVariable( wxString name )
{
	if (name.IsEmpty()) name = GetSelection();
	if (name.IsEmpty()) return;

	if (m_vt)
	{
		m_vt->unassign( (const char*)name.c_str() );
		UpdateView();
	}
}

void DataView::ShowStats( wxString name )
{
	if (name.IsEmpty()) name = GetSelection();
	if (name.IsEmpty()) return;

	if (m_vt)
	{
		var_data *v = m_vt->lookup((const char*) name.c_str() );
		if (!v || v->type != SSC_ARRAY)
		{
			wxMessageBox("variable not found or not of array type.");
			return;
		}

		StatFormDialog dlg(this, "Stats for: " + name);
		dlg.Compute( v->num );
		dlg.ShowModal();
	}
}

void DataView::OnGridLabelRightClick(wxGridEvent &evt)
{
	int col = evt.GetCol();
	if (col < 0 || col >= m_selections.Count()) return;
	
	m_popup_var_name = m_selections[col];

	wxMenu popup;
	popup.Append( ID_POPUP_EDIT, "Edit..." );
	popup.AppendSeparator();
	popup.Append( ID_POPUP_DELETE, "Delete..." );
	popup.AppendSeparator();
	popup.Append( ID_POPUP_STATS, "Statistics...");
	popup.Append( ID_POPUP_PLOT_BAR, "Bar plot (array only)" );
	popup.Append( ID_POPUP_PLOT_LINE, "Line plot (array only)" );

	m_grid->PopupMenu( &popup, evt.GetPosition() );
}

void DataView::OnGridLabelDoubleClick(wxGridEvent &evt)
{
	int col = evt.GetCol();
	if (col < 0 || col >= m_selections.Count()) return;
	EditVariable( m_selections[col] );
}

void DataView::OnPopup(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_POPUP_EDIT:
		EditVariable( m_popup_var_name );
		break;
	case ID_POPUP_DELETE:
		if (wxYES == wxMessageBox("Really delete variable: " + m_popup_var_name, "Query", wxYES_NO ))
			DeleteVariable( m_popup_var_name );
		break;
	case ID_POPUP_STATS:
		ShowStats( m_popup_var_name );
		break;
	case ID_POPUP_PLOT_BAR:
	case ID_POPUP_PLOT_LINE:
		{
			if (!m_vt) return;
			var_data *v = m_vt->lookup( (const char*) m_popup_var_name.c_str() );
			if (!v || v->type != SSC_ARRAY)
			{
				wxMessageBox("variable not found or not of array type.");
				return;
			}

			wxFrame *frm = new wxFrame(this, -1, "plot: " + m_popup_var_name, wxDefaultPosition, wxSize(500,350));

			if ( v->num.length() == 8760 )
			{
				wxDVPlotCtrl *dv = new wxDVPlotCtrl( frm );
				Vector<double> da(8760);
				for (int i=0;i<8760;i++)
					da[i] = v->num[i];

				dv->AddDataSet(  new wxDVArrayDataSet( m_popup_var_name, da ) );
			}
			else
			{
				WPPlotSurface2D *plotsurf = new WPPlotSurface2D( frm );
			
				WPPlotDataArray *pdat = new WPPlotDataArray;
				for (int i=0;i<v->num.length();i++)
					pdat->append(  PointF( i, v->num[i] ) );
			
				WPPlottable2D *plot = NULL;
				if (evt.GetId() == ID_POPUP_PLOT_BAR) plot = new WPBarPlot;					
				else plot = new WPLinePlot;
				plot->SetData( pdat );
				plot->SetLabel( m_popup_var_name );

				plotsurf->Add( plot );
				plotsurf->SetTitle("Plot of: '" + m_popup_var_name + "'");

				plotsurf->SetXAxis1( new WPLinearAxis( -1, v->num.length() ) );
			}

			frm->Show();
		}
		break;
	}
}
