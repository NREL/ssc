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

class DataView::Table : public wxGridTableBase
{
public:
	
	struct Item
	{
		Item() : data_type(SSC_INVALID) { }
		Item(const wxString &name, const wxString &str) : var_name(name), data_type(SSC_STRING), strbuf(str) {  }
		Item(const wxString &name, ssc_number_t value ) : var_name(name), data_type(SSC_NUMBER) { values.resize(1,1); values.at(0,0) = value; }
		Item(const wxString &name, const ssc_number_t *parray, int len) : var_name(name), data_type(SSC_ARRAY) {
			values.resize(len, 1);
			for (int i=0;i<len;i++)
				values.at(i,0) = parray[i];
		}
		Item( const wxString &name, const ssc_number_t *pmatrix, int nr, int nc ) : var_name(name), data_type(SSC_MATRIX) {
			values.resize(nr,nc);
			for (int r=0;r<nr;r++)
				for (int c=0;c<nc;c++)
					values.at(r,c) = pmatrix[r*nc+c];
		}

		wxString var_name;
		int data_type;
		Mat2D<ssc_number_t> values;
		wxString strbuf;
	};

	Table()	{ }
	
	virtual int GetNumberRows()
	{
		int max0 = 0;
		for (int i=0;i<m_data.count();i++)
		{
			int len = 0;
			if ( m_data[i].data_type == SSC_STRING )
				len = 1;
			else len = m_data[i].values.nrows()*m_data[i].values.ncols();

			if (len > max0) max0 = len;
		}
		return max0;
	}
	virtual int GetNumberCols()
	{
		return m_data.count();
	}

	virtual bool IsEmptyCell(int row, int col)
	{
		if ( col < 0 || col >= m_data.count() || row < 0 ) return true;

		if ( m_data[col].data_type == SSC_STRING && row >= 1 ) return true;

		if ( row >= m_data[col].values.nrows()*m_data[col].values.ncols() ) return true;

		return false;
	}

	virtual wxString GetValue( int row, int col )
	{
		if (col >= 0 && col < m_data.length() && row >= 0 
			&& ((m_data[col].data_type != SSC_STRING && row < m_data[col].values.ncells() /* numeric type */)
			    || row < 1 /* string type */))
		{
			if (m_data[col].data_type == SSC_STRING) return m_data[col].strbuf;
			else return wxString::Format("%lg",(double)m_data[col].values.data()[row]);
		}
		else return wxEmptyString;
	}

	virtual void SetValue( int, int, const wxString &)
	{
		/* nothing to do */
	}

	virtual wxString GetColLabelValue(int col)
	{
		if (col >= 0 && col < m_data.count())
			return m_data[col].var_name;
		else
			return "<unknown>";
	}
	
	void SetData( Array<Table::Item> &items )
	{
		m_data = items;
	}

private:
	Array<Item> m_data;
};



enum { ID_COPY_CLIPBOARD = 2315,
	   ID_DELETE_VARIABLES,
	   ID_UNSELECT_ALL,
	   ID_TREE,
	   ID_GRID };

BEGIN_EVENT_TABLE( DataView, wxPanel )
	EVT_BUTTON( ID_COPY_CLIPBOARD, DataView::OnCommand )
	EVT_BUTTON( ID_DELETE_VARIABLES, DataView::OnCommand )
	EVT_BUTTON( ID_UNSELECT_ALL, DataView::OnCommand )
	EVT_TREE_ITEM_ACTIVATED( ID_TREE, DataView::OnVarTree )
END_EVENT_TABLE()


DataView::DataView( wxWindow *parent ) 
	: wxPanel( parent ),
	m_pdata(0),
	m_root_item(0),
	m_grid_table(0)
{
	wxBoxSizer *tb_sizer = new wxBoxSizer(wxHORIZONTAL);
	tb_sizer->Add( new wxStaticText(this, -1, "  Choose Simulation: "), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	tb_sizer->Add( new wxButton( this, ID_COPY_CLIPBOARD, "Copy to clipboard"), 0, wxEXPAND|wxALL, 2);
	tb_sizer->AddStretchSpacer(1);

	wxSplitterWindow *splitwin = new wxSplitterWindow(this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE ); 
	splitwin->SetMinimumPaneSize(210);

	wxPanel *left_panel = new wxPanel(splitwin);

	m_tree = new AFTreeView(left_panel, ID_TREE);

	wxBoxSizer *left_tool_sizer = new wxBoxSizer(wxHORIZONTAL);
	left_tool_sizer->Add( new wxButton(left_panel, ID_DELETE_VARIABLES, "Delete variables"), 0, wxALL|wxEXPAND, 2);

	wxBoxSizer *left_sizer = new wxBoxSizer(wxVERTICAL);
	left_sizer->Add( m_tree, 1, wxALL|wxEXPAND, 0);
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

void DataView::UpdateView()
{
	m_tree_items.clear();
	m_tree->DeleteAllItems();

	m_root_item = m_tree->AddRoot("Variables",
		AFTreeView::ICON_REMOVE, AFTreeView::ICON_REMOVE);

	if (m_pdata != NULL)
	{
		const char *name = ::ssc_data_first( m_pdata );
		while (name)
		{
			wxTreeItemId id = m_tree->AppendItem( m_root_item, name, AFTreeView::ICON_CHECK_FALSE, -1);
			m_tree->Check(id, false);
			m_tree_items.append( id );
			m_tree_names.Add( name );

			name = ::ssc_data_next( m_pdata );
		}
	}
	
	m_tree->Expand(m_root_item);
	m_tree->UnselectAll();

	int i=0;
	while (i<m_tree_selections.Count())
	{
		int idx = m_tree_names.Index( m_tree_selections[i] );
		if (idx < 0)
			m_tree_selections.RemoveAt(i);
		else
		{
			m_tree->Check( m_tree_items[idx], true );
			m_tree->EnsureVisible( m_tree_items[idx] );
			i++;
		}
	}
	
	UpdateGrid();
}
	
void DataView::UpdateGrid()
{
	m_grid->Freeze();
	
	Array<DataView::Table::Item> items;

	if (m_pdata != NULL)
	{
		for (int i=0;i<m_tree_selections.Count();i++)
		{
			int d_type = ::ssc_data_query( m_pdata, m_tree_selections[i].c_str());
			if (d_type == SSC_STRING )
			{
				const char *pstr = ::ssc_data_get_string( m_pdata, m_tree_selections[i].c_str() );
				items.append( Table::Item( m_tree_selections[i], wxString( pstr ) ) );
			}
			else if (d_type == SSC_NUMBER)
			{
				ssc_number_t value = 0;
				::ssc_data_get_number( m_pdata, m_tree_selections[i].c_str(), &value );
				items.append( Table::Item(m_tree_selections[i], value ) );
			}
			else if (d_type == SSC_ARRAY)
			{
				const ssc_number_t *parray; int length;
				parray = ::ssc_data_get_array( m_pdata, m_tree_selections[i].c_str(), &length );
				items.append( Table::Item(m_tree_selections[i], parray, length) );
			}
			else if (d_type == SSC_MATRIX)
			{
				const ssc_number_t *pmatrix; int nrows, ncols;
				pmatrix = ::ssc_data_get_matrix( m_pdata, m_tree_selections[i].c_str(), &nrows, &ncols );
				items.append( Table::Item(m_tree_selections[i], pmatrix, nrows, ncols) );
			}
		}
	}

	m_grid_table = new DataView::Table;
	m_grid_table->SetData( items );
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
	case ID_COPY_CLIPBOARD:
	case ID_DELETE_VARIABLES:
	case ID_UNSELECT_ALL:
	}
}

void DataView::OnVarTree(wxTreeEvent &evt)
{
	wxTreeItemId item = evt.GetItem();
	int idx = m_tree_items.find(item);
	if (idx >= 0)
	{
		wxString name = m_tree_names[idx];

		if ( m_tree->IsChecked(item) && m_tree_selections.Index( name ) == wxNOT_FOUND)
			m_tree_selections.Add( name );
		
		if (!m_tree->IsChecked(item) && m_tree_selections.Index( name ) != wxNOT_FOUND)
			m_tree_selections.Remove( name );

		UpdateGrid();
	}
	evt.Skip();
}