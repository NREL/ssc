#ifndef __dataview_h
#define __dataview_h

#include <wx/panel.h>
#include <cml/afeditctrls.h>
#include <cml/wfgridctrl.h>

#include "dllinvoke.h"


class DataView : public wxPanel
{
public:

	class Table; // forward

	DataView( wxWindow *parent );
	virtual ~DataView();

	void SetDataObject( ssc_data_t p_dat ) { m_pdata = p_dat; UpdateView(); }
	ssc_data_t GetDataObject() { return m_pdata; }

	void UpdateView();	
	void UpdateGrid();

private:
	void OnCommand(wxCommandEvent &evt);
	void OnVarTree(wxTreeEvent &evt);

	ssc_data_t m_pdata;
	WFGridCtrl *m_grid;
	Table *m_grid_table;
	AFTreeView *m_tree;

	wxTreeItemId m_root_item;
	Array<wxTreeItemId> m_tree_items;
	wxArrayString m_tree_names;
	wxArrayString m_tree_selections;

	DECLARE_EVENT_TABLE();
};

#endif
