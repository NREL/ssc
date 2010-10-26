#ifndef __dataview_h
#define __dataview_h

#include <wx/panel.h>
#include <cml/afeditctrls.h>
#include <cml/wfgridctrl.h>
#include <wx/checklst.h>

#include "dllinvoke.h"

class DataView : public wxPanel
{
public:

	class Table; // forward

	DataView( wxWindow *parent );
	virtual ~DataView() { m_vt = NULL; }

	void SetDataObject( var_table *vt ) { m_vt = vt; UpdateView(); }
	ssc_data_t GetDataObject() { return m_vt; }

	void UpdateView();	
	void UpdateGrid();

	void EditVariable(const wxString &name);
	void DeleteVariable(const wxString &name);

	wxArrayString GetSelections();
	void SetSelections(const wxArrayString &sel);

private:
	void OnCommand(wxCommandEvent &evt);
	void OnVarListCheck(wxCommandEvent &evt);
	void OnVarListDClick(wxCommandEvent &evt);

	WFGridCtrl *m_grid;
	Table *m_grid_table;
	wxCheckListBox *m_varlist;

	wxTreeItemId m_root_item;
	Array<wxTreeItemId> m_tree_items;
	wxArrayString m_names;
	wxArrayString m_selections;

	var_table *m_vt;

	DECLARE_EVENT_TABLE();
};

#endif
