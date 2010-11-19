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

#include <cml/sl_exec.h>
#include <cml/sl_invoke.h>
#include <cml/sl_parse.h>
#include <cml/sl_stdlib.h>
#include "cml/sl_invoke.h"

#include "sscdev.h"
#include "dataview.h"
#include "automation.h"

DECL_INVOKEFCN( ssc_start )
{
	app_frame->Start();
	return true;
}

DECL_INVOKEFCN( ssc_set )
{
	wxString name;
	IVKARG(0, name, "string");

	var_table *vt = app_frame->GetVarTable();

	switch( args[1]->DataType() )
	{
	case SLVariant::STRING:
		vt->assign( (const char*)name.c_str(), var_data( (const char*)args[1]->AsString() ));
		break;
	case SLVariant::INTEGER:
		vt->assign( (const char*)name.c_str(), var_data( (ssc_number_t)args[1]->AsInteger() ));
		break;
	case SLVariant::DOUBLE:	
		vt->assign( (const char*)name.c_str(), var_data( (ssc_number_t)args[1]->AsDouble() ));
		break;	
	case SLVariant::BOOLEAN:
		vt->assign( (const char*)name.c_str(), var_data( (ssc_number_t)(args[1]->AsBoolean() ? 1 : 0) ));
		break;	
	case SLVariant::ARRAY:
		{
			ssc_number_t *pvals = new ssc_number_t[ args[1]->Count() ];
			for (int i=0;i<args[1]->Count();i++)
				pvals[i] = (ssc_number_t) args[1]->GetItem(i).AsDouble();

			vt->assign( (const char*)name.c_str(), var_data( pvals, args[1]->Count() ) );

			delete [] pvals;
		}
		break;
	default:
		return false;
	}
	
	app_frame->GetDataView()->UpdateView();
	return true;
}

DECL_INVOKEFCN( ssc_get )
{
	wxString name;
	IVKARG(0, name, "string");
	
	var_table *vt = app_frame->GetVarTable();

	var_data *v = vt->lookup( (const char*)name.c_str() );
	if (!v) return true;

	switch(v->type)
	{
	case SSC_STRING:
		retval->Assign( v->str.c_str() );
		break;
	case SSC_NUMBER:
		retval->Assign( (double) (ssc_number_t) v->num );
		break;
	case SSC_ARRAY:
		{
			retval->MakeEmptyArray();
			retval->Resize( v->num.length() );
			for (int i=0;i<v->num.length();i++)
				retval->GetItem(i).Assign( (double) v->num[i] );
		}
		break;
	}

	return true;
}

DECL_INVOKEFCN( ssc_save_state )
{
	wxString fn;
	IVKARG(0, fn, "string");
	retval->Assign( app_frame->WriteToDisk( fn ) );
	return true;
}

DECL_INVOKEFCN( ssc_load_state )
{
	wxString fn;
	IVKARG(0, fn, "string");
	retval->Assign( app_frame->Load( fn ) );
	return true;
}

void AppendSSCInvokeFunctions(SLInvokeTable *tab)
{
	tab->Add( ssc_start, "ssc_start", 0, "Start SSC simulation", "(NONE):NONE" );
	tab->Add( ssc_set, "ssc_set", 2, "Set variable value", "(STRING:name, VARIANT:value):NONE");
	tab->Add( ssc_get, "ssc_get", 1, "Get variable value", "(STRING:name):VARIANT");
	tab->Add( ssc_save_state, "ssc_save_state", 1, "Save current variable state", "(STRING:file):BOOLEAN");
	tab->Add( ssc_load_state, "ssc_load_state", 1, "Load a variable state file", "(STRING:file):BOOLEAN");
}