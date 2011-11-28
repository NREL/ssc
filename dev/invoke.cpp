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

DECL_INVOKEFCN( ssc_clear )
{
	app_frame->GetVarTable()->clear();
	app_frame->GetDataView()->UpdateView();
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

DECL_INVOKEFCN( ssc_modules )
{
	wxString cms;
	IVKARG(0, cms, "string");

	wxArrayString list = Split( cms, "," );

	bool ok = true;
	app_frame->ClearCMs();
	for (size_t i=0;i<list.Count();i++)
		ok = ok && app_frame->AddCM( list[i] );

	retval->Assign( ok );

	return true;
}

DECL_INVOKEFCN( ssc_param )
{
	if (args.count() < 3)
	{
		ivkobj.Messages.Add("insufficient number of arguments passed to ssc_param");
		return false;
	}

	wxString cm, param;
	IVKARG(0, cm, "string");
	IVKARG(1, param, "string");
	ssc_number_t value_num;
	wxString value_str;
	int type;
	if (args[2]->DataType() == SLVariant::DOUBLE)
	{
		type = SSC_NUMBER;
		value_num = args[2]->AsDouble();
	}
	else if (args[2]->DataType() == SLVariant::INTEGER)
	{
		type = SSC_NUMBER;
		value_num = args[2]->AsInteger();
	}
	else if (args[2]->DataType() == SLVariant::BOOLEAN)
	{
		type = SSC_NUMBER;
		value_num = args[2]->AsBoolean() ? 1.0f : 0.0f;
	}
	else
	{
		type = SSC_STRING;
		value_str = args[2]->AsString();
	}

	if (type == SSC_NUMBER) value_str = wxString::Format("%lg", value_num);

	retval->Assign( app_frame->SetCMParam( cm, param, value_str, type ) );
	return true;
}

DECL_INVOKEFCN( ssc_clear_params )
{
	wxString cm;
	IVKARG(0, cm, "string");
	retval->Assign( app_frame->ClearCMParams( cm )  );
	return true;
}

void AppendSSCInvokeFunctions(SLInvokeTable *tab)
{
	tab->Add( ssc_start, "ssc_start", 0, "Start SSC simulation", "(NONE):NONE" );
	tab->Add( ssc_clear, "ssc_clear", 0, "Clears all variables", "(NONE):NONE" );
	tab->Add( ssc_set, "ssc_set", 2, "Set variable value", "(STRING:name, VARIANT:value):NONE");
	tab->Add( ssc_get, "ssc_get", 1, "Get variable value", "(STRING:name):VARIANT");
	tab->Add( ssc_save_state, "ssc_save_state", 1, "Save current variable state", "(STRING:file):BOOLEAN");
	tab->Add( ssc_load_state, "ssc_load_state", 1, "Load a variable state file", "(STRING:file):BOOLEAN");
	tab->Add( ssc_modules, "ssc_modules", 1, "Sets the current list of compute modules, comma separated list", "(STRING:cm list):BOOLEAN");
	tab->Add( ssc_param, "ssc_param", 3, "Sets a compute module parameter, number or string", "(STRING:cm name, STRING:param name, [STRING|NUMBER]:value):BOOLEAN");
	tab->Add( ssc_clear_params, "ssc_clear_params", 1, "Clears all parameters for a compute module", "(STRING:cm name):BOOLEAN");
}
