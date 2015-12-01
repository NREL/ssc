#include "IOUtil.h"
#include <iostream>
#include <fstream>
#include <string>
#include <string.h>
#include "rapidxml.hpp"
#include "definitions.h"

#ifdef _WIN32
#include <direct.h>
#include <Windows.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

//extern vardefs variable_definition_array;

bool ioutil::file_exists( const char *file )
{
#ifdef _WIN32
	// from wxWidgets: must use GetFileAttributes instead of ansi C 
	// b/c can cope with network (unc) paths
	DWORD ret = ::GetFileAttributesA( file );
	return (ret != (DWORD)-1) && !(ret & FILE_ATTRIBUTE_DIRECTORY);
#else
	struct stat st;
	return stat(file, &st) == 0 && S_ISREG(st.st_mode);
#endif
}

bool ioutil::dir_exists( const char *path )
{
#ifdef _WIN32
	// Windows fails to find directory named "c:\dir\" even if "c:\dir" exists,
	// so remove all trailing backslashes from the path - but don't do this for
	// the paths "d:\" (which are different from "d:") nor for just "\"
	char *wpath = _strdup( path );
	if (!wpath) return false;

	int pos = strlen(wpath)-1;
	while (pos > 1 && (wpath[pos] == '/' || wpath[pos] == '\\'))
	{
		if (pos == 3 && wpath[pos-1] == ':') break;

		wpath[pos] = 0;
		pos--;
	}

	DWORD ret = ::GetFileAttributesA(wpath);
    bool exists =  (ret != (DWORD)-1) && (ret & FILE_ATTRIBUTE_DIRECTORY);

	free( wpath );

	return exists;
#else
	struct stat st;
	return ::stat(path, &st) == 0 && S_ISDIR(st.st_mode);
#endif
}

bool ioutil::remove_file( const char *path )
{
	return 0 == ::remove( path );
}

#ifdef SP_USE_MKDIR

#ifdef _WIN32
#define make_dir(x) ::_mkdir(x)
#else
#define make_dir(x) ::_mkdir(x, 0777)
#endif

bool ioutil::mkdir( const char *path, bool make_full )
{
	if (make_full)
	{
		std::vector<std::string> parts = split( path, "/\\" );
	
		if (parts.size() < 1) return false;
		
		std::string cur_path = parts[0] + path_separator();
		
		for (size_t i=1;i<parts.size();i++)
		{
			cur_path += parts[i];

			if ( !dir_exists(cur_path.c_str()) )
				if (0 != make_dir( cur_path.c_str() ) ) return false;
						
			cur_path += path_separator();
		}

		return true;
	}
	else
		return 0 == make_dir( path );
}
#endif // SP_USE_MKDIR

std::string ioutil::path_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(0, pos);
}

std::string ioutil::name_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}

std::string ioutil::ext_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of('.');
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}
	
char ioutil::path_separator()
{
#ifdef _WIN32
	return '\\';
#else
	return '/';
#endif
}

std::string ioutil::get_cwd()
{
	char buf[2048];
#ifdef _WIN32
	::GetCurrentDirectoryA( 2047, buf );
#else
	::getcwd(buf, 2047);
#endif
	buf[2047] = 0;
	return std::string(buf);
}

bool ioutil::set_cwd( const std::string &path )
{
#ifdef _WIN32
	return ::SetCurrentDirectoryA( path.c_str() ) != 0;
#else
	return ::chdir( path.c_str() ) == 0;
#endif
}


//--------------------

void ioutil::read_chars( FILE *fp, std::string &text, int nchars){
	int c;

	text = "";
	int nc=0;
	while( (c=fgetc(fp)) != EOF && nc < nchars){
		text += (char)c;
		nc++;
	}

}

bool ioutil::read_line( FILE *fp, std::string &buf, int prealloc )
{
	int c;

	buf = "";
	if (prealloc > 10)
		buf.reserve( (size_t)prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += (char)c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.length() == 0 && c == EOF);
}
void ioutil::read_file( const string &fname, string &file, string &eol_marker)
{
	
	file.clear();
	string line;
	ifstream fin(fname.c_str());
	
	eol_marker = "\n";

	if(fin.is_open())
	{
		while( getline(fin, line) )
		{
			file.append(line + "\n");
		}
		fin.close();
	}
	return;

}

void ioutil::parseInputFile(const string &fname,var_set &V, var_set &Defs){
/*

DEPRECATED -- Used only for importing old file versions

This algorithm takes file fname, reads in the contents and assigns the values to a map
structure for later variable assignment. 

The file can contain header lines beginning with the phrase "<HDR>"

The remainder of the file is 2-line combinations following the format:
<VAR> component.instance#.varname units
data line 1
data line 2
...

For example:
<VAR> solarfield.0.tht m
180.000


--The data structure returned is a set of maps 'V' where:
V[<module type, str>][<instance, int>][<variable name, str>] = spvar{name, units, value}

*/
	//Read in the file to a string
	string file;		//contents of the file
	string eol;
	read_file(fname, file, eol);
	
	//Replace all instances of the characters '<' and '>' with '[' and ']', respectively
	ReplaceStringInPlace(file, ">", "]");
	ReplaceStringInPlace(file,"<","[");
	//file.Replace('>',']');
	//file.Replace('<','[');

	std::vector<std::string> lines;
	lines = split(file, eol);

	std::string line, dlines, varmodule, varname, varunits;
	int moduleinst; //vartype, 
	std::vector<std::string> vline, vardef;
	bool inrec=false;
	bool multiline = false;
	//spvar vd; 

	for (unsigned int i=0; i<lines.size(); i++){
		line = lines.at(i);
		if( line.find("[HDR]") != std::string::npos ) continue; // header line, don't parse 
		
		if( line.find("[VAR]") != std::string::npos || i==lines.size()-1) { //new variable line or end of file
			if(! inrec){	// Have we read any data yet?
				//no we haven't...
				inrec = true;	//we have now started reading data that will need to be stored
				//Nothing needs to be stored
			}
			else
			{
				if(i==lines.size()-1){dlines += line;}	//end line of file should be added here

				//Set up variable attributes according to the variable definitions. File data overwritten below...
				V[varmodule][moduleinst][varname] = Defs[varmodule][0][varname];
				//we need to store the info that we've been reading
				V[varmodule][moduleinst][varname].name = varname;
				V[varmodule][moduleinst][varname].units = varunits;
				V[varmodule][moduleinst][varname].value = dlines;

				//clear the data line string
				dlines.clear();
				multiline = false;	//reset the multiline flag
			}

			if(i==lines.size()-1) continue; //on the last line, don't parse as if its a var line

			//parse the variable string, type, and units
			vline = split(line, " ");
			
			//parse the variable string into module, instance, and name
			vardef = split(vline.at(1),".");
			if(vardef.size() != 3){ continue; }
			varmodule = vardef.at(0);
			to_integer(vardef.at(1), &moduleinst);
			varname = vardef.at(2);

			//to_integer(vline.at(2), &vartype);
			if(vline.size() > 2) varunits = vline.at(2);
			

		}
		else
		{
			//Get all of the associated data input lines
			if(multiline) dlines += "\n";	//Prepend a newline character if we're reading a multiline entry
			dlines += line;
			multiline = true;	//Set the multi line flag for the next line, if applicable
						
		}
		
	}

	return;
}

void ioutil::parseXMLInputFile(const string &fname,var_set &V, var_set &Defs, parametric &par_data){
	/*
	This algorithm takes file fname, reads in the contents and assigns the values to a map
	structure for later variable assignment. 

	The data structure for the file is XML format. Expected objects are variables and parametric simulation info.
	Structures are:


	--- 
	note that variables with multiline values (such as the solarfield.x.layout_data variable) would have each line separated by
	the newline "\n" character. 
	----
	<data>
		<version>2013.2.6</version>
		<header>Text for header line</header>

		<variable>
			<component>solarfield</component>
			<instance>0</instance>
			<varname>tht</varname>
			<units>m</units>
			<value>180.000</value>
		</variable>

		<parametric>
			<par_variable>
				<varname>solarfield.0.tht</varname>
				<display_text>Tower height</display_text>
				<units>m</units>
				<data_type>double</data_type>
				<selections>
					<selection>180.0</selection>
					<selection>190.0</selection>
				</selections>
				<choices>
					<choice>170.0</choice>
					<choice>180.0</choice>
					<choice>190.0</choice>
				</choices>
				<sim_values>
					<sim_value>180.0</sim_value>
					<sim_value>180.0</sim_value>
					<sim_value>190.0</sim_value>
					...
				</sim_values>
				<linked>true</linked>
				<layout_required>true</layout_required>
			</par_variable>
			<par_variable>
				...
			</par_variable>
		</parametric>
	</data>

	--The data structure returned is a set of maps 'V' where:
	V[<module type, str>][<instance, int>][<variable name, str>] = spvar{name, units, value}

	*/
	
	using namespace rapidxml;
	//Read in the file to a string
	string file;		//contents of the file
	string eol;
	ioutil::read_file(fname, file, eol);
	
	char *fstr = new char[file.size()+1];
	strncpy(fstr, (const char*)file.c_str(), file.size());
	fstr[file.size()] = 0;	//Null terminator

	xml_document<> doc;
	doc.parse<0>(fstr);
	xml_node<> *top_node = doc.first_node();	//<data>

	//get version
	string version = top_node->first_node("version")->value();

	//skip any header lines for now

    //Clean out the variable map
    V.clear();
    V = Defs;   //makes sure all structures are represented
    
	//Read in all of the variables
	xml_node<> *var_node = top_node->first_node("variable");
    std::string component0 = "";
    int inst0 = -1;
	while(var_node != 0){
		//get the variable name composition
		std::string
			component = (char*)var_node->first_node("component")->value(),
			sinst = (char*)var_node->first_node("instance")->value(),
			varname = (char*)var_node->first_node("varname")->value(),
			units = (char*)var_node->first_node("units")->value();
		int inst;
		to_integer(sinst, &inst);

        //if parsing a new component or instance, first copy the defaults as a basis
        if(component != component0 || inst != inst0 )
            V[component][inst] = Defs[component][0];

        component0 = component;
        inst0 = inst;

		//Set up variable attributes according to the variable definitions. File data overwritten below...
		V[component][inst][varname] = Defs[component][0][varname];
		//we need to store the info that we've been reading
		V[component][inst][varname].name = varname;
		V[component][inst][varname].units = (char*)var_node->first_node("units")->value();
		V[component][inst][varname].value = (char*)var_node->first_node("value")->value();
		
		var_node = var_node->next_sibling("variable");
	}

	//Read in any parametric data
	par_data.clear();
	xml_node<> *par_node = top_node->first_node("parametric");
	if(par_node != 0){
		xml_node<> *par = par_node->first_node("par_variable");
		while(par != 0){
			//Add the variable by reference to the variable map object, then set relevant fields
			par_data.addVar( getVarByString(V, (char*)par->first_node("varname")->value() ) );

			par_variable *pvar = &par_data.back();
			
			//units
			pvar->units = (char*)par->first_node("units")->value();
			//display text
			pvar->display_text = (char*)par->first_node("display_text")->value();
			//data type
			pvar->data_type = (char*)par->first_node("data_type")->value();
			//linked
			pvar->linked = lower_case( (string)(char*)par->first_node("linked")->value() ) == "true";
			//layout required
			pvar->layout_required = lower_case( (string)(char*)par->first_node("layout_required")->value() ) == "true";

			//Selections
			xml_node<> *sel_node = par->first_node("selections")->first_node("selection");
			pvar->selections.Clear();
			while(sel_node != 0){
				pvar->selections.push_back( sel_node->value() );
				sel_node = sel_node->next_sibling();
			}

			//Choices
			xml_node<> *choice_node = par->first_node("choices")->first_node("choice");
			pvar->choices.Clear();
			while(choice_node != 0){
				pvar->choices.push_back( choice_node->value() );
				choice_node = choice_node->next_sibling();
			}

			//Sim values
			xml_node<> *sim_node = par->first_node("sim_values")->first_node("sim_value");
			pvar->sim_values.Clear();
			while(sim_node != 0){
				pvar->sim_values.push_back( sim_node->value() );
				sim_node = sim_node->next_sibling();
			}

			par = par->next_sibling("par_variable");
		}

	}

	return;
}

void ioutil::parseDefinitionArray(var_set &V, string disabled_mods)
{
	//figure out which modules, if any, shouldn't be loaded
	vector<string> vdmods = split(disabled_mods, ";");
	
	//Clean out the variable set
	V.clear();
	string varmodule, var, control, special;
	int i=0; 
	while( true )
	{
		varmodule = variable_definition_array[i].domain; //Variable grouping (solarfield, heliostat...)
		
		//don't even load omitted modules 
		for(vector<string>::iterator mod = vdmods.begin(); mod != vdmods.end(); mod ++){
			if( (string)varmodule == *mod ){
				i++;
				continue;
			}
		}
		
		if( varmodule == "--end--" ) break;

		var = variable_definition_array[i].name; //Variable name
		spvar *vdat = &V[varmodule][0][var];
		vdat->varpath = varmodule + ".0." + var;

		//Get the information for this variable
		vdat->dattype = lower_case(variable_definition_array[i].type); 
		vdat->value = variable_definition_array[i].value; //Default value
		vdat->units = variable_definition_array[i].units; //Units
		vdat->range = variable_definition_array[i].range; //valid range
		vdat->is_param = (string)variable_definition_array[i].isparam == "TRUE";	//is parameterizable?
		control = variable_definition_array[i].control; 
		vdat->ctype = control;	//Control type
		special = variable_definition_array[i].special;
		if(special.size()>0) vdat->choices = split( special, ";" ); 
		vdat->is_disabled = (string)lower_case(variable_definition_array[i].disable) == "x";
		vdat->short_desc = variable_definition_array[i].label;	//Short description - variable label
		vdat->long_desc = variable_definition_array[i].description;	//Long description

		if(control=="combo"){
			//create the options map.. <selection id -> string name>
			for(int j=0; j<(int)vdat->choices.size(); j++){
				vector<string> dat = split(vdat->choices.at(j), "=");
				int vind;
				to_integer(dat.at(1), &vind);
				vdat->index_map[vind] = dat.at(0);
			}
		}
		i++;
		
	}
}

//void ioutil::parseDefinitionFile(const std::string &fname, var_set &V){
//	/*
//	This reads the inputs definition file and parses all of the descriptive data into the variable array. 
//	All of the inputs in the GUI are defined in the GUI text file, and information on labels, units, etc., 
//	are defined within the file. Reading of this file should happen before the input file is parsed so
//	that saved inputs can overwrite the default value.
//	*/
//
//	//Read in the file to a string
//	string file, eol;		//contents of the file
//	read_file(fname, file, eol);
//	
//	std::vector<std::string> lines, line;
//	lines = split(file, eol);
//
//	std::string varmodule, var, control, special;
//
//	int i, nl = lines.size();
//
//	//Clean out the variable set
//	V.clear();
//
//	for(i=0; i<nl; i++){
//		//For each line in the file.. each line is a unique record
//		line = split(lines.at(i), "\t", true);
//		varmodule = line.at(VMAP::VDOMAIN);	//Variable grouping (solarfield, heliostat...)
//		var = line.at(VMAP::STRING_NAME);	//Variable name
//		spvar *vdat = &V[varmodule][0][var];
//		vdat->varpath = varmodule + ".0." + var;
//
//		//Get the information for this variable
//		vdat->dattype = lower_case(line.at(VMAP::TYPE));
//		vdat->value = line.at(VMAP::VALUE);	//Default value
//		vdat->units = line.at(VMAP::UNITS);	//Units
//		vdat->range = line.at(VMAP::RANGE);	//valid range
//		vdat->is_param = line.at(VMAP::IS_PARAM) == "TRUE";	//is parameterizable?
//		control = line.at(VMAP::CONTROL);	
//		vdat->ctype = control;	//Control type
//		if(line.at(VMAP::SPECIAL).size()>0) vdat->choices = split( line.at(VMAP::SPECIAL), ";" ); 
//		vdat->is_disabled = lower_case(line.at(VMAP::UI_DISABLE)) == "x";
//		vdat->short_desc = line.at(VMAP::LABEL);	//Short description - variable label
//		vdat->long_desc = line.at(VMAP::DESCRIPTION);	//Long description
//
//		if(control=="combo"){
//			//create the options map.. <selection id -> string name>
//			for(int j=0; j<(int)vdat->choices.size(); j++){
//				vector<string> dat = split(vdat->choices.at(j), "=");
//				int vind;
//				to_integer(dat.at(1), &vind);
//				vdat->index_map[vind] = dat.at(0);
//			}
//		}
//
//		
//	}
//	
//}

#include <sstream>
template<typename T> static std::string my_to_string( T value )
{
	std::ostringstream os;
	os << value;
	return os.str();
}



bool ioutil::saveXMLInputFile(const string &fname, var_set &V, var_set &Defs, parametric &par_data, const string &version){

	ofstream fobj(fname.c_str());
	if(fobj.is_open())
	{

		//main data structure
		fobj << "<data>\n";
		string t1 = "\t";
		string t2 = "\t\t";
		string t3 = "\t\t\t";
		string t4 = "\t\t\t\t";
	
		//version
		//fobj.AddLine( wxString::Format("%s<version>%s</version>",t1, _software_version) );
		fobj << t1 << "<version>" << version << "</version>\n"; 
		//Add a header line with info on the last save time
		DTobj dt; dt.Now();
		fobj << t1 << "<header>Last saved " << dt._month << "-" << dt._mday << "-" << dt._year << " at " << dt._hour << ":" << dt._min << ":" << dt._sec << "</header>\n";
		
		//fobj.AddLine( wxString::Format("%s<header>Last saved %d-%d-%d at %d:%d:%d</header>", t1, dt._month, dt._mday, dt._year, dt._hour, dt._min, dt._sec) );
	
		//Write each variable
		string module, inst, varname, units;
		for(var_set::iterator it0 = V.begin(); it0 != V.end(); it0++){
			module = it0->first;
			for(map<int, var_map>::iterator it1 = it0->second.begin(); it1 != it0->second.end(); it1++){
				inst = my_to_string(it1->first);
				for(var_map::iterator it2 = it1->second.begin(); it2 != it1->second.end(); it2++){
					varname = it2->first;

					//fobj.AddLine( wxString::Format("%s<variable>",t1) );
					fobj << t1 << "<variable>\n";

					//fobj.AddLine( wxString::Format("%s<component>%s</component>",t2, module) );
					fobj << t2 << "<component>" << module << "</component>\n";
					//fobj.AddLine( wxString::Format("%s<instance>%s</instance>",t2, inst) );
					fobj << t2 << "<instance>" << inst << "</instance>\n";
					//fobj.AddLine( wxString::Format("%s<varname>%s</varname>",t2, varname) );
					fobj << t2 << "<varname>" << varname << "</varname>\n";
					//fobj.AddLine( wxString::Format("%s<units>%s</units>",t2, it2->second.units) );
					fobj << t2 << "<units>" << it2->second.units << "</units>\n";
					//fobj.AddLine( wxString::Format("%s<value>%s</value>",t2, it2->second.value.c_str()) );
					fobj << t2 << "<value>" << it2->second.value << "</value>\n";
				
					//fobj.AddLine( wxString::Format("%s</variable>",t1) );
					fobj << t1 << "</variable>\n";
				}
			}
		}

		//Write any parametric data
		if(par_data.size() > 0){
			//fobj.AddLine( wxString::Format("%s<parametric>",t1) );
			fobj << t1 << "<parametric>\n";
			for(int i=0; i<par_data.size(); i++){
				//fobj.AddLine( wxString::Format("%s<par_variable>",t2) );
				fobj << t2 << "<par_variable>\n";
				par_variable *pv = &par_data[i];

				//varname
				//fobj.AddLine( wxString::Format("%s<varname>%s</varname>",t3, pv->varname) );
				fobj << t3 << "<varname>" << pv->varname << "</varname>\n";
				//display text
				//fobj.AddLine( wxString::Format("%s<display_text>%s</display_text>",t3, pv->display_text) );
				fobj << t3 << "<display_text>" << pv->display_text << "</display_text>\n";
				//units
				//fobj.AddLine( wxString::Format("%s<units>%s</units>",t3, pv->units) );
				fobj << t3 << "<units>" << pv->units << "</units>\n";
				//data type
				//fobj.AddLine( wxString::Format("%s<data_type>%s</data_type>",t3, pv->data_type) );
				fobj << t3 << "<data_type>" << pv->data_type << "</data_type>\n";
				//linked
				//fobj.AddLine( wxString::Format("%s<linked>%s</linked>",t3, (pv->linked ? "true" : "false") ) );
				fobj << t3 << "<linked>" << (pv->linked ? "true" : "false") << "</linked>\n";
				//layout required
				//fobj.AddLine( wxString::Format("%s<layout_required>%s</layout_required>",t3, (pv->layout_required ? "true" : "false") ) );
				fobj << t3 << "<layout_required>" << (pv->layout_required ? "true" : "false") << "</layout_required>\n";

				//Selections
				//fobj.AddLine( wxString::Format("%s<selections>",t3) );
				fobj << t3 << "<selections>\n";
				for(int j=0; j<(int)pv->selections.size(); j++)
					//fobj.AddLine( wxString::Format("%s<selection>%s</selection>",t4, pv->selections[j] ) );
					fobj << t4 << "<selection>" << pv->selections[j] << "</selection>\n";
				//fobj.AddLine( wxString::Format("%s</selections>",t3) );
				fobj << t3 << "</selections>\n";

				//choices
				//fobj.AddLine( wxString::Format("%s<choices>",t3) );
				fobj << t3 << "<choices>\n";
				for(int j=0; j<(int)pv->choices.size(); j++)
					//fobj.AddLine( wxString::Format("%s<choice>%s</choice>",t4, pv->choices[j] ) );
					fobj << t4 << "<choice>" << pv->choices[j] << "</choice>\n";
				//fobj.AddLine( wxString::Format("%s</choices>",t3) );
				fobj << t3 << "</choices>\n";

				//sim_values
				//fobj.AddLine( wxString::Format("%s<sim_values>",t3) );
				fobj << t3 << "<sim_values>\n";
				for(int j=0; j<(int)pv->sim_values.size(); j++)
					//fobj.AddLine( wxString::Format("%s<sim_value>%s</sim_value>",t4, pv->sim_values[j] ) );
					fobj << t4 << "<sim_value>" << pv->sim_values[j] << "</sim_value>\n";
				//fobj.AddLine( wxString::Format("%s</sim_values>",t3) );
				fobj << t3 << "</sim_values>\n";

				//fobj.AddLine( wxString::Format("%s</par_variable>",t2) );
				fobj << t2 << "</par_variable>\n";
			}
			//fobj.AddLine( wxString::Format("%s</parametric>",t1) );
			fobj << t1 << "</parametric>\n";
		}
		//fobj.AddLine("</data>");
		fobj << "</data>\n";

		//fobj.Write();
		//fobj.Close();
		fobj.close();
		//--------------
		return true;
	}
	else{
		return false;
	}


}

string ioutil::getDelimiter(std::string &text){
	if( text == "") return ",";
	//Find the type of delimiter
	vector<string> delims;
	delims.push_back(",");
	delims.push_back(" ");
	delims.push_back("\t");
	delims.push_back(";");
	string delim = "\t";	//initialize
	int ns=0;
	for(int i=0; i<4; i++){
		vector<string> data = split(text, delims[i]);
		if((int)data.size()>ns){ delim = delims[i]; ns = data.size(); }	//pick the delimiter that returns the most entries
	}
	return delim;
}
