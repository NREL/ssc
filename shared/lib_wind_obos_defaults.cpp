#include "lib_wind_obos_defaults.h"

#include <stdexcept>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>

using namespace std;

double dnull = -99999.0;


// Helper for string parsing
string trim(const string& str, const string& whitespace = "# \t") {
    const auto strBegin = str.find_first_not_of(whitespace);
    if (strBegin == std::string::npos) return ""; // no content
    const auto strEnd   = str.find_last_not_of(whitespace);
    const auto strRange = strEnd - strBegin + 1;
    return str.substr(strBegin, strRange);
}


// Class to read csv file by row, parse into vector
// https://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c
class CSVRow {
public:
  string const& operator[](size_t index) const {return m_data[index];}
  size_t size() const {return m_data.size();}
  
  void readNextRow(istream& str) {
    string line, cell;
    getline(str, line);

    // Truncate line after comment character
    string::size_type n = line.find("#");
    if (n != string::npos) line.erase(n);

    stringstream lineStream(line);

    m_data.clear();
    while(getline(lineStream, cell, ',')) {
      // Cleanup entry: remove leading and trailing spaces
      string myString = trim(cell);
      // For data field, use all caps
      if (m_data.size() >= 4)
	std::transform(myString.begin(), myString.end(), myString.begin(), ::toupper);

      // Append to output vector
      m_data.push_back(myString);
    }

    // This checks for a trailing comma with no data after it.
    if (!lineStream && cell.empty()) m_data.push_back("");
  }

  vector<string> get() {return m_data;}
private:
  vector<string> m_data;
};
istream& operator>>(istream& str, CSVRow& data) {data.readNextRow(str); return str;}




variable::variable(vector<string> indata) {
  inout = indata[0]=="INPUT" ? INPUT : OUTPUT;
  name  = indata[1];
  description = indata[2];
  units_openmdao = indata[3];
  units_sam = indata[4];
  valueStr = indata[5];
  try {
    value = stod( indata[5] );
  } catch(exception& e) {
    value = dnull;
  }
  // SAM constraint arguments need a comma sometimes, but this confuses csv-file parser, so we've substituted '_' instead
  constraints = regex_replace(indata[6],regex("_"),",");
}

bool variable::isInput() {return (inout==INPUT);}
bool variable::isOutput() {return (inout==OUTPUT);}
bool variable::isDouble() {return (value!=dnull);}




// Default constructor loads values from text file
wind_obos_defaults::wind_obos_defaults() {

  // File with variable defaults listed
  ifstream infile("wind_obos_defaults.csv"); //.dat

  // Read in row by row
  CSVRow row;
  while (infile >> row) {
    // Check for empty or improper lines
    if (row.size() != 7) continue;

    variables.push_back( variable(row.get()) );
  }
}
