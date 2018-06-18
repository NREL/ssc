#ifndef __wind_obos_defaults_h
#define __wind_obos_defaults_h

#include <vector>
#include <string>

enum {INPUT, OUTPUT};


class variable {
 public:
  int inout;
  std::string name;
  std::string description;
  std::string units_openmdao;
  std::string units_sam;
  double value;
  std::string valueStr;
  std::string constraints;
  
  variable(std::vector<std::string> indata);
  bool isInput();
  bool isOutput();
  bool isDouble();
};

class wind_obos_defaults {
 public:
  std::vector<variable> variables;
  wind_obos_defaults();
};

#endif
