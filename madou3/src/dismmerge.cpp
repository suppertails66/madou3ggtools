#include "util/TStringConversion.h"
#include <string>
#include <iostream>
#include <fstream>

using namespace std;
using namespace BlackT;

bool isAllSpace(const string& str) {
  for (int i = 0; i < str.size(); i++) {
    if (isspace(str[i])) return false;
  }
  
  return true;
}

string stripLeadingWhitespace(const string& str) {
  int i = 0;  
  
  if (str.size() > 0) {
//    if (str[0] == '+') return string("");
    if (str[0] == '+') ++i;
    else if (str[0] == '-') ++i;
    else if (str[0] == '*') ++i;
    else if (str[0] == 'Z') ++i;
  }

  for ( ; i < str.size(); i++) {
    if (!isspace(str[i])) return str.substr(i, string::npos);
  }
  
  return string("");
}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Disassembly comment merger" << endl;
    cout << "Usage: " << argv[0] << " <newdism> <olddism>" << endl;
    return 0;
  }
  
  ifstream ifs1(argv[1]);
  ifstream ifs2(argv[2]);
  
  // merge whitespace and comment lines from content1 into content2
  while (ifs1.good() && ifs2.good()) {
    ifs1.get();
    if (!ifs1.good()) break;
    ifs1.unget();
  
    ifs2.get();
    if (!ifs2.good()) break;
    ifs2.unget();
    
    string content1;
    string content2;
    
    getline(ifs1, content1);
    getline(ifs2, content2);
    string stripped = stripLeadingWhitespace(content2);
    
    while (isAllSpace(content2)
            || (content2.size() == 0)
            || ((stripped.size() > 0)
                && (stripped[0] == ';'))) {
      cout << content2 << endl;
      getline(ifs2, content2);
      stripped = stripLeadingWhitespace(content2);
    }
  
    cout << content1 << endl;
    
    int linenum1 = TStringConversion::stringToInt(
      string("0x") + content1.substr(2, 8));
    int linenum2 = TStringConversion::stringToInt(
      string("0x") + stripped.substr(0, 6));
    
    while ((content1.size() > 0)
        && (content1[0] == '+')) {
      if (!ifs1.good()) break;
      getline(ifs1, content1);
      
      if ((content1.size() <= 0)
        || (content1[0] != '+')) {
        linenum1 = TStringConversion::stringToInt(
          string("0x") + content1.substr(2, 8));
        stripped = stripLeadingWhitespace(content1);
        break;
      }
      
      cout << content1 << endl;
      stripped = stripLeadingWhitespace(content1);
      linenum1 = TStringConversion::stringToInt(
        string("0x") + content1.substr(2, 8));
    }
  
    cerr << hex << linenum1 << " " << linenum2 << endl;
  
    // skip lines in old until we catch up with new
    while (linenum2 < linenum1) {
      if (!ifs2.good()) break;
      getline(ifs2, content2);
      stripped = stripLeadingWhitespace(content2);
      linenum2 = TStringConversion::stringToInt(
        string("0x") + stripped.substr(0, 6));
    }
  
    // print extra lines in new until we catch up with old
    while (linenum1 < linenum2) {
      if (!ifs1.good()) break;
      getline(ifs1, content1);
      cout << content1 << endl;
      stripped = stripLeadingWhitespace(content1);
      linenum1 = TStringConversion::stringToInt(
        string("0x") + content1.substr(0, 6));
    }
  }
  
}

