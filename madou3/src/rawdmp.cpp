#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TGraphic.h"
#include "util/TStringConversion.h"
#include "util/TPngConversion.h"
#include "util/TCsv.h"
#include "util/TSoundFile.h"
#include <string>
#include <iostream>

using namespace std;
using namespace BlackT;
//using namespace Sms;

int main(int argc, char* argv[]) {
  if (argc < 5) {
    cout << "Raw data dumper" << endl;
    cout << "Usage: " << argv[0] << " <infile> <outfile> <offset> <datasize>"
      << endl;
    return 0;
  }
  
  string infile = string(argv[1]);
  string outfile = string(argv[2]);
  int offset = TStringConversion::stringToInt(string(argv[3]));
  int datasize = TStringConversion::stringToInt(string(argv[4]));
  
  TIfstream ifs(infile.c_str(), ios_base::binary);
  ifs.seek(offset);
  
  TOfstream ofs(outfile.c_str(), ios_base::binary);
  for (int i = 0; i < datasize; i++) {
    ofs.put(ifs.get());
  }
  
  return 0;
}
