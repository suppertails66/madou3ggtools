#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TStringConversion.h"
#include "util/TGraphic.h"
#include "util/TPngConversion.h"
#include "util/TThingyTable.h"
#include "util/TParse.h"
#include "util/TOpt.h"
#include "util/TFileManip.h"
#include "sms/SmsPattern.h"
#include "exception/TGenericException.h"
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>

using namespace std;
using namespace BlackT;
using namespace Sms;

TBufStream patchifs(1);
TGraphic grp;
TBufStream ofs(1);

int main(int argc, char* argv[]) {
  if (argc < 5) {
    cout << "Game Gear raw graphic patcher" << endl;
    cout << "Usage: " << argv[0]
      << " <ingraphic> <structfile> <srcfile> <srcoffset> <outfile>" << endl;
    cout << "Options:" << endl;
    cout << "  p    Specify palette file" << endl;
    
    return 0;
  }

  TPngConversion::RGBAPngToGraphic(string(argv[1]), grp);
  patchifs.open(argv[2]);
  
  string infile = string(argv[3]);
  if (TFileManip::fileExists(infile))
    ofs.open(infile.c_str());
  else
    // create new file
    ofs = TBufStream(0x10000);
  
  int srcoffset = TStringConversion::stringToInt(string(argv[4]));
  
  SmsPalette* palptr = NULL;
  char* palettename = TOpt::getOpt(argc, argv, "-p");
  SmsPalette pal;
  bool colorsUsed[16];
  bool colorsAvailable[16];
  if (palettename != NULL) {
//    TIfstream palifs(argv[4], ios_base::binary);
    TIfstream palifs(palettename, ios_base::binary);
    pal.readGG(palifs);
    palptr = &pal;
    for (int i = 0; i < 16; i++) {
      colorsUsed[i] = true;
      colorsAvailable[i] = true;
    }
  }
  
  int w = (grp.w() / SmsPattern::w);
  int h = (grp.h() / SmsPattern::h);
  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
      int patternNum = TParse::matchInt(patchifs);
      if (patternNum == -1) continue;
      
      int x = (i * SmsPattern::w);
      int y = (j * SmsPattern::h);
      
      SmsPattern pattern;
//      pattern.fromGrayscaleGraphic(grp, x, y);
    
      if (palptr != NULL) {
        pattern.approximateGraphic(grp, *palptr, colorsUsed, colorsAvailable,
                                   x, y, false, true, true);
      }
      else {
        pattern.fromGrayscaleGraphic(grp, x, y);
      }
      
      ofs.seek(srcoffset + (patternNum * SmsPattern::size));
      pattern.write(ofs);
    }
  }
  
  ofs.save(argv[5]);
  
  return 0;
}
