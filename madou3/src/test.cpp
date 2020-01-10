#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TGraphic.h"
#include "util/TStringConversion.h"
#include "util/TPngConversion.h"
#include "util/TCsv.h"
#include "util/TSoundFile.h"
#include <vector>
#include <string>
#include <iostream>

using namespace std;
using namespace BlackT;
//using namespace Sms;

string as2bHex(int num) {
  string str = TStringConversion::intToString(num,
                  TStringConversion::baseHex).substr(2, string::npos);
  while (str.size() < 2) str = string("0") + str;
  
//  return "<$" + str + ">";
  return str;
}

string as2bHexPrefix(int num) {
  return "$" + as2bHex(num) + "";
}

int main(int argc, char* argv[]) {
  TIfstream ifs("lastbible.gg", ios_base::binary);
  
/*  for (int i = 0; i < 0x1c/2; i++) {
    ifs.seek(0x867F + (i * 2));
    int addr = ifs.readu16le();
    
    ifs.seek(addr);
    cout << "window type " << i << endl;
    cout << "  w: " << ifs.readu8() << endl;
    cout << "  h: " << ifs.readu8() << endl;
    cout << "  x: " << ifs.readu8() << endl;
    cout << "  y: " << ifs.readu8() << endl;
  } */
  
  vector<int> tilemaps;
  
  ifs.seek(0x196B3);
  for (int i = 0; i < 26; i++) {
    cout << ";=====" << endl;
    cout << "; entry " << i << endl;
    cout << ";=====" << endl;
    
    // ?
    cout << "  ; ?" << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8());
    cout << "," << as2bHexPrefix(ifs.readu8()) << endl;
    
    // explosion animation
    cout << "  ; explosion" << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8()) << endl;
    cout << "  .dw " << as2bHexPrefix(ifs.readu16le()) << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8()) << endl;
    cout << "  ; explosion x/y" << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8());
    cout << "," << as2bHexPrefix(ifs.readu8()) << endl;
    
    // tilemap send
    cout << "  ; tilemap" << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8()) << endl;
    tilemaps.push_back(ifs.readu16le());
    cout << "  .dw introTilemapIdentifier" << as2bHex(i) << endl;
      
    // delay
    cout << "  ; delay" << endl;
    cout << "  .db " << as2bHexPrefix(ifs.readu8());
    cout << "," << as2bHexPrefix(ifs.readu8());
    cout << "," << as2bHexPrefix(ifs.readu8()) << endl;
  }
  
  for (int i = 0; i < tilemaps.size(); i++) {
    cout << ".define introTilemapIdentifier" << as2bHex(i);
    cout << " " << as2bHexPrefix(tilemaps[i]) << endl;
  }
  
  return 0;
}
